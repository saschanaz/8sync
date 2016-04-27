;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of 8sync.
;;;
;;; 8sync is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; 8sync is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with 8sync.  If not, see <http://www.gnu.org/licenses/>.

;;; =====================================================================
;;; Robot Scanner test demo (from XUDD, originally).
;;; 
;;; Here's the premise.  There's a warehouse full of droids, some
;;; infected, and some not.  The SecurityRobot is being sent in to clean
;;; up the mess.  It's capable of sending a message that infected droids
;;; are susceptible to responding in a predictable way.  Once it has
;;; identified that a droid is infected, it shoots it full of holes till
;;; the droid is terminated.  The SecurityRobot goes from room to room
;;; till things are cleared out.
;;; 
;;; Overseeing the operation is the "overseer".  The security robot keeps
;;; the overseer up to date on its progress as it goes.  (For this demo,
;;; the overseer is also responsible for initializing the world and
;;; reporting info back to the user.)
;;; =====================================================================

(use-modules (8sync systems actors)
             (oop goops)
             (ice-9 match))

(set! *random-state* (random-state-from-platform))

(define room-structure
  ;; A list of (clean-droids infected-droids)
  '((3 1)
    (0 2)
    (8 5)
    (5 0)
    (2 1)))

(define-simple-actor <overseer>
  (init-world
   (lambda (actor message)
     ;; Porting mostly straight up from super-imperative XUDD code.
     (define previous-room #f)
     (define first-room #f)

     ;; Set up all rooms
     (for-each
      (match-lambda
        ((clean-droids infected-droids)
         ;; Create this room
         (define room (create-actor* actor <warehouse-room> "room"))
         (define* (init-droid #:key infected)
           (define droid (create-actor* actor <droid> "droid"
                                        #:infected infected
                                        #:room room))
           (<-wait actor droid 'register-with-room))

         ;; Link rooms.
         ;; Couldn't this just be folded into the warehouse room init?
         ;; I guess it stress tests more the message sending process
         (when previous-room
           (<- actor previous-room 'set-next-room
               #:id room)
           (<- actor room 'set-previous-room
               #:id previous-room))

         ;; Set up clean droids in the room
         (for-each
          (lambda _
            (init-droid #:infected #f))
          (iota clean-droids))

         ;; Set up infected droids in the room
         (for-each
          (lambda _
            (init-droid #:infected #t))
          (iota clean-droids))

         (set! previous-room room)
         (if (not first-room)
             (set! first-room room))))
      room-structure)

     ;; Add security robot
     (let ((security-robot
            (create-actor actor <security-robot>)))
       (<- actor security-robot 'begin-mission
           #:starting-room first-room
           #:overseer (actor-id actor)))))

  (transmission
   (lambda (actor message)
     (display (message-ref message 'message))
     (newline))))


;;; A room full of robots.
(define-class <warehouse-room> (<actor>)
  (droids #:init-value '())
  (next-room #:init-value #f)
  (previous-room #:init-value #f)

  (message-handler
   #:init-value
   (make-action-dispatch
    ((set-next-room actor message)
     "Set the room following this"
     (slot-set! actor 'next-room
                (message-ref message 'id)))

    ((set-previous-room actor message)
     "Set the room previous to this"
     (slot-set! actor 'previous-room
                (message-ref message 'id)))

    ((get-next-room actor message)
     "Return a reference to the link following this"
     (<-reply actor message
                    #:id (slot-ref actor 'next-room)))

    ((get-previous-room actor message)
     "Return a reference to the link preceding this"
     (<-reply actor message
                    #:id (slot-ref actor 'previous-room)))

    ((list-droids actor message)
     "Return a list of all the droid ids we know of in this room"
     (<-reply actor message
                    #:droid-ids (slot-ref actor 'droids)))

    ((register-droid actor message)
     "Register a droid as being in this room"
     (slot-set! actor 'droids
                (cons (message-ref message 'droid-id)
                      (slot-ref actor 'droids)))))))


;;; A droid that may or may not be infected!
;;; What will happen?  Stay tuned!
(define-class <droid> (<actor>)
  (infected #:init-keyword #:infected)
  (room #:init-keyword #:room)
  (hp #:init-value 50)

  (message-handler
   #:init-value
   (make-action-dispatch
    ((register-with-room actor message)
     "Register ourselves as being in a room"
     (let ((room-id (slot-ref actor 'room)))
       (<-wait actor room-id
                          'register-droid
                          #:droid-id (actor-id actor))
       (format #t "Droid ~a registered with room ~a\n"
               (actor-id-actor actor)
               (address-actor-id room-id))))

    ((infection-expose actor message)
     "Leak whether or not we're infected to a security droid"
     (<-reply actor message
                    #:is-infected #t))

    ((get-shot actor message)
     "Get shot by bullets"
     (let* ((damage (random 60))
            (new-hp (- (slot-ref actor 'hp) damage))
            (alive (> new-hp 0)))
       ;; Set our health to the new value
       (slot-set! actor 'hp new-hp)
       (<-reply actor message
                      #:hp-left new-hp
                      #:damage-taken damage
                      #:alive alive)
       (when (not alive)
         (format #t "~a: *Kaboom!*\n" (actor-id-actor actor))
         (self-destruct actor)))))))


(define (droid-status-format shot-response)
  (if (message-ref shot-response 'alive)
      (format #f "Droid ~a shot; taken ~a damage. Still alive... ~a hp left."
              (address-actor-id (message-from shot-response))
              (message-ref shot-response 'damage-taken)
              (message-ref shot-response 'hp-left))
      (format #f "Droid ~a shot; taken ~a damage. Terminated."
              (address-actor-id (message-from shot-response))
              (message-ref shot-response 'damage-taken))))


;;; Security robot... designed to seek out and destroy infected droids.
(define-simple-actor <security-robot>
  ((begin-mission actor message)
   ;; used to track the current room / if any rooms are remaining
   (define room (message-ref message 'starting-room))
   (define overseer (message-ref message 'overseer))

   ;; Walk through all rooms, clearing out infected droids
   ;; Continue this whil there's still another room to investigate.
   (define response)
   (while room
     (<- actor overseer
         'transmission
         #:message (format #f "Entering room ~a..."
                           (address-actor-id room)))

     ;; Find all droids in this room and exterminate the infected ones.
     (set! response (<-wait actor room 'list-droids))
     (for-each
      (lambda (droid-id)
        (cond
         ;; Looks like it's infected
         ((message-ref
           (<-wait actor droid-id
                              'infection-expose)
           'is-infected)
          ;; Inform that it's infected
          (<- actor overseer
              'transmission
              #:message
              (format #f "~a found to be infected... taking out"
                      (address-actor-id droid-id)))

          ;; Keep firing till it's dead.
          (let ((still-alive #t))
            (while still-alive
              (let ((response
                     (<-wait actor droid-id 'get-shot)))
                (<- actor overseer 'transmission
                    #:message (droid-status-format response))
                (set! still-alive (message-ref response 'alive))))))

         ;; Not infected... inform and go to the next one
         (else
          (<- actor overseer 'transmission
              #:message
              (format #f "~a is clean... moving on."
                      (address-actor-id droid-id))))))
      (message-ref response 'droid-ids))

     ;; Switch to next room, if there is one.
     (set! room (message-ref
                 (<-wait actor room 'get-next-room)
                 'id)))

   ;; Good job everyone!  Shut down the operation.
   (<- actor overseer 'transmission
       #:message "Mission accomplished.")))

(define (main . args)
  (define hive (make-hive))
  (define overseer (hive-create-actor hive <overseer>))
  (define initial-messages
    (list (bootstrap-message hive overseer 'init-world)))
  (ez-run-hive hive initial-messages))
