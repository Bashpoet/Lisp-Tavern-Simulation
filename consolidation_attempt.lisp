;;;; Rusty Barrel Tavern - A Literary Text Adventure
;;;; Consolidated implementation combining all components

(defpackage :rusty-barrel
  (:use :cl)
  (:export :start))

(in-package :rusty-barrel)

;;; Global State Management
(defparameter *game-state* (make-hash-table :test #'equal))
(defparameter *character-memories* (make-hash-table :test #'equal))

(defun get-state (key)
  (gethash key *game-state*))

(defun set-state (key value)
  (setf (gethash key *game-state*) value))

(defun remember-interaction (character event)
  (let ((memory (gethash character *character-memories*)))
    (unless memory
      (setf memory (make-hash-table :test #'equal))
      (setf (gethash character *character-memories*) memory))
    (setf (gethash event memory) 
          (1+ (or (gethash event memory) 0)))))

;;; Character System
(defstruct character
  name
  description
  dialogue-lines
  role
  mood
  background
  secrets)

;;; Dialogue System
(defstruct dialogue-node
  text
  responses
  condition)

;;; Reality Management System
(defun gather-reality-fragments ()
  (vector
   (capture-state-essence)
   (distill-character-dreams)
   (crystallize-ambient-atmosphere)
   (collect-event-echoes)))

(defun capture-state-essence ()
  (loop for key being the hash-keys of *game-state*
        collect (cons key (get-state key))))

(defun distill-character-dreams ()
  (mapcar #'character-essence *tavern-characters*))

(defun character-essence (char)
  (list (character-name char)
        (character-mood char)
        (gethash char *character-memories*)))

(defun crystallize-ambient-atmosphere ()
  (list (get-state 'time-of-day)
        (get-state 'weather)
        (get-state 'ambiance)))

(defun collect-event-echoes ()
  *recent-events*)

;;; Character Definitions
(defparameter *bartender*
  (make-character 
   :name "Old Tom"
   :description "A silver-haired man with laugh lines etched deep around keen eyes that miss nothing"
   :dialogue-lines '("Welcome to the Rusty Barrel, stranger. What brings you to our humble establishment?"
                    "Careful with that one's tales - they grow taller with each tankard"
                    "Aye, the finest ale this side of the mountain, if I do say so myself")
   :role 'bartender
   :mood 'friendly
   :background "Tom has owned the Rusty Barrel for three decades, inheriting it from his father"
   :secrets "Knows the location of a forgotten wine cellar beneath the tavern"))

(defparameter *bard*
  (make-character
   :name "Melody"
   :description "A lithe figure whose fingers dance across lute strings like autumn leaves in the wind"
   :dialogue-lines '("Would you fancy a tale of adventure, good folk?"
                    "This next song speaks of love lost beneath summer stars"
                    "Every coin in my cup brings forth another verse")
   :role 'musician
   :mood 'inspired
   :background "Trained at the College of Whispers, where she learned both music and secrets"
   :secrets "Carries coded messages in her songs for those who know how to listen"))

(defparameter *mysterious-stranger*
  (make-character
   :name "Hooded Figure"
   :description "A cloaked individual who keeps to the shadows, nursing a single drink all evening"
   :dialogue-lines '("*whispers* Some secrets are worth more than gold..."
                    "I might have information... for the right price"
                    "Watch the corners of your vision, friend. That's where truth hides")
   :role 'patron
   :mood 'secretive
   :background "Nobody knows their true identity"
   :secrets "Actually a noble in disguise, gathering information about a conspiracy"))

(defparameter *tavern-characters* (list *bartender* *bard* *mysterious-stranger*))
(defparameter *recent-events* nil)

;;; Atmosphere and Description System
(defun describe-atmosphere ()
  (let ((time-of-day (or (get-state 'time-of-day) 'evening)))
    (format t "~%~a~%"
            (case time-of-day
              (morning 
               "Golden sunlight streams through dusty windows, catching motes that dance above empty tables.")
              (afternoon 
               "The tavern hums with quiet activity as regulars seek refuge from the day's heat.")
              (evening 
               "Warm lamplight bathes the common room in amber, while shadows gather in corners like conspirators.")
              (night 
               "The hearth's dying embers cast a ruddy glow across the room as night's secrets are whispered over final cups.")))))

;;; Event System
(defparameter *tavern-events*
  '((bard-performance 
     "A wandering bard stands atop a table, testing their lute strings."
     . start-performance)
    (bar-fight
     "Tension crackles as two patrons exchange heated words."
     . handle-conflict)
    (mysterious-stranger
     "The door creaks open, admitting a cloaked figure who moves silently to a corner table."
     . introduce-stranger)))

(defun trigger-random-event ()
  (when (< (random 1.0) 0.2)
    (let* ((event (nth (random (length *tavern-events*)) *tavern-events*))
           (description (second event)))
      (push (cons (get-universal-time) event) *recent-events*)
      (format t "~%~a~%" description))))

;;; Interaction System
(defun create-conversation (character)
  (let ((memory (gethash character *character-memories*)))
    (cond
      ((null memory)
       (make-dialogue-node 
        :text "Well met, stranger. New faces are always welcome at the Rusty Barrel."
        :responses '(("Tell me about yourself" . character-background)
                    ("What's the word around town?" . tavern-rumors)
                    ("I'll have a drink" . order-drink))))
      ((> (gethash 'visits memory 0) 3)
       (make-dialogue-node
        :text "Ah, good to see you again! The usual?"
        :responses '(("Yes, please" . serve-usual)
                    ("Actually, I wanted to ask about something" . deep-conversation)
                    ("Not today, thanks" . friendly-goodbye)))))))

(defun speak (char)
  (let ((line (nth (random (length (character-dialogue-lines char))) 
                   (character-dialogue-lines char))))
    (format t "~%~a says: \"~a\"~%" 
            (character-name char) line)))

(defun display-character-list ()
  (format t "~%Available characters to interact with:~%")
  (loop for char in *tavern-characters*
        for i from 1
        do (format t "~d. ~a - ~a~%" 
                   i 
                   (character-name char)
                   (character-description char))))

;;; ASCII Art
(defun display-tavern-banner ()
  (format t "
    _____     The Rusty Barrel Tavern
   /     \\   
  /       \\     'Where tales flow as freely as the ale'
 /__________\\
 |  []  []  |
 |  []  []  |
 |__[]__[]__|~%~%"))

;;; Main Interface
(defun interact-with-character ()
  (display-character-list)
  (format t "~%Choose a character number (or press Enter to continue): ")
  (let ((choice (read-line)))
    (unless (string= choice "")
      (let ((num (parse-integer choice :junk-allowed t)))
        (when (and num (>= num 1) (<= num (length *tavern-characters*)))
          (let ((char (nth (1- num) *tavern-characters*)))
            (remember-interaction char 'visits)
            (speak char)))))))

(defun advance-time ()
  (let ((current-time (or (get-state 'time-of-day) 'morning)))
    (setf (get-state 'time-of-day)
          (case current-time
            (morning 'afternoon)
            (afternoon 'evening)
            (evening 'night)
            (night 'morning)))))

(defun tavern-simulator ()
  (display-tavern-banner)
  (format t "Welcome to the Rusty Barrel Tavern simulation!~%")
  (format t "Enter 'quit' at any time to exit.~%~%")
  (let ((interaction-count 0))
    (loop
      (describe-atmosphere)
      (interact-with-character)
      (incf interaction-count)
      (when (>= interaction-count 3)
        (setf interaction-count 0)
        (advance-time)
        (trigger-random-event))
      (format t "~%Press Enter to continue (or type 'quit' to exit): ")
      (let ((input (read-line)))
        (when (string-equal input "quit")
          (format t "~%Thank you for visiting the Rusty Barrel Tavern!~%")
          (return))))))

;;; Entry Point
(defun start ()
  (tavern-simulator))
