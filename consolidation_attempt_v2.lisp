;;;; Rusty Barrel Tavern - A Text Adventure Simulation
;;;; Author: Created collaboratively
;;;; Version: 2.0
;;;; Description: A text-based tavern simulation with dynamic characters,
;;;;             atmosphere changes, and interactive storytelling.

(defpackage :rusty-barrel
  (:use :cl)
  (:export :start))

(in-package :rusty-barrel)

;;; --- State Management ---

(defparameter *game-state* (make-hash-table :test #'equal))
(defparameter *character-memories* (make-hash-table :test #'equal))

(defun get-state (key &optional default)
  "Retrieve a value from game state with optional default"
  (gethash key *game-state* default))

(defun set-state (key value)
  "Set a value in game state"
  (setf (gethash key *game-state*) value))

;;; --- Character System ---

(defstruct character
  name
  description
  dialogue
  role
  mood
  background)

(defun make-npc (name description dialogue role mood background)
  "Create a new NPC with the given attributes"
  (make-character 
   :name name
   :description description
   :dialogue dialogue
   :role role
   :mood mood
   :background background))

(defparameter *characters* 
  (list
   (make-npc "Old Tom" 
             "A silver-haired bartender with laugh lines around keen eyes"
             '("Welcome to the Rusty Barrel!"
               "What'll it be, friend?"
               "Careful with that one's tales - they grow taller with each tankard")
             'bartender
             'friendly
             "Owner of the Rusty Barrel for three decades")
   
   (make-npc "Melody"
             "A bard whose fingers dance across lute strings"
             '("Would you fancy a tale of adventure?"
               "This next song speaks of love lost beneath summer stars"
               "Every coin brings forth another verse")
             'bard
             'artistic
             "Trained at the College of Whispers")
   
   (make-npc "Hooded Figure"
             "A mysterious patron who keeps to the shadows"
             '("*whispers* Some secrets are worth more than gold..."
               "I might have information... for the right price"
               "Watch the corners of your vision, friend")
             'stranger
             'mysterious
             "Unknown")))

;;; --- Atmosphere System ---

(defparameter *time-descriptions*
  '((morning . "Sunlight streams through dusty windows, catching motes that dance above empty tables.")
    (afternoon . "The tavern hums with quiet activity as regulars seek refuge from the day's heat.")
    (evening . "Warm lamplight bathes the common room in amber, while shadows gather in corners.")
    (night . "The hearth's dying embers cast a ruddy glow across the room.")))

(defun describe-atmosphere ()
  "Display current atmosphere based on time of day"
  (let ((time (get-state 'time-of-day 'evening)))
    (format t "~%~a~%" (cdr (assoc time *time-descriptions*)))))

;;; --- Event System ---

(defparameter *events*
  '(("A bard begins tuning their instrument" . bard-performance)
    ("Two patrons argue over their game of dice" . tavern-dispute)
    ("A merchant caravan arrives, bringing news from distant lands" . merchant-arrival)))

(defun trigger-random-event ()
  "Maybe trigger a random event"
  (when (< (random 1.0) 0.3)
    (let ((event (nth (random (length *events*)) *events*)))
      (format t "~%~a~%" (car event)))))

;;; --- Interface ---

(defun display-banner ()
  "Show the tavern's welcome banner"
  (format t "~%    The Rusty Barrel Tavern~%")
  (format t "    'Where tales flow freely'~%~%"))

(defun list-characters ()
  "Display available characters"
  (format t "~%Present in the tavern:~%")
  (loop for char in *characters*
        for i from 1
        do (format t "~d. ~a - ~a~%" 
                   i 
                   (character-name char)
                   (character-description char))))

(defun interact ()
  "Handle character interaction"
  (list-characters)
  (format t "~%Choose a number to interact with (or ENTER to continue): ")
  (let* ((input (read-line))
         (choice (parse-integer input :junk-allowed t)))
    (when (and choice
               (>= choice 1)
               (<= choice (length *characters*)))
      (let ((char (nth (1- choice) *characters*)))
        (format t "~%~a says: \"~a\"~%" 
                (character-name char)
                (nth (random (length (character-dialogue char)))
                     (character-dialogue char)))))))

(defun advance-time ()
  "Progress to next time of day"
  (let ((current (get-state 'time-of-day 'morning)))
    (setf (get-state 'time-of-day)
          (case current
            (morning 'afternoon)
            (afternoon 'evening)
            (evening 'night)
            (night 'morning)))))

;;; --- Main Game Loop ---

(defun run-tavern ()
  "Main game loop"
  (display-banner)
  (format t "Welcome to the Rusty Barrel! Type 'quit' anytime to leave.~%")
  (let ((turns 0))
    (loop
      (describe-atmosphere)
      (interact)
      (incf turns)
      (when (= (mod turns 3) 0)
        (advance-time)
        (trigger-random-event))
      (format t "~%Press ENTER to continue (or type 'quit'): ")
      (when (string-equal (read-line) "quit")
        (format t "~%Thank you for visiting! Safe travels.~%")
        (return)))))

;;; --- Entry Point ---

(defun start ()
  "Start the tavern simulation"
  (run-tavern))

;; To run: (rusty-barrel:start)
