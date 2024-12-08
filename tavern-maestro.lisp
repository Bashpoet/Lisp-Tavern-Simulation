;;;; The Rusty Barrel: A Modular Tavern Symphony
;;;; Where Code and Consciousness Converge in Digital Dramaturgy

;;; ========================================
;;; Package Definitions - Our Cosmic Containers
;;; ========================================

(defpackage :rusty-barrel.state
  (:use :cl)
  (:export :*global-state*
           :get-state
           :set-state
           :with-state-transaction))

(defpackage :rusty-barrel.characters
  (:use :cl)
  (:export :character
           :make-character
           :character-name
           :character-mood))

(defpackage :rusty-barrel.dialogue
  (:use :cl)
  (:export :dialogue-node
           :create-conversation
           :process-response))

(defpackage :rusty-barrel.atmosphere
  (:use :cl)
  (:export :describe-scene
           :generate-ambient-description))

(defpackage :rusty-barrel.events
  (:use :cl)
  (:export :trigger-event
           :register-event-handler))

(defpackage :rusty-barrel.main
  (:use :cl :rusty-barrel.state 
           :rusty-barrel.characters
           :rusty-barrel.dialogue
           :rusty-barrel.atmosphere
           :rusty-barrel.events)
  (:export :start-simulation))

;;; ========================================
;;; State Management - The Cosmic Ledger
;;; ========================================

(in-package :rusty-barrel.state)

(defclass tavern-state ()
  ((time-of-day 
    :initform :evening
    :accessor time-of-day)
   (day-of-week
    :initform :moonday
    :accessor day-of-week)
   (ambient-events
    :initform nil
    :accessor ambient-events)
   (inventory
    :initform (make-hash-table)
    :accessor inventory)))

(defparameter *global-state* (make-instance 'tavern-state))

;;; ========================================
;;; Character System - The Soul Forge
;;; ========================================

(in-package :rusty-barrel.characters)

(defclass character ()
  ((name 
    :initarg :name
    :accessor character-name)
   (description
    :initarg :description
    :accessor character-description)
   (mood
    :initarg :mood
    :accessor character-mood)
   (memory
    :initform (make-hash-table)
    :accessor character-memory)
   (relationships
    :initform (make-hash-table)
    :accessor character-relationships)))

;;; ========================================
;;; Dialogue System - The Ethereal Exchange
;;; ========================================

(in-package :rusty-barrel.dialogue)

(defclass dialogue-node ()
  ((text
    :initarg :text
    :accessor node-text)
   (responses
    :initarg :responses
    :accessor node-responses)
   (conditions
    :initarg :conditions
    :accessor node-conditions)))

;;; ========================================
;;; Atmosphere Engine - The Reality Weaver
;;; ========================================

(in-package :rusty-barrel.atmosphere)

(defun generate-ambient-description (state)
  (concatenate 
   'string
   (describe-lighting (time-of-day state))
   (describe-atmosphere (ambient-events state))
   (describe-patrons (character-count state))))

;;; ========================================
;;; Event System - The Chaos Choreographer
;;; ========================================

(in-package :rusty-barrel.events)

(defclass event ()
  ((name
    :initarg :name
    :accessor event-name)
   (description
    :initarg :description
    :accessor event-description)
   (conditions
    :initarg :conditions
    :accessor event-conditions)
   (actions
    :initarg :actions
    :accessor event-actions)))

;;; ========================================
;;; Main Loop - The Grand Conductor
;;; ========================================

(in-package :rusty-barrel.main)

(defun simulation-loop ()
  (loop
    (update-state)
    (render-scene)
    (handle-input)
    (process-events)
    (when (check-exit-condition)
      (return))))
