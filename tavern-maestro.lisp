;;;; The Rusty Barrel: A Modular Tavern Symphony
;;;; Where Code and Consciousness Converge in Digital Dramaturgy

;;; ========================================
;;; Package Definitions - Our Cosmic Containers
;;; ========================================
;;;; tavern-maestro.lisp - Where Reality's Symphony Finds Its Conductor
;;;; Now with Architectural Alchemy!

(defpackage :rusty-barrel.main
  (:use :cl 
        :rusty-barrel.state 
        :rusty-barrel.characters
        :rusty-barrel.dialogue
        :rusty-barrel.atmosphere
        :rusty-barrel.events
        :rusty-barrel.techtonica)  ; <- Behold! Our new architectural ally!
  (:export :start-simulation
           :render-tavern-reality))

(in-package :rusty-barrel.main)

;; The Grand Integration - Where Dreams Become Digital Stone
(defun render-tavern-reality ()
  "Crystallize our tavern's essence into manifest form"
  (let ((dimensional-dream (gather-reality-fragments))
        (architectural-allegory (design-tavern-topology)))
    (with-reality-compression (*global-state*)
      ;; Dance of the Digital Dimensions
      (let ((compressed-reality 
             (compress-reality dimensional-dream))
            (manifest-structure 
             (manifest-metaphor architectural-allegory)))
        ;; The Grand Fusion - Where Poetry Meets Physics
        (merge-realities compressed-reality manifest-structure)))))

;; The Reality Gathering - Where Fragments Find Form
(defun gather-reality-fragments ()
  (vector
   (capture-state-essence *global-state*)
   (distill-character-dreams *tavern-characters*)
   (crystallize-ambient-atmosphere)
   (collect-event-echoes)))
