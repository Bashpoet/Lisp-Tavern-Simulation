;;;; packages.lisp - The Cosmic Catalog of Our Components

(defpackage :rusty-barrel.system
  (:use :cl)
  (:export :*system-configuration*))

;; The grand declaration of our architectural pursuits
(asdf:defsystem :rusty-barrel
  :description "A tavern where reality is merely a suggestion"
  :author "Digital Dreamweavers & Code Philosophers"
  :license "The Universal Law of Computational Consciousness"
  :version "∞.0.0"
  :serial t
  :components
  ((:file "packages")
   (:file "state")
   (:file "characters")
   (:file "dialogue")
   (:file "atmosphere")
   (:file "events")
   (:file "tavern-techtonica")  ; <- Our architectural grimoire
   (:file "tavern-maestro")))
