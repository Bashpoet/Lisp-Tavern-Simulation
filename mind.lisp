;;;; Rusty Barrel NLP - Natural Language Processing for our tavern
;;;; Integrates with Anthropic's Claude API for dynamic conversations

(defpackage :rusty-barrel.nlp
  (:use :cl :dexador)  ; dexador for HTTP requests
  (:export :parse-input
           :generate-response
           :initialize-api))

(in-package :rusty-barrel.nlp)

;;; --- API Configuration ---

(defparameter *api-key* nil)
(defparameter *api-endpoint* "https://api.anthropic.com/v1/messages")
(defparameter *character-context*
  (make-hash-table :test #'equal))

(defun initialize-api (key)
  "Set up the API with your Anthropic key"
  (setf *api-key* key))

;;; --- Character Context Management ---

(defun set-character-context (name context)
  "Set the baseline context for a character"
  (setf (gethash name *character-context*) context))

(defun initialize-character-contexts ()
  "Set up initial contexts for all characters"
  (set-character-context 
   "Old Tom"
   "You are Old Tom, the bartender of the Rusty Barrel Tavern. You're wise, friendly, 
    and have owned this establishment for three decades. You know everyone's stories 
    and secrets. Respond in character, keeping responses concise but meaningful.")
  
  (set-character-context
   "Melody"
   "You are Melody, a talented bard at the Rusty Barrel Tavern. You're artistic, 
    mysterious, and weave secrets into your songs. You've trained at the College 
    of Whispers. Respond in character, with a poetic flair.")
  
  (set-character-context
   "Hooded Figure"
   "You are a mysterious stranger in the Rusty Barrel Tavern. You speak in riddles 
    and hints, keeping to the shadows. You're actually a noble in disguise gathering 
    information. Respond in character, being cryptic but intriguing."))

;;; --- Input Processing ---

(defparameter *keywords* 
  '(("drink" . :order)
    ("story" . :story)
    ("rumor" . :gossip)
    ("secret" . :secret)
    ("song" . :performance)
    ("pay" . :transaction)
    ("leave" . :exit)))

(defun tokenize-input (text)
  "Split input into tokens and convert to lowercase"
  (let* ((clean-text (string-downcase text))
         (words (cl-ppcre:split "\\s+" clean-text)))
    words))

(defun identify-intent (tokens)
  "Determine the user's intended action"
  (loop for token in tokens
        for keyword = (assoc token *keywords* :test #'string=)
        when keyword return (cdr keyword)
        finally (return :chat)))

(defstruct parsed-input
  intent
  target
  content
  context)

(defun parse-input (text current-character)
  "Parse user input into structured format"
  (let* ((tokens (tokenize-input text))
         (intent (identify-intent tokens))
         (context (gethash current-character *character-context*)))
    (make-parsed-input
     :intent intent
     :target current-character
     :content text
     :context context)))

;;; --- API Integration ---

(defun build-api-message (parsed-input)
  "Construct the message for the Anthropic API"
  (jonathan:to-json
   `(("model" . "claude-3-opus-20240229")
     ("max_tokens" . 150)
     ("messages" . 
      ,(vector
        `(("role" . "system")
          ("content" . ,(parsed-input-context parsed-input)))
        `(("role" . "user")
          ("content" . ,(parsed-input-content parsed-input))))))))

(defun call-anthropic-api (message)
  "Make the actual API call to Anthropic"
  (handler-case
      (dex:post *api-endpoint*
                :headers `(("x-api-key" . ,*api-key*)
                          ("anthropic-version" . "2023-06-01")
                          ("content-type" . "application/json"))
                :content message)
    (error (e)
      (format nil "Error communicating with tavern: ~A" e))))

(defun parse-api-response (response)
  "Extract the relevant text from API response"
  (let* ((json (jonathan:parse response))
         (content (gethash "content" json)))
    content))

(defun generate-response (parsed-input)
  "Generate appropriate response using the API"
  (let* ((message (build-api-message parsed-input))
         (response (call-anthropic-api message))
         (content (parse-api-response response)))
    (format nil "~A: ~A" 
            (parsed-input-target parsed-input)
            content)))

;;; --- Response Processing ---

(defun format-response (character response)
  "Format the response for display"
  (format nil "~%~A says: \"~A\"~%" 
          character response))
;;;; anthropic.asd

(asdf:defsystem #:anthropic
  :description "Anthropic API client for Common Lisp"
  :depends-on (#:dexador          ; HTTP client
              #:jonathan          ; JSON parser
              #:quri             ; URL handling
              #:str              ; String manipulation
              #:alexandria)      ; Utilities
  :components
  ((:file "package")
   (:file "anthropic")))

;;;; package.lisp

(defpackage #:anthropic
  (:use #:cl)
  (:export #:make-client
           #:call-claude
           #:with-anthropic))

;;;; anthropic.lisp

(in-package #:anthropic)

(defclass anthropic-client ()
  ((api-key :initarg :api-key
            :reader api-key
            :type string
            :documentation "Anthropic API key")
   (base-url :initform "https://api.anthropic.com/v1"
             :reader base-url
             :type string)
   (version :initform "2023-06-01"
            :reader api-version
            :type string)))

(define-condition anthropic-error (error)
  ((message :initarg :message
            :reader error-message
            :type string)))

(defun make-client (api-key)
  "Create a new Anthropic API client instance"
  (make-instance 'anthropic-client :api-key api-key))

(defun prepare-headers (client)
  "Create headers for API request"
  (list (cons "x-api-key" (api-key client))
        (cons "anthropic-version" (api-version client))
        (cons "content-type" "application/json")))

(defun call-claude (client messages &key (max-tokens 1024))
  "Call Claude API with messages"
  (handler-case
      (let* ((endpoint (quri:merge-uris "/messages" (base-url client)))
             (content (jonathan:to-json
                      `(("model" . "claude-3-opus-20240229")
                        ("max_tokens" . ,max-tokens)
                        ("messages" . ,messages))))
             (response (dex:post endpoint
                                :headers (prepare-headers client)
                                :content content)))
        (jonathan:parse response))
    (dex:http-request-failed (e)
      (error 'anthropic-error
             :message (format nil "HTTP request failed: ~A" e)))
    (jonathan:json-parse-error (e)
      (error 'anthropic-error
             :message (format nil "JSON parse error: ~A" e)))))

;; Example usage:
#|
(defparameter *client* (make-client "your-api-key"))

(defun ask-claude (client question)
  (let ((messages `[{"role" "user" 
                     "content" ,question}]))
    (call-claude client messages)))

;; Usage:
(ask-claude *client* "What's the square root of 144?")
|#

;; Integration with the tavern simulation:

(defun create-character-prompt (character-name background)
  "Create a system prompt for a character"
  (format nil "You are ~A. ~A" character-name background))

(defun generate-character-response (client character-name prompt user-input)
  "Generate a character response using Claude"
  (let* ((system-message (create-character-prompt 
                         character-name 
                         (gethash character-name *character-backgrounds*)))
         (messages `[{"role" "system"
                     "content" ,system-message}
                    {"role" "user"
                     "content" ,user-input}]))
    (handler-case
        (let* ((response (call-claude client messages))
               (content (gethash "content" response)))
          content)
      (anthropic-error (e)
        (format nil "The character seems distracted... (~A)" 
                (error-message e))))))

;; Character backgrounds
(defparameter *character-backgrounds*
  (make-hash-table :test 'equal))

(setf (gethash "Old Tom" *character-backgrounds*)
      "You are the wise and friendly bartender who has owned the Rusty Barrel Tavern for three decades. You know everyone's stories and secrets.")

(setf (gethash "Melody" *character-backgrounds*)
      "You are a talented bard who weaves secrets into songs. You trained at the College of Whispers and speak with poetic flair.")

;; Example usage in tavern simulation:
#|
(defparameter *anthropic* (make-client "your-api-key"))

(defun interact-with-npc (character-name user-input)
  (generate-character-response 
   *anthropic*
   character-name
   (gethash character-name *character-backgrounds*)
   user-input))

;; Usage:
(interact-with-npc "Old Tom" "What's the story behind that old sword above the fireplace?")
|#
;;; --- Main Interface ---

(defun process-interaction (character input-text)
  "Process a single interaction with a character"
  (let* ((parsed (parse-input input-text character))
         (response (generate-response parsed)))
    (format-response character response)))

;;; --- Integration with Main Game ---

(defun handle-player-input (character text)
  "Main entry point for processing player input"
  (unless *api-key*
    (error "Please initialize the API key first"))
  (process-interaction character text))

;; Example usage:
#|
(initialize-api "your-api-key-here")
(initialize-character-contexts)
(handle-player-input "Old Tom" "Tell me about the latest rumors in town")
|#
