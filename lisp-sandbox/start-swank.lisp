;;;; start-swank.lisp
;;;; Starts a Swank server for remote REPL connections

(require :asdf)

;; Load Quicklisp
(load "~/quicklisp/setup.lisp")

;; Load Swank
(ql:quickload :swank :silent t)

;; Configuration - read port from environment variable or use default
(defparameter *swank-port*
  (let ((env-port (uiop:getenv "SWANK_PORT")))
    (if env-port
        (parse-integer env-port)
        4006))
  "The port on which the Swank server will listen.")

;; Start the Swank server
(defun start-swank-server (&optional (port *swank-port*))
  "Start the Swank server on the specified port."
  (format t "~%Starting Swank server on port ~A...~%" port)
  (swank:create-server :port port :dont-close t)
  (format t "Swank server is now running on port ~A~%" port)
  (format t "Connect from Emacs with: M-x slime-connect RET localhost RET ~A~%" port)
  (format t "Or from Node.js swank-client~%"))

;; Sandbox package definition
(defpackage :sandbox
  (:use :cl)
  (:export ;; Demo functions
           :greet
           :add-numbers
           :factorial
           :fibonacci
           ;; File operations
           :list-files
           :read-file
           :write-file
           :load-file))

;; Load sandbox modules
(let ((sandbox-dir (directory-namestring *load-truename*)))
  (load (merge-pathnames "demo.lisp" sandbox-dir))
  (load (merge-pathnames "tools.lisp" sandbox-dir)))

;; Return to CL-USER
(in-package :cl-user)

;; Start the server when this file is loaded
(start-swank-server)

;; Keep the process running
(format t "~%Swank server is ready. Press Ctrl+C, then (quit) to stop.~%")
(loop (sleep 60))
