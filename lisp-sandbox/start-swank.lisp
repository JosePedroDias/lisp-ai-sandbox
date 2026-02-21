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

;; Simple sandbox utilities
(defpackage :sandbox
  (:use :cl)
  (:export :greet
           :add-numbers
           :factorial
           :fibonacci))

(in-package :sandbox)

(defun greet (name)
  "Return a greeting message."
  (format nil "Hello, ~A! Welcome to the Lisp sandbox." name))

(defun add-numbers (&rest numbers)
  "Add a list of numbers together."
  (apply #'+ numbers))

(defun factorial (n)
  "Calculate the factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(defun fibonacci (n)
  "Calculate the Nth Fibonacci number."
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))

;; Return to CL-USER
(in-package :cl-user)

;; Start the server when this file is loaded
(start-swank-server)

;; Keep the process running
(format t "~%Swank server is ready. Press Ctrl+C to stop.~%")
(loop (sleep 60))

