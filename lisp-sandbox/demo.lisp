;;;; demo.lisp
;;;; Demo functions for the sandbox

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

