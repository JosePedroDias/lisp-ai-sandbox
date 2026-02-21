
(defpackage :tictactoe-core
  (:use :cl)
  (:export :*board* :*turn* :reset-game :check-winner :fullp :make-move))

(in-package :tictactoe-core)

(defparameter *board* (make-array 9 :initial-element nil))
(defparameter *turn* 'X)

(defun reset-game ()
  (setf *board* (make-array 9 :initial-element nil)
        *turn* 'X))

(defun check-winner ()
  (let ((lines '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6))))
    (loop for line in lines
          for a = (nth 0 line)
          for b = (nth 1 line)
          for c = (nth 2 line)
          when (and (aref *board* a)
                    (eq (aref *board* a) (aref *board* b))
                    (eq (aref *board* a) (aref *board* c)))
          return (aref *board* a))))

(defun fullp ()
  (notany #'null *board*))

(defun make-move (pos)
  (when (and (>= pos 0) (< pos 9) (null (aref *board* pos)))
    (setf (aref *board* pos) *turn*)
    (setf *turn* (if (eq *turn* 'X) 'O 'X))
    t))
