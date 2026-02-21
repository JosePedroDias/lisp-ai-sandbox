(defpackage :tictactoe
  (:use :cl)
  (:export #:play #:make-move #:reset))

(in-package :tictactoe)

(defparameter *board* (make-array 9 :initial-element nil))

(defun reset ()
  (setf *board* (make-array 9 :initial-element nil))
  (display-board))

(defun display-board ()
  (format t "~%")
  (dotimes (i 3)
    (dotimes (j 3)
      (let ((val (aref *board* (+ (* i 3) j))))
        (format t " ~A " (or val (+ (* i 3) j)))))
    (format t "~%~A~%" (if (< i 2) "----------- " "")))
  (format t "~%"))

(defun winner-p ()
  (let ((lines '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6))))
    (loop for line in lines
          for a = (aref *board* (first line))
          for b = (aref *board* (second line))
          for c = (aref *board* (third line))
          when (and a (eq a b) (eq b c))
          return a)))

(defun make-move (pos player)
  (if (aref *board* pos)
      (error "Position already taken!")
      (setf (aref *board* pos) player))
  (display-board)
  (let ((winner (winner-p)))
    (if winner
        (format nil "~A wins!" winner)
        (if (not (position nil *board*))
            "It's a draw!"
            "Next turn!"))))

(defun ai-move ()
  (let ((available (loop for i from 0 to 8 unless (aref *board* i) collect i)))
    (when available
      (let ((choice (nth (random (length available)) available)))
        (make-move choice 'O)))))

(format t "Tic-Tac-Toe loaded! Use (tictactoe:make-move index 'X) to play.")
