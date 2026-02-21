(defpackage :sudoku
  (:use :cl)
  (:export #:generate #:display #:create-puzzle))

(in-package :sudoku)

(defun display (board)
  (format t "~%-------------------------~%")
  (dotimes (r 9)
    (format t "| ")
    (dotimes (c 9)
      (let ((val (aref board r c)))
        (format t "~A " (if (or (not val) (= val 0)) "." val)))
      (when (= (mod (1+ c) 3) 0)
        (format t "| ")))
    (format t "~%")
    (when (= (mod (1+ r) 3) 0)
      (format t "-------------------------~%"))))

(defun is-safe (board row col num)
  (let ((box-row (* 3 (floor row 3)))
        (box-col (* 3 (floor col 3))))
    ;; Check row, column and 3x3 box
    (and (not (loop for i from 0 to 8 thereis (= (aref board row i) num)))
         (not (loop for i from 0 to 8 thereis (= (aref board i col) num)))
         (not (loop for i from 0 to 2 thereis
                    (loop for j from 0 to 2 thereis
                          (= (aref board (+ box-row i) (+ box-col j)) num)))))))

(defun shuffle (list)
  (let ((vec (coerce list 'vector)))
    (loop for i from (1- (length vec)) downto 1
          for j = (random (1+ i))
          do (rotatef (aref vec i) (aref vec j)))
    (coerce vec 'list)))

(defun fill-board (board)
  (loop for r from 0 to 8 do
    (loop for c from 0 to 8 do
      (when (= (aref board r c) 0)
        (dolist (num (shuffle '(1 2 3 4 5 6 7 8 9)))
          (when (is-safe board r c num)
            (setf (aref board r c) num)
            (if (fill-board board)
                (return-from fill-board t)
                (setf (aref board r c) 0))))
        (return-from fill-board nil))))
  t)

(defun generate ()
  (let ((board (make-array '(9 9) :initial-element 0)))
    (fill-board board)
    board))

(defun create-puzzle (board holes)
  (let ((puzzle (make-array '(9 9))))
    ;; Copy board
    (dotimes (i 9) (dotimes (j 9) (setf (aref puzzle i j) (aref board i j))))
    ;; Remove random cells
    (let ((coords (shuffle (loop for r from 0 to 8 append (loop for c from 0 to 8 collect (list r c))))))
      (loop for i from 0 to (1- (min holes 81))
            for cell = (nth i coords)
            do (setf (aref puzzle (first cell) (second cell)) 0)))
    puzzle))

(format t "Sudoku library loaded. Try (sudoku:display (sudoku:generate))")
