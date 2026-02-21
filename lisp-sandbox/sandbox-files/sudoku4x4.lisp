(defpackage :sudoku4x4
  (:use :cl)
  (:export #:generate
           #:create-puzzle
           #:display
           #:is-valid
           #:place))

(in-package :sudoku4x4)

;; --- Helpers ---

(defun shuffle (list)
  (let ((vec (coerce list 'vector)))
    (loop for i from (1- (length vec)) downto 1
          for j = (random (1+ i))
          do (rotatef (aref vec i) (aref vec j)))
    (coerce vec 'list)))

(defun copy-grid (grid)
  (let ((new-grid (make-array '(4 4))))
    (dotimes (r 4)
      (dotimes (c 4)
        (setf (aref new-grid r c) (aref grid r c))))
    new-grid))

;; --- Core Logic ---

(defun is-valid (grid r c v)
  "Check if placing value V at (R, C) is valid in the 4x4 GRID."
  (and (not (loop for i from 0 to 3 thereis (= (aref grid r i) v)))
       (not (loop for i from 0 to 3 thereis (= (aref grid i c) v)))
       (let ((br (* 2 (floor r 2))) 
             (bc (* 2 (floor c 2))))
         (not (loop for i from 0 to 1 thereis
                    (loop for j from 0 to 1 thereis
                          (= (aref grid (+ br i) (+ bc j)) v)))))))

(defun generate ()
  "Generate a full valid 4x4 Sudoku grid."
  (let ((grid (make-array '(4 4) :initial-contents '((1 2 3 4)
                                                     (3 4 1 2)
                                                     (2 1 4 3)
                                                     (4 3 2 1))))
        (mapping (shuffle '(1 2 3 4))))
    ;; Remap numbers
    (dotimes (r 4)
      (dotimes (c 4)
        (setf (aref grid r c) (nth (1- (aref grid r c)) mapping))))
    ;; Shuffle rows/cols within 2x2 blocks
    (when (zerop (random 2)) (dotimes (c 4) (rotatef (aref grid 0 c) (aref grid 1 c))))
    (when (zerop (random 2)) (dotimes (c 4) (rotatef (aref grid 2 c) (aref grid 3 c))))
    (when (zerop (random 2)) (dotimes (r 4) (rotatef (aref grid r 0) (aref grid r 1))))
    (when (zerop (random 2)) (dotimes (r 4) (rotatef (aref grid r 2) (aref grid r 3))))
    grid))

(defun create-puzzle (&optional (holes 8))
  "Create a puzzle grid by removing HOLES from a full grid."
  (let ((grid (generate)))
    (let ((coords (shuffle (loop for r from 0 to 3 append (loop for c from 0 to 3 collect (list r c))))))
      (loop for i from 0 to (1- (min holes 16))
            for cell = (nth i coords)
            do (setf (aref grid (first cell) (second cell)) 0)))
    grid))

(defun display (grid)
  "Print the 4x4 GRID to standard output."
  (format t "~%+---+---+---+---+~%")
  (dotimes (r 4)
    (format t "|")
    (dotimes (c 4)
      (let ((val (aref grid r c)))
        (format t " ~A |" (if (or (not val) (zerop val)) " " val))))
    (format t "~%+---+---+---+---+~%"))
  (values))

(defun place (grid r c v)
  "Attempt to place value V at (R, C) in GRID. Modifies GRID if valid."
  (cond
    ((or (< r 0) (> r 3) (< c 0) (> c 3)) 
     (error "Row/Col must be 0-3."))
    ((is-valid grid r c v)
     (setf (aref grid r c) v)
     t)
    (t nil)))

(format t "Sudoku4x4 library loaded. Try (sudoku4x4:display (sudoku4x4:create-puzzle))~%")
