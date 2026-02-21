;;;; tools.lisp
;;;; File system operations for the sandbox

(in-package :sandbox)

;;; File system operations
;;; Files are stored in the sandbox-files/ directory relative to the sandbox

(defparameter *sandbox-files-dir*
  (merge-pathnames "sandbox-files/"
                   (make-pathname :directory (pathname-directory *load-truename*)))
  "Directory for sandbox file operations.")

(defun ensure-sandbox-dir ()
  "Ensure the sandbox files directory exists."
  (ensure-directories-exist *sandbox-files-dir*))

(defun sandbox-filepath (filename)
  "Get the full path for a file in the sandbox directory.
   Prevents directory traversal attacks by only using the filename."
  (let ((safe-name (file-namestring (pathname filename))))
    (when (or (string= safe-name "")
              (string= safe-name ".")
              (string= safe-name ".."))
      (error "Invalid filename: ~A" filename))
    (merge-pathnames safe-name *sandbox-files-dir*)))

(defun list-files ()
  "List all files in the sandbox directory."
  (ensure-sandbox-dir)
  (let ((files (directory (merge-pathnames "*.*" *sandbox-files-dir*))))
    (mapcar #'file-namestring files)))

(defun read-file (filename)
  "Read the contents of a file from the sandbox directory."
  (ensure-sandbox-dir)
  (let ((filepath (sandbox-filepath filename)))
    (if (probe-file filepath)
        (with-open-file (stream filepath :direction :input)
          (let ((contents (make-string (file-length stream))))
            (read-sequence contents stream)
            contents))
        (error "File not found: ~A" filename))))

(defun write-file (filename content)
  "Write content to a file in the sandbox directory.
   FILENAME is the name of the file.
   CONTENT is the string content to write."
  (ensure-sandbox-dir)
  (let ((filepath (sandbox-filepath filename)))
    (with-open-file (stream filepath :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string content stream))
    (format nil "File written: ~A" filename)))

