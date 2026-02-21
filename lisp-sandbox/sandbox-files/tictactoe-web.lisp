
(defpackage :tictactoe-web
  (:use :cl :tictactoe-core)
  (:export :start-server :stop-server))

(in-package :tictactoe-web)

(defun render-board ()
  (format nil "<html><head><style>
    body { font-family: sans-serif; text-align: center; background: #f0f2f5; }
    .container { margin-top: 50px; display: inline-block; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
    table { margin: 20px auto; border-collapse: collapse; }
    td { width: 80px; height: 80px; text-align: center; border: 3px solid #eee; font-size: 2.5em; font-weight: bold; }
    a { text-decoration: none; color: #007bff; display: block; width: 100%; height: 100%; line-height: 80px; }
    a:hover { background: #f8f9fa; }
    .winner { color: #28a745; }
    .reset { color: #6c757d; font-size: 0.9em; }
    </style></head><body>
    <div class='container'>
    <h1>Tic Tac Toe</h1>
    <p>Current Player: <b style='color: ~A'>~A</b></p>
    <table>
      ~{~A~}
    </table>
    <br><a href='/reset' class='reset'>[ Reset Game ]</a>
    </div>
    </body></html>"
          (if (eq *turn* 'X) "#e74c3c" "#3498db")
          *turn*
          (loop for i from 0 below 9
                collect (format nil "~A~A~A"
                                (if (= 0 (mod i 3)) "<tr>" "")
                                (format nil "<td>~A</td>"
                                        (cond ((aref *board* i) 
                                               (format nil "<span style='color: ~A'>~A</span>" 
                                                       (if (eq (aref *board* i) 'X) "#e74c3c" "#3498db")
                                                       (aref *board* i)))
                                              (t (format nil "<a href='/move?pos=~A'> </a>" i))))
                                (if (= 2 (mod i 3)) "</tr>" "")))))

(defun handle-request (env)
  (let* ((path (getf env :path-info))
         (query (getf env :query-string))
         (winner (check-winner)))
    (cond
      ((string= path "/reset")
       (reset-game)
       '(200 (:content-type "text/html") ("<meta http-equiv='refresh' content='0;url=/'>")))
      ((and (string= path "/move") (not winner))
       (let ((pos (and query (cl-ppcre:scan-to-strings "\\d+" query)
                       (parse-integer (cl-ppcre:scan-to-strings "\\d+" query) :junk-allowed t))))
         (when pos (make-move pos)))
       '(200 (:content-type "text/html") ("<meta http-equiv='refresh' content='0;url=/'>")))
      (t
       (let ((msg (cond (winner (format nil "<h2 class='winner'>~A Wins!</h2>" winner))
                        ((fullp) "<h2>It's a Draw!</h2>")
                        (t ""))))
         `(200 (:content-type "text/html") (,(concatenate 'string msg (render-board)))))))))

(defvar *server* nil "The running woo server instance")

(defun start-server (&optional (port 8080))
  "Start the tic-tac-toe web server on the specified port"
  (when *server*
    (format t "Server already running. Stop it first with (stop-server)~%")
    (return-from start-server nil))
  (reset-game)
  (setf *server* (bt:make-thread
                   (lambda ()
                     (woo:run #'handle-request :port port))
                   :name "tictactoe-server"))
  (format t "Tic-tac-toe server started on http://localhost:~A~%" port)
  t)

(defun stop-server ()
  "Stop the running web server"
  (when *server*
    (bt:destroy-thread *server*)
    (setf *server* nil)
    (format t "Server stopped~%")
    t))
