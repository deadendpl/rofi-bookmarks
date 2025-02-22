(require 'uiop)

(defvar *bookmarks-file* nil)

(defvar *bookmarks-list* nil)

(defun bookmarks-read ()
  "Add entries from `*bookmarks-file*' to `*bookmarks-list*'."
  (with-open-file (str *bookmarks-file* :direction :input)
    (loop for line = (read-line str nil nil)
          while line do
          (let ((url (subseq line (+ (search "	" line) 1)))
                (name (subseq line 0 (search "	" line))))
            (pushnew `(,name . ,url) *bookmarks-list*)))))

(defun bookmarks-input ()
  "Present the list of bookmarks names in rofi.
Chosen candidate gets returned."
  (let ((string))
    (map 'list (lambda (it)
                 (setf string
                       (concatenate 'string string
                                    (format nil "~a~%" (car it)))))
         *bookmarks-list*)
    ;; removing trailing newline
    (setf string (subseq string 0 (position #\Newline string
                                            :test #'equal :from-end t)))
    (uiop:run-program (concatenate 'string "echo \"" string "\" | "
                                   "rofi -dmenu -p \"Bookmark\" -i")
                      :output '(:string :stripped t))))

(defun bookmarks-get-url ()
  "Return URL matching the output from `bookmark-input'."
  (let ((name (bookmarks-input)))
    (cdr (assoc name *bookmarks-list* :test #'equal))))

(defun bookmarks-open ()
  "Open the URL returned from `bookmarks-get-url'"
  (uiop:launch-program
   (concatenate 'string "xdg-open " (bookmarks-get-url))))

(defun main ()
  "Unless there is no CLI argument given, assign it to *bookmarks-file*,
read it to *bookmarks-list* and open one of URLs."
  (when (eq (length sb-ext:*posix-argv*) 1)
    (format t "File hasn't been provided. Please, provide it!")
    (exit :code 1))
  (setf *bookmarks-file* (car (last sb-ext:*posix-argv*)))
  (bookmarks-read)
  (ignore-errors
    (bookmarks-open)))
