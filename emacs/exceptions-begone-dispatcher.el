;; Exceptions Begone Dispatcher client
;; Antonio Garrote <antoniogarrote@gmail.com>

(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup exceptions-begone nil
  "Exceptions Begone client customization"
  :version "0.1"
  :group 'exceptions-begone)

(defcustom exceptions-begone-host
  "localhost"
  "The host where the exception will be retrieved from"
  :group 'exceptions-begone
  :type 'string)

(defcustom exceptions-begone-port
  "8080"
  "The host where the exception will be retrieved from"
  :group 'exceptions-begone
  :type 'string)

(defcustom exceptions-begone-exception-routes
  "*#*"
  "Whitespace separated list of Controller#actions whose exceptions will be retrieved ('*' can be used as a wildcard)"
  :group 'exceptions-begone
  :type 'string)


;; Helper functions

(defvar *exceptions-begone*
  (generate-new-buffer "*exceptions-begone*"))

(defvar *exceptions-begone-acum*
  (generate-new-buffer "*exceptions-begone-acum*"))


(defvar *exceptions-begone-tmp* "*exceptions-begone-tmp*"
  "The name of the process buffer")

(defun websocket-network-connect (host port)
    (let ((coding-system-for-read 'utf-8)
	  (coding-system-for-write 'utf-8))
      (setq *websocket-connection*
	    (open-network-stream
	     "websocket"
             *exceptions-begone-tmp*
	     host
	     port))))

(defun websocket-network-send (string)
  "Send a string via a plain TCP/IP connection to the Websocket Server."
  (process-send-string *websocket-connection* string))

;;; JSON

(defun find-first-valid-char (str idx)
  (if (= idx (length str))
      0
    (let ((val (aref str idx)))
      (if (= (aref str idx) 0)
          (+ idx 1)
        (find-first-valid-char str (+ idx 1))))))

(defun find-first-char (str chr idx)
    (if (= idx (length str))
        -1
      (let ((val (aref str idx)))
        (if (= (aref str idx) chr)
            (+ idx 1)
          (find-first-char str chr (+ idx 1))))))


(defun clean-string (str)
  (let ((max (- (length str) 0)))
    (substring str (find-first-valid-char str 0) max)))


(defun search-in-list (f l)
  "Searches a value in a list"
  (if (eq nil l)
      nil
    (let ((fst (car l)))
      (if (eq t (apply f (list fst)))
          fst
        (search-in-list f (cdr l))))))


(defun find-in-exception (key exception)
  (let ((res (search-in-list (lambda (dp) (eq (car dp) key)) exception)))
    (if (eq nil res)
        nil
      (cdr res))))


(defun extract-payload (json)
  (find-in-exception 'payload json))


(defun extract-notification (json)
  (rest (first json)))


(defun from-websocket-to-string (data)
  (let* ((json (json-read-from-string data))
         (id (find-in-exception 'identifier (extract-notification json)))
         (url (find-in-exception 'url (extract-payload (extract-notification json)))))
    (concat
     (format-time-string "%D %T")
     " -> "
     id
     " @ "
     url
     "\n")))

(defun from-websocket (process data)
  (let* ((acum-data (buffer-string))
         (parsed (from-websocket-to-string (clean-string acum-data))))
    (progn
      (erase-buffer)
      (with-current-buffer *exceptions-begone*
        (insert parsed)))))

;; list of exceptions option parsing
(defun parse-controller (str idx)
  (let ((pos (find-first-char str 35 idx)))
   (if (= pos -1)
       (substring str idx (length str))
     (substring str idx (- pos 1)))))

(defun parse-action (str idx)
  (let ((pos (find-first-char str 32 idx)))
   (if (= pos -1)
       (substring str idx (length str))
     (substring str idx (- pos 1)))))

(defun parse-controller-idx (str idx)
  (find-first-char str 35 idx))


(defun parse-action-idx (str idx)
  (find-first-char str 32 idx))


(defun build-action-pair (str idx)
  (let* ((controller-idx (parse-controller-idx str idx))
         (controller (if (eq controller-idx -1) "" (parse-controller str idx)))
         (action-idx (parse-action-idx str idx))
         (action (parse-action str controller-idx)))
    (concat "[\"" controller "\",\"" action  "\"]")))

(defun build-list (str idx acum)
  (if (or (= idx -1)
          (>= idx (length str)))
      (string-join "," acum)
    (let ((pair (build-action-pair str idx))
          (next-pos (parse-action-idx str idx)))
      (build-list str next-pos (cons pair acum)))))


;; Main process loop

(defun test-filter (process data)
  (let ((acum-data nil)
        (bs nil)
        (cbs nil))
    (progn
      (set-buffer *exceptions-begone-acum*)
      (insert data)
      (setq bs (buffer-string))
      (setq cbs (clean-string bs))
      (condition-case nil
          (progn
            (setq acum-data (from-websocket-to-string cbs))
            (erase-buffer)
            (set-buffer *exceptions-begone*)
            (insert acum-data))
        (error)))))

(defun exceptions-begone-start-client ()
  (interactive)
  (let ((host (let ((rh (read-string (concat "Exceptions Begone Dispatcher host (" exceptions-begone-host "):"))))
                (if (= 0 (length rh))
                    exceptions-begone-host
                  rh)))
        (port (let ((rp (read-string (concat "Exceptions Begone Dispatcher port (" exceptions-begone-port "):"))))
                (if (= 0 (length rp))
                    exceptions-begone-port
                  rp)))
        (exceptions (concat "[" (build-list exceptions-begone-exception-routes 0 '()) "]")))
    (set-buffer *exceptions-begone-acum*)
    (erase-buffer)
    (message "connection to ws://%s:%s/exceptions -> %s" host port exceptions)
    (websocket-network-connect host port)
    (websocket-network-send "GET /exceptions HTTP/1.0\r\nHost: localhost\r\nUser-Agent: emacs\r\nupgrade: WebSocket\r\n\r\n")
    (with-current-buffer (process-buffer *websocket-connection*)
      (progn
        (set-process-filter *websocket-connection* 'test-filter)
        (websocket-network-send exceptions)
        (switch-to-buffer *exceptions-begone*)))))
