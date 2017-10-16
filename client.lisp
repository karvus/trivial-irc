;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>.
;; See the LICENSE file for licensing information.

(in-package #:trivial-irc)

;;; variables

(defparameter *version* "0.0.4")

(defparameter *default-quit-message*
  (format nil "trivial-irc-~a" *version*))

(defparameter *message-scanner*
  (cl-ppcre:create-scanner "^(:([^ ]+) +)?([^ ]+)( +(.+))?"))

(defmacro with-client-stream ((var client) &body body)
  `(let ((,var (usocket:socket-stream (socket ,client))))
     ,@body))

;;; conditions

(define-condition connection-closed ()
  ((client :initarg :client))
  (:report (lambda (condition stream)
	     (with-slots (client) condition
	       (format stream "Connection closed for ~a" client))))
  (:documentation "Signalled by `disconnect'.

Disconnecting is the default action whenever an error occurs, so this
signal can for example be handled to reconnect."))

(define-condition connection-lost (error)
  ((client :initarg :client))
  (:report (lambda (condition stream)
	     (with-slots (client) condition
	       (format stream "Connection lost for ~a" client))))
  (:documentation "Signalled when connection is lost.

Currently signalled when an error occurs during trying to receive a
message from the server."))

(define-condition connection-failed (error)
  ((client :initarg :client)
   (error :initarg error))
  (:report (lambda (condition stream)
	     (with-slots (client) condition
	       (format stream "Connection failed for ~a" client))))
  (:documentation "Signalled by `connect'."))

;;; macros

(defmacro define-handler ((command class-spec prefix-var arguments-var)
			  &body body)
  "Define handling for /command/.

This is currently a convenience for specializing on `handle'.  An
example is the handler for PING messages (which by default is the only
handler specialization).

--
;; (define-handler (:ping client prefix arguments)
;;    (send-pong client (first arguments)))
--

If you wanted to use a different variable-name for the client
variable, you could also have written it as

--
;; (define-handler (:ping (client client) prefix arguments)
;;   (send-pong client (first arguments)))
--
"
  (let ((%command-var (gensym "g-command-var")))
    `(defmethod handle ((,%command-var (eql ,command))
			,(if (listp class-spec)
			     class-spec
			     `(,class-spec ,class-spec))
			,prefix-var ,arguments-var)
       ,@body)))

;;; generics


(defgeneric change-nick (client new-nickname)
  (:documentation "Send NICK message to server, and set the `nickname'
  of /client."))

(defgeneric connect (client)
  (:documentation
   "Connect and register /client/ with an IRC server.

This also sets up some of the slots, and opens the log-stream."))

(defgeneric connected-p (client)
  (:documentation
   "Return `t' if /client/ is connected, `nil' otherwise."))

(defgeneric disconnect (client &key message)
  (:documentation
   "Send QUIT message to server, close the socket and close the log-stream.

Always signals `connection-closed'."))

(defgeneric handle (command client prefix arguments)
  (:documentation "Called by `receive-message' after parsing the raw message.

Specialize this with the macro `define-handler' for customizing the
behaviour.

There is a default method that spits out the unhandled message
to `*standard-output*'."))

(defgeneric nickname (client)
  (:documentation "Return current nickname of /client/."))

(defgeneric send-join (client channel &key password)
  (:documentation "Send JOIN message."))

(defgeneric send-pong (client message)
  (:documentation "Send PONG command to server."))

(defgeneric send-privmsg (client victim message)
  (:documentation "Send /message/ to /victim/.

/victim/ can be either a channel-name or a nickname."))

(defgeneric socket (client)
  (:documentation "Return the /client/'s socket."))

(defgeneric receive-message (client)
  (:documentation "Read a message from /connnection/, parse it,
  `handle', and return the parsed bits.

The return value is a list with (/raw-prefix/ /command/ /parsed-parameters/), where

- /raw-prefix/ is the raw prefix string, or `nil' if prefix wasn't present,
- /command/ is a keyword with a name corresponding to the command from the RFC and
- /parsed-parameters/ is a list of strings representing the arguments in the message.

If an error occurs during the reading, the client will be
disconnected, and the `connection-closed' will be signalled."))

;;; class

(defclass client ()
  ((log-pathname
    :initarg :log-pathname
    :initform nil)
   (log-stream
    :initform nil)
   (nickname
    :initarg :nickname
    :reader nickname
    :initform (error "must supply :nickname"))
   (password
    :initarg :password
    :initform nil)
   (port
    :initarg :port
    :initform 6667)
   (realname
    :initarg :realname
    :initform nil)
   (server
    :initarg :server
    :initform (error "must supply :server"))
   (socket
    :initarg :socket
    :reader socket)
   (username
    :initarg :username
    :initform nil))
  (:documentation "A client connection to an IRC server.

Valid initargs are:

- `:nickname' -- the nickname  use when connecting (required)
- `:server' -- the hostname of the server to connect to as a string (required)
- `:port' -- the port to connect to as an integer (optional)
- `:username' -- the username to register with (optional)
- `:realname' -- the realname to register with (optional)
- `:password' -- the password to regiseter with (optional)
- `:log-pathname' -- pathname for packet-log pathname (optional)

Please note that you call `connect' on an instance of `client', instead of
having `connect' return a an instance instance."))

;;; implementation

(defmethod change-nick ((client client) new-nickname)
  (prog1
      (send-raw-message client (format nil "NICK ~a" new-nickname))
    (setf (slot-value client 'nick) new-nickname)))

(defmethod connect ((client client))
  (with-slots (log-pathname log-stream nickname password port realname
			    server socket username)
      client
    (when log-pathname
      (setf log-stream (open log-pathname
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)))
    ;; try to connect, signal connection-failed if
    (handler-case
	(progn 
	  (setf socket
		(usocket:socket-connect server port))
	  (unless username
	    (setf username nickname))
	  (unless realname
	    (setf realname username))
	  (.password client)
	  (.user client)
	  (.nick client))
      (error (error) (error 'connection-failed
			    :client client
			    :error error))))
  client)

(defmethod connected-p ((client client))
  (if (socket client)
      t
      nil))

(defmethod disconnect ((client client)
		       &key (message *default-quit-message*))
  (with-slots (socket log-stream) client
    (when socket
      (send-raw-message client
			(format nil "QUIT :~a" message)))
    (setf socket nil)
    (when log-stream
      (close log-stream))
    (setf log-stream nil))
  (signal 'connection-closed :client client)
  client)

(defmethod handle (command (client client) prefix arguments)
  (format t "Unhandled: ~a ~a ~{~a ~}~%" prefix command arguments))

(defmethod receive-message ((client client))
  (let ((raw-message (receive-raw-message client)))
    (record client raw-message)
    (destructuring-bind (prefix command arguments)
	(parse-raw-message raw-message)
      (handle command client prefix arguments))
    raw-message))

(defmethod send-join ((client client) channel &key password)
  (send-raw-message client
		    (format nil "JOIN ~a~@[ ~a~]"
			    channel password)))

(defmethod send-privmsg ((client client) victim message)
  (send-raw-message client
		    (format nil "PRIVMSG ~a :~a" victim message)))

(defmethod send-pong ((client client) message)
  (send-raw-message client
		    (format nil "PONG :~a" message)))

;;;  handlers

(define-handler (:ping client prefix arguments)
  (send-pong client (first arguments)))

;;; raw-message

(defun receive-raw-message (client)
  "Receive and return a single, raw message from /client/.

If any errors occur during the reading, the connection is silently
shut down."
  (handler-case
      (with-output-to-string (message)
	(with-client-stream (stream client)
	  (loop for char = (read-char stream)
		until (and (eql #\Return char)
			   (eql #\Linefeed (peek-char nil stream)))
		do (write-char char message)
		finally (read-char stream))))
    (error () (error 'connection-lost :client client))))

(defun send-raw-message (client raw-message)
  "Send /raw-message/ and CRLF to the socket associated with /client/.

Outside of the few send-* functions, this is what you have to use to
send messages to the server."
  (with-client-stream (stream client)
    (write-string raw-message stream)
    (write-char #\Return stream)
    (write-char #\Linefeed stream)
    (finish-output stream))
  (record client raw-message)
  raw-message)

(defun parse-raw-message (raw-message)
  "Return a list on the form (prefix command arguments).

/prefix/ can be `nil', or servername / ( nickname [ [ \"!\" user ] \"@\" host ]
See also the `parse-prefix' function.

/command/ is a keyword either made from the alpha-characters, or a
keyword looked up with `find-reply-name'.

/arguments/ is a list of the command arguments."
  (multiple-value-bind (match-string match-vector)
      (cl-ppcre:scan-to-strings *message-scanner* raw-message)
    (declare (ignore match-string))
    (let* ((prefix (aref match-vector 1))
	   (command-string (aref match-vector 2))
	   (command (handler-case (find-reply-name (parse-integer command-string))
		      (error () (intern (string-upcase command-string) :keyword))))
	   (arguments (parse-argument-string (aref match-vector 3))))
      (list prefix command arguments))))

(defun parse-argument-string (argument-string)
  (let ((trailing-pos (search " :" argument-string)))
    (append (split-sequence:split-sequence #\Space argument-string
					   :remove-empty-subseqs t
					   :end trailing-pos)
	    (if trailing-pos
		(list (subseq argument-string (+ 2 trailing-pos)))
		nil))))


;;; "private" functions

(defun .nick (client)
  "Send /client/'s nickname to server.

Called by `connect' during registration."
  (with-slots (nickname) client
    (send-raw-message client (format nil "NICK ~a" nickname))))

(defun .password (client)
  "Send /client/'s password if a password is associated with /client/."
  (with-slots (password) client
    (when password
      (send-raw-message client
			(format nil "PASSWORD ~a" password)))))

(defun .user (client)
  "Send /client/'s username and realname to server.

Called by `connect' during registration."
  (with-slots (username realname) client
    (send-raw-message client
		      (format nil "USER ~a 0 * :~a"
			      username realname))))

(defun .quit (client &optional (message *default-quit-message*))
  (let ((socket client))
    (when socket
      (send-raw-message client
			(format nil "QUIT :~a" message)))))

(defun prefix-nickname (prefix)
  "Return the nickname in extracted from /prefix/."  
  (first (parse-prefix prefix)))

(defun prefix-servername (prefix)
  "Return the servername extracted from /prefix/."  
  (first (parse-prefix prefix)))

(defun servername-or-nickname (prefix)
  "Return the servername or nickname from /prefix-string/."
  (first (parse-prefix prefix)))

(defun parse-prefix (prefix)
  "Return a list of the components in prefix.

It is a list on the form ( /server-or-nickname/ /username/ /host/) where

- /servername-or-nickname/ is a servername or a nickname,
- /username/ is a username, or `nil'
- /host/ is a hostname, or `nil'

This can potentially be used to build other abstractions later."
  (when prefix
    (cl-ppcre:split "(!|@)" prefix)))

(defun record (client string)
  (with-slots (log-stream) client
    (when log-stream
      (format log-stream "[~a] ~a~%"
	      (universal-time->iso-8601-string (get-universal-time))
	      string)
      (finish-output log-stream))))

(defun universal-time->iso-8601-string (universal-time &key (detail :second))
  (multiple-value-bind
	(second minute hour date month year)
      (decode-universal-time universal-time 0)
    (with-output-to-string (iso-8601)
      (format iso-8601 "~4,'0D-~2,'0D-~2,'0D" year month date)
      (ecase detail
	(:date nil)
	(:minute (format iso-8601 " ~2,'0D:~2,'0D UTC" hour minute))
	(:second (format iso-8601 " ~2,'0D:~2,'0D:~2,'0d" hour minute second))))))