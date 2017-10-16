;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>

;; See the LICENSE file for licensing information.

;; A simple example of using the low-level API to trivial-irc.
;;
;; To test it out, configure the variables below, and (START) it up.
;; All it does is echo any messages it sees.  Hence, joining a channel
;; that is in actual use well probably upset some people. 

(in-package #:trivial-irc-echobot)

;;; variables

(defparameter *channels* '()
  "A list of channels to after registering with server.")

(defvar *echobot* nil
  "The current `echobot'.")

(defparameter *nickname* "mynick"
  "Nickname of the `echobot'.")

(defparameter *log-pathname* nil)

(defparameter *server* "example.com"
  "The server to connect to.")

(defparameter *port* 6667
  "The port to connect to.")

;;; echobot class 

(defclass echobot (client)
  ()
  (:documentation "A simple irc-bot that echoes anything it sees.

Valid initargs are the same as for `trivial-irc:client'."))

(defun start ()
  "Set up echobot, bind it to `*echobot*' and enter the `receive-loop'."
  (assert (null *echobot*))
  (setf *echobot* (make-instance 'echobot
				 :log-pathname *log-pathname*
				 :server *server*
				 :port *port*
				 :nickname *nickname*))
  (connect *echobot*)
  (catch 'stop
    (restart-case
	(receive-loop)
      (disconnect ()
	:report (lambda (stream)
		  (format stream "Disconnect echobot"))
	:interactive (lambda ()
		       (throw 'stop nil)))))
  (stop))

(defun stop ()
  "Stop and `disconnect' the echobot."
  (ignore-errors (disconnect *echobot*))
  (setf *echobot* nil))

(defun receive-loop ()
  "Receive messages until interrupted."
  (with-simple-restart (abort "Exit from receive-loop without quitting")
  (loop
    (with-simple-restart (continue "Continue echobot recieve-loop")
      (receive-message *echobot*)))))

;;; handlers

;; join our channel as soon as server welcomes us
(define-handler (:rpl_welcome echobot prefix arguments)
  (dolist (channel *channels*)
    (send-join echobot channel)))

;; if privmsg to echobot, reply to sender, otherwise to victim
;; (which is most likely a channel that echobot has joined)
(define-handler (:privmsg (echobot echobot) prefix arguments)
  (destructuring-bind (victim message) arguments
    (if (string-equal (nickname echobot) victim)
	(send-privmsg echobot (prefix-nickname prefix) message)
	(send-privmsg echobot victim message))))

