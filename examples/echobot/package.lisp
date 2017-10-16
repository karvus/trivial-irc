;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>.

;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:trivial-irc-echobot
  (:use #:common-lisp
	#:trivial-irc)
  (:export #:*channels*
	   #:*echobot*
	   #:*nickname*
	   #:*port*
	   #:*server*
	   #:echobot
	   #:receive-loop
	   #:start
	   #:stop)
  (:documentation "A trivial echobot.

The echobot will echo all messages sent to it, or to any channel it's
in.

This is meant as a simple example of how to use the `trivial-irc'
library.  To give it a sping, customize the variables in echobot.lisp,
and do something like this:

--
;; (asdf:oos 'asdf:load-op :trivial-irc-echobot)
;; (trivial-echobot:start)
--

Note that you probably don't want to make it join a channel someone
actually uses, as it's quite annoying."))