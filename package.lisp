;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>.
;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:trivial-irc
  (:use #:common-lisp)
  (:nicknames)
  (:export
   #:client
   #:connected-p
   #:connect
   #:connection-closed
   #:connection-failed
   #:connection-lost
   #:define-handler
   #:disconnect
   #:handle
   #:nickname
   #:parse-prefix
   #:prefix-nickname
   #:prefix-servername
   #:receive-message
   #:send-join
   #:send-privmsg
   #:send-pong
   #:send-raw-message
   #:server-or-nick
   #:user
   )
  (:documentation "Trivial IRC client library.

See `examples/echobot/' in the distribution directory for a simple
example of how to use it.

Currently, the exposed API is a very thin abstraction of the IRC
protocol, and you probably need the IRC RFC to make use of it.

The current version was slapped together in a few hours, to fill a
specific need I had.  It's not certain that it will evolve with much,
but I'm open to suggestions and requests.

If you have a better fit for trivial-irc and want to claim the
name, just let me know and I'll retire this."))

