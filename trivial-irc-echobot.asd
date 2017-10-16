;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>.
;; See the LICENSE file for licensing information.

(asdf:defsystem #:trivial-irc-echobot
  :author "Thomas Stenhaug <thomas.stenhaug@gmail.com>"
  :maintainer "Thomas Stenhaug <thomas.stenhaug@gmail.com>"
  :version "0.0.3"
  :components
  ((:module "examples"
	    :components
	    ((:module "echobot"
		      :serial t
		      :components ((:file "package")
				   (:file "echobot"))))))
  :depends-on (#:trivial-irc))