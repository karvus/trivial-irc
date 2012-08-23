;; Copyright (c) 2008 Thomas Stenhaug <thomas.stenhaug@gmail.com>.
;; See the LICENSE file for licensing information.

(asdf:defsystem #:trivial-irc
  :author "Thomas Stenhaug <thomas.stenhaug@gmail.com>"
  :maintainer "Thomas Stenhaug <thomas.stenhaug@gmail.com>"
  :version "0.0.4"
  :serial t
  :components ((:file "package")
               (:file "replies")
               (:file "client"))
  :depends-on (#:cl-ppcre
               #:split-sequence
               #:usocket))
