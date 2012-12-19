;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-tex
  :depends-on (#:anaphora
               #:logv
               #:split-sequence
               #:iterate
               #:local-time
               #:arnesi
               #:series
               #:osicat
               #:cl-inotify
               #:utils-frahm-common)
  :serial T
  :components ((:file "package")
               (:file "utils")
               (:file "tex")))
