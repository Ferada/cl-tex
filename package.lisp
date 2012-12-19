;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-tex
  (:use #:cl #:utils-frahm #:anaphora #:logv #:iterate #:cl-inotify)
  (:import-from #:arnesi #:eval-always)
  (:export #:tex #:run-tex
           #:tex-runtime-error))
