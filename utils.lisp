;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-tex; -*-

(in-package #:cl-tex)

(defun successful-process-p (process)
  (eql 0 (sb-ext:process-exit-code process)))

(defun failed-process-p (process)
  (not (successful-process-p process)))

(defun which (program-name)
  "Calls which(1) to determine the path for PROGRAM-NAME.  Returns either
a STRING or NIL."
  (let ((process (sb-ext:run-program
                  "/usr/bin/which" (list program-name)
                  :input NIL
                  :output :stream
                  :error NIL)))
    (when (successful-process-p process)
      (values (read-line (sb-ext:process-output process))))))

(defmacro with-current-directory (directory &body body)
  "Sets the current working DIRECTORY for the duration of the BODY.
If DIRECTORY is NIL, this only resets after the BODY was run."
  (let ((old-directory-sym (gensym))
        (directory-sym (gensym)))
    `(let ((,old-directory-sym (osicat:current-directory))
           (,directory-sym ,directory))
       (unwind-protect
            (progn
              (when ,directory-sym
                (osicat-posix:chdir (namestring ,directory-sym)))
              ,@body)
         (setf (osicat:current-directory) ,old-directory-sym)))))

;; TODO: surely there is a standard way to do this?
;; TODO: and if not, do this destructive on LIST
(defun filter-key-args (list &rest keys)
  (iterate
    (for (key arg) on list by #'cddr)
    (unless (member key keys :test #'eq)
      (collect key)
      (collect arg))))
