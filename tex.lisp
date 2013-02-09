;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-tex; -*-

(in-package #:cl-tex)

(defvar *temporary-directory-template* "/tmp/cl-tex-XXXXXX"
  "Default template parameter for mkdtemp(3).")

(defun calculate-output-directory (directory &optional pathname (template *temporary-directory-template*))
  "Calculates (and possibly creates) the output directory.  Returns a STRING.

If DIRECTORY is a STRING or PATHNAME, it's passed through.

If it's :TEMPORARY, a new temporary directory is created based on the given
TEMPLATE, which defaults to *TEMPORARY-DIRECTORY-TEMPLATE*.

If it's T, then the directory of PATHNAME is returned if it's an absolute
path or the current working directory else.

If it's NIL, the current working directory is used.

All other cases result in an error."
  (cond
    ((stringp directory) directory)
    ((pathnamep directory) (namestring directory))
    ((eq directory :temporary)
     ;; TODO: handle OSICAT-POSIX:POSIX-ERROR
     (truename (osicat-posix:mkdtemp template)))
    ((eq directory T)
     (namestring
      (if (or (not pathname) (osicat:relative-pathname-p pathname))
          (osicat:current-directory)
          (osicat:pathname-directory-pathname pathname))))
    ((null directory)
     (namestring (osicat:current-directory)))
    (T
     (error "invalid directory specifier: ~A" directory))))

(defvar *tex-compilers*
  '((:lualatex (:pdf :dvi) (:output-format))
    (:luatex (:pdf :dvi) (:output-format))
    (:pdflatex (:pdf :dvi) (:output-format))
    (:latex (:dvi)))
  "LIST of (La,...)TeX compilers.  The format for each entry is
\(COMPILER OUTPUT-FORMATS OPTIONS), where the latter two are LISTS of
KEYWORDS and COMPILER is a KEYWORD as well.")

(defun valid-option-p (program option)
  "Returns true if the given OPTION is valid for PROGRAM.  PROGRAM is a
string designator, whereas OPTION is a KEYWORD.  See *TEX-COMPILERS* for
valid values."
  (member option
          (caddr (assoc program *tex-compilers*
                        :test #'string-equal))
          :test #'eq))

(defun valid-output-format-p (program output-format)
  "Returns true if the given OUTPUT-FORMAT is valid for PROGRAM.  PROGRAM
is a string designator, whereas OUTPUT-FORMAT is a KEYWORD.  See
*TEX-COMPILERS* for valid values."
  (member output-format
          (cadr (assoc program *tex-compilers*
                       :test #'string-equal))
          :test #'string-equal))

(defun default-output-format (program)
  "Returns the default output format for PROGRAM, which is a string
designator.  See *TEX-COMPILERS* for valid values."
  (and (not (string-equal program :latex))
       (caadr (assoc program *tex-compilers* :test #'string-equal))))

(defun format-tex-symbol (string-designator)
  "Formats a STRING-DESIGNATOR for use as a command line option.  Which
means lowercasing SYMBOLS and keeping STRINGS.  Other types are errors."
  (etypecase string-designator
    (symbol (string-downcase string-designator))
    (string string-designator)))

(defun filter-invalid-args (args)
  (filter-key-args args :interaction :output-directory :tex :tex-args
                        :jobname :check-arguments :output-format
                        :collect-written-files :tex-error))

(define-condition tex-runtime-error (error)
  ((written-files :initarg :written-files
                  :accessor written-files)
   (process :initarg :process
            :accessor process))
  (:report (lambda (condition stream)
             (cond
               ((failed-process-p (process condition))
                (format stream "TeX quit with non-zero exit status."))
               (T
                (format stream "An error occured during a TeX run."))))))

(defun maybe-tex-runtime-error (error process &optional (written NIL written-p))
  (when (and error
             (not (sb-ext:process-alive-p process))
             (failed-process-p process))
    (if written-p
        (error 'tex-runtime-error :process process :written-files written)
        (error 'tex-runtime-error :process process))))

;; TODO: figure out interactive, i.e. streaming usage
;; TODO: unit/execution tests for this, maybe a dry-run option?
;; TODO: -jobname, also output name calculation on this
(defun run-tex (pathname
                &rest rest
                &key (check-arguments T)
                     (interaction :nonstopmode)
                     (output-directory T)
                     jobname
                     (tex :pdflatex)
                     (output-format NIL output-format-p)
                     tex-args
                     (search T searchp) ; see SB-EXT:RUN-PROGRAM
                     (tex-error T)
                &allow-other-keys)
  "Runs the TEX program on file PATHNAME while applying the given
options.  PATHNAME may also contain an arbitrary STRING which is then
passed on to the TEX program and should contain a set of TeX commands,
beginning with a backslash.  See also the man-page for pdftex(1).

If OUTPUT-DIRECTORY is a PATHNAME designator, the working directory is
temporarily changed so the files created during the run don't clutter
whatever path was previously set.  If set to T, the directory is changed
to the dirname(1) of PATHNAME.  If set to :TEMPORARY, the directory is
changed to an empty temporary directory.

TEX-ARGS are additional parameters which will be passed to the program
after all other arguments were calculated, but before the given
PATHNAME.

If CHECK-ARGUMENTS is true, some arguments are checked for validity
according to the available documentation for the various (...)(La)TeX
binaries.  Thus, if some of these checks are bogus on a specific system,
this flag can be unset as a workaround.

Other keys are permitted as well and passed on to SB-EXT:RUN-PROGRAM.

Two values are returned: the first is the value of OUTPUT-DIRECTORY when
the TEX program was invoked (i.e. after having performed the
calculations described above), the second is the SB-EXT:PROCESS
structure.  Since we can't reliably calculate the output filename (if
there is an output file), either some assumptions are made by the
caller, or some other measures are taken, e.g. examining the
OUTPUT-DIRECTORY to look for output files.  (The order is chosen to
support one-shot operations with the :WAIT parameter set to true for
SB-EXT:RUN-PROGRAM.)

If ERROR is set, an ERROR is raised if the exit status wasn't zero."
  (with-current-directory
      (when output-directory
        (setf output-directory (calculate-output-directory output-directory pathname)))
    (when (and output-format check-arguments)
      (when (not (valid-option-p tex :output-format))
        (error "OUTPUT-FORMAT is invalid for program ~S" tex))
      (when (not (valid-output-format-p tex output-format))
        (error "invalid OUTPUT-FORMAT for program ~S: ~S" tex output-format)))
    (when (and (not output-format-p)
               (valid-option-p tex :output-format))
      (setf output-format (default-output-format tex)))
    (let ((args `("-interaction" ,(string-downcase (symbol-name interaction))
                  ,.(and output-directory `("-output-directory" ,output-directory))
                  ,.(and jobname `("-jobname" ,jobname))
                  ,.(and output-format `("-output-format" ,(format-tex-symbol output-format)))
                  ;; don't be destructive here
                  ,@tex-args
                  ,.(when pathname
                      (list (etypecase pathname
                              (pathname (namestring pathname))
                              (string pathname))))))
          ;; strictly speaking we could also use :allow-other-keys T here to ignore these
          (filtered-rest (filter-invalid-args rest)))
      (unless searchp
        (setf filtered-rest (list* :search search filtered-rest)))
      (let* ((program (format-tex-symbol tex))
             (process (apply #'sb-ext:run-program program args filtered-rest)))
        (maybe-tex-runtime-error tex-error process NIL)
        (values output-directory process)))))

;; TODO: make conditional
(defun run-tex/trace-written-files (pathname &rest rest &key (output-directory T) (tex-error T) &allow-other-keys)
  "Same as RUN-TEX, but collects written-to files and returns them as third return value."
  (let ((directory (calculate-output-directory output-directory pathname))
        written)
    (flet ((run ()
             (run-inotify-program
              (lambda (args &rest rest)
                (multiple-value-bind (directory process)
                    (apply #'run-tex args rest)
                  (declare (ignore directory))
                  process))
              pathname
              `(:output-directory ,directory :wait NIL ,.(filter-key-args rest :function :output-directory))
              (list directory)
              :close-write
              :event-handler (lambda (event)
                               (pushnew (parse-namestring (inotify-event-name event)) written
                                        :test #'equal)))))
      (let ((process (run)))
        (maybe-tex-runtime-error tex-error process written)
        (values directory process written)))))

(defconstant +tex-output-types+
  '("pdf" "dvi" "aux" "log"))

(defconstant +tex-output-files+
  '(#P"texput.log"))

(defun tex-output-filename-type-p (filename)
  "Returns true of the type of FILENAME is one of +TEX-OUTPUT-FILENAMES+."
  (member (pathname-type filename) +tex-output-types+ :test #'string=))

(defun tex-output-filename-p (filename)
  (member filename +tex-output-files+ :test #'equal))

;; http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs

(defconstant +unix-epoch-difference+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun tex-output-filename-guess-written-files (directory pathname jobname timestamp)
  "Guesses the newly written or created files based on the JOBNAME or the
PATHNAME of the input file.  Filters for files created after TIMESTAMP and
with a type from the list +TEX-OUTPUT-TYPES+.  Returns a LIST of PATHNAMES."
  (let ((pattern (merge-pathnames
                  (make-pathname :type :wild)
                  (or jobname (pathname-name pathname))))
        result)
    (osicat:mapdir (lambda (pathname)
                     (when (and (or (pathname-match-p pathname pattern)
                                    (tex-output-filename-p pathname))
                                (tex-output-filename-type-p pathname)
                                (>= (osicat-posix:stat-mtime
                                     (osicat-posix:stat pathname))
                                    timestamp))
                       (push pathname result)))
                   directory)
    result))

(defun run-tex/guess-written-files (pathname &rest rest &key (output-directory T) jobname (tex-error T) &allow-other-keys)
  "Returns the directory where the output files were generated, the PROCESS
structure and a LIST of written filenames (as STRINGS).

See also RUN-TEX/TRACE-WRITTEN-FILES."
  (let ((directory (calculate-output-directory output-directory pathname))
        (timestamp (get-unix-time)))
    (multiple-value-bind (directory process)
        (apply #'run-tex pathname :tex-error NIL :output-directory directory rest)
      (let ((written (tex-output-filename-guess-written-files
                      directory pathname jobname timestamp)))
        (maybe-tex-runtime-error tex-error process written)
        (values directory process written)))))

(defun tex (pathname &rest rest &key (collect-written-files T) &allow-other-keys)
  "If COLLECT-WRITTEN-FILES is set, all written files are returned as third
return value.  :TRACE uses a file monitor to gather written files, :GUESS
just looks on modified files based on the file type and NIL disables it
entirely.  With T, :GUESS is used if :TRACE is not available."
  ;; use inotify if possible, else guess
  (ecase collect-written-files
    ((T :trace)
     (apply #'run-tex/trace-written-files pathname rest))
    (:guess
     (apply #'run-tex/guess-written-files pathname rest))
    ((NIL) (apply #'run-tex pathname rest))))
