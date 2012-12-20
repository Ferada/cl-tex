<!-- -*- mode: markdown; -->

This is a package to run TeX and its derivatives while also collecting
additional information about the generated files and so forth.

While we provide manual methods by which the renderer can be run, it is
far more worthwile to use some sort of file monitor facility like
CL-INOTIFY, which allows us to monitor the run, looking for interesting
files.

Currently runs only SBCL, although there are not many reasons for this
restriction.

Requires cl-inotify and osicat in particular.  A replacement for
`SB-EXT:RUN-PROGRAM` would be nice, although it would require
integration with an event handler library, e.g. iolib.

# USAGE

Provide the filename and the defaults should be sensible
(i.e. `pdflatex` with PDF output).  Otherwise there are many things to
configure; you may even supply additional parameters to the underlying
`RUN-PROGRAM`, or a similar facility to execute other programs.

    > (tex "filename")
    => "/home/username/"
       #<SB-IMPL::PROCESS :EXITED 0>
       (#P"filename.log" #P"filename.pdf" #P"filename.aux")

Errors are raised if the corresponding parameter `:TEX-ERROR` is set
(`:ERROR` is already used by the underlying `RUN-PROGRAM`).  Currently
the condition `TEX-RUNTIME-ERROR` for problems concerning the process
structure exists.  The slots `PROCESS` contains the `SB-IMPL::PROCESS`
structure and `WRITTEN-FILES` contains a `LIST` of written files, if
those were collected via the `:COLLECT-WRITTEN-FILES` parameter (`T`
by default).

    > (tex "no-such-filename")
    => TeX quit with non-zero exit status.
          [Condition of type TEX-RUNTIME-ERROR]
