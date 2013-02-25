<!-- -*- mode: markdown; fill-column: 72; coding: utf-8-unix; -->

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

Using `:INPUT-FORMAT` and `:OUTPUT-FORMAT` the best path through all
renderers and postprocessing tools may be automatically derived.

The input format is determined by looking at the first 4k byte of the
file and looking for patterns (currently only `"documentclass"` to check
for LaTeX, everything else is regarded as TeX files).

E.g. given a regular LaTeX file the following interaction allows a
succinct way to describe the desired output:

    > (tex "data/test-2.tex" :output-format :ps)
    => #P"test-2.ps"
       #P"/home/user/src/"
       #<SB-IMPL::PROCESS :EXITED 0>
       (#P"test-2.aux" #P"test-2.pdf" #P"test-2.log" #P"test-2.ps")

Behind the scenes the best path was derived as
`:LUALATEX :PDF :PDFTOPS`, which means running LuaLaTeX producing a PDF
file (which is also the default for that renderer) and then
postprocessing the output with `"pdftops"` (or `"pdf2ps"`, if that
program wasn't available on the machine).

This facility is quite flexible and may even be efficient if some more
work is put into caching the results of the program lookup in the
environment `PATH` variable.  The current render route only allows for
one conversion at all, so it would need to be fixed for machines without
the necessary direct converters.
