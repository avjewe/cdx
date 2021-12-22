Command line data mining refers to using text files and shell scripts to do data manipulation,
rather than database systems and complex formats. As it turns out, this is not only simple and powerful,
but more performant in many situations.

Command line data mining tools have a dual nature. They are excellent for large production systems,
where terabytes of data are constantly being processed by robust fault tolerant scripts.
They also excel at ad-hoc exploration of data from a live command line.

The command line tool `cdx` is a set of tools reminiscent of the old gnu textutils (cut, sort, join et al) 
but far more flexible and powerful. The tools are well documented at (https://avjewe.github.io/cdxdoc).

The “main” program for each tool is quite simple; parsing command line arguments and assembling 
functionality from a broad toolbox. If no tools quite meets your needs, it should be quite 
simple to roll your own.
