Shared-Space
============

This tool provides a quick analysis of shared space usage between
trees of hard-linked files.

For example, if using `rsync` with the `--link-dest` parameter to take
full-system backups, this creates fairly large trees that each
superficially (e.g. with `du`) appear to be the size of a single
full-backup.  The total of all the trees is much smaller, as typically
there is massive sharing of data between them.  Concisely answering
the question of how much data is shared or unshared within a specific
tree-root is difficult to do without more specialized tools.

This is that tool.

Example usage
-------------

```
$ time sudo dist/build/shared-space/shared-space /tbackup/chrysalis-201*
Root: FilePath "/tbackup/chrysalis-20150722-222942Z/"
  unshared file count:   47.29 k
  unshared file  size:   10.27 GiBytes
  unshared  dir count:   93.46 k
  unshared  dir  size:  376.41 MiBytes
    shared file count:  521.67 k
    shared file  size:  205.09 GiBytes
Root: FilePath "/tbackup/chrysalis-20150807-194733Z/"
  unshared file count:   35.66 k
  unshared file  size:    5.23 GiBytes
  unshared  dir count:   95.91 k
  unshared  dir  size:  386.63 MiBytes
    shared file count:  563.37 k
    shared file  size:  221.06 GiBytes
Root: FilePath "/tbackup/chrysalis-20151018-082220Z/"
  unshared file count:  117.30 k
  unshared file  size:   30.46 GiBytes
  unshared  dir count:   68.37 k
  unshared  dir  size:  278.55 MiBytes
    shared file count:  486.38 k
    shared file  size:  170.71 GiBytes
Root: FilePath "/tbackup/chrysalis-20160310-182623Z/"
  unshared file count:  265.30 k
  unshared file  size:   63.79 GiBytes
  unshared  dir count:   75.21 k
  unshared  dir  size:  305.50 MiBytes
    shared file count:  377.64 k
    shared file  size:  134.94 GiBytes
Grand total:
  unshared file count:  465.56 k
  unshared file  size:  109.77 GiBytes
  unshared  dir count:  332.96 k
  unshared  dir  size:    1.31 GiBytes
    shared file count:  614.33 k
    shared file  size:  226.64 GiBytes

real	0m52.998s
user	0m45.204s
sys	0m7.720s
```

