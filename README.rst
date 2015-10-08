.. Readme published for the overview of the Code_Aster SRC repository

:home page: http://www.code-aster.org/
:forge: https://bitbucket.org/code_aster/
:dev documentation: https://bitbucket.org/code_aster/codeaster-src/src/default/doc/
:user documentation: http://www.code-aster.org/V2/spip.php?rubrique5

Repositories
============

Code_Aster source files are stored into several repositories:

- `src`_: source files (C, fortran, python), most of the verification testcases and build scripts (~350MB)
- *validation*: a few testcase files with non-public data (~1GB)
- *data*: data about materials that can not be freely distributed
- `perf`_: this one containing some testcase files used for performance
  benchmarking.


Other independent repositories exist:

- `devtools`_: contains helper scripts for the developers


Branches and tags
=================

The branches are:

* ``default``: currently version 13, the development branch where the current work goes

* ``v12``: version 12, the maintenance branch for the stable version 12

* ``v11``: version 10, the branch for the old stable version 11

Each published version is tagged with its number. Examples: 12.3.8, 13.0.9.

Two tags are used aliases and moved when new versions are published:

* ``stable``: the latest frozen state of the stable version in the
  maintenance branch (ex. 12.4.0).

* ``testing``: the latest frozen state of the development version in the
  development branch (ex. 13.1.0).

Two names are often used in discussions to identify the code during its
enhancements:

* ``unstable``: the head of the development branch

* ``stable-updates``: the head of the maintenance branch

.. _src: https://bitbucket.org/code_aster/codeaster-src
.. _perf: https://bitbucket.org/code_aster/codeaster-perf
.. _devtools: https://bitbucket.org/code_aster/codeaster-devtools

Installation
============

Look at the doc/ directory in the source tree.
