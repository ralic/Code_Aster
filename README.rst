.. Readme published for the overview of the Code_Aster SRC repository

:home page: http://www.code-aster.org/
:forge: https://bitbucket.org/code_aster/
:dev documentation: https://bitbucket.org/code_aster/codeaster-src/src/default/doc/
:user documentation: http://www.code-aster.org/V2/spip.php?rubrique5

Repositories
============

Code_Aster source files are stored into three repositories:

- `src`_: source files (C, fortran, python) and build scripts (~350MB)
- *validation*: a few testcase files with non-public data (~1GB)
- *data*: data about materials that can not be freely distributed

Other independent repositories exist:

- `devtools`_: contains helper scripts
- `i18n`_: repository created to share the gettext files for Code_Aster
  messages

Branches and tags
=================

The branches are:

* ``default``: version 12, the development branch where the current work goes

* ``v11``: version 11, the maintenance branch for the stable version 11

* ``v10``: version 10, the branch for the old stable version 10

Each published version is tagged with its number. Examples: 11.3.22, 12.1.13.

Two tags are used aliases and moved when new versions are published:

* ``stable``: the latest frozen state of the stable version in the
  maintenance branch (ex. 11.4.0).

* ``testing``: the latest frozen state of the development version in the
  development branch (ex. 12.1.0).

Two names are often used in discussions to identify the code during its
enhancements:

* ``unstable``: the head of the development branch

* ``stable-updates``: the head of the maintenance branch

.. _src: https://bitbucket.org/code_aster/codeaster-src
.. _devtools: https://bitbucket.org/code_aster/codeaster-devtools
.. _i18n: https://bitbucket.org/code_aster/codeaster-i18n

Installation
============

Look at the doc/ directory in the source tree.
