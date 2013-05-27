.. Readme published for the overview of the Code_Aster SRC repository


Code_Aster source files are stores three repositories:

- `src`_: this one containing C, fortran, python source files and its
  build scripts,
- *validation*: few testcase files with proprietary datas,
- *data*: material datas that can not be freely distributed.


Other independant repositories exist:

- `devtools`_: contains helper scripts. 
- `i18n`_: repository created to share the gettext files for Code_Aster
  messages.


The size of the `src`_ repository is about 350 MB (for *validation*, it is
about 1 GB).

The main branches are:

* ``default``: the default branch containing all the history of the development
  version from and to the next testing state (ex.: 11.3.24).

* ``v11``: maintenance branch for the version 11, starting from 11.4.0.

The main tags are:

* ``stable``: the last frozen state of the stable version in the
  current maintenance branch (ex. 11.4.0).

* ``testing``: the last frozen state of the development version in the
  ``default`` branch (ex. 12.1.0).

* ``unstable``: the last state of the development version (ex. 12.1.3).

* ``stable-updates``: the last state of the stable version (ex. 11.4.2). It is
  a snapshot between two successive ``stable`` versions.


.. _src: https://bitbucket.org/code_aster/codeaster-src
.. _devtools: https://bitbucket.org/code_aster/codeaster-devtools
.. _i18n: https://bitbucket.org/code_aster/codeaster-i18n
