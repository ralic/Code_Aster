.. Readme published for the overview of the Code_Aster SRC repository

.. note::

   This repository should still be considered under construction!
   It may be entirely re-generated.
   
   It have been rebuilt on November, 14th.


Code_Aster source files are stores in a main repository *codeaster* (not yet
published). This one is splitted into 4 subrepositories:

- `src`_: this one containing C, fortran, python source files and its build scripts,
- *tests*: most of the testcase files (will be available in the future),
- *tests_intranet*: few testcase files with proprietary datas,
- *data*: material datas that can not be freely distributed.


Other independant repositories exist:

- `devtools`_: contains helper scripts. 
- `i18n`_: repository created to share the gettext files for Code_Aster messages.


The size of the `src`_ repository is about 250 MB (for *tests*, it will be about
3,5-4 GB).

The main branches are:

* ``testing``: the frozen states of the development versions (ex.: 11.2.0).

* ``unstable``: the default branch containing all the history of the development version
  from and to the next testing state (ex.: 11.2.24).

* ``stable``: contains the states of the stable version (ex.: 10.7.0).

* ``stable-updates``: contains the history of the fix updates of the stable version
  from and to its next stable state (ex.: 10.7.5).

* ``build`` : the history of the build scripts.



.. _src: https://bitbucket.org/code_aster/codeaster-src
.. _devtools: https://bitbucket.org/code_aster/codeaster-devtools
.. _i18n: https://bitbucket.org/code_aster/codeaster-i18n
