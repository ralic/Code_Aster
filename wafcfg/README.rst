
`waf`_ configuration files
--------------------------

This directory contains configuration files used by `waf`_ to build `Code_Aster`_.
See the wiki on `bitbucket`_ for more informations.

It contains generic script (as ``intel.py``) and some files daily used to build
the last development version on EDF servers.
These files can be also used as examples for other servers.

This repository should be written for everyone. You can share your own
configuration files and/or use the others as examples.

The naming convention: ``<hostname or distro>_<variant>.py``

Note that several files can be used to build a variant.
For example::

    waf configure --use-config=intel,aster4,aster4_mpi --prefix=.....

This will define the compilers to be ``icc`` and ``ifort`` (including ``intel``),
load the ``aster4`` configuration and just add few values for a MPI parallel build
(through ``aster4_mpi``).

This combinaison can be replaced by importing the mpdules.
For example: ``aster4.py`` contains::

    import intel
    
    def configure(self):
        intel.configure(self)
        ...


PLEASE DO NOT CHANGE the files related to the verification servers:

- ``aster4.py``

- ``calibre7.py``

- ``clap0f0q.py``

- ``ivanoe.py``

- and their variants.


.. _Code_Aster: http://www.code-aster.org
.. _waf: http://code.google.com/p/waf/
.. _bitbucket: https://bitbucket.org/code_aster/
