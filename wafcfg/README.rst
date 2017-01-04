
`waf`_ configuration files
--------------------------

This directory contains configuration files used by `waf`_ to build `Code_Aster`_.
See the wiki on `bitbucket`_ for more informations.

It contains generic script (as ``intel.py``) and some files daily used to build
the last development version on EDF servers.
These files can be also used as examples for other servers.

The naming convention: ``<hostname or distro>_<variant>.py``

Note that several files can be used to build a variant.
For example::

    waf configure --use-config=intel,aster,aster_mpi --prefix=.....

This will define the compilers to be ``icc`` and ``ifort`` (including ``intel``),
load the ``aster`` configuration and just add few values for a MPI parallel build
(through ``aster_mpi``).

This combinaison can be replaced by importing the modules.
For example: ``aster.py`` contains::

    import intel

    def configure(self):
        intel.configure(self)
        ...


PLEASE DO NOT CHANGE the files related to the verification servers:

- ``aster*.py``

- ``athosdev*.py``

- ``calibre*.py``


.. _Code_Aster: http://www.code-aster.org
.. _waf: https://waf.io/
.. _bitbucket: https://bitbucket.org/code_aster/
