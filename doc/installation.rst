====================================
 Building and installing Code_Aster
====================================

Getting the sources
===================

The Code_Aster sources are under Mercurial_ version control system.
Use this command to check out the latest project source code::

    $ hg clone https://bitbucket.org/code_aster/codeaster-src

In this manual, we assume that the working directory corresponds to the Code_Aster
source folder (``cd path/to/codeaster-src``).

Required dependencies
=====================

Python 2.6 or later with development files (under Debian python-dev)

Building Code_Aster
===================

To build Code_Aster enter::

    $ ./waf configure build -p

Check the help to tune the build process::

    $ ./waf --help

For example, to build a sequential production version on Debian Squeeze enter::

    $ INCLUDES="/usr/include/mpi /usr/include/scotch" \
    LIBPATH=/usr/lib/gcc/x86_64-linux-gnu/4.3 \
    ./waf configure build -p \
    --mumps-libs="dmumps_seq zmumps_seq smumps_seq cmumps_seq mumps_common_seq \
                  pord_seq mpiseq_seq"


For more build examples, have a look at the appendix.

Installing Code_Aster
=====================

To install Code_Aster enter::

    $ ./waf install

You can build and install with a single command::

   $ ./waf configure build install

Locations can be configured with options. For example, to install in a custom
place (``install`` folder in the current directory for instance)::

    $ ./waf configure install --prefix=$PWD/install

Then you must set your environment variables, e.g. (with python 2.6)::

    $ export ASTERPATH=$PWD/install/share/aster
    $ export PYTHONPATH=$PWD/install/lib/python2.6/site-packages
    $ export LD_LIBRARY_PATH=$PWD/install/lib
    $ export PATH=$PWD/install/bin:$PATH

Running the validation tests
============================

To run a single test, use::

    $ ./waf test <testcasename>

To run all the test cases, get the devtools and run the script::

    $ hg clone https://bitbucket.org/code_aster/codeaster-devtools
    $ cd codaster-devtools
    $ ./bin/run_testcases

Appendices
==========

Appendix: examples for building libaster with different configurations
----------------------------------------------------------------------

Building sequential debugging version on Debian::

    $ INCLUDES="/usr/include/mpi /usr/include/scotch" \
    LIBPATH=/usr/lib/gcc/x86_64-linux-gnu/4.3 \
    ./waf configure build -p --debug -o build_dbg \
    --mumps-libs="dmumps_seq zmumps_seq smumps_seq cmumps_seq mumps_common_seq \
                  pord_seq mpiseq_seq"

Building parallel version on Debian with petsc support::

    $ INCLUDES="/usr/include/mpi" ./waf configure build -p --parallel \
    --maths-libs='scalapack-openmpi blacs-openmpi' --enable-petsc

Building sequential production version on Debian with Python2.6::

    $ INCLUDES="/usr/include/mpi" PYTHON="python2.6" \
    ./waf configure build -p -o py2.6-build \
    --mumps-libs="dmumps_seq zmumps_seq smumps_seq cmumps_seq \
                  mumps_common_seq pord_seq mpiseq_seq"

Building debugging version on ArchLinux with python2.7 and without med, scotch
or mumps::

    $ INCLUDES='/usr/include/mpi
    /usr/lib/python2.7/site-packages/numpy/core/include' \
    PYTHON=/usr/bin/python2 \
    python2 waf configure build -p\
    --disable-med --disable-scotch --disable-mumps \
    -o build_arch --debug

Appendix: Installing non free or legacy dependencies
----------------------------------------------------

In case you want to pass all the test cases, some dependencies are
required. Their source can be found in the main aster package [2].
The examples are given with the following installation tools directory
as an example: ``/opt/users/dede/outils``

For installing the EDF metis version, the steps are::

    $ tar xzf metis-edf-4.1-2.noarch.tar.gz
    $ cd metis-edf-4.1/
    $ FFLAGS='-O2 -fdefault-double-8  -fdefault-integer-8  -fdefault-real-8' \
    CFLAGS='-O2' make
    $ cp onmetis.exe onmetis kmetis /opt/users/dede/outils

For installing Homard, the steps are::

    $ tar xzf homard-9.8-1.all.tar.gz
    $ cd homard-9.8
    $ echo "PYTHON | python | 2.4 | /usr/bin/python2.6" > config.txt
    $ echo "REPOUT | exec   | 04  | /opt/users/dede/outils" >> config.txt
    $ echo $PWD/config.txt | python setup_homard.py
    $ rm config.txt

XMgrace is a package available on Debian (called grace), however the command
must be found in the tools directory::

    $ ln -s /usr/bin/xmgrace /opt/users/dede/outils
    $ ln -s /usr/bin/gracebat /opt/users/dede/outils

For installing Astk, still used by some macro commands written in Python,
the steps are::

    $ tar xzf astk-1.8.4.tar.gz
    $ cd astk-1.8.4
    $ python setup.py install --prefix=/opt/users/dede/outils
    $ export ASTER_ROOT=/opt/users/dede/outils

For installing Gibi, the steps are::

    $ tar xzf gibi-2000-6.i686.tar.gz
    $ mv gibi-2000 /opt/users/dede/outils/
    $ sed -i 's:?HOME_GIBI?:/opt/users/dede/outils/gibi-2000:' \
    /opt/users/dede/outils/gibi-2000/gibi_aster.py
    $ sed -i 's:?PYTHON_EXE?:/usr/bin/python:' \
    /opt/users/dede/outils/gibi-2000/gibi_aster.py
    $ ln -s /opt/users/dede/outils/gibi-2000/gibi_aster.py  \
    /opt/users/dede/outils/gibi

Gmsh is a package available on Debian, the command just needs to be found in
the tools directory::

    $ ln -s /usr/bin/gmsh /opt/users/dede/outils

Appendix: running tests with ASTK for comparing results
-------------------------------------------------------

Running test in a dev machine::

    $ cp dbg-build/aster /usr/lib/codeaster/STA10.3/asteru_py2.6
    $ sed -i back -e 's:\(.*liste_internet.*\):#\1:' \
    -e 's:#\(.*liste_short_without_homard.*\):\1:' \
    /usr/lib/codeaster/STA10.3/astout.export
    $ as_run --run /usr/lib/codeaster/STA10.3/astout.export

[1] http://www.code-aster.org/V2/doc/default/man_u/u2/u2.10.01.pdf
[2] You can get them at the address:
http://www.code-aster.org/V2/UPLOAD/DOC/Telechargement/aster-full-src-10.3.0-3.noarch.tar.gz
