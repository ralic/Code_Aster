# coding: utf-8

"""
Configuration for aster5 + Intel MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf_mpi configure --use-config=aster5_mpi --prefix=../install/mpi
waf_mpi install -p
"""

# currently exactly the same (revert in history if changes are needed)
from athosdev_mpi import configure
