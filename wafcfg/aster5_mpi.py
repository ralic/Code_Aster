# coding: utf-8

"""
Configuration for aster5 + Intel MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf_mpi configure --use-config=aster5_mpi --prefix=../install/mpi
waf_mpi install -p
"""

import os
import aster5
YAMMROOT = aster5.YAMMROOT

def configure(self):
    opts = self.options

    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    aster5.configure(self)
    self.env['ADDMEM'] = 400

    self.env.append_value('OPT_ENV', [
        'module load intel_mpi'])

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + 'prerequisites/Mumps_mpi_20141/lib',        
        YAMMROOT + 'prerequisites/Petsc_mpi_petsc_aster/lib'])
    self.env.prepend_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Petsc_mpi_petsc_aster/include'])

    opts.enable_petsc = True

    # allow to compile the elements catalog using the executable on one processor
    self.env['CATALO_CMD'] = 'I_MPI_FABRICS=shm'
