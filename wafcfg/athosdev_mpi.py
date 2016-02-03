# coding: utf-8

"""
Configuration for athosdev + Intel MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf_mpi configure --use-config=athosdev_mpi --prefix=../install/mpi
waf_mpi install -p
"""

import athosdev
ASTER_ROOT = athosdev.ASTER_ROOT
YAMMROOT = athosdev.YAMMROOT 

def configure(self):
    opts = self.options

    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    athosdev.configure(self)
    self.env['ADDMEM'] = 400

    self.env.append_value('OPT_ENV', [
        'module load intel_mpi/5.1.1.109'])

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3-1/MPI/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi-petsc_aster-1/lib',
    ])

    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3-1/MPI/include',
        YAMMROOT + '/prerequisites/Petsc_mpi-petsc_aster-1/include',
    ])

    opts.enable_petsc = True

    # allow to compile the elements catalog using the executable on one processor
    self.env['CATALO_CMD'] = 'I_MPI_FABRICS=shm'
