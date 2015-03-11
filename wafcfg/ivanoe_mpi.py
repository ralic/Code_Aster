# coding: utf-8

"""
Configuration for ivanoe_mpi

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf configure --use-config=ivanoe_mpi --prefix=../install/mpi
waf install -p -j 8
"""

import ivanoe
ASTER_ROOT = ivanoe.ASTER_ROOT
YAMMROOT = ivanoe.YAMMROOT + '_mpi'

def configure(self):
    opts = self.options

    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    ivanoe.configure(self)
    self.env['ADDMEM'] = 400

    self.env.append_value('OPT_ENV', [
        '. ' + ASTER_ROOT + '/etc/codeaster/profile_impi.sh'])

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps_mpi_20151/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/lib',
        '/logiciels/intel/composerxe-2011.3.174/mkl/lib/intel64'])
    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/include'])

    opts.enable_petsc = True
