# coding: utf-8

"""
Configuration for Calibre 7 + Intel MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf configure --use-config=calibre7_mpi --prefix=../install/mpi
waf install -p
"""

import calibre7
YAMMROOT = calibre7.YAMMROOT + '_mpi'

def configure(self):
    opts = self.options

    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    calibre7.configure(self)
    self.env['ADDMEM'] = 400

    self.env.append_value('OPT_ENV', [
        '. /home/aster/etc/codeaster/profile_impi.sh'])

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps_mpi_20151/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/lib'])
    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/include'])

    opts.enable_petsc = True
