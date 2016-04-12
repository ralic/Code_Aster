# coding: utf-8

"""
Configuration for Calibre 7 + Intel MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf configure --use-config=calibre7_mpi --prefix=../install/mpi
waf install -p
"""

import calibre7
YAMMROOT = calibre7.YAMMROOT

def configure(self):
    opts = self.options

    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    # because of incompatibility between openmp and mpi on calibre7
    opts.openmp = False
    calibre7.configure(self)
    self.env['ADDMEM'] = 400

    self.env.append_value('OPT_ENV', [
        '. /home/aster/etc/codeaster/profile_impi.sh'])

    self.env['OPTLIB_FLAGS'] = [
        '-Wl,-Bstatic', '-Wl,--start-group',
        '-lmkl_intel_lp64', '-lmkl_sequential', '-lmkl_core',
        '-lmkl_scalapack_lp64', '-lmkl_blacs_intelmpi_lp64',
        '-Wl,--end-group']

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster5/MPI/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi-petsc-3.6.3_aster/lib',
    ])

    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster5/MPI/include',
        YAMMROOT + '/prerequisites/Petsc_mpi-petsc-3.6.3_aster/include',
    ])

    opts.enable_petsc = True
