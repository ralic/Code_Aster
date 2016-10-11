# coding: utf-8

"""
Configuration for Calibre 9  MPI

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf_mpi configure --use-config=calibre9_mpi --prefix=../install/mpi
waf_mpi install -p
"""

import calibre9
YAMMROOT = calibre9.YAMMROOT

def configure(self):
    opts = self.options

    opts.parallel = True
    calibre9.configure(self)
    self.env['ADDMEM'] = 400

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster5/MPI/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi-373_aster/lib',
    ])

    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster5/MPI/include',
        YAMMROOT + '/prerequisites/Petsc_mpi-373_aster/include',
    ])

    opts.enable_petsc = True
