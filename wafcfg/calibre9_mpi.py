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
    self.env['ADDMEM'] = 500

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Parmetis_aster-403_aster/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster6/MPI/lib',
        YAMMROOT + '/prerequisites/Mumps-502_consortium_aster1/MPI/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi-373_aster/lib',
    ])

    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Parmetis_aster-403_aster/include',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster6/MPI/include',
        YAMMROOT + '/prerequisites/Mumps-502_consortium_aster1/MPI/include',
        YAMMROOT + '/prerequisites/Petsc_mpi-373_aster/include',
    ])

    opts.enable_petsc = True    
    self.env.append_value('LIB_METIS', ('parmetis'))
    self.env.append_value('LIB_SCOTCH', ('ptscotch','ptscotcherr','ptscotcherrexit'))


