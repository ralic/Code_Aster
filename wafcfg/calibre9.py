# coding: utf-8

"""
Configuration for Calibre 9

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=calibre9 --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']
YAMMROOT = os.environ['ROOT_SALOME']

def configure(self):
    opts = self.options

    self.env.append_value('CXXFLAGS', ['-D_GLIBCXX_USE_CXX11_ABI=0'])

    self.env['ADDMEM'] = 350

    self.env.append_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Hdf5-1814/lib',
        YAMMROOT + '/tools/Medfichier-320/lib',
        YAMMROOT + '/prerequisites/Metis_aster-510_aster1/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster6/SEQ/lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/lib',
        YAMMROOT + '/prerequisites/Mumps-502_consortium_aster1/SEQ/lib',
    ])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Hdf5-1814/include',
        YAMMROOT + '/tools/Medfichier-320/include',
        YAMMROOT + '/prerequisites/Metis_aster-510_aster1/include',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster6/SEQ/include',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/include',
        YAMMROOT + '/prerequisites/Mumps-502_consortium_aster1/SEQ/include',
        YAMMROOT + '/prerequisites/Mumps-502_consortium_aster1/SEQ/include_seq',
    ])

    self.env.append_value('LIB', ('pthread', 'util'))
    self.env.append_value('LIB_SCOTCH', ('scotcherrexit'))
    # to fail if not found
    opts.enable_hdf5 = True
    opts.enable_med = True
    opts.enable_metis = True
    opts.enable_mumps = True
    opts.enable_scotch = True
    opts.enable_mfront = True

    opts.enable_petsc = False

