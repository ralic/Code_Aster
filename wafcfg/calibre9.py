# coding: utf-8

"""
Configuration for Calibre 9

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=calibre9 --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']
YAMMROOT = ASTER_ROOT + '/public/V7_6_0_201510'

def configure(self):
    opts = self.options

    self.env['ADDMEM'] = 250 

    self.env.append_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Hdf5-1810/lib',
        YAMMROOT + '/tools/Medfichier-308/lib',
        YAMMROOT + '/prerequisites/Metis-40/lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/lib',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/lib',
    ])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Hdf5-1810/include',
        YAMMROOT + '/tools/Medfichier-308/include',
        YAMMROOT + '/prerequisites/Metis-40/Lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/include_seq',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/include',
    ])

    # to fail if not found
    opts.enable_hdf5 = True
    opts.enable_med = True
    opts.enable_metis = True
    opts.enable_mumps = True
    opts.enable_scotch = True
    opts.enable_mfront = True

    opts.enable_petsc = False
