# coding: utf-8

"""
Configuration for Calibre 7

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=calibre7 --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']
YAMMROOT = ASTER_ROOT + '/public/V7_6_0_201509'

import intel

def configure(self):
    opts = self.options

    intel.configure(self)

    self.env['ADDMEM'] = 250
    self.env.append_value('OPT_ENV', [
        '. ' + ASTER_ROOT + '/etc/codeaster/profile.sh',
        '. ' + ASTER_ROOT + '/etc/codeaster/profile_intel-12.sh'])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas-base/atlas',                       # for NumPy, see issue18751
        YAMMROOT + '/prerequisites/Python-273/lib',
        YAMMROOT + '/prerequisites/Hdf5-1810/lib',
        YAMMROOT + '/tools/Medfichier-308/lib',
        YAMMROOT + '/prerequisites/Metis-40/lib',
        # YAMMROOT + '/prerequisites/Mfront-TFEL203/lib',
        YAMMROOT + '/../V7_6_0_201506/prerequisites/Mfront-TFEL202/lib',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster2/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/lib',
    ])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Python-273/include/python2.7',
        YAMMROOT + '/prerequisites/Hdf5-1810/include',
        YAMMROOT + '/tools/Medfichier-308/include',
        YAMMROOT + '/prerequisites/Metis-40/Lib',
        # YAMMROOT + '/prerequisites/Mfront-TFEL203/include',
        YAMMROOT + '/../V7_6_0_201506/prerequisites/Mfront-TFEL202/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster2/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster2/include_seq',
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
