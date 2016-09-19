# coding: utf-8

"""
Configuration for aster5

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=aster5 --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']

YAMMROOT = ASTER_ROOT + '/public/default'

import intel

def configure(self):
    opts = self.options

    intel.configure(self)

    # enable TEST_STRICT on the reference server
    self.env.append_value('DEFINES', ['TEST_STRICT'])

    self.env['ADDMEM'] = 250
    self.env.append_value('OPT_ENV', [
        '. /etc/profile.d/003_modules.sh',
        'module load intel_compilers/16.0.0.109 '])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas-base/atlas',                       # for NumPy, see issue18751
        YAMMROOT + '/prerequisites/Python-273/lib',
        YAMMROOT + '/prerequisites/Hdf5-1814/lib',
        YAMMROOT + '/tools/Medfichier-320/lib',
        YAMMROOT + '/prerequisites/Metis_aster-510_aster/lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203-1/lib',
        '/home/A19043/MUMPS_SNAPSHOT-2015-07-23consortium_LR/lib',
        '/home/A19043/MUMPS_SNAPSHOT-2015-07-23consortium_LR/libseq',
        '/home/A19043/parmetis-4.0.3/Lib_parmetis4/lib',
        '/home/A19043/scotch_6.0.4/lib',
    ])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Python-273/include/python2.7',
        YAMMROOT + '/prerequisites/Hdf5-1814/include',
        YAMMROOT + '/tools/Medfichier-320/include',
        YAMMROOT + '/prerequisites/Metis_aster-510_aster/include',
        YAMMROOT + '/prerequisites/Mfront-TFEL203-1/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster4/SEQ/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster4/SEQ/include_seq',
        '/home/A19043/scotch_6.0.4/include',
        '/home/A19043/parmetis-4.0.3/include',
        '/home/A19043/parmetis-4.0.3/Lib_parmetis4/include',
        '/home/A19043/parmetis-4.0.3/metis/GKlib',
        '/home/A19043/parmetis-4.0.3/metis/programs',
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
