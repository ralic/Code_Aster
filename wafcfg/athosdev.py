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
        'module load intel_compilers/14.0.0.144'])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas-base/atlas',                       # for NumPy, see issue18751
        YAMMROOT + '/prerequisites/Python-273/lib',
        YAMMROOT + '/prerequisites/Hdf5-1810/lib',
        YAMMROOT + '/tools/Medfichier-308/lib',
        YAMMROOT + '/prerequisites/Metis-40/lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/lib',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/SEQ/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/lib',
    ])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Python-273/include/python2.7',
        YAMMROOT + '/prerequisites/Hdf5-1810/include',
        YAMMROOT + '/tools/Medfichier-308/include',
        YAMMROOT + '/prerequisites/Metis-40/Lib',
        YAMMROOT + '/prerequisites/Mfront-TFEL203/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/SEQ/include',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster3/SEQ/include_seq',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/include',
    ])

    self.env.append_value('LIB', ('pthread', 'util'))

    # to fail if not found
    opts.enable_hdf5 = True
    opts.enable_med = True
    opts.enable_metis = True
    opts.enable_mumps = True
    opts.enable_scotch = True
    opts.enable_mfront = True

    opts.enable_petsc = False
