# coding: utf-8

"""
Configuration for aster5

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=aster5 --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']
YAMMROOT = ASTER_ROOT + '/public/V7_5_1_201502'

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
        YAMMROOT + '/prerequisites/Python_273/lib',
        YAMMROOT + '/prerequisites/Hdf5_1810/lib',
        YAMMROOT + '/tools/Medfichier_308/lib',
        YAMMROOT + '/prerequisites/Metis_40/lib',
        YAMMROOT + '/prerequisites/Mfront_TFEL201/lib',
        YAMMROOT + '/prerequisites/Mumps_20151/lib',
        YAMMROOT + '/prerequisites/Scotch_5111/lib'])

    self.env.append_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Python_273/include/python2.7',
        YAMMROOT + '/prerequisites/Hdf5_1810/include',
        YAMMROOT + '/tools/Medfichier_308/include',
        YAMMROOT + '/prerequisites/Metis_40/Lib',
        YAMMROOT + '/prerequisites/Mfront_TFEL201/include',
        YAMMROOT + '/prerequisites/Scotch_5111/include'])

    self.env.append_value('LIB', ('pthread', 'util'))

    # to fail if not found
    opts.enable_hdf5 = True
    opts.enable_med = True
    opts.enable_metis = True
    opts.enable_mumps = True
    opts.enable_scotch = True
    opts.enable_mfront = True

    opts.enable_petsc = False
