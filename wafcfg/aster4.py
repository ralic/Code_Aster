# coding: utf-8

"""
Configuration for aster4

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=aster4 --prefix=../install/std
waf install -p
"""

YAMMROOT = '/aster/yamm/V7_3_0/'

import intel

def configure(self):
    opts = self.options

    intel.configure(self)

    self.env['ADDMEM'] = 250
    self.env.append_value('OPT_ENV', [
        '. /aster/etc/codeaster/profile.sh',
        '. /aster/etc/codeaster/profile_intel-11.sh',
        '. /aster/etc/codeaster/profile_zmat.sh',
        '. /aster/etc/codeaster/profile_mfront.sh'])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas',                           # for NumPy, see issue18751
        YAMMROOT + 'prerequisites/Python_273/lib',
        YAMMROOT + 'prerequisites/Hdf5_1810/lib',
        YAMMROOT + 'tools/Medfichier_307/lib',
        YAMMROOT + 'prerequisites/Mumps_for_aster/lib',
        YAMMROOT + 'prerequisites/Mumps_for_aster/libseq',
        YAMMROOT + 'prerequisites/Metis_40/lib',
        YAMMROOT + 'prerequisites/Scotch_5111/lib'])

    self.env.append_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Python_273/include/python2.7',
        YAMMROOT + 'prerequisites/Hdf5_1810/include',
        YAMMROOT + 'tools/Medfichier_307/include',
        YAMMROOT + 'prerequisites/Metis_40/Lib',
        YAMMROOT + 'prerequisites/Scotch_5111/include'])

    self.env.append_value('LIB', ('pthread', 'util'))

    opts.enable_petsc = False
