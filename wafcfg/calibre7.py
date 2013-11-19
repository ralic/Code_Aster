# coding: utf-8

"""
Configuration for Calibre 7

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=calibre7 --prefix=../install/std
waf install -p
"""

YAMMROOT = '/home/aster/yamm/V7_main/'

import intel

def configure(self):
    from Options import options as opts

    intel.configure(self)

    self.env['ADDMEM'] = 250
    self.env.append_value('OPT_ENV', [
        '. /home/aster/etc/codeaster/profile.sh',
        '. /home/aster/etc/codeaster/profile_intel-12.sh',
        '. /home/aster/etc/codeaster/profile_zmat.sh',
        '. /home/aster/etc/codeaster/profile_mfront.sh'])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas-base/atlas',                # for NumPy, see issue18751
        YAMMROOT + 'prerequisites/Python_273/lib',
        YAMMROOT + 'prerequisites/Hdf5_1810/lib',
        YAMMROOT + 'tools/Medfichier_307rc1/lib',
        YAMMROOT + 'prerequisites/Mumps_4100_aster/lib',
        YAMMROOT + 'prerequisites/Mumps_4100_aster/libseq',
        YAMMROOT + 'prerequisites/Metis_40/lib',
        YAMMROOT + 'prerequisites/Scotch_5111/lib'])

    self.env.append_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Python_273/include/python2.7',
        YAMMROOT + 'prerequisites/Hdf5_1810/include',
        YAMMROOT + 'tools/Medfichier_307rc1/include',
        YAMMROOT + 'prerequisites/Metis_40/Lib',
        YAMMROOT + 'prerequisites/Scotch_5111/include'])

    opts.enable_petsc = False
