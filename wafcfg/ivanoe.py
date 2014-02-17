# coding: utf-8

"""
Configuration for ivanoe

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=ivanoe --prefix=../install/std
waf install -p -j 8
"""

YAMMROOT = '/home/projets/aster/yamm/V7_3_0_201402/'

import intel

def configure(self):
    opts = self.options

    intel.configure(self)

    self.env['ADDMEM'] = 280
    self.env.append_value('OPT_ENV', [
        '. /home/projets/aster/etc/codeaster/profile.sh',
        '. /home/projets/aster/etc/codeaster/profile_intel.sh',
        '. /home/projets/aster/etc/codeaster/profile_zmat.sh'])

    self.env.append_value('LIBPATH', [
        '/usr/lib/atlas-base/atlas',                # for NumPy, see issue18751
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

    # to fail if not found
    self.options.enable_hdf5 = True
    self.options.enable_med = True
    self.options.enable_metis = True
    self.options.enable_mumps = True
    self.options.enable_scotch = True

    opts.enable_petsc = False
