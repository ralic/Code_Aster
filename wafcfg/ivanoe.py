# coding: utf-8

"""
Configuration for ivanoe

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=ivanoe --prefix=../install/std
waf install -p -j 8
"""

YAMMROOT = '/home/projets/aster/yamm/V7_main/'

import intel
import mkl64

def configure(self):
    from Options import options as opts

    intel.configure(self)
    mkl64.configure(self)
    # path required on ivanoe
    # perhaps because LIBRARY_PATH is not set loading mkl modules
    self.env.prepend_value('OPTLIB_FLAGS',
        '-L/logiciels/intel/composerxe-2011.3.174/mkl/lib/intel64')

    self.env['ADDMEM'] = 280
    self.env.append_value('OPT_ENV', [
        '. /home/projets/aster/etc/codeaster/profile.sh',
        '. /home/projets/aster/etc/codeaster/profile_intel.sh',
        '. /home/projets/aster/etc/codeaster/profile_zmat.sh'])

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

    self.env.append_value('LIB', ('pthread', 'util'))

    opts.enable_med = True

    opts.enable_mumps = True
    opts.mumps_version = '4.10.0'
    opts.mumps_libs = 'dmumps zmumps smumps cmumps mumps_common pord metis'
    opts.embed_mumps = True

    opts.enable_petsc = False

    opts.enable_scotch = True
    opts.embed_scotch = True
    
    opts.embed_aster = True
    opts.embed_fermetur = True
