# coding: utf-8

"""
Configuration for aster5

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=aster5 --prefix=../install/std
waf install -p
"""

YAMMROOT = '/aster/yamm/V7_3_0_201402/'

import intel

def configure(self):
    opts = self.options

    intel.configure(self)

    self.env['ADDMEM'] = 250
    self.env.append_value('OPT_ENV', [
        'module load intel_compilers/14.0.0.080'
#        'module load intel_mpi'])
                                                 ])

    self.env.append_value('LIBPATH', [
        #'/usr/lib/atlas',                           # for NumPy, see issue18751
        YAMMROOT + 'prerequisites/Hdf5_1810/lib',
        YAMMROOT + 'tools/Medfichier_307/lib',
        YAMMROOT + 'prerequisites/Mumps_20141/lib',
        YAMMROOT + 'prerequisites/Mumps_20141/libseq',
        YAMMROOT + 'prerequisites/Metis_40/lib',
        YAMMROOT + 'prerequisites/Scotch_5111/lib'])

    self.env.append_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Hdf5_1810/include',
        YAMMROOT + 'tools/Medfichier_307/include',
        YAMMROOT + 'prerequisites/Metis_40/Lib',
        YAMMROOT + 'prerequisites/Scotch_5111/include'])

    self.env.append_value('LIB', ('pthread', 'util'))

    # to fail if not found
    self.options.enable_hdf5 = True
    self.options.enable_med = True
    self.options.enable_metis = True
    self.options.enable_mumps = True
    self.options.enable_scotch = True

    opts.enable_petsc = False
