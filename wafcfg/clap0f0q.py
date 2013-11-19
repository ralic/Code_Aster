# encoding: utf-8

"""
Configuration for clap0f0q (gfortran + openblas)

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=clap0f0q --prefix=../install/std
waf install -p
"""

YAMMROOT = '/home/aster/yamm/V7_main/'

def configure(self):
    from Options import options as opts
    self.env['ADDMEM'] = 300
    self.env.append_value('OPT_ENV', [
        '. /home/aster/etc/codeaster/profile.sh',
        '. /home/aster/etc/codeaster/profile_gcc47.sh'])

    self.env.append_value('LIBPATH', [
        YAMMROOT + 'prerequisites/Python_273/lib',
        YAMMROOT + 'prerequisites/Hdf5_1810/lib',
        YAMMROOT + 'tools/Medfichier_307rc1/lib',
        YAMMROOT + 'prerequisites/Mumps_4100_aster/lib',
        YAMMROOT + 'prerequisites/Mumps_4100_aster/libseq',
        YAMMROOT + 'prerequisites/Metis_40/lib',
        YAMMROOT + 'prerequisites/Scotch_5111/lib',
        '/home/aster/public/lib',])

    self.env.append_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Python_273/include/python2.7',
        YAMMROOT + 'prerequisites/Hdf5_1810/include',
        YAMMROOT + 'tools/Medfichier_307rc1/include',
        YAMMROOT + 'prerequisites/Metis_40/Lib',
        YAMMROOT + 'prerequisites/Scotch_5111/include'])

    opts.enable_petsc = False
