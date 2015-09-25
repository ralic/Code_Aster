# encoding: utf-8

"""
Configuration for clap0f0q (gfortran + openblas)

. $HOME/dev/codeaster/devtools/etc/env_unstable.sh

waf configure --use-config=clap0f0q --prefix=../install/std
waf install -p
"""

import os
ASTER_ROOT = os.environ['ASTER_ROOT']
YAMMROOT = ASTER_ROOT + '/public/V7_6_0_201509'

def configure(self):
    opts = self.options

    self.env['ADDMEM'] = 300
    self.env.append_value('OPT_ENV', [
        '. ' + ASTER_ROOT + '/etc/codeaster/profile.sh',
        '. ' + ASTER_ROOT + '/etc/codeaster/profile_gcc47.sh'])

    self.env.append_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Python-273/lib',
        YAMMROOT + '/prerequisites/Hdf5-1810/lib',
        YAMMROOT + '/tools/Medfichier-308/lib',
        YAMMROOT + '/prerequisites/Metis-40/lib',
        # YAMMROOT + '/prerequisites/Mfront-TFEL203/lib',
        YAMMROOT + '/../V7_6_0_201506/prerequisites/Mfront-TFEL202/lib',
        YAMMROOT + '/prerequisites/Mumps-501_consortium_aster2/lib',
        YAMMROOT + '/prerequisites/Scotch_aster-604_aster/lib',
        # for openblas
        ASTER_ROOT + '/public/lib',
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

    # openblas from $ASTER_ROOT/public/lib embeds lapack
    opts.maths_libs = 'openblas'

    # to fail if not found
    opts.enable_hdf5 = True
    opts.enable_med = True
    opts.enable_metis = True
    opts.enable_mumps = True
    opts.enable_scotch = True
    opts.enable_mfront = True

    opts.enable_petsc = False
