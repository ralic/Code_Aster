# coding=utf-8

import os.path as osp
import re
from functools import partial
from waflib import Options, Configure, Logs, Utils, Errors

def options(self):
    group = self.add_option_group('Mumps library options')
    group.add_option('--disable-mumps', action='store_false', default=None,
                    dest='enable_mumps', help='Disable MUMPS support')
    group.add_option('--enable-mumps', action='store_true', default=None,
                    dest='enable_mumps', help='Force MUMPS support')
    group.add_option('--mumps-version', type='string',
                    dest='mumps_version', default=None,
                    help='mumps headers version to use inside bibfor')
    group.add_option('--mumps-libs', type='string', dest='mumps_libs',
                    default=None,
                    help='mumps librairies to use when linking')
    group.add_option('--embed-mumps', dest='embed_mumps',
                    default=False, action='store_true',
                    help='Embed MUMPS libraries as static library')

def configure(self):
    from Options import options as opts
    try:
        self.check_mumps()
    except Errors.ConfigurationError:
        if opts.enable_mumps == True:
            raise
    else:
        self.define('_HAVE_MUMPS', 1)
        self.define('HAVE_MUMPS', 1)

###############################################################################
@Configure.conf
def check_mumps(self):
    from Options import options as opts
    if opts.enable_mumps == False:
        raise Errors.ConfigurationError('MUMPS disabled')
    self.get_mumps_version()
    if opts.mumps_libs is None:
        opts.mumps_libs = 'dmumps zmumps smumps cmumps mumps_common pord'
        # metis has been checked before (and is mandatory)
        metis = self.env.get_flat('LIB_METIS') or self.env.get_flat('STLIB_METIS')
        if metis:
            opts.mumps_libs += " " + metis
    if not opts.parallel:
        opts.mumps_libs += ' mpiseq'
    if opts.mumps_libs:
        self.check_mumps_libs()
    self.set_define_from_env('MUMPS_INT_SIZE',
                             'Setting size of Mumps integers',
                             'unexpected value for mumps int: %(size)s',
                             into=(4, 8), default=4)

@Configure.conf
def check_mumps_libs(self):
    from Options import options as opts
    check_mumps = partial(self.check_cc, uselib_store='MUMPS', use='MPI',
                          mandatory=True)
    if opts.embed_all or opts.embed_mumps:
        check = lambda lib: check_mumps(stlib=lib)
    else:
        check = lambda lib: check_mumps(lib=lib)
    map(check, Utils.to_list(opts.mumps_libs))

@Configure.conf
def detect_last_includes(self):
    """Detect the more recent version of Mumps includes in src"""
    pattern = 'bibfor/include_mumps-'
    incs = self.srcnode.ant_glob(pattern + '*/*.h')
    exist = set([osp.dirname(i.srcpath()) for i in incs])
    mpi = [i for i in exist if i.endswith('_mpi')]
    incdirs = list(exist.difference(mpi))
    if self.env.HAVE_MPI:
        incdirs = mpi
    regexp = re.escape(pattern) + '(?P<vers>[0-9]+\.[0-9]+\.[0-9]+)(?:|_mpi)'
    allvers = re.compile(regexp).findall(' '.join(incdirs))
    vers = [tuple(map(int, i.split('.'))) for i in allvers]
    vers.sort()
    return '.'.join(map(str, vers[-1]))

@Configure.conf
def get_mumps_version(self):
    from Options import options as opts
    self.start_msg('Getting mumps version')
    try:
        if opts.mumps_version:
            ret = opts.mumps_version
        else:
            ret = self.detect_last_includes()
        # if C include is available
        #incdir = osp.abspath('bibfor/include_mumps-%s/' % ret)
        #frag = '\n'.join(('#include <stdio.h>', '#include <zmumps_c.h>',
                          #'int main(){printf(MUMPS_VERSION);return 0;}'))
        #ret = self.check_cc(fragment=frag, execute=True, define_ret=True,
                            #mandatory=True, cflags='-I%s' % incdir)
        to_search = 'bibfor/include_mumps-%s*/' % ret
        if not self.srcnode.ant_glob(to_search, src=True, dir=True):
            raise Errors.ConfigurationError('"%s" not compatible (see bibfor/include_mumps*)' % ret)
        self.define('MUMPS_VERSION', ret)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg(ret)
