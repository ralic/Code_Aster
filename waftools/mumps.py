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
    group.add_option('--mumps-libs', type='string', dest='mumps_libs',
                    default=None,
                    help='mumps librairies to use when linking')
    group.add_option('--embed-mumps', dest='embed_mumps',
                    default=False, action='store_true',
                    help='Embed MUMPS libraries as static library')

def configure(self):
    try:
        self.check_mumps()
    except Errors.ConfigurationError:
        self.define('_DISABLE_MUMPS', 1)
        self.undefine('HAVE_MUMPS')
        if self.options.enable_mumps == True:
            raise
    else:
        self.define('_HAVE_MUMPS', 1)
        self.define('HAVE_MUMPS', 1)

###############################################################################
@Configure.conf
def check_mumps(self):
    opts = self.options
    if opts.enable_mumps == False:
        raise Errors.ConfigurationError('MUMPS disabled')
    self.check_mumps_headers()
    self.check_mumps_version()
    if opts.mumps_libs is None:
        opts.mumps_libs = 'dmumps zmumps smumps cmumps mumps_common pord'
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
    opts = self.options
    check_mumps = partial(self.check_cc, uselib_store='MUMPS', use='METIS MPI',
                          mandatory=True)
    if opts.embed_all or opts.embed_mumps:
        check = lambda lib: check_mumps(stlib=lib)
    else:
        check = lambda lib: check_mumps(lib=lib)
    map(check, Utils.to_list(opts.mumps_libs))

@Configure.conf
def check_mumps_headers(self):
    fragment = r'''
      PROGRAM MAIN
      INCLUDE '{0}'
      PRINT *, 'ok'
      END PROGRAM MAIN
'''
    headers = [i + 'mumps_struc.h' for i in 'sdcz'] + ['mpif.h']
    if self.get_define('HAVE_MPI'):
        for path in self.env['INCLUDES'][:]:
            if "include_seq" in path:
                self.env['INCLUDES'].remove(path)
                self.start_msg('Removing path from INCLUDES')
                self.end_msg(path, 'YELLOW')
    for inc in headers:
        try:
            self.start_msg('Checking for {0}'.format(inc))
            self.check_fc(fragment=fragment.format(inc),
                          compile_filename='test.F',
                          uselib_store='MUMPS', uselib='MUMPS MPI',
                          mandatory=True)
        except:
            self.end_msg('no', 'YELLOW')
            raise
        else:
            self.end_msg('yes')

@Configure.conf
def check_mumps_version(self):
    # translate special tags, not yet used
    dict_vers = { '5.0.1consortium' : '5.0.1' , '5.0.2consortium' : '5.0.2'}
    fragment = r'''
#include <stdio.h>
#include "smumps_c.h"

int main(void){
    printf("%s", MUMPS_VERSION);
    return 0;
}'''
    self.start_msg('Checking mumps version')
    try:
        ret = self.check_cc(fragment=fragment, use='MUMPS',
                            mandatory=True, execute=True, define_ret=True)
        self.env['MUMPS_VERSION'] = ret
        if dict_vers.get(ret, ret) != '5.0.1' and dict_vers.get(ret, ret) != '5.0.2':
            raise Errors.ConfigurationError("expected versions: {0}".format('5.0.1/2'))
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.define('ASTER_MUMPS_VERSION', ret)
        self.end_msg( self.env['MUMPS_VERSION'] )
