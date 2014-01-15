# coding=utf-8

import os.path as osp
import re
from functools import partial
from waflib import Options, Configure, Logs, Utils, Errors

def options(self):
    group = self.add_option_group("Petsc library options")
    group.add_option('--disable-petsc', dest='enable_petsc',
                   action='store_false', default=None,
                   help='Disable PETSC support')
    group.add_option('--enable-petsc', dest='enable_petsc',
                   action='store_true', default=None,
                   help='Force PETSC support')
    group.add_option('--petsc-libs', type='string',
                   dest='petsc_libs', default=None,
                   help='petsc librairies used when linking')
    group.add_option('--embed-petsc', dest='embed_petsc',
                    default=False, action='store_true',
                    help='Embed PETSC libraries as static library')


def configure(self):
    try:
        self.env.stash()
        self.check_petsc()
    except Errors.ConfigurationError:
        self.env.revert()
        self.define('_DISABLE_PETSC', 1)
        self.undefine('HAVE_PETSC')
        if self.options.enable_petsc == True:
            raise
    else:
        self.define('_HAVE_PETSC', 1)
        self.define('HAVE_PETSC', 1)

###############################################################################
@Configure.conf
def check_petsc(self):
    opts = self.options
    if opts.enable_petsc == False:
        raise Errors.ConfigurationError('PETSC disabled')

    optlibs = None
    if opts.petsc_libs is None:
        opts.petsc_libs = 'petsc'
        # add optional libs
        optlibs ='ml HYPRE stdc++'
    if opts.petsc_libs:
        self.check_petsc_libs(optlibs)

    self.check_petsc_headers()
    self.check_petsc_version()

@Configure.conf
def check_petsc_libs(self, optlibs):
    opts = self.options
    keylib = ('st' if opts.embed_all or opts.embed_scotch else '') + 'lib'
    for lib in Utils.to_list(opts.petsc_libs):
        self.check_cc(uselib_store='PETSC', use='MPI', mandatory=True, **{ keylib: lib})
    for lib in Utils.to_list(optlibs or ''):
        self.check_cc(uselib_store='PETSC', use='MPI', mandatory=False, **{ keylib: lib})

@Configure.conf
def check_petsc_headers(self):
    check = partial(self.check, header_name='petsc.h', uselib='PETSC')

    self.start_msg('Checking for header petsc.h')
    try:
        if not check(mandatory=False):
            if not check(includes=[osp.join(self.env.INCLUDEDIR, 'petsc')], mandatory=False):
                check(includes=[osp.join(self.env.OLDINCLUDEDIR, 'petsc')], mandatory=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('yes')

@Configure.conf
def check_petsc_version(self):
    fragment = r'''
#include <stdio.h>
#include <petsc.h>
int main(void){
#if defined(PETSC_VERSION_MAJOR) && defined(PETSC_VERSION_MINOR) && defined(PETSC_VERSION_SUBMINOR) && defined(PETSC_VERSION_PATCH)
    printf("PETSCVER: %d.%d.%d.%d", PETSC_VERSION_MAJOR, PETSC_VERSION_MINOR, PETSC_VERSION_SUBMINOR, PETSC_VERSION_PATCH);
    return 0;
#endif
/* unexpected */
    return 1;
}'''
    self.start_msg('Checking petsc version')
    try:
        ret = self.check_cc(fragment=fragment, use='PETSC',
                            mandatory=True, execute=True, define_ret=True)
        mat = re.search('PETSCVER: *(?P<vers>[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)', ret)
        vers = mat and mat.group('vers')
        major, minor, sub, patch = [int(i) for i in vers.split('.')]
        vers = '%d.%d.%dp%d' % (major, minor, sub, patch)
        if major < 3 or (major == 3 and minor < 2):
            self.end_msg('unsupported petsc version: %s (expected 3.2.* or newer)' % vers, 'RED')
            raise Errors.ConfigurationError
        self.define('ASTER_PETSC_VERSION', vers)
    except:
        self.end_msg('can not get version', 'RED')
        raise
    else:
        self.end_msg(vers)
