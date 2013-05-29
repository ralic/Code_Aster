# coding=utf-8

import os.path as osp
from functools import partial
from waflib import Options, Configure, Logs, Utils, Errors

def options(self):
    group = self.add_option_group("Scotch library options")
    group.add_option('--disable-scotch', action='store_false', default=None,
                   dest='enable_scotch', help='Disable SCOTCH support')
    group.add_option('--enable-scotch', action='store_true', default=None,
                   dest='enable_scotch', help='Force scotch support')
    group.add_option('--scotch-libs', type='string',
                   dest='scotch_libs', default=None,
                   help='scotch librairies used when linking')
    group.add_option('--embed-scotch', dest='embed_scotch',
                    default=False, action='store_true',
                    help='Embed SCOTCH libraries as static library')

def configure(self):
    from Options import options as opts
    try:
        self.check_scotch()
    except Errors.ConfigurationError:
        if opts.enable_scotch == True:
            raise
        self.env.append_value('DEFINES', '_DISABLE_SCOTCH')
    else:
        self.env.HAVE_SCOTCH = True

###############################################################################

@Configure.conf
def check_scotch(self):
    from Options import options as opts
    if opts.enable_scotch == False:
        raise Errors.ConfigurationError('SCOTCH disabled')

    self.check_scotch_headers()
    self.check_scotch_version()

    if opts.scotch_libs is None:
        if self.env.SCOTCH_VERSION and self.env.SCOTCH_VERSION[0] < 5:
            opts.scotch_libs = 'scotch scotcherr scotcherrexit'
        else:
            # default or SCOTCH_VERSION >= 5
            opts.scotch_libs = 'esmumps scotch scotcherr'

    # Code_Aster v11.0.1: FICHE 016627
    if 'scotchmetis' in opts.scotch_libs:
        raise Errors.ConfigurationError('scotchmetis variant library is not compatible with Code_Aster')

    if opts.scotch_libs:
        self.check_scotch_libs()


@Configure.conf
def check_scotch_libs(self):
    from Options import options as opts
    check_scotch = partial(self.check_cc, mandatory=True, uselib_store='SCOTCH', use='MPI')
    if opts.embed_all or opts.embed_scotch:
        check_lib = lambda lib: check_scotch(stlib=lib)
    else:
        check_lib = lambda lib: check_scotch(lib=lib)
    map(check_lib, Utils.to_list(opts.scotch_libs))

@Configure.conf
def check_scotch_headers(self):
    from Options import options as opts
    self.start_msg('Checking for header scotch.h')
    headers = 'stdio.h stdlib.h scotch.h'
    try:
        check = partial(self.check, header_name=headers,
                        uselib_store='SCOTCH', uselib='SCOTCH MPI')

        if not check(mandatory=False):
            if not check(includes=[osp.join(self.env.INCLUDEDIR, 'scotch')], mandatory=False):
                check(includes=[osp.join(self.env.OLDINCLUDEDIR, 'scotch')], mandatory=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('yes')

@Configure.conf
def check_scotch_version(self):
    # scotch.h may use int64_t without including <stdint.h >
    fragment = r'''
#include <stdio.h>
#include <stdint.h>
#include "scotch.h"

int main(void){
    printf("(%d, %d, %d)", (int)SCOTCH_VERSION, (int)SCOTCH_RELEASE,
           (int)SCOTCH_PATCHLEVEL);
    return 0;
}'''
    self.start_msg('Checking scotch version')
    try:
        ret = self.check_cc(fragment=fragment, use='SCOTCH', uselib_store='SCOTCH',
                            mandatory=True, execute=True, define_ret=True)
        self.env.append_value('SCOTCH_VERSION', eval(ret))
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg( '.'.join([str(i) for i in eval(ret)]) )

