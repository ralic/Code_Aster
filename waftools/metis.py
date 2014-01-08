# coding=utf-8

import os.path as osp
import re
from functools import partial
from waflib import Options, Configure, Logs, Utils, Errors

def options(self):
    group = self.add_option_group('Metis library options')
    group.add_option('--disable-metis', action='store_false', default=None,
                    dest='enable_metis', help='Disable METIS support')
    group.add_option('--enable-metis', action='store_true', default=None,
                    dest='enable_metis', help='Force METIS support')
    group.add_option('--metis-libs', type='string', dest='metis_libs',
                    default=None,
                    help='metis librairies to use when linking')
    group.add_option('--embed-metis', dest='embed_metis',
                    default=False, action='store_true',
                    help='Embed METIS libraries as static library')

def configure(self):
    try:
        self.check_metis()
    except Errors.ConfigurationError:
        self.define('_DISABLE_METIS', 1)
        self.undefine('HAVE_METIS')
        if self.options.enable_metis == True:
            raise
    else:
        self.define('_HAVE_METIS', 1)
        self.define('HAVE_METIS', 1)

###############################################################################
@Configure.conf
def check_metis(self):
    opts = self.options
    if opts.enable_metis == False:
        raise Errors.ConfigurationError('METIS disabled')
    if opts.metis_libs is None:
        opts.metis_libs = 'metis'
    if opts.metis_libs:
        self.check_metis_libs()
    self.check_metis_headers()
    self.check_metis_version()

@Configure.conf
def check_metis_libs(self):
    opts = self.options
    check_metis = partial(self.check_cc, uselib_store='METIS', mandatory=True)
    if opts.embed_all or opts.embed_metis:
        check = lambda lib: check_metis(stlib=lib)
    else:
        check = lambda lib: check_metis(lib=lib)
    map(check, Utils.to_list(opts.metis_libs))

@Configure.conf
def check_metis_headers(self):
    check = partial(self.check_cc, header_name='metis.h', uselib_store='METIS', use='METIS')
    self.start_msg('Checking for header metis.h')
    try:
        if not check(mandatory=False):
            if not check(includes=[osp.join(self.env.INCLUDEDIR, 'metis')], mandatory=False):
                check(includes=[osp.join(self.env.OLDINCLUDEDIR, 'metis')], mandatory=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('yes')

@Configure.conf
def check_metis_version(self):
    fragment = r'''
#include <stdio.h>
#include <metis.h>
int main(void){
#ifdef METISTITLE
/* metis 4 */
    printf("METISTITLE: %s", METISTITLE);
    return 0;
#endif
#if defined(METIS_VER_MAJOR) && defined(METIS_VER_MINOR) && defined(METIS_VER_SUBMINOR)
    printf("METISVER: %d.%d.%d", METIS_VER_MAJOR, METIS_VER_MINOR, METIS_VER_SUBMINOR);
    return 0;
#endif
/* unexpected */
    return 1;
}'''
    self.start_msg('Checking metis version')
    try:
        ret = self.check_cc(fragment=fragment, use='METIS',
                            mandatory=True, execute=True, define_ret=True)
        mat4 = re.search('METISTITLE: *METIS *(?P<vers>[0-9]+\.[0-9]+\.\w+) ', ret)
        mat5 = re.search('METISVER: *(?P<vers>[0-9]+\.[0-9]+\.\w+)', ret)
        vers = (mat4 and mat4.group('vers')) or (mat5 and mat5.group('vers'))
        major = int(vers.split('.')[0])
        if major != 4:
            self.end_msg('unsupported metis version: %s' % vers, 'RED')
            raise Errors.ConfigurationError
    except:
        self.end_msg('can not get version', 'RED')
        raise
    else:
        self.end_msg(vers)
