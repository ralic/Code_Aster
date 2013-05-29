# coding=utf-8

import os.path as osp
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
    from Options import options as opts
    try:
        self.check_metis()
    except Errors.ConfigurationError:
        if opts.enable_metis == True:
            raise
    else:
        self.env.append_value('DEFINES', ['_HAVE_METIS'])
        self.env.HAVE_METIS = True

###############################################################################
@Configure.conf
def check_metis(self):
    from Options import options as opts
    if opts.enable_metis == False:
        raise Errors.ConfigurationError('METIS disabled')
    if opts.metis_libs is None:
        opts.metis_libs = 'metis'
    if opts.metis_libs:
        self.check_metis_libs()
    self.check_metis_headers()

@Configure.conf
def check_metis_libs(self):
    from Options import options as opts
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
