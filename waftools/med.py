# coding=utf-8

import os.path as osp
from functools import partial
from waflib import Options, Configure, Logs, Utils, Errors

def options(self):
    group = self.add_option_group("HDF5/Med libraries options")
    group.add_option('--med-libs', type='string',
                    dest='med_libs', default=None,
                    help='MED librairies to link against med')
    group.add_option('--embed-med', dest='embed_med',
                    default=False, action='store_true',
                    help='Embed MED libraries as static library')
    group.add_option('--disable-med', dest='enable_med',
                    default=None, action='store_false',
                    help='disable the MED support')
    group.add_option('--enable-med', dest='enable_med',
                    default=None, action='store_true',
                    help='force the MED support')

    group.add_option('--hdf5-libs', type='string',
                    dest='hdf5_libs', default=None,
                    help='HDF5 librairies to link with')
    group.add_option('--embed-hdf5', dest='embed_hdf5',
                    default=None, action='store_true',
                    help='Embed HDF5 libraries as static library')
    group.add_option('--disable-hdf5', dest='enable_hdf5',
                    default=True, action='store_false',
                    help='disable the HDF5 and MED support')
    group.add_option('--enable-hdf5', dest='enable_hdf5',
                    default=None, action='store_true',
                    help='disable the HDF5 and MED support')

def configure(self):
    opts = self.options
    try:
        self.check_hdf5()
    except Errors.ConfigurationError:
        if opts.enable_hdf5 == True:
            raise
        self.define('_DISABLE_HDF5', 1)
        self.define('_DISABLE_MED', 1)
        self.undefine('HAVE_HDF5')
        self.undefine('HAVE_MED')
    else:
        self.define('HAVE_HDF5', 1)

    try:
        self.check_med()
    except Errors.ConfigurationError, err:
        if opts.enable_med == True:
            raise
        self.define('_DISABLE_MED', 1)
        self.undefine('HAVE_MED')
    else:
        self.define('HAVE_MED', 1)
        self.env.BUILD_MED = True

###############################################################################

@Configure.conf
def check_hdf5(self):
    opts = self.options
    if opts.enable_hdf5 == False:
        raise Errors.ConfigurationError('HDF5 disabled')

    if opts.hdf5_libs is None:
        opts.hdf5_libs = 'hdf5'

    if opts.hdf5_libs:
        self.check_hdf5_libs()
    self.check_hdf5_headers()
    self.check_hdf5_version()
    self.check_hdf5_api()

@Configure.conf
def check_hdf5_libs(self):
    opts = self.options
    check_hdf5 = partial(self.check_cc, mandatory=True, uselib_store='HDF5', use='HDF5 MATH')
    if opts.embed_all or opts.embed_hdf5:
        check_lib = lambda lib: check_hdf5(stlib=lib)
    else:
        check_lib = lambda lib: check_hdf5(lib=lib)
    map(check_lib, Utils.to_list(opts.hdf5_libs))

@Configure.conf
def check_hdf5_headers(self):
    check = partial(self.check_cc, header_name='hdf5.h', uselib_store='HDF5', use='HDF5 MATH')
    self.start_msg('Checking for header hdf5.h')
    try:
        if not check(mandatory=False):
            if not check(includes=[osp.join(self.env.INCLUDEDIR, 'hdf5')], mandatory=False):
                check(includes=[osp.join(self.env.OLDINCLUDEDIR, 'hdf5')], mandatory=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('yes')

@Configure.conf
def check_hdf5_version(self):
    fragment = r'''
#include <stdio.h>
#include <hdf5.h>
int main(void){
    int ier;
    unsigned int n1=0, n2=0, n3=0;
    ier = (int)H5get_libversion( &n1, &n2, &n3 );
    printf("%d.%d.%d", n1, n2, n3);
    return 0;
}'''
    self.start_msg('Checking hdf5 version')
    try:
        ret = self.check_cc(fragment=fragment, use='HDF5 MATH', uselib_store='HDF5',
                            mandatory=True, execute=True, define_ret=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg(ret)

@Configure.conf
def check_hdf5_api(self):
    fragv18 = "#include <hdf5.h>\nint main(){hid_t st=0;H5Eclear(st);return 0;}"

    self.start_msg('Checking for API hdf5 v18')
    try:
        self.to_log('check the v18 api and set H5_NO_DEPRECATED_SYMBOLS if it fails')
        check = partial(self.check_cc, execute=True, uselib_store='HDF5', use='HDF5 MATH')
        v18 = check(fragment=fragv18, mandatory=False)
        if not v18:
            self.define('H5_NO_DEPRECATED_SYMBOLS', 1)
        self.to_log('try again by using H5_NO_DEPRECATED_SYMBOLS')
        check(fragment=fragv18, mandatory=True)
    except:
        self.end_msg('no', 'RED')
        raise
    else:
        self.end_msg(v18 and 'default v18' or '-DH5_NO_DEPRECATED_SYMBOLS')

@Configure.conf
def check_med(self):
    opts = self.options
    if opts.enable_med == False:
        raise Errors.ConfigurationError('MED disabled')

    if opts.med_libs is None:
        opts.med_libs = 'med'

    if opts.med_libs:
        self.check_med_libs()
    self.check_med_headers()
    self.check_sizeof_med_int()
    self.check_med_version()

@Configure.conf
def check_med_libs(self):
    opts = self.options
    check_med = partial(self.check_cc, features='cxx cxxprogram',
                        mandatory=True, uselib_store='MED', use='MED HDF5')
    if opts.embed_all or opts.embed_med:
        check_lib = lambda lib: check_med(stlib=lib)
    else:
        check_lib = lambda lib: check_med(lib=lib)
    map(check_lib, Utils.to_list(opts.med_libs))

@Configure.conf
def check_med_headers(self):
    check = partial(self.check_cc, header_name='med.h', uselib_store='MED', use='MED HDF5')
    self.start_msg('Checking for header med.h')
    try:
        if not check(mandatory=False):
            if not check(includes=[osp.join(self.env.INCLUDEDIR, 'med')], mandatory=False):
                check(includes=[osp.join(self.env.OLDINCLUDEDIR, 'med')], mandatory=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('yes')


@Configure.conf
def check_med_version(self):
    fragment = r'''
#include <stdio.h>
#include <hdf5.h>
#include <med.h>
int main(void){
    med_bool test; /* med >3.0 */
    med_int n1=0, n2=0, n3=0;
    MEDlibraryNumVersion( &n1, &n2, &n3 );
    printf("%d.%d.%d", n1, n2, n3);
    return 0;
}'''
    self.start_msg('Checking med version')
    try:
        ret = self.check_cc(fragment=fragment, use='MED HDF5', uselib_store='MED',
                            mandatory=True, execute=True, define_ret=True)
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg(ret)

@Configure.conf
def check_sizeof_med_int(self):
    fragment = r'''
#include <stdio.h>
#include <hdf5.h>
#include <med.h>
int main(void){
    med_int integer;
    printf("%d", (int)sizeof(integer));
    return 0;
}'''
    self.code_checker('MED_INT_SIZE', self.check_cc, fragment,
                      'Checking size of med_int integers',
                      'unexpected value for sizeof(med_int): %s',
                      into=(4, 8))
    #XXX compatibility
    if self.env['MED_INT_SIZE'] == 4 and self.is_defined('_USE_64_BITS'):
        self.define('_USE_MED_SHORT_INT', 1)
