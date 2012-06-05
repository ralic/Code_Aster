# encoding: utf-8

top = '.'
out = 'build'

import os
import os.path as osp
import zlib
import base64
from functools import partial
from itertools import chain
from waflib import Configure, Logs, Utils, Build

def options(self):
    ori_get_usage = self.parser.get_usage
    def _usage():
        return ori_get_usage() + os.linesep.join((
        '',
        'Environment variables:',
        '  INCLUDES  : space separated list of directories extending the include'
        ' path',
        '  CC        : C compiler',
        '  FC        : Fortran compiler',
        '  CXX       : C++ compiler',
        '  INCLUDES  : extra include paths',
        '  DEFINES   : extra preprocessor defines',
        '  LINKFLAGS : extra C linker options',
        '  LINKFLAGS : extra Fortran linker options',
        '  LIBPATH   : extra paths where to find libraries',
        '  LIB       : extra libraries to link with',
        '  STLIB     : extra static libraries to link with',
        '  CFLAGS    : extra C compilation options',
        '  FCFLAGS   : extra Fortrant compilation optins',
        '  PREFIX    : default installation prefix to be used, '
        'if no --prefix option is given.',
        '',))
    self.parser.get_usage = _usage

    self.load('use_config', tooldir='waftools')
    self.load('gnu_dirs')

    group = self.add_option_group('Aster options')

    self.load('parallel', tooldir='waftools')
    self.load('mathematics', tooldir='waftools')
    self.load('med', tooldir='waftools')
    self.load('mumps', tooldir='waftools')
    self.load('scotch', tooldir='waftools')
    self.load('petsc', tooldir='waftools')
    self.load('legacy', tooldir='waftools')

    group.add_option('-g', '--enable-debug', dest='debug',
                    action='store_true', default=False,
                    help='Generate debug information when compiling')
    group.add_option('-E', '--embed-all', dest='embed_all',
                    action='store_true', default=False,
                    help='activate all embed-* options')
    self.recurse('bibfor')
    self.recurse('bibc')
    group.add_option('--ignore-fermetur', dest='add_fermetur',
                   action='store_false', default=True,
                   help='ignore fermetur code (you must provide symbols)')
    self.recurse('fermetur')

def configure(self):
    from Options import options as opts

    self.setenv('default')

    self.load('use_config', tooldir='waftools')
    self.load('gnu_dirs')
    self.env.append_value('ASTERDATADIR', osp.join(self.env.DATADIR, 'aster'))
    self.env['BIBPYTPATH'] = self.path.find_dir('bibpyt').abspath()

    self.env.ASTER_EMBEDS = []
    self.env.ADD_FERMETUR = opts.add_fermetur

    self.add_os_flags('FLAGS')
    self.add_os_flags('CFLAGS')
    self.add_os_flags('FCLAGS')
    self.add_os_flags('LINKFLAGS')
    self.add_os_flags('FCLINKFLAGS')
    self.add_os_flags('LIB')
    self.add_os_flags('LIBPATH')
    self.add_os_flags('STLIB')
    self.add_os_flags('STLIBPATH')
    self.add_os_flags('INCLUDES')
    self.add_os_flags('DEFINES')

    # Add *LIBPATH paths to LD_LIBRARY_PATH
    libpaths = list(chain(*[Utils.to_list(self.env[key]) for key in self.env.table
                            if 'libpath' in key.lower()]))
    ldpaths = [p for p in os.environ.get('LD_LIBRARY_PATH', '').split(os.pathsep)]
    paths =  libpaths + ldpaths
    os.environ['LD_LIBRARY_PATH'] = os.pathsep.join(p for p in paths if p)

    self.load('scm_aster', tooldir='waftools')
    self.load('parallel', tooldir='waftools')
    self.check_platform()

    self.load('mathematics', tooldir='waftools')

    self.env.append_value('FCFLAGS', ['-fPIC'])
    self.env.append_value('CFLAGS', ['-fPIC'])

    self.load('med', tooldir='waftools')
    self.load('mumps', tooldir='waftools')
    self.load('scotch', tooldir='waftools')
    self.load('petsc', tooldir='waftools')

    paths = self.srcnode.ant_glob('bibc/include', src=True, dir=True)
    paths = [d.abspath() for d in paths]
    self.env.append_value('INCLUDES', paths)

    self.recurse('bibfor')
    self.recurse('bibf90')
    if opts.add_fermetur:
        self.recurse('fermetur')
    self.recurse('bibc')
    self.load('legacy', tooldir='waftools')
    self.check_optimization_options()

def build(self):
    from Options import options as opts
    get_srcs = self.path.get_src().ant_glob
    if not self.variant:
        self.fatal('Call "waf build_debug" or "waf build_release", and read ' \
                   'the comments in the wscript file!')

    self.recurse('bibfor')
    self.recurse('bibf90')
    if self.all_envs['default'].ADD_FERMETUR:
        self.recurse('fermetur')
    self.recurse('bibc')

    self.recurse('bibpyt')
    for optional in ('materiau', 'datg', 'catapy', 'catalo'):
        if osp.exists(osp.join(optional, 'wscript')):
            self.recurse(optional)
    self.load('legacy', tooldir='waftools')

def build_elements(self):
    self.recurse('catalo')

def init(self):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext
    _all = (BuildContext, CleanContext, InstallContext, UninstallContext)
    for x in ['debug', 'release']:
        for y in _all:
            name = y.__name__.replace('Context','').lower()
            class tmp(y):
                cmd = name + '_' + x
                variant = x
    # default to release
    for y in _all:
       class tmp(y):
           variant = 'release'

def all(self):
    from waflib import Options
    lst = ['install_release', 'install_debug']
    Options.commands = lst + Options.commands

###############################################################################
class BuildElementContext(Build.BuildContext):
    """execute the build for elements catalog only using an installed Aster (also performed at install)"""
    cmd = 'buildelem'
    fun = 'build_elements'

###############################################################################

@Configure.conf
def uncompress64(self, compressed):
    return zlib.decompress(base64.decodestring(compressed))

@Configure.conf
def check_platform(self):
    self.start_msg('Getting platform')
    # convert to Code_Aster terminology
    os_name = self.env.DEST_OS
    if os_name.startswith(('darwin', 'cygwin')):
        os_name = 'linux'
    elif os_name == 'sunos':
        os_name = 'solaris'
    if self.env.DEST_CPU.endswith('64') and not os_name.startswith('win'):
        os_name += '64'
        self.env.HAVE_64_BITS = True
    os_name = os_name.upper()
    self.env.append_unique('DEFINES', [os_name])
    self.end_msg('(-D%s)' % os_name)

@Configure.conf
def check_optimization_options(self):
    """adapt the environment of the build variants"""
    self.setenv('debug', env=self.all_envs['default'])
    self.setenv('release', env=self.all_envs['default'])
    # these functions must switch between each environment
    self.check_optimization_cflags()
    self.check_optimization_fcflags()
    self.check_optimization_python()
