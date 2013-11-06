# coding=utf-8

"""
Build script for Code_Aster


Note:
- All defines conditionning the compilation must be set using `conf.define()`.
  They will be exported into `asterc_config.h`/`asterf_config.h`.

- If some of them are also required during the build step, another variable
  must be passed using `env` (ex. BUILD_MED)
"""

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
        '  INCLUDES       : space separated list of directories extending the include'
        ' path',
        '  CC             : C compiler',
        '  FC             : Fortran compiler',
        '  CXX            : C++ compiler',
        '  INCLUDES       : extra include paths',
        '  DEFINES        : extra preprocessor defines',
        '  LINKFLAGS      : extra C linker options',
        '  FCLINKFLAGS    : extra Fortran linker options',
        '  LIBPATH        : extra paths where to find libraries',
        '  LIB            : extra libraries to link with',
        '  STLIB          : extra static libraries to link with',
        '  OPTLIB_FLAGS   : extra linker flags inserted after static libs '
        '(for example when -Wl,start-group options are necessary)',
        '  CFLAGS         : extra C compilation options',
        '  FCFLAGS        : extra Fortran compilation options',
        '  PREFIX         : default installation prefix to be used, '
        'if no --prefix option is given.',
        '  BLAS_INT_SIZE  : kind of integers to use in the fortran blas/lapack '
        'calls (4 or 8, default is 4)',
        '  MUMPS_INT_SIZE : kind of integers to use in the fortran mumps calls '
        ' (4 or 8, default is 4)',
        '',))
    self.parser.get_usage = _usage

    self.load('use_config', tooldir='waftools')
    self.load('gnu_dirs')

    group = self.add_option_group('Code_Aster options')

    self.load('parallel', tooldir='waftools')
    self.load('mathematics', tooldir='waftools')
    self.load('med', tooldir='waftools')
    self.load('mumps', tooldir='waftools')
    self.load('metis', tooldir='waftools')
    self.load('scotch', tooldir='waftools')
    self.load('petsc', tooldir='waftools')
    self.load('legacy', tooldir='waftools')
    self.load('runtest', tooldir='waftools')

    group.add_option('-E', '--embed-all', dest='embed_all',
                    action='store_true', default=False,
                    help='activate all embed-* options')
    group.add_option('--install-tests', dest='install_tests',
                    action='store_true', default=False,
                    help='install the testcases files')
    self.recurse('bibfor')
    self.recurse('bibc')
    self.recurse('i18n')

def configure(self):
    from Options import options as opts

    self.setenv('default')

    self.load('use_config', tooldir='waftools')
    self.load('gnu_dirs')
    self.env.append_value('ASTERDATADIR', osp.join(self.env.DATADIR, 'aster'))
    self.env['BIBPYTPATH'] = self.path.find_dir('bibpyt').abspath()

    self.env.ASTER_EMBEDS = []

    self.add_os_flags('FLAGS')
    self.add_os_flags('CFLAGS')
    self.add_os_flags('FCFLAGS')
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
    self.load('metis', tooldir='waftools')
    self.load('scotch', tooldir='waftools')
    self.load('petsc', tooldir='waftools')

    paths = self.srcnode.ant_glob('bibc/include', src=True, dir=True)
    paths = [d.abspath() for d in paths]
    self.env.append_value('INCLUDES', paths)

    self.recurse('bibfor')
    self.recurse('bibc')
    self.recurse('i18n')
    self.load('legacy', tooldir='waftools')
    # keep compatibility for as_run
    if self.env.HAVE_MPI:
        self.env.ASRUN_MPI_VERSION = 1
    # variants
    self.check_optimization_options()
    # only install tests during release install
    self.setenv('release')
    self.env.install_tests = opts.install_tests
    self.write_config_headers()

def build(self):
    from Options import options as opts
    get_srcs = self.path.get_src().ant_glob
    if not self.variant:
        self.fatal('Call "waf build_debug" or "waf build_release", and read ' \
                   'the comments in the wscript file!')

    self.recurse('bibfor')
    self.recurse('bibc')

    self.recurse('bibpyt')
    self.recurse('i18n')
    lsub = ['materiau', 'datg', 'catapy', 'catalo']
    if opts.install_tests or self.env.install_tests:
        lsub.extend(['astest', '../validation/astest'])
    for optional in lsub:
        if osp.exists(osp.join(optional, 'wscript')):
            self.recurse(optional)
    self.load('scm_aster', tooldir='waftools')
    self.load('legacy', tooldir='waftools')

def build_elements(self):
    self.recurse('catalo')

def init(self):
    from waflib.Build import BuildContext, CleanContext, InstallContext, UninstallContext
    _all = (BuildContext, CleanContext, InstallContext, UninstallContext, TestContext)
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

class BuildElementContext(Build.BuildContext):
    """execute the build for elements catalog only using an installed Aster (also performed at install)"""
    cmd = 'buildelem'
    fun = 'build_elements'

def runtest(self):
    """Run a testcase"""
    self.load('runtest', tooldir='waftools')

class TestContext(Build.BuildContext):
    """Facility to execute a testcase"""
    cmd = 'test'
    fun = 'runtest'

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
    if self.env.DEST_CPU.endswith('64'):
        os_name += '64'
        self.define('_USE_64_BITS', 1)
    os_name = os_name.upper()
    if not os_name.startswith('win'):
        self.define('_POSIX', 1)
        self.undefine('_WINDOWS')
        self.define(os_name, 1)
    else:
        self.define('_WINDOWS', 1)
        self.undefine('_POSIX')
    self.end_msg(os_name)

@Configure.conf
def check_optimization_options(self):
    """adapt the environment of the build variants"""
    self.setenv('debug', env=self.all_envs['default'])
    self.setenv('release', env=self.all_envs['default'])
    # these functions must switch between each environment
    self.check_optimization_cflags()
    self.check_optimization_fcflags()
    self.check_optimization_python()

# same idea than waflib.Tools.c_config.write_config_header
# but defines are not removed from `env`
from waflib.Tools.c_config import DEFKEYS
CMT = { 'C' : '/* %s */', 'Fortran' : '! %s' }

@Configure.conf
def write_config_headers(self):
    """Write both xxxx_config.h files for C and Fortran,
    then remove entries from DEFINES"""
    for variant in ('debug', 'release'):
        self.setenv(variant)
        self.write_config_h('Fortran', variant)
        self.write_config_h('C', variant)
        for key in self.env[DEFKEYS]:
            self.undefine(key)
        self.env[DEFKEYS] = []

@Configure.conf
def write_config_h(self, language, variant, configfile=None, env=None):
    """Write a configuration header containing defines
    ASTERC defines will be used if language='C', not 'Fortran'.
    """
    self.start_msg('Write config file')
    assert language in ('C', 'Fortran')
    cmt = CMT[language]
    configfile = configfile or 'aster%s_config.h' % language[0].lower()
    env = env or self.env
    guard = Utils.quote_define_name(configfile)
    lst = [
        cmt % "WARNING! Automatically generated by `waf configure`!",
        "", "",
        "#ifndef %s" % guard, "#define %s" % guard, "",
        self.get_config_h(language),
        "", "#endif", "",
    ]
    node = self.bldnode or self.path.get_bld()
    node = node.make_node(osp.join(variant, configfile))
    node.parent.mkdir()
    node.write('\n'.join(lst))
    self.env.append_unique('INCLUDES', node.parent.abspath())
    # config files are not removed on "waf clean"
    env.append_unique(Build.CFG_FILES, [node.abspath()])
    self.end_msg(node.bldpath())

@Configure.conf
def get_config_h(self, language):
    """Create the contents of a ``config.h`` file from the defines
    set in conf.env.define_key / conf.env.include_key. No include guards are added.
    """
    cmt = CMT[language]
    lst = []
    for x in self.env[DEFKEYS]:
        if language != 'C' and x.startswith('ASTERC'):
            continue
        if self.is_defined(x):
            val = self.get_define(x)
            lst.append('#define %s %s' % (x, val))
        else:
            lst.append(cmt % '#undef %s' % x)
    return "\n".join(lst)
