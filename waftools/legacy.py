# coding=utf-8

import sys
import os
import os.path as osp
from itertools import chain
from waflib import Task, TaskGen, Utils

def options(self):
    group = self.get_option_group("Code_Aster options")
    # default=False in libaster, True in Code_Aster
    group.add_option('--legacy', dest='legacy',
                    default=True, action='store_true',
                    help='create some legacy files')
    group.add_option('--nolegacy', dest='legacy',
                    action='store_false',
                    help='do not create legacy files')

def configure(self):
    from Options import options as opts
    self.env.legacy = opts.legacy

def build(self):
    if not self.env.legacy:
        return
    self(
        features = 'build_legacy',
            name = 'legacy',
    install_path = '${ASTERDATADIR}',
    )

@TaskGen.feature('build_legacy')
def apply_legacy(self):
    """Create files required by asrun"""
    config = self.bld.bldnode.make_node('config.txt')
    profile = self.bld.bldnode.make_node('profile.sh')
    self.create_task('create_config_txt', tgt=config)
    self.create_task('create_profile', tgt=profile)
    self.bld.install_files(self.install_path, [config, profile])

class create_config_txt(Task.Task):
    vars = ['PREFIX', 'DEFINES', 'PYTHON',
            'FC', 'FCFLAGS',
            'PYTHONDIR', 'LIBPATH', 'LIBDIR', 'CFG_PYTHONPATH',
            'ASTERDATADIR', 'OPT_ENV', 'install_tests']

    def run(self):
        from Options import options as opts
        bld = self.generator.bld
        src = bld.srcnode.abspath()
        install_tests = self.env.install_tests or opts.install_tests
        dico = _env2dict(src, self.env, install_tests)
        cfg = self.outputs[0].abspath()
        open(cfg, 'w').write(TMPL_CONFIG_TXT % dico)

class create_profile(Task.Task):
    vars = ['PREFIX', 'DEFINES', 'PYTHON',
            'FC', 'FCFLAGS',
            'PYTHONDIR', 'LIBPATH', 'LIBDIR', 'CFG_PYTHONPATH',
            'ASTERDATADIR', 'OPT_ENV', 'install_tests']

    def run(self):
        from Options import options as opts
        bld = self.generator.bld
        src = bld.srcnode.abspath()
        install_tests = self.env.install_tests or opts.install_tests
        dico = _env2dict(src, self.env, install_tests)
        cfg = self.outputs[0].abspath()
        open(cfg, 'w').write(TMPL_PROFILE % dico)

def _env2dict(src, env, install_tests):
    """build dict informations"""
    env = env.copy()
    env['LD_LIBRARY_PATH'] = os.environ.get('LD_LIBRARY_PATH', '').split(os.pathsep)
    ld_path = list(chain(*[Utils.to_list(env[name])
                   for name in ('LIBPATH', 'LIBDIR', 'LD_LIBRARY_PATH') if env[name]]))
    ld_path = [path for path in ld_path if path]
    sep = os.pathsep + '\\\n'
    dico = dict([(k, as_str(env[k])) \
                    for k in ('PREFIX', 'PYTHON', 'PYTHONDIR', 'ASTERDATADIR')])
    dico['DEFINES'] = ' '.join([d.split('=')[0] for d in env['DEFINES']])
    dico['CFG_PYTHONPATH'] = os.pathsep.join(Utils.to_list(env['CFG_PYTHONPATH']))
    dico['PYTHONHOME'] = sys.prefix + '' if sys.prefix == sys.exec_prefix \
                                         else ':' + sys.exec_prefix
    # as_run compatibility
    if env.ASRUN_MPI_VERSION:
        dico['DEFINES'] += ' _USE_MPI'
    dico['LD_LIBRARY_PATH'] = sep.join(ld_path)
    dico['SRC'] = src
    dico['FC'] = env['FC']
    flags = [' '.join(env[i]) for i in env.keys() if i.startswith('FCFLAGS')]
    dico['FCFLAGS'] = ' '.join(flags)
    dico['OPT_ENV'] = env['OPT_ENV'] and os.linesep.join(env['OPT_ENV']) or ''
    dico['ADDMEM'] = env['ADDMEM'] or 250
    if install_tests:
        dico['srctest'] = TMPL_TEST % '$ASTER_VERSION_DIR/tests'
    else:
        dico['srctest'] = os.linesep.join([
            TMPL_TEST % '%(SRC)s/astest' % dico,
            TMPL_TEST % '%(SRC)s/../validation/astest' % dico])
    return dico


def as_str(value):
    return value if type(value) not in (list, tuple) else ' '.join(value)


TMPL_CONFIG_TXT = r"""# Configuration file created by waftools/legacy
# Libraries, compilers are not relevant 
#
ENV_SH         | env     | -     | profile.sh
ADDMEM         | memory  | -     | %(ADDMEM)s
DEFS           | defined | -     | %(DEFINES)s
#
PYTHON         | python  | -     | %(PYTHON)s
#
BIN_NODBG      | bin     | -     | %(PREFIX)s/bin/aster
BIN_DBG        | bin     | -     | %(PREFIX)s/bin/asterd
BINCMDE        | bin     | -     | %(PYTHONDIR)s/Cata
BINELE         | bin     | -     | $ASTER_VERSION_DIR/elements
BINPICKLED     | bin     | -     | %(SRC)s/build/release/catalo/cata_ele.pickled
#
# for as_run --make_shared...
F90            | compil  | -     | %(FC)s
OPTF90_O       | compil  | -     | %(FCFLAGS)s
#
# for as_run --messages, --get, --show..., and astout
SRCFOR         | src     | -     | %(SRC)s/bibfor
SRCC           | src     | -     | %(SRC)s/bibc
SRCPY          | src     | -     | %(PYTHONDIR)s
SRCCATA        | src     | -     | %(SRC)s/catalo
SRCCAPY        | src     | -     | %(SRC)s/catapy
%(srctest)s
SRCMAT         | src     | -     | %(SRC)s/../data/materiau
SRCHIST        | src     | -     | %(SRC)s/histor
#
REPPY          | exec    | -     | Python
ARGPYT         | exec    | -     | Execution/E_SUPERV.py
ARGEXE         | exec    | -     | -eficas_path ./Python
#
REPOUT         | exec    | -     | $ASTER_ROOT/outils
REPMAT         | exec    | -     | $ASTER_VERSION_DIR/materiau
REPDEX         | exec    | -     | $ASTER_VERSION_DIR/datg
"""

TMPL_TEST = """SRCTEST        | src     | -     | %s"""

TMPL_PROFILE = r"""# created by waftools/legacy

LD_LIBRARY_PATH=$ASTER_VERSION_DIR/lib:\
%(LD_LIBRARY_PATH)s:\
$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

PYTHONPATH=%(CFG_PYTHONPATH)s:\
$PYTHONPATH
export PYTHONPATH

PYTHONHOME=%(PYTHONHOME)s
export PYTHONHOME

%(OPT_ENV)s
"""
