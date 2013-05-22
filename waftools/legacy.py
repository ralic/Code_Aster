# encoding: utf-8

import os
import os.path as osp
from itertools import chain
from waflib import Task, Utils, Errors

def options(self):
    group = self.get_option_group("Aster options")
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


class create_asrun_files(Task.Task):
    def __init__(self, *k, **kw):
        self.src = kw['src']
        self._relevant_env_keys = ('PREFIX', 'DEFINES', 'PYTHON',
                                   'FC', 'FCFLAGS',
                                   'PYTHONARCHDIR', 'LIBPATH', 'LIBDIR',
                                   'ASTERDATADIR', 'OPT_ENV', 'install_tests')
        Task.Task.__init__(self, *k, **kw)

    def run(self):
        from Options import options as opts
        cfg = self.outputs[0].abspath()
        prof = self.outputs[1].abspath()
        env = self.env.copy()
        env['LD_LIBRARY_PATH'] = os.environ.get('LD_LIBRARY_PATH', '').split(os.pathsep)
        ld_path = list(chain(*[Utils.to_list(env[name])
                       for name in ('LIBPATH', 'LIBDIR', 'LD_LIBRARY_PATH') if env[name]]))
        ld_path = [path for path in ld_path if path]
        sep = os.pathsep + '\\\n'
        dico = dict([(k, as_str(env[k])) \
                        for k in ('PREFIX', 'PYTHON', 'PYTHONARCHDIR', 'ASTERDATADIR')])
        dico['DEFINES'] = ' '.join([d.split('=')[0] for d in env['DEFINES']])
        dico['LD_LIBRARY_PATH'] = sep.join(ld_path)
        dico['SRC'] = self.src
        dico['FC'] = env['FC']
        flags = [' '.join(env[i]) for i in env.keys() if i.startswith('FCFLAGS')]
        dico['FCFLAGS'] = ' '.join(flags)
        dico['OPT_ENV'] = self.env['OPT_ENV'] and os.linesep.join(self.env['OPT_ENV']) or ''
        dico['ADDMEM'] = self.env['ADDMEM'] or 250
        if self.env.install_tests or opts.install_tests:
            dico['srctest'] = TMPL_TEST % '$ASTER_VERSION_DIR/tests'
        else:
            dico['srctest'] = os.linesep.join([
                TMPL_TEST % '%(SRC)s/astest' % dico,
                TMPL_TEST % '%(SRC)s/../validation/astest' % dico])
        open(cfg, 'w').write(TMPL_CONFIG_TXT % dico)
        open(prof, 'w').write(TMPL_PROFILE % dico)
        return 0
    
    def sig_explicit_deps(self):
        #XXX seems not work
        m = Utils.md5()
        m.update(repr([self.env[k] for k in self._relevant_env_keys]))
        return m.digest()

def build(self):
    if not self.env.legacy:
        return
    tgt = ['config.txt', 'profile.sh']
    # force rebuild
    bldpath = self.path.get_bld().abspath()
    for fname in tgt:
        try:
            os.remove(osp.join(bldpath, fname))
        except (OSError, IOError):
            pass
    cfg = create_asrun_files(src=self.path.abspath(), env=self.env)
    cfg.set_outputs(map(self.path.find_or_declare, tgt))
    self.add_to_group(cfg)
    self.install_files(self.env.ASTERDATADIR[0], tgt)

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
BINCMDE        | bin     | -     | %(PYTHONARCHDIR)s/Cata
BINELE         | bin     | -     | $ASTER_VERSION_DIR/elements
BINPICKLED     | bin     | -     | %(SRC)s/build/release/catalo/cata_ele.pickled
#
# for as_run --make_shared...
F90            | compil  | -     | %(FC)s
OPTF90_O       | compil  | -     | %(FCFLAGS)s
#
# for as_run --messages, --get, --show..., and astout
SRCFOR         | src     | -     | %(SRC)s/bibfor
SRCF90         | src     | -     | %(SRC)s/bibf90
SRCFERM        | src     | -     | %(SRC)s/fermetur
SRCC           | src     | -     | %(SRC)s/bibc
SRCPY          | src     | -     | %(PYTHONARCHDIR)s
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

%(OPT_ENV)s
"""
