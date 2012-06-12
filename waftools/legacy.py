# encoding: utf-8

import os
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
        Task.Task.__init__(self, *k, **kw)
        self.src = kw['src']
        self._relevant_env_keys = ('PREFIX', 'DEFINES', 'PYTHON',
                                   'PYTHONARCHDIR', 'LIBPATH', 'LIBDIR',
                                   'ASTERDATADIR')

    def run(self):
        cfg = self.outputs[0].abspath()
        prof = self.outputs[1].abspath()
        env = self.env.copy()
        env['LD_LIBRARY_PATH'] = os.environ.get('LD_LIBRARY_PATH', '').split(os.pathsep)
        ld_path = list(chain(*[Utils.to_list(env[name])
                       for name in ('LIBPATH', 'LIBDIR', 'LD_LIBRARY_PATH') if env[name]]))
        sep = os.pathsep + '\\\n'
        dico = dict([(k, as_str(env[k])) \
                        for k in ('PREFIX', 'PYTHON', 'PYTHONARCHDIR', 'ASTERDATADIR')])
        dico['DEFINES'] = ' '.join([d for d in env['DEFINES'] if '=' not in d])
        dico['LD_LIBRARY_PATH'] = sep.join(ld_path)
        dico['SRC'] = self.src
        open(cfg, 'w').write(TMPL_CONFIG_TXT % dico)
        open(prof, 'w').write(TMPL_PROFILE % dico)
        return 0
    
    def sig_explicit_deps(self):
        m = Utils.md5()
        m.update(repr([self.env[k] for k in self._relevant_env_keys]))
        return m.digest()

def build(self):
    if not self.env.legacy:
        return
    tgt = ['config.txt', 'profile.sh']
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
DEFS           | defined | ?     | %(DEFINES)s
#
PYTHON         | python  | 2.7   | %(PYTHON)s
#
BIN_NODBG      | bin     | 11-04 | %(PREFIX)s/bin/aster
BIN_DBG        | bin     | 11-04 | %(PREFIX)s/bin/asterd
BINCMDE        | bin     | 11-04 | %(PYTHONARCHDIR)s/Cata
BINELE         | bin     | 11-04 | $ASTER_VERSION_DIR/elements
BINPICKLED     | bin     | 11-04 | %(SRC)s/build/release/catalo/cata_ele.pickled
#
# pour as_run --messages, --get, --show...
SRCFOR         | src     | 11-04 | %(SRC)s/bibfor
SRCF90         | src     | 11-04 | %(SRC)s/bibf90
SRCFERM        | src     | 11-04 | %(SRC)s/fermetur
SRCC           | src     | 11-04 | %(SRC)s/bibc
SRCPY          | src     | 11-04 | %(PYTHONARCHDIR)s
SRCCATA        | src     | 11-04 | %(SRC)s/catalo
SRCCAPY        | src     | 11-04 | %(SRC)s/catapy
SRCTEST        | src     | 11-04 | %(SRC)s/../tests/astest
SRCMAT         | src     | 11-04 | %(SRC)s/../data/materiau
SRCHIST        | src     | 11-04 | %(SRC)s/histor
#
REPPY          | exec    | 11-04 | Python
ARGPYT         | exec    | 03-02 | Execution/E_SUPERV.py
ARGEXE         | exec    | 03-02 | -eficas_path ./Python
#
REPOUT         | exec    | 11-04 | $ASTER_ROOT/outils
REPMAT         | exec    | 02-05 | $ASTER_VERSION_DIR/materiau
REPDEX         | exec    | 02-05 | $ASTER_VERSION_DIR/datg
"""

TMPL_PROFILE = r"""# created by waftools/legacy

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:\
$ASTER_VERSION_DIR/lib:\
%(LD_LIBRARY_PATH)s

export LD_LIBRARY_PATH
"""
