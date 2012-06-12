#@ MODIF E_Core Execution  DATE 12/06/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

"""
Conceptuellement, les objets définis ici pourraient l'être dans le module
C/Python 'aster_core' (qui est en C pour l'interface avec le fortran).
Il est plus simple et plus naturel de les écrire en Python.

Ces fonctions sont indépendantes des étapes (sinon elles seraient dans
B_ETAPE/E_ETAPE) et des concepts/ASSD.
"""

# This package modify the sys.path

import sys
import os.path as osp
import time
from datetime import datetime
import platform
from optparse import OptionParser

# Pas d'import des autres packages d'aster
# car le premier import de ce module est fait avant l'ajout des paths.

def check_value(option, opt, value, parser):
    """Callback to check some values."""
    if opt == '--bibpyt':
        if not osp.isdir(value):
            parser.error("option '%s' expects an existing directory" % opt)
        if not osp.isdir(osp.join(value, 'Accas')):
            parser.error("option '%s' should define a directory containing 'Accas'." % opt)
    if opt == '--commandes':
        if not osp.isfile(value):
            parser.error("option '%s' expects an existing file" % opt)
    setattr(parser.values, option.dest, value)

class CoreOptions(object):
    """Classe de stockage des arguments et options de la ligne de commande
    afin de permettre une interrogation ultérieure depuis n'importe quelle
    partie du code.
    On centralise également le stockage d'informations de base comme le nom
    de la machine, la plate-forme, etc.

    :Attention: ``sys.path`` est modifiée juste après le l'interprétation de
                la ligne de commande. La liste est enrichie avec la valeur de
                ``--bibpyt`` (ajouté en debut de list).
                "." et "bibpyt/Cata"  sont aussi ajoutés.
    """
    doc = """usage: ./%%prog %s [-h|--help] [options]""" % sys.argv[0]

    def __init__(self):
        """Initialisation."""
        self._dbg = False
        self.opts = None
        self.args = None
        self.info = {}
        self.parser = parser = OptionParser(usage=self.doc,
            prog=osp.basename(sys.executable))
        parser.add_option('--commandes', dest='fort1', type='str', metavar='FILE',
            action='callback', callback=check_value,
            help="Code_Aster command file")
        parser.add_option('--bibpyt', dest='bibpyt', type='string', metavar='DIR',
            action='callback', callback=check_value,
            help="path to Code_Aster python source files")

        parser.add_option('--memjeveux', dest='memjeveux', type='float', action='store',
            help="maximum size of the memory taken by the execution (in Mw)")
        parser.add_option('--memory', dest='memory', type='float', action='store',
            help="maximum size of the memory taken by the execution (in MB)")
        parser.add_option('--tpmax', dest='tpmax', type='float', action='store',
            help="limit of the time of the execution (in seconds)")
        parser.add_option('--max_base', dest='maxbase', type='float', action='store',
            help="limit of the size of the results database")

        parser.add_option('--dbgjeveux', dest='dbgjeveux',
            action='store_true',
            help="maximum size of the memory taken by the execution in Mw")

        parser.add_option('--num_job', dest='jobid', action='store',
            help="job ID of the current execution")
        parser.add_option('--mode', dest='mode', action='store',
            help="execution mode (interactive or batch)")
        parser.add_option('--interact', dest='interact',
            action='store_true', default=False,
            help="as 'python -i' works, it allows to enter commands after the "
                 "execution of the command file.")

        parser.add_option('--rep_outils', dest='repout', type='str', metavar='DIR',
            action='store',
            help="directory of Code_Aster tools (ex. $ASTER_ROOT/outils)")
        parser.add_option('--rep_mat', dest='repmat', type='str', metavar='DIR',
            action='store',
            help="directory of materials properties")
        parser.add_option('--rep_dex', dest='repdex', type='str', metavar='DIR',
            action='store',
            help="directory of external datas (geometrical datas or properties...)")
        parser.add_option('--rep_glob', dest='repglob', type='str', metavar='DIR',
            action='store', default='.',
            help="directory of the results database")
        parser.add_option('--rep_vola', dest='repvola', type='str', metavar='DIR',
            action='store', default='.',
            help="directory of the temporary database")

        parser.add_option('--suivi_batch', dest='suivi_batch',
            action='store_true', default=False,
            help="force to flush of the output after each line")
        parser.add_option('--totalview', dest='totalview',
            action='store_true', default=False,
            help="required to run Code_Aster through the Totalview debugger")
        parser.add_option('--verif', dest='verif',
            action='store_true', default=False,
            help="only check the syntax of the command file is done")

    def parse_args(self, argv):
        """Analyse les arguments de la ligne de commmande."""
        argv = _bwc_arguments(argv)
        self.opts, self.args = self.parser.parse_args(argv[1:])
        self.set_path()
        self.default_values()
        self.init_info()
        if self._dbg:
            print 'options   :', self.opts
            print 'arguments :', self.args

    def set_path(self):
        sys.path.insert(0, '.')
        bibpyt = self.get_option('bibpyt')
        if bibpyt:
            sys.path.insert(0, osp.abspath(bibpyt))
            sys.path.append(osp.join(osp.abspath(bibpyt), 'Cata'))

    def init_info(self):
        """Stocke les informations générales (machine, os...)."""
        import aster_core
        import aster
        # hostname
        self.info['hostname'] = platform.node()
        # ex. i686/x86_64
        self.info['processor'] = platform.machine()
        # ex. Linux
        self.info['system'] = platform.system()
        # ex. 32bit/64bit
        self.info['architecture'] = platform.architecture()[0]
        # ex. 2.6.32...
        self.info['osrelease'] = platform.release()
        # Code_Aster version
        #   version=11.1.4, versionD0=11.01.04, versMAJ=11, versMIN=1, versSUB=4
        for attr in ('version', 'versionD0', 'versionSTA', 'versLabel', 'date', 'exploit'):
            self.info[attr] = None
        self.info['versMAJ'] = self.info['versMIN'] = self.info['versSUB'] = 0
        if self.opts.bibpyt:
            from Accas import properties
            for attr in ('version', 'date', 'exploit', 'parentid',
                         'branch', 'from_branch', 'changes', 'uncommitted'):
                self.info[attr] = getattr(properties, attr)
            vers = self.info['version']
            if vers:
                aster_core.__version__ = vers
                # for backward compatibility
                aster.__version__ = aster_core.__version__
                lv = vers.split('.')
                try:
                    self.info['versMAJ'] = int(lv.pop(0))
                    self.info['versMIN'] = int(lv.pop(0))
                    self.info['versSUB'] = int(lv.pop(0))
                except (IndexError, ValueError):
                    pass
            self.info['versionD0'] = '%d.%02d.%02d' % (self.info['versMAJ'],
                self.info['versMIN'], self.info['versSUB'])

    def set_info(self, key, value):
        """Définit la valeur d'une information générale."""
        assert self.info.has_key(key), "general information '%s' not defined" % key
        self.info[key] = value

    def default_values(self):
        """Définit les valeurs par défaut pour certaines options."""
        if self.opts.tpmax is None and platform.system() == 'Linux':
            # use rlimit to set to the cpu "ulimit"
            import resource
            limcpu = resource.getrlimit(resource.RLIMIT_CPU)[0]
            if limcpu < 0:
                limcpu = int(1.e18)
            self.opts.tpmax = limcpu
        if not self.opts.memory and self.opts.memjeveux:
            import aster_core # depend on self.opts.bibpyt (see parse_args)
            self.opts.memory = self.opts.memjeveux * aster_core.LONG_INTEGER_MOTS

    def sub_tpmax(self, tsub):
        """Soustrait `tsub` au temps cpu maximum."""
        self.opts.tpmax = self.opts.tpmax - tsub

    def get_option(self, option, default=None):
        """Retourne la valeur d'une option ou d'une information de base."""
        assert self.opts, 'options not initialized!'
        if hasattr(self.opts, option):
            value = getattr(self.opts, option)
        else:
            value = self.info.get(option, default)
        if type(value) in (str, unicode):
            from strfunc import convert
            value = convert(value)
        if self._dbg:
            print "<CoreOptions.get_option> option=%r value=%r" % (option, value)
        return value

def getargs(argv=None):
    """
    Récupération des arguments passés à la ligne de commande
    """
    coreopts = CoreOptions()
    coreopts.parse_args(argv or sys.argv)
    return coreopts

def checksd(nomsd, typesd):
    """Vérifie la validité de la SD `nom_sd` (nom jeveux) de type `typesd`.
    Exemple : typesd = sd_maillage
    C'est le pendant de la "SD.checksd.check" à partir d'objets nommés.
    Code retour :
      0 : tout est ok
      1 : erreurs lors du checksd
      4 : on n'a meme pas pu tester
    """
    from Utilitai.Utmess import UTMESS
    nomsd  = nomsd.strip()
    typesd = typesd.lower().strip()
    # import
    iret = 4
    try:
        sd_module = __import__('SD.%s' % typesd, globals(), locals(), [typesd])
    except ImportError, msg:
        UTMESS('F', 'SDVERI_1', valk=typesd)
        return iret
    # on récupère la classe typesd
    clas = getattr(sd_module, typesd, None)
    if clas:
        objsd = clas(nomj=nomsd)
        chk = objsd.check()
        ichk = min([1,] + [level for level, obj, msg in chk.msg])
        if ichk == 0:
            iret = 1
        else:
            iret = 0
    # on imprime les messages d'erreur (level=0):
    for level, obj, msg in chk.msg:
        if level == 0 :
            aster.affiche('MESSAGE',repr(obj)+msg)
    return iret

def get_branch_orig():
    """Retourne la branche d'origine dont sont issus les sources"""
    from Accas import properties
    return properties.from_branch

def _print_header():
    """Appelé par entete.F pour afficher des informations sur
    la machine."""
    import aster_core
    from i18n import localization
    from Utilitai.Utmess import UTMESS
    from Accas import properties
    names = {
        'stable' : _(u"""EXPLOITATION (stable)"""),
        'stable-updates' : _(u"""CORRECTIVE AVANT STABILISATION (stable-updates)"""),
        'testing' : _(u"""DÉVELOPPEMENT STABILISÉE (testing)"""),
        'unstable' : _(u"""DÉVELOPPEMENT (unstable)"""),
    }
    unkn = _(u"""DÉVELOPPEMENT (%s)""") % aster_core.get_option('from_branch')
    typvers = names.get(aster_core.get_option('from_branch'), unkn)
    aster_core.set_info('versLabel', typvers)
    lang_settings = '%s (%s)' % localization.get_current_settings()

    date_build = aster_core.get_option('date')
    changes = aster_core.get_option('changes')
    uncommitted = aster_core.get_option('uncommitted')
    UTMESS('I', 'SUPERVIS2_4', valk=typvers)
    if not changes and not uncommitted:
        UTMESS('I', 'SUPERVIS2_9',
            valk=(aster_core.get_option('version'), date_build,),)
    else:
        UTMESS('I', 'SUPERVIS2_23',
            valk=(aster_core.get_option('version'), date_build,
                  aster_core.get_option('parentid'), aster_core.get_option('branch')),)
    UTMESS('I', 'SUPERVIS2_10',
        valk=("1991", time.strftime('%Y'),
              time.strftime('%c'),
              aster_core.get_option('hostname'),
              aster_core.get_option('architecture'),
              aster_core.get_option('processor'),
              aster_core.get_option('system') + ' ' + aster_core.get_option('osrelease'),
              lang_settings,),)
    # avertissement si la version a plus de 15 mois
    if aster_core._NO_EXPIR == 0:
        try:
            d0, m0, y0 = map(int, date_build.split('/'))
            tbuild = datetime(y0, m0, d0)
            tnow = datetime.today()
            delta = (tnow - tbuild).days
            if delta > 550:
                UTMESS('A', 'SUPERVIS2_2')
        except ValueError:
            pass

def _print_alarm():
    import aster_core
    from Utilitai.Utmess import UTMESS
    changes = aster_core.get_option('changes')
    uncommitted = aster_core.get_option('uncommitted')
    if changes:
        UTMESS('A+', 'SUPERVIS_41', valk=aster_core.get_option('version'), vali=changes)
    if uncommitted and type(uncommitted) is list:
        fnames = ', '.join(uncommitted)
        UTMESS('A+', 'SUPERVIS_42', valk=(aster_core.get_option('parentid'), fnames),)
    UTMESS('I', 'VIDE_1')

def print_header(part):
    """Appelé par entete.F pour afficher des informations sur la machine.
    Certaines informations étant obtenues en fortran, une partie des messages
    est imprimée par le fortran. On a donc découpé en plusieurs morceaux.
    part = 1 : entête principal : ici
    part = 2 : informations librairies : dans entete.F
    part = 3 : message d'alarme en cas de modification du code source : ici
    """
    if part == 1:
        _print_header()
    elif part == 3:
        _print_alarm()
    else:
        raise ValueError("unknown value for 'part'")
    

def _bwc_arguments(argv):
    """Fonction de compatibilité de transition vers des options "GNU".
    """
    from warnings import warn, simplefilter
    # DeprecationWarning are ignored in python2.7 by default
    simplefilter('default')

    inew = max([a.startswith('--commandes') for a in argv])
    if inew:
        return argv
    long_opts = (
        'eficas_path', 'commandes', 'num_job', 'mode',
        'rep_outils', 'rep_mat', 'rep_dex', 'rep_vola', 'rep_glob',
        'memjeveux', 'tpmax', 'memory', 'max_base',
    )
    # boolean options
    long_opts_sw = (
        'suivi_batch', 'interact', 'verif', 'totalview', 'dbgjeveux',
    )
    # removed options
    long_opts_rm = ('rep', 'mem', 'mxmemdy', 'memory_stat', 'memjeveux_stat',
                    'type_alloc', 'taille', 'partition',
                    'origine', 'ORBInitRef',)
    # renamed options
    long_opts_mv = { 'eficas_path' : 'bibpyt' }
    orig = argv[:]
    new = []
    buffer = ''
    while len(orig) > 0:
        arg = orig.pop(0)
        opt = arg.startswith('-') and arg.replace('-', '')
        opt2 = long_opts_mv.get(opt, opt)
        if opt in long_opts:
            val = orig.pop(0)
            new.append('--%s=%s' % (opt2, val))
        elif opt in long_opts_sw:
            new.append('--' + opt2)
        elif opt in long_opts_rm:
            val = orig.pop(0)
            warn("this command line option is deprecated : --%s" % opt,
                 DeprecationWarning, stacklevel=3)
        else:
            new.append(arg)
    return new
