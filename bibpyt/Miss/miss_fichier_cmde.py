# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mathieu.courtois at edf.fr

"""Construction d'un fichier de commandes Miss"""

import os
import os.path as osp
import re
from pprint import pformat
from math import sqrt
import tempfile
import unittest

from Miss.miss_utils import dict_format
from Miss.miss_domain import MissDomains

from Utilitai import test_utils


class MissCmdeGenerator(object):

    """Construit un fichier de commandes Miss"""
    _dbg = False

    def __init__(self, param, struct, filename_callback):
        """Initialisation
        `filename_callback` fournit un nom de fichier 'local' pour un
        type donné"""
        self.param = param
        self.fname = filename_callback
        self.dinf = {
            "titre": struct.titre,
            "fich_mvol": self.fname('mvol'),
            "fich_chp": self.fname('chp'),
            "fich_sol": self.fname('sol'),
            "fich_impe": self.fname('resu_impe'),
            "fich_forc": self.fname('resu_forc'),
            "fich_ext": self.fname('ext'),
            "binaire": "",
            "surf": "",
            "rfic": "",
        }
        if self.param['AUTO'] == "OUI":
            if self.param['OPTION_RFIC'] == 'OUI':
                self.dinf["rfic"] = str(self.param['RFIC'])
        else:
            if self.param['RFIC'] != 0.:
                self.dinf["rfic"] = str(self.param['RFIC'])
        
        if self.param["TYPE"] == "BINAIRE":
            self.dinf["binaire"] = "BINA"
        if self.param["SURF"] == "OUI":
            self.dinf["surf"] = "SURF"

        self.domain = MissDomains(self.param['_hasPC'],
                                  self.param['ISSF'] == 'OUI')
        # lignes du fichier
        self.lines = []
        # pour savoir si forces/impédances ont été calculées
        self._impe_calc = False
        self._forc_calc = False

    def build(self):
        """Construit et retourne le fichier"""
        lines = ['* Debut de l etude Miss',
                 '* ---------------------']
        lines.extend(self.build_data())
        lines.extend(self.build_calcul())
        lines.extend(self.build_post())
        lines.extend(['*', '* Fin de l etude Miss',
                           '* -------------------',
                      'FIN'])
        content = os.linesep.join(lines)
        text = remove_empty_lines(content)
        if self._dbg:
            dtmp = tempfile.mkdtemp(prefix=self.param["PROJET"] + '_')
            open(osp.join(dtmp, 'command'), 'wb').write(text)
            open(osp.join(dtmp, 'para'), 'wb').write(pformat(self.param))
            print '#dbg command file:', osp.join(dtmp, 'command')
        return text

    def build_data(self):
        """Définition du menu DATA"""
        lines = [
            '*', '* Nom generique des fichiers MISS',
                 '* --------------------------------',
            'GENER %s' % self.param['PROJET'],
            '*', '* Debut du menu DATA',
                 '* ------------------',
            'DATA',
            '*', '* Titre de l etude',
                 '*-----------------',
            'TITRE',
            '%s' % self.dinf['titre'],
            '*', '* Lecture du maillage',
                 '*--------------------',
            'MVOL %s' % self.dinf['fich_mvol'],
        ]
        lines.extend(self.bloc_group())
        lines.extend(self.bloc_modes())
        lines.extend(self.bloc_integr())
        lines.extend(self.bloc_lfreq())
        lines.extend(self.bloc_domain())
        lines.extend(['*', '* Fin du menu DATA',
                           '* ----------------',
                      'FIND'])
        return lines

    def build_calcul(self):
        """Définition des calculs"""
        lines = ['*', '* Debut de l execution',
                      '* --------------------']
        lines.extend(self.calcul_champ_incident())
        _use_bcle_freq = self.param['ISSF'] == 'OUI' or self.param['_hasPC']
        if _use_bcle_freq:
            lines.extend([
                '*',
                '* Boucle sur les frequences',
                '* -------------------------',
                'DOFReq TOUTES SAVE MVFD TOT UI TUI IMPD FORCE', ])
        lines.extend(self._chargement_domaine('sol'))
        lines.extend(self.calcul_fonction_green())
        lines.extend(self.calcul_champs())
        lines.extend(self.calcul_global())
        if _use_bcle_freq:
            lines.extend(['*', '* Fin de la boucle sur les frequences',
                               '* -----------------------------------',
                          'ENDFreq'])
        lines.extend(['*', '* Fin de l execution',
                           '* ------------------'])
        return lines

    def build_post(self):
        """Définition des post-traitements"""
        lines = ['*', '* Debut du post-traitement',
                      '* ------------------------']
        if not self.param['_hasPC']:
            if self._impe_calc:
                lines.extend(['*',
                              '* Post-traitement des impédances',
                              '* ------------------------------',
                              'POST',
                              'FICH %s  %s'
                              % (self.dinf['fich_impe'], self.dinf['binaire']),
                              'IMPDC',
                              'FREQ TOUTES',
                              'CHPU TOUS',
                              'CHPT TOUS',
                              'FINP'])
            if self._forc_calc:
                lines.extend(['*',
                              '* Post-traitement des forces sismiques',
                              '* ------------------------------------',
                              'POST',
                              'FICH %s' % self.dinf['fich_forc'],
                              'FORCE',
                              'FREQ TOUTES',
                              'DDL TOUS',
                              'UI TOUS',
                              'FINP'])
        else:
            lines.extend(['*',
                          '* Definition du signal',
                          '* --------------------',
                          'SIGNAL LIRE %s' % self.fname("01.sign")])
            lines.extend(self._chargement_domaine('sol'))
            lines.extend(['*',
                          '* Post-traitement aux points de controle',
                          '* --------------------------------------',
                          'POST',
                        ('SPEC NF=%%%(I)s FMAX=%%%(R)s' % dict_format)
                % (self.param['_nbfreq'], self.param['FREQ_MAX']),
                'FICH %s' % self.fname("01.csol.a"),
                'CSOL LEGENDE ACCELERATIONS',
                'FREQ TOUTES',
                'CHAMP DE    1 A    3  PAS 1',
                'POINTS DE    1 A    %d  PAS 1' % self.param['_nbPC'],
                'DDL TOUS',
                'FINS',
                '*',
                'FINP'])
        lines.extend(['*', '* Fin du post-traitement',
                           '* ----------------------'])
        return lines

    def bloc_group(self):
        """Déclaration des groupes"""
        lines = ['*', '* Definition des groupes',
                      '* ----------------------',
                 'GROUP']
        group = self.domain.group
        # for interf in ('sol-struct', 'fluide-struct', 'sol-fluide',
                       #'sol libre'):
            # if group.get(interf):
                # lines.extend(['%4d SURF' % group[interf], 'FIN'])
        if self.domain.def_all_domains:
            for volu in ('pc', 'struct'):
                if group.get(volu):
                    lines.extend(['%4d VOLUME' % group[volu], 'FIN'])
        lines.append('FING')
        return lines

    def bloc_modes(self):
        """Déclaration des modes"""
        lines = ['*', '* Definition des modes',
                      '* --------------------',
                 'CHAMP',
                 'LIRE %s' % self.dinf['fich_chp']]
        return lines

    def bloc_integr(self):
        """Paramètres d'intégration"""
        lines = ['*', '* Parametres d integration',
                      '* ------------------------',
                 'INTEGRATION RECT 6 8 TRIANGLE 12 12']
        return lines

    def bloc_lfreq(self):
        """Définition des fréquences de calcul"""
        lines = ['*', '* Definition de la plage de frequences',
                      '* ------------------------------------']
        freq_min = self.param["FREQ_MIN"]
        freq_max = self.param["FREQ_MAX"]
        freq_pas = self.param["FREQ_PAS"]
        freq_imag = self.param['FREQ_IMAG']
        _nbfreq = 0
        # formats possibles pour les fréquences
        assert (self.param["FREQ_MIN"],
                self.param["LIST_FREQ"]).count(None) == 1, \
            'expect FREQ_MIN xor LIST_FREQ, not together'
        if self.param["FREQ_MIN"] is not None:
            lines.append(("FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s "
                          "PAS %%(freq_pas)%(R)s\n" % dict_format) % locals())
            _nbfreq = int((freq_max - freq_min) / freq_pas) + 2
        if self.param["LIST_FREQ"] is not None:
            lfreq = list(self.param['LIST_FREQ'])
            nbf = len(lfreq)
            lines.extend([
                ("FREQUENCE %%%(I)s" % dict_format) % nbf,
                (dict_format['sR'] * nbf) % tuple(lfreq),
            ])
            _nbfreq = nbf + 1
        if self.param['FREQ_IMAG'] is not None:
            lines.append(("IMGO %%%(R)s\n" % dict_format) % freq_imag)
        # _nbfreq will be used in miss_post
        self.param.set('_nbfreq', _nbfreq)
        return lines

    def bloc_domain(self):
        """Définition des sous-domaines"""
        lines = [
            '*', '* Definition des sous-domaines',
                 '* ----------------------------']
        if self.domain.def_all_domains:
            lines.extend([
                '*', '* Definition du sous-domaine structure',
                     '* ------------------------------------',
                'SDOMAINE %4d GROUPE ' % self.domain['struct'][0] +
                ''.join(['%4d' % i for i in self.domain['struct'][1]]),
                'KCM',
                'FINS'])
        lines.extend([
            '*', '* Definition du sous-domaine sol',
                 '* ------------------------------',
            'SDOMAINE %4d GROUPE ' % self.domain['sol'][0] +
            ''.join(['%4d' % i for i in self.domain['sol'][1]]),
            self._materiau_sol(),
            'FINS'])
        return lines

    def calcul_champ_incident(self):
        """Calcul des champs incidents"""
        lines = []
        if self.param['_hasPC']:
            lines.extend(['*',
                          '* Chargement du domaine structure',
                          '* -------------------------------',
                          'DOMAINE %4d' % self.domain['struct'][0],
                          'EXTERIEUR',
                          'LIRE %s' % self.dinf['fich_ext'],
                          'FINE'])
        lines.extend(self._chargement_domaine('sol'))
        lines.extend(self._stratification_sol())
        lines.extend(self._source_sol())
        if self.param['_hasPC']:
            lines.extend(['*',
                          '* Calcul des champs incidents aux points de controle',
                          '* --------------------------------------------------',
                          'EXEC CONTROLE UI', ])
        return lines

    def calcul_fonction_green(self):
        """Calcul des fonctions de Green"""
        lines = []
        strat = self._stratification_sol()
        if strat:
            lines.extend(strat)
            lines.extend(['*',
                          '* Calcul des fonctions de Green',
                          '* -----------------------------',
                          'EXEC SPFR',
                          ])
        return lines

    def calcul_champs(self):
        """Calcul des champs rayonnés aux interfaces, et/ou assemblage des
        impédances et forces sismiques induites"""
        lines = []
        args = 'IMPEdance FORCe'
        if self.param['_hasPC']:
            args = 'UD0 CHAMP IMPEdance FORCe'
        lines.extend(['*',
                      '* Calcul des forces et impedances',
                      '* -------------------------------',
                      'EXEC UGTG %s' % args + self._rfic(),
                      ])
        self._impe_calc = "IMPEdance" in args
        self._forc_calc = "FORCe" in args
        return lines

    def calcul_global(self):
        """Résolution globale si nécessaire"""
        lines = []
        if self.param['_hasPC'] or self.param['ISSF'] == 'OUI':
            lines.extend(['*',
                          '* Resolution du probleme d interaction',
                          '* ------------------------------------',
                          'EXEC GLOBAL'])
        if self.param['_hasPC']:
            lines.extend(self._chargement_domaine('sol'))
            lines.extend(['*',
                          '* Synthese des champs sur les interfaces',
                          '* --------------------------------------',
                          'EXEC DIFFracte UTOT TTOT'])
            lines.extend(self._stratification_sol())
            lines.extend(['*',
                          '* Synthese des champs sur les points de controle',
                          '* ----------------------------------------------',
                          'EXEC CONTrole UTOT TTOT NOGEO'])
        return lines

    def _materiau_sol(self):
        """Définition du matériau sol : stratifié ou homogène"""
        if self.param.get('MATER_SOL'):
            val = self.param['MATER_SOL']
            young = val['E']
            vnu = val['NU']
            rho = val['RHO']
            beta = val['AMOR_HYST'] or 0.
            mat = 'MATE RO %%%(R)s VP %%%(R)s VS %%%(R)s BETA %%%(R)s' % dict_format
            vp = sqrt(young * (1. - vnu) / (
                rho * (1. + vnu) * (1. - 2. * vnu)))
            vs = sqrt(young / (2. * rho * (1. + vnu)))
            line = mat % (rho, vp, vs, beta)
        else:
            line = 'STRAtifie'
        return line

    def _stratification_sol(self):
        """Définition de la stratification du sol"""
        if self.param.get('MATER_SOL'):
            return []
        z0 = self.param['Z0']
        lines = ['*',
                 '* Definition de la stratification du sol',
                 '* --------------------------------------',
                 ('DOS2M Z0 %%%(R)s  %%s' %
                  dict_format) % (z0, self.dinf['surf']),
                 'LIRE %s' % self.dinf['fich_sol'], ]
        return lines

    def _source_sol(self):
        """Définition des sources dans le sol"""
        lines = []
        src = self.param.get('SOURCE_SOL')
        if not src:
            z0 = self.param['Z0']
            lines.extend(['*',
                          '* Definition des champs incidents',
                          '* -------------------------------',
                          'INCI 3',
                        ('DPLANE SV 1. Z0 %%%(R)s' % dict_format) % z0,
                '0. 0. 1.',
                ('DPLANE SH 1. Z0 %%%(R)s' % dict_format) % z0,
                '0. 0. 1.',
                ('DPLANE P 1. Z0 %%%(R)s' % dict_format) % z0,
                '0. 0. 1.',
            ])
        else:
            lines.extend(['*',
                          '* Definition de source dans le sol',
                          '* --------------------------------',
                          'INCI 1',
                        ('SOURCE %%%(R)s %%%(R)s %%%(R)s' %
                         dict_format) % src['POINT'],
                ('       %%%(R)s %%%(R)s %%%(R)s' %
                 dict_format) % src['DIRECTION'],
            ])
        lines.extend(['*',
                      '* Calcul des champs incidents',
                      '* ---------------------------',
                      'EXEC INCI',
                      ])
        return lines

    def _chargement_domaine(self, dom):
        """Chargement d'un domaine"""
        lines = ['*',
                 '* Chargement du domaine %s' % dom,
                 '* -------------------------------',
                 'DOMAINE %4d' % self.domain[dom][0]]
        return lines

    def _rfic(self):
        """Retourne les paramètres si RFIC (résonances fictives) est utilisé"""
        line = ""
        if self.dinf['rfic']:
            line = " RFIC %s %s" % (self.dinf['rfic'], self.dinf['rfic'])
        return line


class MissCmdeGeneratorInci(MissCmdeGenerator):

    """Construit un fichier de commandes Miss
    Calcul du champ incident pour la methode Laplace-temps"""

    def bloc_lfreq(self):
        """Définition des fréquences de calcul"""
        nbf = int(self.param['INST_FIN'] / self.param['PAS_INST'])
        frq = 1. / self.param['PAS_INST']
        self.param['FREQ_MAX'] = frq
        self.param['FREQ_MIN'] = frq / nbf
        self.param['FREQ_PAS'] = frq / nbf
        # car FREQ_IMAG ne sert qu'à déclencher le traitement
        self.param.set('FREQ_IMAG', None)
        return super(MissCmdeGeneratorInci, self).bloc_lfreq()

    def calcul_champs(self):
        """Assemblage des forces sismiques induites"""
        lines = ['*',
                 '* Calcul des forces sismiques',
                 '* ---------------------------',
                 'EXEC UGTG FORCe' + self._rfic()]
        self._forc_calc = True
        return lines


class MissCmdeGeneratorISSF(MissCmdeGenerator):

    """Construit un fichier de commandes Miss dans le cas ISSF"""

    def bloc_domain(self):
        """Définition des sous-domaines"""
        lines = super(MissCmdeGeneratorISSF, self).bloc_domain()
        lines.extend([
            '*', '* Definition du sous-domaine sol',
                 '* ------------------------------',
            'SDOMAINE %4d GROUPE ' % self.domain['fluide'][0] +
            ''.join(['%4d' % i for i in self.domain['fluide'][1]]),
            self._materiau_fluide(),
            'FINS'])
        return lines

    def calcul_champ_incident(self):
        """Calcul des champs incidents"""
        lines = super(MissCmdeGeneratorISSF, self).calcul_champ_incident()
        lines.extend(self._chargement_domaine('fluide'))
        lines.extend(self._source_fluide())
        return lines

    def calcul_champs(self):
        """Calcul des champs rayonnés aux interfaces, et/ou assemblage des
        impédances et forces sismiques induites"""
        lines = super(MissCmdeGeneratorISSF, self).calcul_champs()
        lines.extend(self._chargement_domaine('fluide'))
        lines.extend(['*',
                      '* Calcul des forces et impedances',
                      '* -------------------------------',
                      'EXEC UGTG IMPEdance FORCe' + self._rfic(),
                      ])
        return lines

    def _source_sol(self):
        """Définition des sources dans le sol"""
        if not self.param.get('SOURCE_FLUIDE'):
            return super(MissCmdeGeneratorISSF, self)._source_sol()
        else:
            return []

    def _materiau_fluide(self):
        """Définition des propriétés du fluide acoustique homogène"""
        assert self.param.get('MATER_FLUIDE'), "'MATER_FLUIDE' must be defined"
        # si SURF='OUI': demi-espace fluide, sinon FLUIDE
        mat = '%%s RO %%%(R)s CELER %%%(R)s BETA %%%(R)s SURF 0.' % dict_format
        typ = 'FLUIDE'
        val = self.param['MATER_FLUIDE']
        if val['DEMI_ESPACE'] == 'OUI':
            typ = 'DFLUIDE'
        rho = val['RHO']
        celr = val['CELE']
        beta = val['AMOR_BETA'] or 0.
        line = mat % (typ, rho, celr, beta)
        return line

    def _source_fluide(self):
        """Définition des sources dans le fluide"""
        lines = []
        src = self.param.get('SOURCE_FLUIDE')
        if src:
            lines.extend(['*',
                          '* Definition de source dans le fluide',
                          '* -----------------------------------',
                          'INCI 1',
                        ('SOURCE %%%(R)s %%%(R)s %%%(R)s' %
                         dict_format) % src['POINT'],
                '       1.',
                '*',
                '* Calcul des champs incidents',
                '* ---------------------------',
                'EXEC INCI',
            ])
        return lines


def MissCmdeGen(param, struct, filename_callback, lapl_temps=False):
    """Return un objet générateur de fichier de commandes Miss"""
    if lapl_temps:
        return MissCmdeGeneratorInci(param, struct, filename_callback)
    elif param['ISSF'] == 'OUI':
        return MissCmdeGeneratorISSF(param, struct, filename_callback)
    else:
        return MissCmdeGenerator(param, struct, filename_callback)


def remove_empty_lines(text):
    """Remove empty lines from `text`"""
    lines = [line for line in text.splitlines() if line.strip()]
    lines.append('')
    return os.linesep.join(lines)


def remove_comments(text):
    """Remove Miss comments from `text`"""
    recmt = re.compile('^\*')
    lines = [line for line in text.splitlines() if not recmt.search(line)]
    return os.linesep.join(lines)


class TestMissCmde(unittest.TestCase):

    """test generator of miss commands file"""

    def setUp(self):
        """set up parameters"""
        class Parameters(dict):

            """fake MISS_PARAMETERS for unittests"""

            def set(self, key, value):
                """assign a value"""
                self[key] = value

        class Struct(object):

            """fake structure"""
            titre = "PRODUIT PAR CALC_MISS"
        self.struct = Struct()
        self.par = Parameters()
        self.par.update({
            # valeurs par défaut des mots-clés, cf. calc_miss.capy
            'PROJET': "MODELE",
            'FREQ_MIN': None,
            'FREQ_MAX': None,
            'FREQ_PAS': None,
            'LIST_FREQ': None,
            'FREQ_IMAG': None,
            'Z0': 0.,
            'SURF': 'NON',
            'ISSF': 'NON',
            'ALLU': 0.,
            'RFIC': 0.,
            'ALGO': None,
            'DREF': None,
            'SPEC_MAX': None,
            'SPEC_NB': None,
            'OFFSET_MAX': None,
            'OFFSET_NB': None,
            'TYPE': 'ASCII',
            '_hasPC': False,
            '_nbPC': 0,
        })
        self._write = False

    def fname(self, ext):
        """use PROJET"""
        return './' + self.par['PROJET'] + '.' + ext

    def test01_sdlx103a(self):
        """use LIST_FREQ"""
        refe = """
eJzdVsuOmzAU3fsr7jpVUsho2m4pOAwqr4IZjboZUeIklnikPKr+Ur+jP9ZrJ9OQCQm0yyKi2LJ9
7rnnPswMLP61a2HNIQfedvjviaYhM5gPPWSGK35VwJaXvBbfOo4nG9iIbCd43YDnxPG1s30Ym/o0
gthyn3TtzlCoRx4dFLzswDKYMQhEDiu4xkRb8x5xMrvczBwWURJGgZU4DEIjAtNwzecDTcRwedZ2
EgXNpiLP0+0QjETyHgMXFm9fKC+K71V+5L0RpWhFVSoptnXV7flVAdH1KEhCAgBLQMjEo2Tl+PJn
D8EV1foqGDEfDC8krhPRPrNst1dIYVqnBUeNGliDKFu+rVOJeyM+xPEZtSODOYEPETUZvIMPwCLH
8G2Xgr7E95Il5CnspXRyvKk5ZkWZ3ZDglc1VRD8n1DcpqOdOaqMvlst7TdPoG00/TOWsN33/sjok
WlN1zXxdYUTLURoX57uz49C0dadyZKo7sRV4huMrb3RQ0T6OMeTkk+nJWMejZmV2jRg8M7U8mZrr
JGaR0YqN4CdjK1GOVZfcbPWLUdXWD551N/JG7Td3ab3liK0q+G+cwILuOTGcXBiDVHqTpSetJiCf
DMRLD75o0sL9QtOOmYTzi9pphos626XFvsEiysQafZyQ2lhJpoO5bIWugb7Fj5i0AxSItgB89cWf
jQ8TN4Zj+1RY0jzr8n9wgdAnaoL04b8Mb0+XTVVm0mwjydg157f6Y0+bOFxFl1g19j28jUAUe75O
p3XBA15iMxscL6TqGKyCyOSnuh0vxPM7dF817RxlFa2K2q2Wr66K8+3KGXTh18+JPpAwiBl2D/Oh
rzbePN2zVALgo+MbBL2zTNXugQUJozFeYGEix2rEDiPsQeFVUkeFG9EU8uNj8h1znZ9EJFJtesbM
stwDncR5RevYR6cq/CqGI19Y6lvgN0yQXTY=
"""
        self.par.update({
            'PROJET': "SDLX103A",
            'LIST_FREQ': (12.25, 12.50, 12.75),
            'TYPE': 'BINAIRE',
            'Z0': 5.0,
            'DREF': 1.0,
            'ALGO': 'REGU',
            'OFFSET_MAX': 20,
            'OFFSET_NB': 200,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test01_sdlx103a.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test02_zzzz200b(self):
        """use FREQ_MIN/FREQ_MAX/FREQ_PAS"""
        refe = """
eJzdVtmumzAQffdXzHOqpEmqVn1F4HBR2WrMVdWXK0qcxBJLylL1l/od/bGOTXTJQgLtYxERVhif
OXNmMTOwxLe2ga2ADETT4tOTdU1mMB+6yAzf+GUOe1GISn5vBe6sYSfTgxRVDZ4TRff2nsPY1KcM
vMCiLtWYJxYt5KJowTK4MQhDujf4jsumEme0yezWmDucURKywIodDqHBwDRc86UjiRiuSJtWoaDb
RGZZsh+CUUjec+DC4m1HeJH/KLMT650sZCPLQsuwr8r2KO6Kh2GzIA4JAKwBAWOPko3jq589BJeX
27tgxHwyvJC4DqM9r/Rw1DhhUiW5QH1q2IIsGrGvEoX6IDPE8Tm1mcGdwAdGTQ4f4CNw5hi+7VJY
rfG+5QhZAkclm1rvKoH1UKQPBLjyuWH0c0x9k4JFURRYLZb6om+WSzCu/llhAqNrqyHZ6rKt59sS
M1qMUrnZ315sh7qpWl0jU0OKrMAzHL8LB3S+T2tMOvlkeirb0ahbVV8jDi9crXtX8xWJODMauZOi
d7aRxVh3KWPrvBl1b/0UafugdrS9eUiqvUBs3cF/EwQ29FkQwwWGOUhUNGnSazUBuXcQrT34ulTS
fFgs378WGEQx21x1UD3c2OkhyY81tlIqtxjlhALHfjIdeEes0DUwuugZy3aABFkuAO/V4tXwaaJh
OGanE5NkaZv9QwiEfqEmqBj+0wSfKbMri1Q5rhUduxLi0Zw8UycKN+wWq8L5hycSyPwotsm0adjh
xTa3wfFCqrfBJmCm6Ht3vBkvz9FjWTdzFFY2Om+PRr8+Mi7NdTAYwu9fE2MgYRBxnCDmU681nj/t
i9IBgGBclqkHPvAg5jTCAyyM1VqveLfCCRTepXPStpZ1rj48Jp8y95gpPKJUphe8LMvtyMTOFanT
DJ2q7FXuRr6t9JfAH4AuXdY=
"""
        self.par.update({
            'FREQ_MIN': 1.,
            'FREQ_MAX': 10.,
            'FREQ_PAS': 1.,
            'Z0': -6.05,
            'SURF': 'OUI',
            'ALGO': 'DEPL',
            'OFFSET_MAX': 60,
            'OFFSET_NB': 400,
            'SPEC_MAX': 0.03,
            'SPEC_NB': 2048,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test02_zzzz200b.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test03_fdlv112b(self):
        """use ISSF"""
        refe = """
eJzdVt1umzAUvvdTnOtUyUi6TdslAZMiEWDGRNNuKkqcxBKBlJ9pr7Tn2Ivt2KQNaX5brTdDRXGN
fc53vvPbA1s8NDXMBWQg6gZ/p7KqSA/6xx7Swy9+sYalyEUpHxuBNytYyHQlRVnB1I2iU3e7YibU
pwwc25sNh6OxlrrF0cBa5A3YJjePCiLtF/zGZV2KDnDSOzzMXc4oCVlgxy6H0GRgmZ5138JEGZ5I
60ZJQbWJzLJkeUyMkjSdBR4MPjxBHqx/FtkW90LmspZFrqlYlkWzEScJRNNZEIfEcf3JsevrYn7y
MrHuzGlIPJfRLpJ0tdGSwqRM1gI5qWAOMq/FskyU3DP+IK7P6YSZ3A18YNTi8Bm+AGeu6U88CsMR
/h2ihCyBjaJKrRelwCjI0zMmv9DpMPotpr5FwaaAz3Bg6If2jSGYauf2aefGMNBpkdobDb4+7x0j
riqaqj8v0Iv5RSgH95u96/hPdtEYEtnB1HT91gTQXlXr/lAZgO9HEnFm1nIhhfJ29M+VjjpKR/je
EtvxYhdJZcEerzfGLVjUw4TTu592u2PKNeFGl/AoZg7u7EA7Mr+Ul+qw3U1jnZW/RNqciUB93lol
5VKgbJ37ryEDS0HHA8fDtKpVDmB5SnacXyF5pyAaTeGHoTR86nIEB1lYHS8H6SpZbypMx1TO0cYr
kgRz0nIBnRl6JtoWzdBlRyAQY4BOwm/PB++uPBheOqfdkmRpk73BBEK/UwuUDWfcu8gaqQr2azzc
FqJx0aQZxkdTQqYaz1XVB8U4TDwCD2JOI4jMGYXpzLFxg0PsAsfXnYY2OAGz6H8Zlh1/Loo8VWor
BWZSCnGuQ7QKtU+j0GGHskrkHvsvyPVGzJPr+kArL57wiaKd6muae/EuMfPeeJlAlpsnT27K4iFD
9NsmXCbphSZ8qGniBWPT29VeHTMPb4n8rmTfdvDOntQLJXp/LtsUVd3HwJW19s25sUKPI/vHNf1I
+p/fV7JOwiDi2Fesu24843TT3CvfYayrlLX0OLHNbByQwlit9Yq3K+xM4UlA23ioZLVWw+zVM8xp
bEoiaetIF5ltey0cVW72YG2767XsvvDfhYldqSF/Ac9u8B8=
"""
        self.par.update({
            'PROJET': "FDLV112B",
            'FREQ_MIN': 0.1,
            'FREQ_MAX': 3.0,
            'FREQ_PAS': 2.9,
            'Z0': 5.,
            'SURF': 'NON',
            'ALGO': 'REGU',
            'OFFSET_MAX': 1000,
            'OFFSET_NB': 5000,
            'ISSF': 'OUI',
            'MATER_FLUIDE': {'RHO': 1000., 'CELE': 1500, 'AMOR_BETA': 0.,
                             'DEMI_ESPACE': 'OUI'},
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test03_fdlv112b.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test04_miss03c_inci(self):
        """first file built when FICHIERS_TEMPS is enabled"""
        refe = """
eJzdVtuOmzAQffdXzHOq0CTVVvuKwGFRudWYVdWXFSVOYolLiqHq53fs3W1IwoZ0H4uIIDA+M+fM
xczAFT/6DjYCShBdj9dQKkVmMB87yAzfRE0FO1GLVv7sBa5UsJXFXopWQein6VtrhzAejSgznp6C
/FDmhTDIL7H0UIm6B9fm9igYeX6D77jsWjEInswujbnPGSUJi93M55DYDBw7cJ6eQ0WMQBRdr1HQ
bS7LMt+NwWik8DEOwPo4DNuqfjXlS+xbWctONrWRZNc2/UG8KSRKwOIsIQCwAoTNQkrWfqR/3hhc
1WzeBCPOgx0mJPAZPY+u2B8MWpK3eSVQKwUbkHUndm2usa/kivgRpx6zuR9HwKjD4TPcA2e+HXkB
heUKz8tIoczhoCXU99tWYIXUxRUZznyuGf2a0cih4FKUBu6shTnofLEEWz9ZvT75gE8SOz23GhNP
Nb2abxrMbj0ZysX6/mQ5qK7tTb3cSil149D2I0NnCSbrL/dIhnxxQp3zdNKtrrIJhyeuVkdX8yVJ
ObM7uZXi6Gwt66lO08busDFNn/0WRX+ldoy9s8/bnUBs083/QgKbe0BivMAwB7lmU+RHrW5APjpI
VyF8X2hp7qz713LC/2nG1qN9pMabvNjn1UFhQxVyg1xvKHPsKseHT8RNAhs5po+wtEZCIQsL8Fxa
fw0fbjRMpuxMevKy6Mt3UCD0G3VAc/iv0zzQZ9vUhXavdFBeK8S1mTnQKE3W7BKrLfQ4kqrSW+dN
Wmce92AdM0ccu3a6DU9300OjujmKKTuTq2tD32wWp+bvCP2Il8QpxzHiPJxLjVtR/6RRiSZHzeQH
HmecpsR1A32bksx/vuIcSoZD61ZCZ5JNfN6YDfgPEoFDsQ==
"""
        self.par.update({
            'PROJET': "Miss_Laplace",
            'ALGO': 'DEPL',
            'DREF': 5,
            'FREQ_IMAG': 1.0,
            'INST_FIN': 2.0,
            'OFFSET_MAX': 40,
            'OFFSET_NB': 400,
            'PAS_INST': 0.05,
            'RFIC': 0.0,
            'SPEC_MAX': 0.075,
            'SPEC_NB': 16384,
            'SURF': 'OUI',
            'Z0': -5.8,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname, lapl_temps=True)
        txt = gen.build()
        if self._write:
            open('/tmp/test04_miss03c_inci.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test05_miss03c_loop(self):
        """using LAPL_TEMPS"""
        refe = """
eJzdVtuO2jAQffdXzDMVlMB2u32MEpONmlsTZ1X1ZZUGA5ZyoblU/aV+R3+sY4NKgEDSPjYCYeHx
mTNnLs4ETP61bWDNIQPetPjriromE5j2PWSCO16Zw5YXvBLfWo4na9iIdCd4VYNrR9Gts10Yi3o0
VJ5enWSfJSlXyEcuLeS8aMHUmd4LRg47uMdEU/EOeTK5NmY2CykJQt+MbQaBHoKhO8brgSpiODxt
WomCbhORZcm2D0YiuS++A7O3Xdqz/HuZHblvRCEaURZKkm1Vtnt+U0iUIPTjgADAAhA2dilZ2Z78
Wn1webm+CUaMZ90NiGOH9JJdutsrtCCpkpyjVjWsQRQN31aJxL6TK2J7jFqhzmzfg5AaDB7hCVho
657lUNAW+LlmClkCeymhXG8qjhVSpHdkuPC5CumnmHoGBfVoUh9tNl8sl09L+mY+J7Zr+XLncfbw
4f3D4zv8U+vTqy7berouMaHFoPer8+3ZcaibqlUlMjaKyPRd3fZUEBqoRB/XmG3y0XBlmqNBt7Kw
BhyeuVqcXE01ErFQb8RG8JOzlSiGmksam91eVK31g6ftnXJR9sYuqbYcsVUD/00Q2M+dIPprCnOQ
yGjS5KTVCOSTg2jhwpe5lObd7GkuH1lPAFEcrnpbp+7v63SX5PsaeygVa4x1RGVjIxk2LIkZODrG
GL1gRfdQIfMZ4Eeb/TF8HmkYDNmp9CRZ2mb/EAKhn6kBMob/Os0dfTZlkUr3tSRlVZzfG5MdjaJg
FV5jVTj+8HICke/5Ohk3DA94scUssN2AqmOw8kODn/p4uDHPr9R9WTdTlFc0Knv3Jr+6Mc7NVTAY
wq+fI2MggR8xnCbG86XieAm1r1INABzngWmoqQ/MjxmN8C4LYrlWK3ZY4UwKbpI6KlyLOpfvI6Ov
mvv8JCqRitMzdqbpHCjF9gW142wdq/JFHgdevNSrwW/+xHAQ
"""
        self.par.update({
            'PROJET': "Miss_Laplace",
            'DREF': 5,
            'FICHIER_SOL_INCI': './Miss_Laplace.sol.inci',
            'FREQ_IMAG': 64.97465,
            'INST_FIN': 2.0,
            'LIST_FREQ': (1.023383,),
            'NB_MODE': 6,
            'OFFSET_MAX': 40,
            'OFFSET_NB': 400,
            'PAS_INST': 0.05,
            'PRECISION': 1e-10,
            'RFIC': 0.0,
            'SPEC_MAX': 0.075,
            'SPEC_NB': 16384,
            'SURF': 'OUI',
            'Z0': -5.8,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test05_miss03c_loop.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test06_zzzz108b(self):
        """ISS using TABLE_CONTROL"""
        refe = """
eJzdV0uPozgQvvtX1LlHyZKMZjWXOdBgaLS81jat0VxGNHESJAIZHquefz9lJ5mQJ6i3D6tFITxs
f19VfWW7eABbvnQtLCQUINsOr0HeNOQBJtcO8oAtYbWBlSxlnf/oJI5sYJln61zWDQQe57fG9mFc
GlIG3/CYGZ8fNerejg42suzANoV5FYjsWrBN5G0te4aTh8vOwhOMkphFduIJiE0Glulb33dmIoYv
s7ZTKEib5kWRrq7BKKTgOfJh+sfB5Onmn6rY273My7zNq1KHYlVX3VbeDCC6zqIkJgAwB4RMAkoc
L1TPH/vPeLrX4DfV4iY4sZ7MICa+x2jf0my91UhxWqcbiTFrYAF52cpVnSrcO3oRLxTUZabwohAY
tQT8CZ9BMM8MXZ/CbI6/SyuhSGGrQqnul7XELCmzOyE543QY/TuhoUXBpqDiNP1kqINOjBmY6s2n
qbF78wHfxCY/73UtcE3VNZNFhSqXg6ZcjO9OhkPT1p3Om7EucTsKTC/U7sxAZ8D+HmUnf1mB0psP
0qqMGyA8oZofqSaKak64YGabL3N5JHTycmjWqc52f5LqOfcqs+5O/uj+1jqtVxKx9cx+Q/xIP3KE
fhWUeTRhF0kuX1tlJb3HOiJ8J3w3UhutT1UMs/So0gjkIwGfB/DNUAzGMZHxmSfMuXCsub7MZOt0
s21wGmf5Av0cMblwLlseZpsd+yb6x59hNr1iBjGm+Ixtvzs+jewYD/XT0qRF1hVvcAG1pxYoH4Zw
IO1eYVvl6hYVy6qyrati7Fy9xmpFoWARLnjJjvyx6rICE6qroVC736glDoV3mPwBIkoE5cDNZwrB
s2PjC4HAIPD0gtgGJ2LW/zePe8ItqzJT1Fomt5by3lbUk4PHDrvEqjH+WAhAvtnKRTpuw9nhJa5w
IbEN0PunEoFqAK2E1ExMov3dIU7bunopUJf9Plqn2cA+esnp+tGj6b+/yvxn2a5lI/sz45Cn2tZl
On4rPhhre46jnJSQqGQV+PdfSql7Pr/DOnBcA9TwYwggjFwaHfdQHYGXtywNfa7QdnDMCerAVnta
PW+rpp2gDHmrU+pecXel3MhXZXpLMMI9NzR9OJfBmE3VuPfP5fjUlX+7rpM44oLwGMUMnS+KBatH
JzC/fjkrKbGQsJ7OPMzQh2lKLI5fAT7FzxesTU3Loj7dVcdcl637xX1XiO/L133N+hF0pTpDK7Co
5jcabdtXGPxQnuEl7ldpY9U9y5+B7zr9xfELc+l7sg==
"""
        self.par.update({
            'PROJET': "ZZZZ108B",
            'FREQ_MIN': 0.25,
            'FREQ_MAX': 50.0,
            'FREQ_PAS': 0.25,
            'OFFSET_MAX': 60,
            'OFFSET_NB': 600,
            'SURF': 'OUI',
            'ALGO': 'DEPL',
            'Z0': 0.0,
            '_hasPC': True,
            '_nbPC': 3,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test06_zzzz108b.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test07_fdlv112e(self):
        """use ISSF + control points"""
        refe = """
eJzdV1mP4kYQfu9fUc+zGsKRifKyDx677bHia7vbaJWXyGMasGRs1kc0+fepbiAYMOCdzEpREIfp
46vjq66uegBLvrYNLCTkIJsWf/2srskDPPa9yAPOBOUGVrKQVfatlbizhmWWrjNZ1eC7nF/b24Vx
aEAZ2JY3n0ymVKPu9WhhI4sWLEMYvUBkN4NzImsq2VGcPFwuFq5glEQstGJXQGQwMA3P/GOnJmJ4
Mm1ahYJikyzPk1UfjELy56EHo58OKo82f5b5Xu9lVmRNVhbaFauqbLfyqgPRdBbGEQGAJ0DI2KfE
dgP1/5fuf/w4ffCbcnEVnJgvhh8Rz2W0q2m63mqkKKmSjUSf1bCArGjkqkoU7g2+iBsI6jBDuGEA
jJoClfwVBHONwPEoTKb4vtQS8gS2ypXqeVlJjJIiveGSM5k2o19iGpgULIpugZ9HY/2in8ZjMNTI
U3ckMrgam3TG+hxXl239uCiR5eKuKhf725PtUDdVq+NmqEncCn3DDbQ5E9ARsH8GmCrqyW+mrzjn
d0WrqLsj9ETc9CjuUYmbKY8qHxIumNFky0z+GMGzjmC08XFGLNuLXeSUhWeEzcCkHuYDPfp0HH2m
QvM97vLNY2bjyFFpOyvupQ212OpmGZ003mTa3jgAer25TqqVRGydmt4RAKRLPaFfBWUujdnFKZVv
jdKS3pI6gIITeVfOJmqfKOLT5Mj0AOSjAD714ffxxUmEC6Pq/hyZrpPNtsYclGYLtHFAZsBEZLqA
IRR5BtrG5xgoPSqQ8QhDA+f+WfgycGF0b52mJcnTNn+HCcg7NUHZcA8HkvYNtmWmHpGttCyaqsyH
Jpo+qWYYCBZito7dG7G1zNts8Z3hPNN4z2Wb5hicbQW5KgUG5XuEsZn8BiKMBeXAjTkFf25bOCBQ
URD4cf3IAjtk5v/zTHSCYFkWqRKrKXcqKW/dyTuBmloe2ewSq0LfY0UE2WYrF8mwm3eHFzvCgdga
gy4kFAFUA2gW5A+Jno/UvE9fJtHf7YHTbVW+5qj9vgCqkvROAXQpyfHCZ8P7+IjkfxXNWtaymxUO
Z0rrukyG11AHZS3XtpWREmJ1sAR+/VfC/5a9H5D/jrlPbT+aD0Ho0PBYN2jrX9+TwrqyAsvGPSeo
d8qL05ZnW9bNI1KQNTqcblXkPWVatiqSa2QR7jqB4cE5DePJSO37+DiOTk35t/cZiUIuCI+QzMD+
DLvC0vaNr5/Pb2rbNV/OLEzRhlFCTI6tm0ex58Ti0zB1ralbGq57jf0ltOue9j3HZNdoYLGs2osJ
aoGdEL8yaVmewuCHkhR/om5lOpTds/i504zrNvFvcCbg8g==
"""
        self.par.update({
            'PROJET': "FDLV112E",
            'FREQ_MIN': 4.0,
            'FREQ_MAX': 5.0,
            'FREQ_PAS': 1.0,
            'Z0': 5.,
            'SURF': 'NON',
            'ALGO': 'REGU',
            'OFFSET_MAX': 1000,
            'OFFSET_NB': 5000,
            'ISSF': 'OUI',
            'MATER_FLUIDE': {'RHO': 1000., 'CELE': 1500, 'AMOR_BETA': 0.,
                             'DEMI_ESPACE': 'OUI'},
            '_hasPC': True,
            '_nbPC': 3,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test07_fdlv112e.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test08_fdlv113a(self):
        """idem fdlv112e + matériau homogène"""
        refe = """
eJy9Vtty2jAQfddX7HM6UAyh6atjy8QzxnZlmelbxjECPOML8aXTX+p39Me6kiEx4WLSaapBINba
o7O72l3fgCmemhqWAlIQdYO/86SqyA0MTg1yg0/cIoO1yEWZPDcCNStYJfEmEWUFczsIzul2YWbU
pQws01lo2kRXqDseDWQib8DUuX4SiLRP8BlP6lJ0iJOb483c5owSn3lmaHPwdQaG7hiPLU3EcERc
NxIFj42SNI3Wp2Ak0nzhOTD8vKc8zH4U6Y73KsmTOily5Yp1WTRbcdaBaDrzQp9Ytjs7pZ4Vy7PK
xHjQ5z5xbEa7TOLNViH5URllAn1SwRKSvBbrMpK4F+JBbJfTGdO57bnAqMHhC3wFzmzdnTkUtDF+
jllCGsFWukquV6XAW5DHF0x+c6bF6LeQugYFkwIObThSg34ajUCXkvFQ20s0DFrQyl52aaccVxVN
NVgWGMW8l8qRfnOgjn/SXmNIYHpz3XZbE0BFVa4HGn5NcN6Suc4pMK9lP92zn8DCl6LpcHp3dze5
RdEYFsrGyfBWm36ZjpTonnLljFHHPfLaBP+c/bjDfoxzQiwntM099058JmBQBxNXSV8sOs0VgpBZ
KHnlbCV5X37LzWa3HKjs/ini5sJNVvuNTVSuBWKrGvIeX2BJ6UTyAtgqbRJZZt6Ddyp9MFpljMkT
5RWk18MqaNs1bCQZeCEz6JHLe/4TaIc2bK2M0rhJVfbEmyjbVlg1YqSS1z0JROh3aoCkonDuiyZG
O6qmRHOqKysCOsli4hm4F3IaQKAvKMwXlokCDqENHKc9902wPLT0A0L8avyqwHBU2EQgybYCw3JV
MWudEM74TPKkSk2RFcAs20DHT+X8kPv0X8kzgS5u9qVmWxZPKZqy6zFlFPf0mONjZ453rzuvJUE1
lKe/uURdZNe0UOcAtadyHL52bIuqHtRllNQqUJe6puq2h9tVLDACv39dGQLiewHHcmc8dHs5Nu/m
UQYSgMjbb6huuUsS7P9+KNdqxdsVFkz/LKHd5aiSKpPvale36PPcJCJpU7LLzDSdlo7M3ANau6J/
rXffxK/nhVQeQ/4A5Lekkg==
"""
        self.par.update({
            'PROJET': "FDLV113A",
            'FREQ_MIN': 1.0,
            'FREQ_MAX': 21.0,
            'FREQ_PAS': 20.0,
            'SURF': 'NON',
            'RFIC': 0.5,
            'ISSF': 'OUI',
            'MATER_SOL': {'E': 7.e8, 'RHO': 2500., 'NU': 0.2, 'AMOR_HYST': 0.},
            'MATER_FLUIDE': {'RHO': 1000., 'CELE': 150, 'AMOR_BETA': 0.,
                             'DEMI_ESPACE': 'NON'},
            'SOURCE_FLUIDE': {'POINT': (0., 0., 0.)},
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test08_fdlv113a.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def _diffcompress(self, refe, new):
        """Compare files without comment"""
        sref = remove_comments(test_utils.uncompress64(refe))
        snew = remove_comments(new)
        return test_utils.difftxt(sref, snew)

if __name__ == '__main__':
    # ( cd $HOME/dev/codeaster/src/bibpyt ; PYTHONPATH=.:$PYTHONPATH python Miss/miss_fichier_cmde.py )
    unittest.main()
