# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""Construction d'un fichier de commandes Miss"""

import os
import os.path as osp
import re
from pprint import pformat
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
        self.dinf =  {
            "titre"  : struct.titre,
            "fich_mvol" : self.fname('mvol'),
            "fich_chp" : self.fname('chp'),
            "fich_sol" : self.fname('sol'),
            "fich_impe" : self.fname('resu_impe'),
            "fich_forc" : self.fname('resu_forc'),
            "fich_ext" : self.fname('ext'),
            "binaire"  : "",
            "surf"  : "",
            "rfic"  : "",
        }
        if self.param["TYPE"] == "BINAIRE":
            self.dinf["binaire"] = "BINA"
        if self.param["SURF"] == "OUI":
            self.dinf["surf"] = "SURF"
        if self.param['RFIC'] != 0.:
            self.dinf["rfic"] = str(self.param['RFIC'])
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
                'DOFReq TOUTES SAVE MVFD TOT UI TUI IMPD FORCE',])
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
                    'FICH %s  %s' \
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
                ('SPEC NF=%%%(I)s FMAX=%%%(R)s' % dict_format) \
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
        #for interf in ('sol-struct', 'fluide-struct', 'sol-fluide',
                       #'sol libre'):
            #if group.get(interf):
                #lines.extend(['%4d SURF' % group[interf], 'FIN'])
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
            lines.append(("FREQUENCE DE %%(freq_min)%(R)s A %%(freq_max)%(R)s "\
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
                'SDOMAINE %4d GROUPE ' % self.domain['struct'][0] + \
                ''.join(['%4d' % i for i in self.domain['struct'][1]]),
                'KCM',
                'FINS'])
        lines.extend([
            '*', '* Definition du sous-domaine sol',
                 '* ------------------------------',
            'SDOMAINE %4d GROUPE ' % self.domain['sol'][0] + \
            ''.join(['%4d' % i for i in self.domain['sol'][1]]),
            'STRAtifie',
            'FINS'])
        if self.domain.get('fluide'):
            lines.extend([
                '*', '* Definition du sous-domaine sol',
                     '* ------------------------------',
                'SDOMAINE %4d GROUPE ' % self.domain['fluide'][0] + \
                ''.join(['%4d' % i for i in self.domain['fluide'][1]]),
                'DFLUI RO 1000 CELER 1500 SURF 0.',     # en dur ?!
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
        # sol
        z0 = self.param['Z0']
        lines.extend(self._chargement_domaine('sol'))
        lines.extend(self._stratification_sol())
        lines.extend(['*',
            '* Definition des champs incidents',
            '* -------------------------------',
            'INCI 3',
            ('DPLANE SV 1. Z0 %%%(R)s' % dict_format ) % z0,
            '0. 0. 1.',
            ('DPLANE SH 1. Z0 %%%(R)s' % dict_format ) % z0,
            '0. 0. 1.',
            ('DPLANE P 1. Z0 %%%(R)s' % dict_format ) % z0,
            '0. 0. 1.',
            '*',
            '* Calcul des champs incidents',
            '* ---------------------------',
            'EXEC INCI',
            ])
        if self.param['_hasPC']:
            lines.extend(['*',
                '* Calcul des champs incidents aux points de controle',
                '* --------------------------------------------------',
                'EXEC CONTROLE UI',])
        return lines

    def calcul_fonction_green(self):
        """Calcul des fonctions de Green"""
        lines = []
        lines.extend(self._chargement_domaine('sol'))
        lines.extend(self._stratification_sol())
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
        # cas du calcul sous la boucle LAPLACE_TEMPS
        if self.param['FREQ_IMAG'] is not None:
            args = "IMPEdance"
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

    def _stratification_sol(self):
        """Définition de la stratification du sol"""
        z0 = self.param['Z0']
        lines = ['*',
            '* Definition de la stratification du sol',
            '* --------------------------------------',
            ('DOS2M Z0 %%%(R)s  %%s' % dict_format) % (z0, self.dinf['surf']),
            'LIRE %s' % self.dinf['fich_sol'],]
        return lines

    def _chargement_domaine(self, dom):
        """Chargement d'un domaine"""
        lines = ['*',
            '* Chargement du domaine %s' % dom,
            '* -------------------------------',
            'DOMAINE %4d' % self.domain[dom][0]]
        return lines

    def _rfic(self):
        """Retourne les paramètres si RFIC (résonnances fictives) est utilisé"""
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

    def calcul_champs(self):
        """Calcul des champs rayonnés aux interfaces, et/ou assemblage des
        impédances et forces sismiques induites"""
        lines = super(MissCmdeGeneratorISSF, self).calcul_champs()
        lines.extend(self._chargement_domaine('fluide'))
        lines.extend(['*',
            '* Calcul des forces et impedances',
            '* -------------------------------',
            'EXEC UGTG IMPEdance FORCe',
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
            'PROJET' : "MODELE",
            'FREQ_MIN' : None,
            'FREQ_MAX' : None,
            'FREQ_PAS' : None,
            'LIST_FREQ' : None,
            'FREQ_IMAG' : None,
            'Z0' : 0.,
            'SURF' : 'NON',
            'ISSF' : 'NON',
            'ALLU' : 0.,
            'RFIC' : 0.,
            'ALGO' : None,
            'DREF' : None,
            'SPEC_MAX' : None,
            'SPEC_NB' : None,
            'OFFSET_MAX' : None,
            'OFFSET_NB' : None,
            'TYPE' : 'ASCII',
            '_hasPC' : False,
            '_nbPC' : 0,
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
            'PROJET' : "SDLX103A",
            'LIST_FREQ' : (12.25, 12.50, 12.75),
            'TYPE' : 'BINAIRE',
            'Z0' : 5.0,
            'DREF' : 1.0,
            'ALGO' : 'REGU',
            'OFFSET_MAX' : 20,
            'OFFSET_NB' : 200,
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
            'FREQ_MIN' : 1.,
            'FREQ_MAX' : 10.,
            'FREQ_PAS' : 1.,
            'Z0' : -6.05,
            'SURF' : 'OUI',
            'ALGO' : 'DEPL',
            'OFFSET_MAX' : 60,
            'OFFSET_NB' : 400,
            'SPEC_MAX' : 0.03,
            'SPEC_NB' : 2048,
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
eJzdVslu2zAQvfMr5uzCru20RXtUJMoRoK0UbRS9BIpM2wS0OJJY9Jf6Hf2xDmknluM1QdFDhQhm
KM7Mmzcbe+CIB9XCXEAOolX4G8imIT3oH3tID7+EVQFLUYpaPiqBkg0sZLaSom4g8JLklGxXzYSG
lIHr+LPRaHxrtG5xKChEqcCxuHVUEdl8wW9ctrXoACe9w8Pc44ySmEXO1OMQWwxsy7fvNzBRhy+y
VmktaDaVeZ4uj6nRmoJZ5MPg/RPkQfGjyre4F7KUraxKQ8WyrtRanCQQXWfRNCauF06OiRfV/KQw
se+sICa+x2gXSbZaG01xWqeFQE4amIMsW7GsU633TDyIF3I6YRb3ohAYtTl8gs/AmWeFE5/CaIx/
hyghT2GtqdLrRS0wC8rsjMsvbLqMfp3S0KbgUMBnNBiah/aHI7D0zs3TzrvhEIOW6L3x4Mvz3jHi
mko1/XmFUSwvQjmQV3vi+E9+0RmSOFFgeeHGBTBR1ev+SDuA7weScGa1ciGFjnby142OO0bH+N4Q
x/WnHrAIRkgU2NTHGht9xGUyZS4MBzscriwvlZo+7HQr0xTaT5GpM0llzturtF4K1G3K+TX+YXV3
SD2eeU2r0xo7Trqj8QrNOwPJOIDvQ23hYzfP4KCwmuMVnq3SYt1ghWVyjj5ekfdYZrYHGJ/Yt9C3
ZIY5fwQCGQ4wSPjt+eDdlQfjS+dMWNI8U/kbXCD0G7VB+2D03FYqyzGeqoZc9/6rGgDy7jLxCDya
cppAYs0oBDPXwQ0OmLQcXy+IHXAjZtP/Mo06/C+qMtNmGw1mUgtxrkl3YpDELjvUVSP3OAJBFmsx
T69rxRt90wmfaNqpETPcizPcL3Il9Zx9Df3jf4KXCWRZPUVyXVcPOaLfzsE6zS7MwUNLEz+6tfxd
rzQ58/CWzO9qDh0XZfa0Xmip+1ejddW0fUxc2ZrYnJvs5kawf9zQj6T//nUl6ySOEo5zwL7r5jNe
MNS9jh3mui5Z20z0bWXjHSWe6rVZ8c0KJ0l8EtA2HxrZFPo+efU14jQ2rZFs+kgXmeP4Gzi63ezB
2k7Da9l9Eb8Ll2ZthvwB+KvUFQ==
"""
        self.par.update({
            'PROJET' : "FDLV112B",
            'FREQ_MIN' : 0.1,
            'FREQ_MAX' : 3.0,
            'FREQ_PAS' : 2.9,
            'Z0' : 5.,
            'SURF' : 'NON',
            'ALGO' : 'REGU',
            'OFFSET_MAX' : 1000,
            'OFFSET_NB' : 5000,
            'ISSF' : 'OUI',
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
            'PROJET' : "Miss_Laplace",
            'ALGO' : 'DEPL',
            'DREF' : 5,
            'FREQ_IMAG' : 1.0,
            'INST_FIN' : 2.0,
            'OFFSET_MAX' : 40,
            'OFFSET_NB' : 400,
            'PAS_INST' : 0.05,
            'RFIC' : 0.0,
            'SPEC_MAX' : 0.075,
            'SPEC_NB' : 16384,
            'SURF' : 'OUI',
            'Z0' : -5.8,
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
eJzdVtuOmzAQffdXzHOq0JBst9tHRBwWlVvBrKq+rChxEkvcyqXqL/U7+mMdO6uGJGygfawFioXH
Z2bO3DKDNf/atbDlkAFvO/x1RdOQGcyHFpnhiVfmsOcFr8W3juPNBnYiPQheN+DaUfTa3T6MRT0a
Kk3PTlJlScoV8ostHeS86GBtMGMQjBxP8IyJtuY948nsWpjZLKQkCP11bDMIjBBMwzGfj6YihsPT
tpMoqDYRWZbsh2AkkvvkO6C97Zut5d/L7MX2nShEK8pCUbKvy67irxKJFIR+HBAAWALCxi4lG9uT
rzUEl5fbV8GI+Wi4AXHskF5alx4qhRYkdZJz5KqBLYii5fs6kdg3YkVsj1ErNJjtexBSk8E9PAAL
bcOzHAr6Ep9rSyFLoJIUyv2u5pghRXqDhgudm5B+iqlnUlBLl/zo2mK5Wj2s6JvFgtiu5cuTe+3u
w/u7+3f4UR/iqym7Zr4tMaDFqPar+93ZdWjaulMpMtWLaO27hu0pJ3RQgX7ZY7TJR9OVYY5G1crE
GlF4pmp5UjXXScRCoxU7wU/KNqIYKy4pvO7XoiqtHzztbqSLkjcPSb3niK0K+G+cwHruOTGcUxiD
RHqTJieuJiCfFERLF74sJDXvtIeFXDKfAKI43AyWTjNc1+khyasGaygVW/R1QmZjIZk2rMg6cAz0
MXrCjB4whSw0wEfX/gg+ThQMxuRUeJIs7bJ/cIHQz9QE6cN/HeYeP7uySKX6Rhpl1ZzfapM9jqJg
E15j1dj+cDiByCu+TaY1wyNebDELbDeg6tqpgsdL8nyYVmXTzpFY0aq43er5alaciys30PhfPyda
TwI/YthHzMdLrnH8dM+SBwBs5MHaVP0emB8zGuEUC2K5Vzt23GE3Cvqta6orF2SN/K9Rk/c3wPRD
RA==
"""
        self.par.update({
            'PROJET' : "Miss_Laplace",
            'DREF' : 5,
            'FICHIER_SOL_INCI' : './Miss_Laplace.sol.inci',
            'FREQ_IMAG' : 64.97465,
            'INST_FIN' : 2.0,
            'LIST_FREQ' : (1.023383,),
            'NB_MODE' : 6,
            'OFFSET_MAX' : 40,
            'OFFSET_NB' : 400,
            'PAS_INST' : 0.05,
            'PRECISION' : 1e-10,
            'RFIC' : 0.0,
            'SPEC_MAX' : 0.075,
            'SPEC_NB' : 16384,
            'SURF' : 'OUI',
            'Z0' : -5.8,
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
            'PROJET' : "ZZZZ108B",
            'FREQ_MIN' : 0.25,
            'FREQ_MAX' : 50.0,
            'FREQ_PAS' : 0.25,
            'OFFSET_MAX' : 60,
            'OFFSET_NB' : 600,
            'SURF' : 'OUI',
            'ALGO' : 'DEPL',
            'Z0' : 0.0,
            '_hasPC' : True,
            '_nbPC' : 3,
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
eJzdV1uP6jYQfvevmOetlhK2W/XlPOQkTjZqbrUTdNSXo2wwYCkknFyq7b/v2EAJECBnu5WqIi4h
sb/5Zr7xePwAtnjtWlgIKEC0Hf4GsmnIAzwOvcgDPgmrDaxEKWr5rRM4s4GlzNdS1A0EHufX5vZh
XBpSBo7tzw1jRjXqnkcHG1F2YJuJOQhEdk/wWSLbWvSIk4fLwYmXMEpiFtmpl0BsMrBM3/q6o4kY
vsjbTqGg2UwWRbYaglFIwTzyYfLjgfJk80dV7HkvZSlbWZU6FKu66rbiagDRdRalMQGAZ0DINKDE
8UL1/+f+f/y4Q/CbanEVnFgvZhAT32O0zzRfbzVSnNXZRmDMGliALFuxqjOFe0Mv4oUJdZmZeFEI
jFoJkvwFEuaZoetTMGb4vmQJRQZbFUp1vawFZkmZ3wjJmU2H0d9SGloUbIphgZ8mU/2iP0ynYKo7
z/07scnVPaN3byhwTdU1j4sKVS7vUrmY351Mh6atO503Y13idhSYXqjdMUBnwP4aYKakJ79agdKc
3zWtsu6O0RNzs6O5R2XuSUVUxZDwhJmtXErx7xh+6hlGHx+fiO34qQcsAgN1Aov6WAKMZ7zkKXNg
OjnycGR5rxKowXa/cOg68Cby7kZO6/HWOqtXArF1tXmHpqSvJqFfEso8mrKLhSfeWsWS3rI6Iqon
9q4sN2SfKS3z7CjeCOSjAT4L4PfpxeKCC6ea4bKXr7PNtsGykssF+jhisWNtsTzArIh9E33jc1zC
AxTIdIKpgc/+HvgycmB8b5yWJSvyrniHC6g7tUD5cA8Hsu4NtpVUl6hWXpVtXRVja8eQVSsKExZh
AU53xj9XXV5gMnU1FGo3HlVyUXSHiW+QRGlCOXBzTiGYOzbeSBAYEvx4QWyDEzHr/5nDPdGWVZkr
s1oitxbi1rbYk4LHDrvEqjH22JSA3GzFIhu3+e3wUjdxIbWxPKq9XAlANYBWQdxQYVl0cvGdxevp
w5kP8WUC490dNN3W1WuB7Pc9SJ3ld3qQS0uuH302/Y/PSP5n2a5FI/qr+LCmNNdlNr6NOZC1PcdR
TgpI1cJK8Ou/kv63/P2AenWsVWr60X0II5dGx31ee//6nhLWtxXaDs45Qb3TDpyeOrZV0z6iBLLV
6XSrKR7olOSqzK6JRbjnhqYP5zJMjYma9/F5HJ+68k/3HxJHPCE8RjFD5xPsejsnML98Ot9ZHc96
OfMwRx8mGbE4np58isc+7OlNS/d++lTBdbu/34R2B5h922/sen3sV1WHbyALPIzwKw9t21cY/NBC
4k/c7yTHqnuWP3fOw/qk9hdKKMTn
"""
        self.par.update({
            'PROJET' : "FDLV112E",
            'FREQ_MIN' : 4.0,
            'FREQ_MAX' : 5.0,
            'FREQ_PAS' : 1.0,
            'Z0' : 5.,
            'SURF' : 'NON',
            'ALGO' : 'REGU',
            'OFFSET_MAX' : 1000,
            'OFFSET_NB' : 5000,
            'ISSF' : 'OUI',
            '_hasPC' : True,
            '_nbPC' : 3,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test07_fdlv112e.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def _diffcompress(self, refe, new):
        """Compare files without comment"""
        sref = remove_comments(test_utils.uncompress64(refe))
        snew = remove_comments(new)
        return test_utils.difftxt(sref, snew)

if __name__ == '__main__':
    # cd $HOME/dev/codeaster/src/bibpyt
    # PYTHONPATH=.:$PYTHONPATH python Miss/miss_fichier_cmde.py
    unittest.main()
