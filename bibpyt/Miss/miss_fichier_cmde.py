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
from pprint import pprint, pformat
import tempfile
import unittest

from Miss.template_miss import (
    main_template,
    def_freq_min_max, def_freq_range, def_freq_imag,
    def_sous_domaine, use_domaine,
    def_chargement_pc, def_chargement_green, def_chargement_decl,
    def_ondes_non_inclinees,
    def_calcul_sol, def_calcul_impedances, def_calcul_global,
    def_exec, def_exec_pc, def_exec_manuel,
    init_post, post_impe, post_forc, fin_post,
    init_spec, fin_spec, lire_signal, post_spec,
    init_boucle, fin_boucle,
    def_fin,
)
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
            "projet" : param["PROJET"],
            "titre"  : struct.titre,
            "fich_mvol" : self.fname('mvol'),
            "fich_chp" : self.fname('chp'),
            "fich_sol" : self.fname('sol'),
            "fich_impe" : self.fname('resu_impe'),
            "fich_forc" : self.fname('resu_forc'),
            "fich_ext" : self.fname('ext'),
            "binaire"  : "",
            "z0" : param["Z0"],
            "surf"  : "",
            "issf"  : "",
            "rfic1"  : "",
            "rfic2"  : "",
            "champPC" : "",
            "nbPC" : "",
        }
        if self.param["TYPE"] == "BINAIRE":
            self.dinf["binaire"] = "BINA"
        if self.param["SURF"] == "OUI":
            self.dinf["surf"] = "SURF"
        if self.param["ISSF"] == "OUI":
            self.dinf["issf"] = "ISSF"
        if self.param['RFIC'] != 0.:
            self.dinf["rfic1"] = "RFIC"
            self.dinf["rfic2"] = str(self.param['RFIC'])
        if self.param['_hasPC']:
            self.dinf['champPC'] = "UD0 CHAMP"
            self.dinf['nbPC'] = param['_nbPC']

    def build(self):
        """Construit et retourne le fichier"""
        self.bloc_group()
        self.bloc_lfreq()
        self.bloc_sous_domaine()
        self.bloc_calcul()
        self.bloc_post()
        self.bloc_fin()
        glob_template = main_template() % self.dinf
        try:
            content = glob_template % self.dinf
        except (KeyError, TypeError):
            print '\nMissCmde template:\n', glob_template
            pprint(self.dinf)
            raise
        text = remove_empty_lines(content)
        if self._dbg:
            dtmp = tempfile.mkdtemp(prefix=self.param["PROJET"] + '_')
            open(osp.join(dtmp, 'command'), 'wb').write(text)
            open(osp.join(dtmp, 'para'), 'wb').write(pformat(self.param))
        return text

    def bloc_group(self):
        """Déclaration des groupes"""
        cmd = "    2 VOLUME\nFIN"
        if self.param['_hasPC']:
            cmd += "\n    3 VOLUME\nFIN"
        self.dinf['bloc_group'] = cmd

    def bloc_lfreq(self):
        """Définition des fréquences de calcul"""
        self.dinf.update({
            "freq_min" : self.param["FREQ_MIN"],
            "freq_max" : self.param["FREQ_MAX"],
            "freq_pas" : self.param["FREQ_PAS"],
            "freq_imag" : self.param['FREQ_IMAG'],
            "_nbfreq" : 0,  # == nbfreq + 1 ?
            "bloc_lfreq" : "",
        })
        # formats possibles pour les fréquences
        assert (self.param["FREQ_MIN"],
                self.param["LIST_FREQ"]).count(None) == 1, \
                'expect FREQ_MIN xor LIST_FREQ, not together'
        if self.param["FREQ_MIN"] is not None:
            self.dinf['bloc_lfreq'] += def_freq_min_max() % self.dinf
            self.dinf['_nbfreq'] = int((self.dinf['freq_max'] - \
                             self.dinf['freq_min']) / self.dinf['freq_pas']) + 2
        if self.param["LIST_FREQ"] is not None:
            lfreq = list(self.param['LIST_FREQ'])
            self.dinf['bloc_lfreq'] += def_freq_range(lfreq)
            self.dinf['_nbfreq'] = len(lfreq) + 1
        if self.param['FREQ_IMAG'] is not None:
            self.dinf['bloc_lfreq'] += def_freq_imag() % self.dinf
        self.param.set('_nbfreq', self.dinf['_nbfreq'])

    def bloc_sous_domaine(self):
        """Définition des sous-domaines"""
        if self.param['_hasPC']:
            cmd = def_sous_domaine(1, (1, 3), "KCM")
            cmd += def_sous_domaine(2, (-1, 2), "STRAtifie")
        else:
            cmd = def_sous_domaine(1, (1, 2), "KCM")
            cmd += def_sous_domaine(2, (-1, ), "STRAtifie")
        self.dinf['bloc_sous_domaine'] = cmd

    def bloc_calcul(self):
        """Chargements & calculs"""
        cmd = ""
        if self.param['_hasPC']:
            cmd += def_chargement_pc(1)
        cmd += def_chargement_decl(2)
        cmd += def_chargement_green()
        cmd += def_ondes_non_inclinees()
        if self.param['_hasPC']:
            cmd += def_exec_pc()
            cmd += init_boucle()
        cmd += def_chargement_decl(2)
        cmd += def_chargement_green()
        cmd += def_calcul_sol()
        cmd += def_calcul_impedances()
        cmd += self.cmd_calc()
        if self.param['_hasPC']:
            cmd += def_calcul_global()
            cmd += def_chargement_decl(2)
            cmd += def_exec_manuel("DIFFRACTE UTOT TTOT")
            cmd += def_chargement_green()
            cmd += def_exec_manuel("CONTROLE UTOT TTOT NOGEO")
            cmd += fin_boucle()
        self.dinf['bloc_calcul'] = cmd

    def bloc_post(self):
        """Post-traitements"""
        cmd = self.cmd_post_impe()
        cmd += self.cmd_post_forc()
        cmd += self.cmd_post_pc()
        self.dinf['bloc_post'] = cmd

    def bloc_fin(self):
        """Fin exécution"""
        self.dinf['bloc_fin'] = def_fin()

    # calculs ou post-traitements ajoutés selon les cas
    def cmd_calc(self):
        """Calcul des impédances et/ou forces sismiques"""
        what = ""
        if self.param['_calc_impe'] or self.param['_hasPC']:
            what += " IMPEDANCE"
        if self.param['_calc_forc'] or self.param['_hasPC']:
            what += " FORCE"
        what = what.strip()
        if not what:
            return ""
        return def_exec(what)

    def cmd_post_impe(self, num=None):
        """Post des impédances"""
        if not self.param['_calc_impe']:
            return ""
        cmd = init_post()
        cmd += post_impe(num)
        cmd += fin_post()
        return cmd

    def cmd_post_forc(self, num=None):
        """Calcul des forces sismiques"""
        if not self.param['_calc_forc']:
            return ""
        cmd = init_post()
        cmd += post_forc(num)
        cmd += fin_post()
        return cmd

    def cmd_post_pc(self):
        """Post-traitement pour les points de contrôle"""
        if not self.param['_hasPC']:
            return ""
        cmd = ""
        cmd += lire_signal(self.fname("01.sign"))
        cmd += use_domaine(2)
        cmd += init_post()
        cmd += init_spec()
        cmd += post_spec("ACCELERATIONS", self.fname("01.csol.a"))
        cmd += fin_spec()
        cmd += fin_post()
        return cmd

class MissCmdeGeneratorInci(MissCmdeGenerator):
    """Construit un fichier de commandes Miss
    Calcul du champ incident pour la methode Laplace-temps"""

    def bloc_lfreq(self):
        """Définition des fréquences de calcul"""
        N = int(self.param['INST_FIN'] / self.param['PAS_INST'])
        Fs = 1. / self.param['PAS_INST']
        self.param['FREQ_MAX'] = Fs
        self.param['FREQ_MIN'] = Fs / N
        self.param['FREQ_PAS'] = Fs / N
        # car FREQ_IMAG ne sert qu'à déclencher le traitement
        self.param.set('FREQ_IMAG', None)
        MissCmdeGenerator.bloc_lfreq(self)

class MissCmdeGeneratorISSF(MissCmdeGenerator):
    """Construit un fichier de commandes Miss dans le cas ISSF"""

    def __init__(self, param, struct, filename_callback):
        """Initialisation"""
        super(MissCmdeGeneratorISSF, self).__init__(param, struct, filename_callback)
        assert not param['_hasPC'], 'ISSF and PC not compatible'

    def bloc_group(self):
        """Vide"""
        self.dinf['bloc_group'] = ""

    def bloc_sous_domaine(self):
        """Définition des sous-domaines"""
        #XXX d'où sortent ces valeurs ?!
        prty = "DFLUI RO 1000 CELER 1500 SURF 0."
        cmd = def_sous_domaine(1, (-1, 3, 4), "STRAtifie")
        cmd += def_sous_domaine(2, (-2, -3), prty)
        self.dinf['bloc_sous_domaine'] = cmd

    def bloc_calcul(self):
        """Calculs"""
        cmd = ""
        cmd += def_chargement_decl(1)
        cmd += def_chargement_green()
        cmd += def_ondes_non_inclinees()
        cmd += def_chargement_decl(2)
        cmd += init_boucle()
        cmd += def_chargement_decl(1)
        cmd += def_chargement_green()
        cmd += def_calcul_sol()
        cmd += def_calcul_impedances()
        cmd += def_exec("IMPEDANCE FORCE")
        cmd += def_chargement_decl(2)
        cmd += def_exec("IMPEDANCE FORCE", mini=True)
        cmd += def_calcul_global()
        cmd += fin_boucle()
        self.dinf['bloc_calcul'] = cmd

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
            '_calc_impe' : False,
            '_calc_forc' : False,
            '_hasPC' : False,
            '_nbPC' : 0,
        })
        self._write = True

    def fname(self, ext):
        """use PROJET"""
        return './' + self.par['PROJET'] + '.' + ext

    def test01_miss04a(self):
        """use LIST_FREQ"""
        refe = """
eJzdVcuO0zAU3fsr7rqoJckwwDYkbsYir0mcEWIzCqnbWsqj5IH4fK6dFjKd0CnSrLDSypavzzm+
Ly/IAsKmgp2oRSu/DwI2ooOtLPZStB0ELE3RYvnCIB4NaaKtjXc2WeARV3wbetgMUIl6ANfm9iwO
GXdwj8u+VexQguiHjSCL58ac8YSSOIncjHGI7QQc23ceR5WI4YuiHxQK0uayLPPdHIxCCh4iH1Zv
j4pX1Y+mPMreylr2sqkVyK5thgMqkgJyKHPo+nbQDPOo865JoiwmgMMCJM0CStYsVD/vGSO6vmrw
/y/wxLmzg5j4LKET7cX+oIHivM0rgU7sYAOy7sWuzRXsBa14joWceonNWRRCQh0O7+Ej8ITZoedT
MC38RnTlTBWebSswTepCHJPjEvg6ofcZDR0KetwoN5gry7o1DIO+McxxqVaT5YfT7px7umbolpsG
o1srN13MzJmATo8rReblQCJC6kaBzUJ9AxN0MI9zDCj57AQqkjr7XiSz/onM+kO2NEnKE7uXWymm
dGtZn1XYbBBY6JLFK49pjeuS/SmK4UK2HRU7+7zdCRSsW8NT31yKJZk6ZgZL9aymLpSATgnyWiHq
KxqXRk6tAL4aCvp2ZRjHZMT1eaF1Y494npTFPq8OHdZcITcop4MrmAErz2FwA8SNfRtvlj5g8s/p
IMYK8DNXvy3vrrWMXzJUSpUr87IYytm7XNP96RfqgLrO/xXlk1dyZCsF6I25J2zx1INzEkcXpfE6
OcNGe1kdBHIUqp+dLDOPewAsiKlrq/a5jhLdRMfTqh03Xb/s21z2o29mlJE4SjlWv3M3uR2+DsOj
YgT4xEKbIIXr6D4NPMo4TfGNiTM11zM+zrCFxK9BvG3agui7PKF0XX/kydg536nLnfWYuZDrZ/UX
QIMYtQ==
"""
        self.par.update({
            'PROJET' : "MISS04A",
            'LIST_FREQ' : (12.25, 12.50, 12.75),
            'TYPE' : 'BINAIRE',
            'Z0' : 5.0,
            'DREF' : 1.0,
            'ALGO' : 'REGU',
            'OFFSET_MAX' : 20,
            'OFFSET_NB' : 200,
            '_calc_impe' : True,
            '_calc_forc' : True,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test01_miss04a.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test02_zzzz200b(self):
        """use FREQ_MIN/FREQ_MAX/FREQ_PAS"""
        refe = """
eJzdVctu2zAQvPMr9uzCru2iQa+CRCtE9SpFBUEvgSrTNgE9XD2Kfn6XlJM4Cuu4QE4lZIGwljPD
2eVyRmYQNRXsZS1b9XOQsJUd7FRxULLtIGRpihHzNwbxaUQ5hLFHA0pmuMKTP4YetgNUsh7Ac4Rj
hSHjF/wmVN9qcihB9sNWktnrYMEEpyThsZcxAYnDwXUC92EUiRiBLPpBoyBtrsoy39tgNFJ4Fwew
+DgKXlS/mvKkeqdq1aum1hj7thmOKEhJyKHMoevbwRDYQe3G8DhLCOBYA3JmISUbFumf/4oRja8a
fP8Fnri3TpiQgHH6LL04HA1Okrd5JdHCDrag6l7u21yjXpCK61gkqM8dweIIOHUF3MAXEJw5kR9Q
WK3xGdG1lTo5u1ZijdSFPFXGJfANp98yGrkUPKoNWC2WZtAPyyU4k39WmM50GmVzqGuGbr5tML+1
dupiaVpyer7c0F3OJSKkXhw6LBq3ACafpznmlHx1Q51MU39vkq3/iWz9TDZfkVRwp1c7Jc/pNqqe
nDFrIljkkdk7j/NTbg7tb1kMFyrupNg95O1eomDTHF56cymX5NwYC5ZuWk1daAGdFuS3UtZXdC6D
nK5D+L7UPt8slp+fahTSjG8m560bO8XruiwOeXXs8OgVaouKOriCHPAAugw+AfGSwMHNpXdY/zYp
ZLkAfFaLp8jbayOTtwK1Uu1mXhZDad3LNTcAvacu6O38d4l+NCZHwlKC+WC7ymYvTbSpHF1Kkw2f
YGO8qo4SOQrd1R4jM1/4ACxMqOfoRrqJuaudGVfrxtx0/bxvc9WP9liUkSROBfYA9/Z5c3hNDA+a
EKEQ3XNNswYRZ4KmeM8kmZ6bmRhn2EOSd+DcNW1BzC5eMHpeMNJkbEr32OUmPcaWb3Oz/gFd9Rqe
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
            '_calc_impe' : True,
            '_calc_forc' : True,
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
eJzdVk1vm0AQve+vmLMru4Y0VXsksBAkDHRZrKqXiOC1g4TB5aPqz+8M4IjExHarnIqMtVqW92bf
m5llxmbgl3vYqUJV2c9WwUbVsM3Sp0xVNazcKMIV8wsXc7jPBdiWt9Y0/Y7N8B1LPbYNbFrYq6IF
y5DGJBDrn+AzmTUV0UMOqmk3is1OF0tXCs5CEVixKyE0BJiGZz70YSKGp9KmJRSkTbI8T3ZTMIS0
WgceLD4eQ17sf5X5EPc2K7ImKwtC2VVle8CQMgUJ5AnUTdV2FNOw0+KIIA6Z7frOCQFqvS/x/w00
Zt4bq5B5ruDjWNOnQ4cUJlWyV6haDRvIikbtqoRwz8SG77m+5I4wpBv4ILgp4TN8ASlcw3c8DpqO
vx6d1CM/tpXCxChSNaTDOXBb8G8x900OFge8tMWyu/h8qYFBMzfHmQ/LJToY0Zy++Po8N6VRXbb1
fFOipQVpdTYfJ0wcv94Fdd48RIisYGW4fr8F6Ayk8VyjDeD9iUVSGE22zRQZ2yXfRVr9r2j1Ea2O
9w2zbC92QQSgoVRgcg9LTrvFYRQLG5aLcSR2VryqvUm3XN9is3e+xtXfFfNvlbZn0nKI2HxKqp3C
gLum8dKtc4azsVUTWNTOyiKlAGoKyKmUKq7oaR1ypK/gx5Kgb8dpCyclWffd4zR106dkf6ixOtNs
g/HUcAU1YI2aLiYas0LPwK1FayykqUDYcoHG48PnlffXrgwvLaRIScskT9t8ci/XnAz8OzeBtnPJ
Zv16m/UB665s01xB3VaQk83HPnVBZISyBcggljyCyFhzWK1tCyckYH1JvN1VaIEdCJP/Z8l5NDNB
OlKOHkwdyrOXxk/F2DsbhbZ4hY3rs/1BIUdKzfq4MnakA6Qstww6Hzp5MfZ3z4u32QYqoXDf7bFL
H6ryMUfi4Qitkm6n15Rpz9/ROV5wZ3jj3qvoa+Hx3xJ0jO5b9gAblnUzb6oka3qZJiBYGEQSu7p5
P7Yfvw/aB7IExabMNrtTeigA/MQIYxp3I9mP8FwI34V1W1Yp67Ufc1qW1xNRvb0kHOk3Pjmm1KK3
2B/ikZBZ
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
            '_calc_impe' : True,
            '_calc_forc' : True,
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
eJzdVduO2jAQffdXzDMVKVBtta9RYrJRc6vjrKq+rNxgwFIuNJeqn9+xA1uWdVkq9alWgJCMz5w5
c/GMzCBpa9jJRnbq+yhhI3vYqnKvZNdDHOY5WszfWCSgCWUQq75/isShEqUkM9zny2/jAJsRatmM
4LvctYKR6Q2+42roNAWoQA7jBlFeG/OQM0oylvpFyCFzGXhu5D1NVBEjkuUwahR0K1RViZ0NRiPF
j2kEzvtz2k79o62O3LeqUYNqG42069rxgLSUBAGVgH7oRuPGDm0XiaVFRgDXCtBzEVOyDhP9CV55
xCTULX7/AZ54D26ckShk9DKAcn8waJnoRC1Rzh42oJpB7jqhsa8Qxn1hwmnAXB6mCTDqcfgI98BZ
6CZBRGG5wmtC17LqRG07iVXTlPJYK9fA14x+LmjiUfCpluHOWZhF54sluEaY05N3+CRz80srm059
O/bzTYu5brReV4vVktnz7drd8npGESH309gNExPCEkxWj/cYAPnkxTqlphbfdLb6K2er387mS5Jz
5g5qq+S5u7VqLvrNmogw8cnsH6/zjjcN/FOW45WKOzL29qLbSSRsBsVLba7lkpwLY8HSY6xtSk2g
14SCTsrmhllmkPNVDF8XWuc75/5Ukfg/L9ja2nX9NDVeV2e5F/WhxwYs1QZ59XADBcA29EL4AMTP
IhdDzB9h6dgIkYUDeC2dZ8uHWy2ztww1U62pqMqxssZyy8lAv1APdDj/abpP8gh0W0kwL2xH3Oyl
lDauk1Z5tmYX2Giv6oNEH6WecCfLIuABwDplnlZl2qNHc9sP86ETapiksfAhWZpznALew2VgeFyM
T9u2K4nBNSMbeFpwmhPfj/RtTopw+sUxkp3PnYuut2lvTrxfbSYLhg==
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
            '_calc_forc' : True,
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
eJzdVcuO0zAU3fsr7rqooWmHYVhGiZuJyIvEGSE2I5O6raU8Sh6Iz+fa6UCnYzpFYoWVRFZ8fe7x
uQ/PyAzitoadaEQnv40CNqKHrSz3UnQ9REGeo8X8lUF8GtMMItn3jyE/VLwUZIb7PPF1HGAzQi2a
ETyHOUYwMq3gGpNDpyhABWIYN4jy0pgFLKMkzRKvCBikTgauE7qPE1XECEU5jAoF3XJZVXxnglFI
0UMSgvX2lLZVf2+rI/etbOQg20Yh7bp2PCAtKYBDxaEfulG7MUObRcqSIiWAYwnouYgoWQexev0X
HjEIdYvfP8AT996JUhIGGT0/QLk/aLSUd7wWKGcPG5DNIHYdV9gXCOO+IGbUzxwWJDFk1GVwC3fA
ssCJ/ZCCvcRnQleyqkBtO4FZ05TimCuXwNcZ/VTQ2KWgh620sK3FcrW6W9E3iwUJIj9RK7fWzYf3
N7fv8KdtkqZvx36+aTG8jZLoYn4agnm6fSJyMYiIkHtJ5ASxJm6DDuRxjsEkH91IRVGn36vOln/l
bPnb2dwmOcucQW6lOHW3ls1ZiRm1D2KPzP7xOC1yXbM/RDleSLIjY3fPu51Awro3PNfmUizJqTAG
LNW52qZUBHpFyO+EaK5oXxo5X0bwZaF0fmfdLdRQKQmQF9naWGj91CheZme55/Whx5or5QZ59XAF
BcDKcwNYAfHS0MEj5g9YGiZCZGEBPrb1y/L+Wsv0NUPFVGnKq3KsjGe55jKgn6kL6jj/abif5OHo
thKgF0y32uy5lCauk1Z5us7OsNFe1geBPkrV4Z4sC5/5AEGUUs+Z+ui0T3Xkth/mQ8flMMlj4ETS
JGfYCdz788PhLTE+KocIiOieq3s1sKRgNMfLJi3UXM/YNMN+kp42oLPyNwVB33Y/AesBCyk=
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
            '_calc_impe' : True,
        })
        gen = MissCmdeGen(self.par, self.struct, self.fname)
        txt = gen.build()
        if self._write:
            open('/tmp/test05_miss03c_loop.in', 'wb').write(txt)
        diff = self._diffcompress(refe, txt)
        assert diff.strip() == "", diff

    def test06_zzzz108b(self):
        """using TABLE_CONTROL"""
        refe = """
eJzdVkuTmzgQvvMr+uxde8GpbOWSgwYEQy2vCDE1lUuKwRqPqjB4eWzl529L4BmCie2kcthalZ+S
+Lr1fd2tXhkriOoD7EUlGvl3L2AnWniWxYsUTQuhn6a4Y31lGB6NKIPPOCzzw52xwmcc8dR3sOvh
IKoeHMLJIpAxrOAal12jzEMJout3wlidb+Y+Z9RIWOxkPoeEMLBJYH8Z3ESMQBRdr1DQbC7LMt8v
wSik8CEOYPPHyeXN4Z+6HP1+lpXsZF0plH1T90d0SQrIocyh7Zpem1iGXSaHxVliAI4toNUspIbr
R3ri3XQC396ZC6jGocbP79gz7HsSJkbgMzo9TfFy1EhJ3uQHgby2sANZdWLf5Ar3gvf4nB9x6jHC
/TgCRm0Of8IH4MwnkRdQsLb4GtAVv0qx50Zg6FSFGAPmErjL6KeMRjYFh2pONu9NNejatIComfcb
c5j5DWcSks53LXHU1n273tUoeqW4uhixCzJPH1fmrMvyIkLqxCHxI30EC7TE429U1fjLDpWcOiiv
Gtv+kLHtm7G1pZ9OOSOdfJZiatKV1Sz5FsXwI8dY/eIxTX+dzV9F0V+IutFj+yVv9gId1lXjWzEu
6WlMlbiI9Zq8cEtNW0GYd40sdIQf8rYVv0Mj93InO4EVCopXGzegTc5KHzllPs3YWdaKrx389FBa
UrjG5fZ2LrcLWOpuqKtCidkqWrxGiOqm4yNyug3hs6mgzbcMx/9pxtwzLtqhHJ9nOtJ+OLZYzAq5
Q5/aW7QELGm2j9XWcJKA4PHSB7A2S84Y5gYncPF15/2tO5NrG5Wnis+8LPpy8Sy3hCV9pDao42h6
9D87jjiLsTRn/sjZXd0XJQZ830CpNDvV5ytsoUguAx5nnKaQkgcK4YPr4ARHaOD49sPEATdmNv0f
RtpJmRxNKvbUwlLLsvpWxSU/B2HSxGUzbNwvD0eBNgp1UZ12Zh73IHNM0Pe5opk6RF2SmmuV3gMM
E+hVf7pJjk39VCJh4+Xe5NqPWzJiOIg27QXxHQl+sZwa2fFdlxGbY2CqEOL48R9QepYyJ88gij0a
v92dQrV7Tz+XR1OKI8fVh059LyIBzH0yrU0r9ypi5gkBSd12667J8b7RTC2YNJI45Uaa4Iki96N6
ElsmNySPH2d9FN4O9v3MboF8bHLDTrETDii28NiQEdumAR16v1T3amM5GBrNsWcbG7V3oNszC73A
ljH9zqLjBAojPXUn+JVMm5RZi7BEq26P/wXiY//w
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

    def _diffcompress(self, refe, new):
        """Compare files without comment"""
        sref = remove_comments(test_utils.uncompress64(refe))
        snew = remove_comments(new)
        return test_utils.difftxt(sref, snew)

if __name__ == '__main__':
    unittest.main()
