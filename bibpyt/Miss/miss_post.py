# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""
Module permettant le post-traitement d'un calcul MISS3D

Les concepts temporaires de calcul sont nommés "__xxxx" (soit .9000001)
automatiquement supprimés en sortie de la macro.
Les concepts dont on garde une référence dans les résultats sont nommés
avec "_xxxx" (soit _9000002).
"""

import os
import shutil
import traceback
import os.path as osp

from math import pi

from Accas import _F

import numpy as NP
from numpy.fft import ifft, fft

import aster
from Cata.cata import (
    _F, DETRUIRE, DEFI_FICHIER,
    NUME_DDL_GENE, PROJ_VECT_BASE, PROJ_MATR_BASE, COMB_MATR_ASSE,
    LIRE_IMPE_MISS, LIRE_FORC_MISS,
    DYNA_LINE_HARM, REST_SPEC_TEMP,
    DEFI_LIST_REEL, CALC_FONCTION, RECU_FONCTION, DEFI_FONCTION,
    FORMULE, CALC_FONC_INTERP, CREA_TABLE, LIRE_FONCTION,
    DEFI_CONSTANTE, CREA_CHAMP, AFFE_CHAR_MECA_F
)

from Utilitai.Table import Table
from Utilitai.Utmess import UTMESS
from Utilitai.utils import set_debug, _print, _printDBG
from Miss.miss_resu_miss import MissCsolReader
from Utilitai.force_iss_vari import force_iss_vari

# correspondance
FKEY = {
    'DX': 'FONC_X',
    'DY': 'FONC_Y',
    'DZ': 'FONC_Z',
}

class PostMiss(object):
    """Définition d'un post-traitement MISS3D."""

    def __init__(self, parent, param):
        """Initialisations"""
        self.parent = parent
        self.param = param
        self.verbose = param['INFO'] >= 2
        self.debug = self.verbose
        # liste de concepts intermédiaires à supprimer à la fin
        self._to_delete = []
        self.initco()
        if self.debug:
            set_debug(True)
        self.list_freq_DLH = None
        self.methode_fft = None
        self.fname = None

    def set_filename_callback(self, callback):
        """Enregistre la fonction qui fournit le nom du fichier associé à un type"""
        self.fname = callback

    def run(self):
        """Enchaine les tâches élémentaires"""
        self.argument()
        self.execute()
        self.sortie()

    def argument(self):
        """Vérification des arguments d'entrée."""
        # fréquences du calcul Miss
        info_freq(self.param)
        # interpolation des accéléros si présents (à supprimer sauf si TABLE)
        self.excit_kw = self.param['EXCIT_HARMO']
        if self.excit_kw is None:
            tmax = self.param['INST_FIN']
            pasdt = self.param['PAS_INST']
            __linst = DEFI_LIST_REEL(DEBUT=0.,
                                     INTERVALLE=_F(JUSQU_A=tmax - pasdt,
                                                   PAS=pasdt),)
            linst = __linst.Valeurs()
            UTMESS('I', 'MISS0_10', valr=(min(linst), max(linst), pasdt), vali=len(linst))
            if self.param['ACCE_X']:
                _acx = CALC_FONCTION(COMB=_F(FONCTION=self.param['ACCE_X'], COEF=1.0,),
                                     LIST_PARA=__linst)
                self.acce_x = _acx
            if self.param['ACCE_Y']:
                _acy = CALC_FONCTION(COMB=_F(FONCTION=self.param['ACCE_Y'], COEF=1.0,),
                                     LIST_PARA=__linst)
                self.acce_y = _acy
            if self.param['ACCE_Z']:
                _acz = CALC_FONCTION(COMB=_F(FONCTION=self.param['ACCE_Z'], COEF=1.0,),
                                     LIST_PARA=__linst)
                self.acce_z = _acz
            if self.param.get('GROUP_NO') is None:
                if self.param.get('DEPL_X'):
                    _acx = CALC_FONCTION(COMB=_F(FONCTION=self.param['DEPL_X'], COEF=1.0,),
                                         LIST_PARA=__linst)
                    self.depl_x = _acx
                if self.param.get('DEPL_Y'):
                    _acy = CALC_FONCTION(COMB=_F(FONCTION=self.param['DEPL_Y'], COEF=1.0,),
                                         LIST_PARA=__linst)
                    self.depl_y = _acy
                if self.param.get('DEPL_Z'):
                    _acz = CALC_FONCTION(COMB=_F(FONCTION=self.param['DEPL_Z'], COEF=1.0,),
                                         LIST_PARA=__linst)
                    self.depl_z = _acz

    def execute(self):
        """Lance le post-traitement"""
        raise NotImplementedError('must be defined in a derivated class')

    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        raise NotImplementedError('must be defined in a derivated class')

    def concepts_communs(self):
        """Construction des concepts partagés entre
        les différentes étapes du post-traitement"""
        _nddlg = NUME_DDL_GENE(BASE=self.param['BASE_MODALE'],
                                STOCKAGE= 'PLEIN')
        self.nddlgen = _nddlg
        __rigigen = PROJ_MATR_BASE(BASE=self.param['BASE_MODALE'],
                                   NUME_DDL_GENE=self.nddlgen,
                                   MATR_ASSE=self.param['MATR_RIGI'])
        self.rigigen = __rigigen
        __massgen = PROJ_MATR_BASE(BASE=self.param['BASE_MODALE'],
                                   NUME_DDL_GENE=self.nddlgen,
                                   MATR_ASSE=self.param['MATR_MASS'])
        self.massgen = __massgen
        if self.param['MATR_AMOR']:
            __amorgen = PROJ_MATR_BASE(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    MATR_ASSE=self.param['MATR_AMOR'])
            self.amorgen = __amorgen
        self.set_fft_accelero()
        self.set_freq_dlh()

    def set_fft_accelero(self):
        """Calcul des FFT des accélérogrammes."""
        if self.acce_x:
            _xff = CALC_FONCTION(FFT=_F(FONCTION=self.acce_x, METHODE=self.methode_fft,),)
            self.xff = _xff
        if self.acce_y:
            _yff = CALC_FONCTION(FFT=_F(FONCTION=self.acce_y, METHODE=self.methode_fft,),)
            self.yff = _yff
        if self.acce_z:
            _zff = CALC_FONCTION(FFT=_F(FONCTION=self.acce_z, METHODE=self.methode_fft,),)
            self.zff = _zff

        if self.depl_x:
            _xff = CALC_FONCTION(FFT=_F(FONCTION=self.depl_x, METHODE=self.methode_fft,),)
            self.xff = _xff
        if self.depl_y:
            _yff = CALC_FONCTION(FFT=_F(FONCTION=self.depl_y, METHODE=self.methode_fft,),)
            self.yff = _yff
        if self.depl_z:
            _zff = CALC_FONCTION(FFT=_F(FONCTION=self.depl_z, METHODE=self.methode_fft,),)
            self.zff = _zff

    def set_freq_dlh(self):
        """Déterminer les fréquences du calcul harmonique."""
        if self.excit_kw is not None:
            if self.param['FREQ_MIN'] is not None:
                freq_min = self.param['FREQ_MIN']
                freq_max = self.param['FREQ_MAX']
                df = self.param['FREQ_PAS']
                lfreq = NP.arange(freq_min, freq_max + df, df).tolist()
                UTMESS('I', 'MISS0_12', valr=(freq_min, freq_max, df), vali=len(lfreq))
            else:
                lfreq = self.param['LIST_FREQ']
                UTMESS('I', 'MISS0_11', valk=repr(lfreq), vali=len(lfreq))
        else:
            fft = self.xff or self.yff or self.zff
            assert fft is not None
            tf_fft = fft.convert('complex')
            df = tf_fft.vale_x[1] - tf_fft.vale_x[0]
            med = len(tf_fft.vale_x) / 2 + 1
            lfreq = tf_fft.vale_x[:med].tolist()
            freq_max = lfreq[-1]
            UTMESS('I', 'MISS0_12', valr=(0., freq_max, df), vali=len(lfreq))
        self.list_freq_DLH = lfreq

    def suppr_acce_fft(self):
        """Marque pour suppression les accéléros interpolés et leur FFT."""
        for co in (self.acce_x, self.acce_y, self.acce_z,
                   self.depl_x, self.depl_y, self.depl_z,
                   self.xff, self.yff, self.zff):
            if co:
                self._to_delete.append(co)

    def initco(self):
        """Deux fonctions :
        - initialiser les attributs de stockage des concepts communs,
        - libèrer les références avant la sortie de la macro pour destruction
          propre.
        """
        self.nddlgen = self.rigigen = self.massgen = None
        self.amorgen = None
        self.acce_x = self.acce_y = self.acce_z = None
        self.depl_x = self.depl_y = self.depl_z = None
        self.xff = self.yff = self.zff = None
        if len(self._to_delete) > 0:
            DETRUIRE(CONCEPT=_F(NOM=tuple(self._to_delete),),)
        # variables autres que concepts
        self._tkeys = self._torder = self._tline = None
        self.tab = None

    def check_datafile_exist(self):
        """Vérifie l'existence des fichiers."""
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_IMPE'])
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_FORC'])

    def init_table(self):
        """Initialise la table"""
        # pour la construction de la table
        # mêmes paramètres pour TABLE / TABLE_CONTROL
        self._tkeys = ('GROUP_NO', 'NOM_CHAM', 'NOM_PARA')
        self._torder = list(self._tkeys) + ['FONC_X', 'FONC_Y', 'FONC_Z']
        # pour stocker la correspondance clé_primaire : numéro_ligne
        self._tline = {}
        # table de stockage des fonctions résultats
        self.tab = Table()

    def add_line(self, gno, cham, para, **kwargs):
        """Pour simplifier l'ajout d'une ligne dans la table.
        Arguments optionnels supportés : fonc_x, fonc_y, fonc_z."""
        primkey = (gno, cham, para)
        index = self._tline.get(primkey, -1)
        values = dict([(k, v) for k, v in kwargs.items() \
                          if k in self._torder and v is not None])
        if index > 0:
            row = self.tab.rows[index]
            row.update(values)
        else:
            row = dict(zip(self._tkeys, primkey))
            row.update(values)
            self._tline[primkey] = len(self.tab)
            self.tab.append(row)

class PostMissTran(PostMiss):
    """Post-traitement de type 1, sortie tran_gene"""

    def __init__(self, parent, param):
        """Initialisation."""
        super(PostMissTran, self).__init__(parent, param)
        self.methode_fft = 'PROL_ZERO'
        self._suppr_matr = True
        self.excit_harmo = []

    def argument(self):
        """Vérification des arguments d'entrée."""
        super(PostMissTran, self).argument()
        # s'assurer que les unités logiques sont libérées (rewind)
        self.check_datafile_exist()
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_IMPE'])
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_FORC'])
        self.suppr_acce_fft()

    def execute(self):
        """Lance le post-traitement"""
        self.concepts_communs()
        self.boucle_dlh()


    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.parent.DeclareOut('resugene', self.parent.sd)
        self._to_delete.append(self.dyge)
        # XXX on pensait qu'il fallait utiliser PROL_ZERO mais miss01a
        #    devient alors NOOK. A clarifier/vérifier en passant
        #    d'autres tests avec ce post-traitement.
        resugene = REST_SPEC_TEMP(RESU_GENE = self.dyge,
                                  METHODE = 'TRONCATURE',
                                  SYMETRIE = 'NON',
                                  TOUT_CHAM = 'OUI')
        self.initco()


    def initco(self):
        """Ajoute les concepts"""
        super(PostMissTran, self).initco()
        self.dyge = None


    def boucle_dlh(self):
        """Exécution des DYNA_LINE_HARM"""
        first = True
        for freq in self.list_freq_DLH:
            opts = {}
            _printDBG("Calcul pour la fréquence %.2f Hz" % freq)
            __impe = LIRE_IMPE_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    UNITE_RESU_IMPE=self.param['UNITE_RESU_IMPE'],
                                    FREQ_EXTR=freq,
                                    TYPE=self.param['TYPE'],)

            _rito = COMB_MATR_ASSE(COMB_C=(_F(MATR_ASSE=__impe, COEF_C=1.0+0.j,),
                                            _F(MATR_ASSE=self.rigigen, COEF_C=1.0+0.j,),),
                                    SANS_CMP='LAGR',)
            if not first:
                opts = { 'RESULTAT' : self.dyge, 'reuse' : self.dyge }
            self.dyge = self.iteration_dlh(_rito, freq, opts)

            DETRUIRE(CONCEPT=_F(NOM=__impe,),)
            if self._suppr_matr:
                self._to_delete.append(_rito)
            first = False


    def iteration_dlh(self, rigtot, freq, opts):
        """Calculs à une fréquence donnée."""
        excit = []
        if self.acce_x:
            __fosx = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DX',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosx,
                            FONC_MULT_C = self.xff))
        if self.acce_y:
            __fosy = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DY',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosy,
                            FONC_MULT_C = self.yff))
        if self.acce_z:
            __fosz = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DZ',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosz,
                            FONC_MULT_C = self.zff))
        if self.depl_x:
            __fosx = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DX',
                                    NOM_CHAM='DEPL',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosx,
                            FONC_MULT_C = self.xff))
        if self.depl_y:
            __fosy = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DY',
                                    NOM_CHAM='DEPL',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosy,
                            FONC_MULT_C = self.yff))
        if self.depl_z:
            __fosz = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    NOM_CMP='DZ',
                                    NOM_CHAM='DEPL',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE_GENE = __fosz,
                            FONC_MULT_C = self.zff))
        if len(self.excit_harmo) > 0:
            excit.extend( self.excit_harmo )
        if self.amorgen :
            dyge = self.dyna_line_harm(MATR_MASS=self.massgen,
                                     MATR_AMOR=self.amorgen,
                                     MATR_RIGI=rigtot,
                                     FREQ=freq,
                                     EXCIT=excit,
                                     **opts)
        else :
            dyge = self.dyna_line_harm(MATR_MASS=self.massgen,
                                     MATR_RIGI=rigtot,
                                     FREQ=freq,
                                     AMOR_MODAL=_F(
                                     AMOR_REDUIT=self.param['AMOR_REDUIT'],),
                                     EXCIT=excit,
                                     **opts)
        return dyge


    def dyna_line_harm(self, **kwargs):
        """Execution de DYNA_LINE_HARM. Produit un concept temporaire."""
        __dyge = DYNA_LINE_HARM(**kwargs)
        return __dyge



class PostMissHarm(PostMissTran):
    """Post-traitement de type 1, sortie harm_gene"""

    def __init__(self, parent, param):
        """Initialisation."""
        super(PostMissHarm, self).__init__(parent, param)
        self.methode_fft = 'COMPLET'
        self._suppr_matr = False

    def argument(self):
        """Vérification des arguments d'entrée."""
        super(PostMissHarm, self).argument()
        self.parent.DeclareOut('trangene', self.parent.sd)
        self.suppr_acce_fft()

    def concepts_communs(self):
        """Construction des concepts spécifiques au cas HARMO."""
        super(PostMissHarm, self).concepts_communs()
        for excit_i in self.excit_kw:
            dExc = excit_i.cree_dict_valeurs(excit_i.mc_liste)
            for mc in dExc.keys():
                if dExc[mc] is None:
                    del dExc[mc]
            if dExc.get('VECT_ASSE') is not None:
                __excit = PROJ_VECT_BASE(BASE=self.param['BASE_MODALE'],
                                         NUME_DDL_GENE=self.nddlgen,
                                         VECT_ASSE=dExc['VECT_ASSE'])
                dExc['VECT_ASSE_GENE'] = __excit
                del dExc['VECT_ASSE']
            self.excit_harmo.append(dExc)

    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.initco()

    def dyna_line_harm(self, **kwargs):
        """Execution de DYNA_LINE_HARM. Produit le concept définitif."""
        trangene = DYNA_LINE_HARM(**kwargs)
        return trangene


class PostMissTabl(PostMiss):
    """Post-traitement de type 2"""

    def __init__(self, parent, param):
        """Initialisation."""
        super(PostMissTabl, self).__init__(parent, param)
        self.methode_fft = 'COMPLET'
        self.init_table()

    def argument(self):
        """Vérification des arguments d'entrée."""
        super(PostMissTabl, self).argument()
        # s'assurer que les unités logiques sont libérées (rewind)
        self.check_datafile_exist()
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_IMPE'])
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_FORC'])

    def execute(self):
        """Lance le post-traitement"""
        self.concepts_communs()
        self.boucle_dlh()
        self.recombinaison()

    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.parent.DeclareOut('tabout', self.parent.sd)
        self.tab.OrdreColonne(self._torder)
        dprod = self.tab.dict_CREA_TABLE()
        if self.verbose:
            aster.affiche('MESSAGE', repr(self.tab))

        # type de la table de sortie
        tabout = CREA_TABLE(TYPE_TABLE='TABLE',
                            **dprod)
        self.initco()

    def initco(self):
        """Ajoute les concepts"""
        super(PostMissTabl, self).initco()
        self.dyge_x = self.dyge_y = self.dyge_z = None

    def boucle_dlh(self):
        """Exécution des DYNA_LINE_HARM dans les 3 directions (chargement unitaire)"""
        first = True
        for freq in self.list_freq_DLH:
            opts = {}
            _printDBG("Calcul pour la fréquence %.2f Hz" % freq)
            __impe = LIRE_IMPE_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    ISSF=self.param['ISSF'],
                                    UNITE_RESU_IMPE=self.param['UNITE_RESU_IMPE'],
                                    FREQ_EXTR=freq,
                                    TYPE=self.param['TYPE'],)

            _rito = COMB_MATR_ASSE(COMB_C=(_F(MATR_ASSE=__impe, COEF_C=1.0+0.j,),
                                            _F(MATR_ASSE=self.rigigen, COEF_C=1.0+0.j,),),
                                    SANS_CMP='LAGR',)
            if self.acce_x:
                if not first:
                    opts = { 'RESULTAT' : self.dyge_x, 'reuse' : self.dyge_x }
                self.dyge_x = self.iteration_dlh('DX', _rito, freq, opts)

            if self.acce_y:
                if not first:
                    opts = { 'RESULTAT' : self.dyge_y, 'reuse' : self.dyge_y }
                self.dyge_y = self.iteration_dlh('DY', _rito, freq, opts)

            if self.acce_z:
                if not first:
                    opts = { 'RESULTAT' : self.dyge_z, 'reuse' : self.dyge_z }
                self.dyge_z = self.iteration_dlh('DZ', _rito, freq, opts)

            DETRUIRE(CONCEPT=_F(NOM=__impe,),)
            self._to_delete.append(_rito)
            first = False

    def iteration_dlh(self, dir, rigtot, freq, opts):
        """Exécution d'un DYNA_LINE_HARM"""
        __fosx = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                NUME_DDL_GENE=self.nddlgen,
                                ISSF=self.param['ISSF'],
                                NOM_CMP=dir,
                                NOM_CHAM='ACCE',
                                UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                FREQ_EXTR=freq,)
        __dyge = DYNA_LINE_HARM(MATR_MASS=self.massgen,
                                MATR_RIGI=rigtot,
                                FREQ=freq,
                                AMOR_MODAL=_F(
                                AMOR_REDUIT=self.param['AMOR_REDUIT'],),
                                EXCIT=_F(VECT_ASSE_GENE=__fosx,
                                         COEF_MULT=1.0),
                                **opts)
        DETRUIRE(CONCEPT=_F(NOM=__fosx),)
        return __dyge

    def recombinaison(self):
        """Recombinaison des réponses unitaires."""
        # stockage des accéléro et leur fft
        self.add_line('', 'ACCE', 'INST',
                      FONC_X=getattr(self.acce_x, 'nom', None),
                      FONC_Y=getattr(self.acce_y, 'nom', None),
                      FONC_Z=getattr(self.acce_z, 'nom', None))
        self.add_line('', 'ACCE', 'FREQ',
                      FONC_X=getattr(self.xff, 'nom', None),
                      FONC_Y=getattr(self.yff, 'nom', None),
                      FONC_Z=getattr(self.zff, 'nom', None))

        # conversion faite une fois
        if self.acce_x:
            self.tf_xff = self.xff.convert('complex')
        if self.acce_y:
            self.tf_yff = self.yff.convert('complex')
        if self.acce_z:
            self.tf_zff = self.zff.convert('complex')
        # boucle sur les group_no
        for gno in self.param['GROUP_NO']:
            for cham in ('DEPL', 'VITE', 'ACCE'):
                if self.acce_x:
                    self.gen_funct(gno, cham, 'DX')
                if self.acce_y:
                    self.gen_funct(gno, cham, 'DY')
                if self.acce_z:
                    self.gen_funct(gno, cham, 'DZ')

    def gen_funct(self, gno, cham, dir):
        """Calcul la réponse en un noeud particulier dans la direction 'dir'
        sur les fréquences 'lfreq'."""
        _printDBG("Calcul de la réponse en %s, direction %s" % (gno, dir))
        tfc = 0.
        to_del = []
        if self.acce_x:
            _repx = RECU_FONCTION(RESU_GENE=self.dyge_x,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repx.convert('complex') * self.tf_xff + tfc
            to_del.append(_repx)
        if self.acce_y:
            _repy = RECU_FONCTION(RESU_GENE=self.dyge_y,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repy.convert('complex') * self.tf_yff + tfc
            to_del.append(_repy)
        if self.acce_z:
            _repz = RECU_FONCTION(RESU_GENE=self.dyge_z,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repz.convert('complex') * self.tf_zff + tfc
            to_del.append(_repz)

        tffr = tfc.evalfonc(self.list_freq_DLH)
        tffr.para['NOM_PARA'] = 'FREQ'
        fft = tffr.fft('COMPLET', 'NON')
        # création des concepts
        _repfreq = DEFI_FONCTION(VALE_C=tffr.tabul(),
                                 **tffr.para)
        _reptemp = DEFI_FONCTION(VALE=fft.tabul(),
                                 **fft.para)
        opts = {}
        if self.param['LIST_FREQ_SPEC_OSCI']:
            opts['LIST_FREQ'] = self.param['LIST_FREQ_SPEC_OSCI']
        _spec = CALC_FONCTION(SPEC_OSCI=_F(FONCTION=_reptemp,
                                           NORME=self.param['NORME'],
                                           AMOR_REDUIT=self.param['AMOR_SPEC_OSCI'],
                                           **opts),)
        DETRUIRE(CONCEPT=_F(NOM=tuple(to_del),),)
        self.add_line(gno, cham, 'INST', **{ FKEY[dir] : _reptemp.nom })
        self.add_line(gno, cham, 'FREQ', **{ FKEY[dir] : _repfreq.nom })
        self.add_line(gno, cham, 'SPEC_OSCI', **{ FKEY[dir] : _spec.nom })

class PostMissFichier(PostMiss):
    """Pas de post-traitement, car on ne sort que les fichiers."""

    def argument(self):
        """Vérification des arguments d'entrée."""
        # fréquences du calcul Miss
        info_freq(self.param)

    def execute(self):
        """Lance le post-traitement"""

    def sortie(self):
        """Prépare et produit les concepts de sortie."""

class PostMissControl(PostMiss):
    """Produit la table des grandeurs aux points de contrôles"""

    def __init__(self, parent, param):
        """Initialisation."""
        super(PostMissControl, self).__init__(parent, param)
        self.init_table()
        self.methode_fft = 'COMPLET'

    def execute(self):
        """Lecture du fichier produit par Miss"""
        reader = MissCsolReader(self.param['_nbPC'], self.param['_nbfreq'])
        lfreq, values = reader.read(self.fname("01.csol.a"))
        self.tab = tab = Table()
        # stockage des accéléro et leur fft
        self.set_fft_accelero()
        self.recombinaison()
        for ipc, respc in enumerate(values):
            nompc = 'PC%d' % (ipc + 1)
            # stocke les fonctions de transfert telles quelles
            fct = []
            for iddl, comp in enumerate(('X', 'Y', 'Z')):
                valpc = respc.comp[iddl]
                fct.append(self._fonct_transfert('FREQ', lfreq, *valpc))
            self.add_line(nompc, 'TRANSFERT', 'FREQ',
                          FONC_X=fct[0].nom,
                          FONC_Y=fct[1].nom,
                          FONC_Z=fct[2].nom)
            if self.acce_x:
                self.gen_funct(nompc, 'ACCE', 'FONC_X', fct[0], self.tf_xff)
            if self.acce_y:
                self.gen_funct(nompc, 'ACCE', 'FONC_Y', fct[1], self.tf_yff)
            if self.acce_z:
                self.gen_funct(nompc, 'ACCE', 'FONC_Z', fct[2], self.tf_zff)

    def sortie(self):
        """Prépare et produit les concepts de sortie"""
        self.parent.DeclareOut('tabout', self.parent.sd)
        self.tab.OrdreColonne(self._torder)
        dprod = self.tab.dict_CREA_TABLE()
        if self.verbose:
            aster.affiche('MESSAGE', repr(self.tab))
        # type de la table de sortie
        tabout = CREA_TABLE(TYPE_TABLE='TABLE',
                            **dprod)
        self.initco()

    def recombinaison(self):
        """Recombinaison des réponses unitaires."""
        # ici il n'y a pas de recombinaison - par similitude avec TABLE
        # stockage des accéléro et leur fft
        self.add_line('', 'ACCE', 'INST',
                      FONC_X=getattr(self.acce_x, 'nom', None),
                      FONC_Y=getattr(self.acce_y, 'nom', None),
                      FONC_Z=getattr(self.acce_z, 'nom', None))
        self.add_line('', 'ACCE', 'FREQ',
                      FONC_X=getattr(self.xff, 'nom', None),
                      FONC_Y=getattr(self.yff, 'nom', None),
                      FONC_Z=getattr(self.zff, 'nom', None))
        # conversion faite une fois
        if self.acce_x:
            self.tf_xff = self.xff.convert('complex')
        if self.acce_y:
            self.tf_yff = self.yff.convert('complex')
        if self.acce_z:
            self.tf_zff = self.zff.convert('complex')

    def _fonct_transfert(self, nom_para, absc, real, imag):
        """Produit une fonction à stocker dans la table"""
        tab = NP.array([absc, real, imag])
        vale_c = tab.transpose().ravel().tolist()
        _fct = DEFI_FONCTION(NOM_PARA=nom_para,
                             NOM_RESU='ACCE',
                             VALE_C=vale_c)
        return _fct

    def gen_funct(self, nompc, cham, fonct_i, ftri, tfa_i):
        """Calcul le produit 'fonction de transfert' * fft(acce_i)"""
        tfft = ftri.convert('complex')
        tfc = tfft * tfa_i
        tffr = tfc.evalfonc(tfft.vale_x)
        tffr.para['NOM_PARA'] = 'FREQ'
        fft = tffr.fft('COMPLET', 'NON')
        # création des concepts
        _repfreq = DEFI_FONCTION(VALE_C=tffr.tabul(),
                                 **tffr.para)
        _reptemp = DEFI_FONCTION(VALE=fft.tabul(),
                                 **fft.para)
        opts = {}
        if self.param['LIST_FREQ_SPEC_OSCI']:
            opts['LIST_FREQ'] = self.param['LIST_FREQ_SPEC_OSCI']
        _spec = CALC_FONCTION(SPEC_OSCI=_F(FONCTION=_reptemp,
                                           NORME=self.param['NORME'],
                                           AMOR_REDUIT=self.param['AMOR_SPEC_OSCI'],
                                           **opts),)
        self.add_line(nompc, cham, 'INST', **{ fonct_i : _reptemp.nom })
        self.add_line(nompc, cham, 'FREQ', **{ fonct_i : _repfreq.nom })
        self.add_line(nompc, cham, 'SPEC_OSCI', **{ fonct_i : _spec.nom })

class PostMissFichierTemps(PostMissFichier):
    """Pas de post-traitement, car on ne sort que les fichiers."""

    def init_attr(self):
        """Initialisations"""
        self.dt = self.param['PAS_INST']
        N_inst = int(self.param['INST_FIN']/self.param['PAS_INST'])
        factor = self.param['COEF_SURECH']
        self.L_points = factor*N_inst
        self.nbr_freq = self.L_points/2 + 1
        eps = self.param['PRECISION']
        self.rho = eps**(1./(2.*N_inst))
        self.nrows = self.param['NB_MODE']
        self.ncols = self.param['NB_MODE']
        self.reducFactor = self.param['FACTEUR_INTERPOL']
        self.cutOffValue = self.param['PCENT_FREQ_CALCUL']/100.0
        self.Mg = 0
        self.Kg = 0
        self.Cg = 0

        self.Z_temps = NP.zeros((self.nrows, self.ncols, self.L_points), float)
        self.Fs_temps = NP.zeros((self.ncols, self.L_points), float)

    def execute(self):
        """Lance le post-traitement (passage Laplace-Temps)"""
        self.init_attr()
        self.calc_impe_temps()
        self.ecri_impe(self.Z_temps)

        if self.param['EXCIT_SOL']:
            self.calc_forc_temps()
            self.ecri_forc(self.Fs_temps)


    def calc_impe_temps(self):
        """Calcul de l'impédance dans le domaine temporel"""
        fid = open(self.fname("impe_Laplace"), 'r')
        reduc_factor = self.reducFactor
        angle_seuil = self.cutOffValue
        if angle_seuil == 1:
            fc = self.cutOffValue*self.nbr_freq/float(reduc_factor) % self.nbr_freq
        else:
            fc = NP.int_(NP.ceil(self.cutOffValue*self.nbr_freq/float(reduc_factor)))
        freq_list1 = NP.arange(0, fc*reduc_factor)
        freq_list2 = NP.arange(fc*reduc_factor, self.nbr_freq, reduc_factor)
        real_size = len(freq_list1.tolist() + freq_list2.tolist())
        impe_Laplace = NP.zeros((self.nrows, self.ncols, self.nbr_freq), complex)

        for k in range(0, real_size):
            for n in range(0, self.nrows):
                for m in range(0, self.ncols):
                    txt = fid.readline()
                    if( txt[0:7] == 'MATRICE' ):
                        tmp = fid.readline()
                        k = k + 1
                    data = fid.readline().split()
                    impe_Laplace[n, m, k-1] = float(data[1]) + 1j*float(data[2])

        fid.close()

        freq_vect  = NP.arange(0, self.L_points)
        Z_Laplace    = NP.zeros((self.nrows, self.ncols, self.L_points), complex)
        Z_Laplace_re = NP.zeros((self.nrows, self.ncols, self.L_points), complex)
        Z_Laplace_im = NP.zeros((self.nrows, self.ncols, self.L_points), complex)

        # Ce ne sont pas des vecteurs de fréquences mais des vecteurs d'indices
        freq_list1 = list(NP.arange(0, reduc_factor*fc))
        freq_list2 = list(NP.arange(reduc_factor*fc, self.L_points - fc*reduc_factor, reduc_factor))
        if reduc_factor is not 1 and angle_seuil < 1:
            freq_list3 = list(NP.arange(self.L_points - fc*reduc_factor, self.L_points))
        else:
            freq_list3 = []
        freq_reduc = freq_list1 + freq_list2 + freq_list3

        if len(freq_list2) % 2 == 0:
            length = len(freq_list1) + len(freq_list2)/2
        else:
            length = len(freq_list1) + (len(freq_list2)-1)/2 + 1
        Z_reduced = NP.zeros((self.nrows, self.ncols, len(freq_reduc)), complex)
        Z_reduced[:,:, 0:length+1] = NP.conj(impe_Laplace[:,:, 0:length+1])
        Z_reduced[:,:, length+1:] = impe_Laplace[:,:, length-1:0:-1]

        Z_redu_r = NP.real(Z_reduced)
        Z_redu_i = NP.imag(Z_reduced)

        for n in range(0, self.nrows):
            for m in range(0, self.ncols):
                Z_Laplace_re[n, m,:] = NP.interp(freq_vect, freq_reduc, Z_redu_r[n, m,:]);
                Z_Laplace_im[n, m,:] = NP.interp(freq_vect, freq_reduc, Z_redu_i[n, m,:]);

        for k in range(0, self.L_points):
            Z_Laplace[:,:, k] = Z_Laplace_re[:,:, k] + 1j*Z_Laplace_im[:,:, k];

        MATR_GENE = self.param['MATR_GENE']
        if MATR_GENE:
            z_complex = NP.exp(1j*2*pi/self.L_points)  # 'pi' imported from math
            GammaZ = NP.zeros(self.L_points, complex)
            order = 2
            for k in range(0, self.L_points):
                for m in range(1, order+1):
                    GammaZ[k] = GammaZ[k] + 1./m * (1 - self.rho*z_complex**(-k))**m
            s_laplace = GammaZ / self.dt

            if MATR_GENE['MATR_AMOR']:
                self.Cg = MATR_GENE['MATR_AMOR'].EXTR_MATR_GENE()

            if MATR_GENE['MATR_RIGI']:
                self.Kg = MATR_GENE['MATR_RIGI'].EXTR_MATR_GENE()

            if MATR_GENE['DECOMP_IMPE'] == 'PRODUIT':
                if MATR_GENE['MATR_MASS']:
                    self.Mg = MATR_GENE['MATR_MASS'].EXTR_MATR_GENE()
                Hyst = NP.imag(Z_Laplace[:,:, 0])
                for r in range(0, self.L_points):
                    mat = self.Mg*(s_laplace[r]**2) + self.Cg*s_laplace[r] + self.Kg
                    mat_inv = NP.linalg.inv(mat)
                    if MATR_GENE['AMOR_HYST'] == 'DANS_MATR_AMOR':
                        Z_Laplace[:,:, r] = Z_Laplace[:,:, r] - 1j*Hyst*NP.sign(NP.imag(s_laplace[r]))
                    Z_Laplace[:,:, r] = Z_Laplace[:,:, r]*mat_inv
            else:
                if MATR_GENE['MATR_MASS'] or MATR_GENE['AMOR_HYST']:
                    UTMESS('A', 'MISS0_19')
                for r in range(0, self.L_points):
                    Z_Laplace[:,:, r] = Z_Laplace[:,:, r] - self.Cg*s_laplace[r] - self.Kg

        rhoinv = 1./self.rho
        for r in range(0, self.nrows):
            for l in range(0, self.ncols):
                fact = 1
                x_lapl = Z_Laplace[r, l,:]
                x_t = NP.real(ifft(x_lapl))
                for k in range(0, len(x_t)):
                    x_t[k] = fact * x_t[k]  # x_t[k] = rho**(-k) * x_t[k]
                    fact = fact*rhoinv
                self.Z_temps[r, l,:] = x_t

        termes_diag = NP.diag(self.Z_temps[:,:, 0])
        liste_ddl_pb = ''
        for k in range(0, len(termes_diag)):
            if termes_diag[k] <= 0:
                liste_ddl_pb = liste_ddl_pb + str(k) + ', '
        if ( len(liste_ddl_pb) > 0 ):
            UTMESS('A', 'MISS0_20', valk = liste_ddl_pb )


    def calc_forc_temps(self):
        """Calcul de l'effort sismique dans le domaine temporel"""
        F_sism_temps1 = NP.zeros((self.nrows, self.L_points), float)
        F_sism_temps2 = NP.zeros((self.nrows, self.L_points), float)
        F_sism_temps3 = NP.zeros((self.nrows, self.L_points), float)

        __linst = DEFI_LIST_REEL(DEBUT=0.,
                    INTERVALLE=_F(JUSQU_A= self.param['INST_FIN'] - self.dt, PAS=self.dt),)
        # Rq. Même UL que le fichier effort sismique de MISS
        unit = self.param['EXCIT_SOL']['UNITE_RESU_FORC']
        nbmod = self.param['NB_MODE']

        if self.param['EXCIT_SOL']['CHAM_X']:
            deplx_aster = self.calc_depl('CHAM_X', __linst)
            __fdpx = CALC_FONCTION( FFT=_F(FONCTION= deplx_aster, METHODE='COMPLET'))
            for mode in range(0, nbmod):
                F_sism_temps1[mode,:] = self.calc_forc_comp_temps(unit,
                                              __linst, __fdpx, mode + 1)
            DETRUIRE(CONCEPT=_F(NOM=__fdpx))

        if self.param['EXCIT_SOL']['CHAM_Y']:
            deply_aster = self.calc_depl('CHAM_Y', __linst)
            __fdpy = CALC_FONCTION( FFT=_F(FONCTION= deply_aster, METHODE='COMPLET'))
            for mode in range(0, nbmod):
                F_sism_temps2[mode,:] = self.calc_forc_comp_temps(unit,
                                              __linst, __fdpy, nbmod + mode + 1)
            DETRUIRE(CONCEPT=_F(NOM=__fdpy))

        if self.param['EXCIT_SOL']['CHAM_Z']:
            deplz_aster = self.calc_depl('CHAM_Z', __linst)
            __fdpz = CALC_FONCTION( FFT=_F(FONCTION= deplz_aster, METHODE='COMPLET'))
            for mode in range(0, nbmod):
                F_sism_temps3[mode,:] = self.calc_forc_comp_temps(unit,
                                              __linst, __fdpz, 2*nbmod + mode + 1)
            DETRUIRE(CONCEPT=_F(NOM=__fdpz))

        self.Fs_temps = F_sism_temps1 + F_sism_temps2 + F_sism_temps3


    def calc_forc_comp_temps(self, unit, linst, fdepl, indice):
        """???"""
        __lfreq = DEFI_LIST_REEL(DEBUT=0.0,
                       INTERVALLE=_F(JUSQU_A=1./self.param['PAS_INST']/2.0,
                       PAS=1./self.param['PAS_INST']/self.L_points,),)

        __fHr = LIRE_FONCTION(UNITE=unit, NOM_PARA='FREQ',
                       INDIC_PARA=[1, 1], INDIC_RESU=[indice, 2],
                         )

        __fHi = LIRE_FONCTION(UNITE=unit, NOM_PARA='FREQ',
                       INDIC_PARA=[1, 1], INDIC_RESU=[indice, 3],
                         )

        __fH = CALC_FONCTION(COMB_C=(
                       _F(FONCTION=__fHr, COEF_C=1+0j),
                       _F(FONCTION=__fHi, COEF_C=0-1j),
                             ),
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                         )

        self.parent.update_const_context({ '__fH' : __fH, '__fdepl' : fdepl })
        __fF = FORMULE(NOM_PARA='FREQ', VALE_C='__fdepl(FREQ) * __fH(FREQ)')

        __fT0 = CALC_FONC_INTERP(FONCTION=__fF, NOM_PARA = 'FREQ',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=__lfreq)


        __fTT = CALC_FONCTION( FFT=_F(FONCTION=__fT0, METHODE='COMPLET',
                            SYME='NON'))

        self.parent.update_const_context({'__fTT' : __fTT})
        __fTTF = FORMULE(NOM_PARA='INST', VALE='__fTT(INST) - __fTT(0)')

        __fT1 = CALC_FONC_INTERP(FONCTION=__fTTF, NOM_PARA = 'INST',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=linst)

        force = __fT1.Valeurs()[1]
        DETRUIRE(CONCEPT=_F(NOM=(__fHr, __fHi, __fH, __fF, __fT0, __fTT, __fTTF, __fT1, __lfreq)))
        return force


    def ecri_impe(self, Z_temps):
        """Ecriture des 3 types d'impédance dans les 3 fichiers de sortie"""
        for n in range(0, self.L_points):
            # Symmetric impedance
            Z_temps[:,:, n] = (Z_temps[:,:, n] + Z_temps[:,:, n].transpose())/2.

        if self.param['MATR_GENE']:
            if self.param['MATR_GENE']['MATR_MASS']:
                Z_temps_M = NP.zeros((self.nrows, self.ncols, self.L_points), float)
                for n in range(0, self.L_points):
                    Z_temps_M[:,:, n] = NP.dot(Z_temps[:,:, n], self.Mg)
                self.impr_impe(Z_temps_M, self.param['UNITE_RESU_MASS'])

            if self.param['MATR_GENE']['MATR_AMOR']:
                Z_temps_C = NP.zeros((self.nrows, self.ncols, self.L_points), float)
                for n in range(0, self.L_points):
                    Z_temps_C[:,:, n] = NP.dot(Z_temps[:,:, n], self.Cg)
                self.impr_impe(Z_temps_C, self.param['UNITE_RESU_AMOR'])

            if self.param['MATR_GENE']['MATR_RIGI']:
                Z_temps_K = NP.zeros((self.nrows, self.ncols, self.L_points), float)
                for n in range(0, self.L_points):
                    Z_temps_K[:,:, n] = NP.dot(Z_temps[:,:, n], self.Kg)
                self.impr_impe(Z_temps_K, self.param['UNITE_RESU_RIGI'])
        else:
            self.impr_impe(Z_temps, self.param['UNITE_RESU_RIGI'])


    def impr_impe(self, Zdt, unite_type_impe):
        """Ecriture d'une impédance quelconque dans le fichier de sortie en argument"""
        fname = self.fname(unite_type_impe)
        fid = open(fname, 'w')

        if self.param['NB_MODE'] < 6:
            nb_colonne = self.param['NB_MODE']
        else:
            nb_colonne = 6

        fmt_ligne = " %13.6E" * nb_colonne
        # residu = NP.remainder(self.param['NB_MODE'],NB_COLONNE)
        # fmt_ligne_fin = " %13.6E" * residu

        txt = []
        txt.append('%s %s' % (str(self.L_points), str(self.dt)))
        for n in range(0, self.L_points):
            txt.append('%s' % str(n*self.dt))
            for l in range(0, self.nrows):
                for c in range(0, self.ncols, nb_colonne):
                    txt.append(fmt_ligne % tuple(Zdt[l, c:c+nb_colonne, n]))
        fid.write(os.linesep.join(txt))
        fid.close()
        shutil.copyfile(fname, osp.join( self.param['_INIDIR'], 'fort' + '.' + str(unite_type_impe) ))


    def ecri_forc(self, fs_temps):
        """Ecriture de l'effort sismique dans le fichier de sortie"""
        ul = self.param['EXCIT_SOL']['UNITE_RESU_FORC']
        fname = self.fname(ul)
        fid = open(fname, 'w')

        if self.param['NB_MODE'] < 6:
            nb_colonne = self.param['NB_MODE']
        else:
            nb_colonne = 6

        fmt_ligne = " %13.6E" * nb_colonne
        # residu = NP.remainder(self.param['NB_MODE'],NB_COLONNE)
        # fmt_ligne_fin = " %13.6E" * residu

        txt = []
        txt.append('%s %s' % (str(self.L_points), str(self.dt)))
        for n in range(0, self.L_points):
            txt.append('%s' % str(n*self.dt))
            for mode in range(0, self.param['NB_MODE'], nb_colonne):
                txt.append(fmt_ligne % tuple(fs_temps[mode:mode+nb_colonne, n]))
        fid.write(os.linesep.join(txt))
        fid.close()
        shutil.copyfile(fname, osp.join( self.param['_INIDIR'], 'fort' + '.' + str(ul)))


    def cumtrapz(self, a):
        """Integration en temps (accélération -> vitesse -> déplacement)"""
        length = len(a)
        b = NP.zeros(length, float)
        for k in range(0, length - 1):
            b[k+1] = NP.add(a[k], a[k+1])/2.
        c = NP.add.accumulate(b)
        return c


    def calc_depl(self, champ_dir, __linst):
        """Lecture du fichier déplacement/vitesse/accélération pour le calcul de l'effort sismique"""
        __champ = CALC_FONCTION(COMB=_F(FONCTION=self.param['EXCIT_SOL'][champ_dir],
                                        COEF=1.0,),
                                LIST_PARA=__linst)
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'ACCE'):
            __vite = CALC_FONCTION( INTEGRE=_F(  FONCTION = __champ) )
            __vite_der = CALC_FONCTION( DERIVE=_F(  FONCTION = __vite) )
            __depl = CALC_FONCTION( INTEGRE=_F(  FONCTION = __vite_der) )
            __depl_der = CALC_FONCTION( DERIVE=_F(  FONCTION = __depl) )
            __depl_champ =  __depl_der
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'VITE'):
            __depl = CALC_FONCTION( INTEGRE=_F(  FONCTION = __champ) )
            __depl_der = CALC_FONCTION( DERIVE=_F(  FONCTION = __depl) )
            __depl_champ =  __depl_der
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'DEPL'):
            __depl_champ =  __champ
        return __depl_champ

    # --- utilitaires internes
    def _fichier_tmp(self, ext):
        """Retourne le nom d'un fichier MISS dans WRKDIR.
        """
        fich = '%s.%s' % (self.param['PROJET'], ext)
        return osp.join(self.param['_WRKDIR'], fich)
        
class PostMissChar(PostMiss):
    """Post-traitement avec sortie charge"""

    def argument(self):
        """Initialisations"""

        self.MODELE = self.param['MODELE']
        self.fonc_depl  =  self.param['FONC_SIGNAL']
        self.List_Noeu_Fictif = self.param['NOEUD_AFFE']
        self.nbno = len(self.List_Noeu_Fictif)
        self.ncols = 6*self.nbno
        self.dt = self.param['PAS_INST']
        fonc_signal = self.param['FONC_SIGNAL']
        tt, vale_s = fonc_signal.Valeurs()
        self.dt = tt[1]-tt[0]
        NBTIME = len(vale_s)
        self.tmax = tt[NBTIME-1]
        self.df = 1.0/(self.tmax+self.dt)
        self.fmax = 1.0/(2.0*self.dt)
        self.nb_freq = 1+int(self.fmax/self.df)
        self.Force_Nodale = []

    def execute(self):
        """Lance le post-traitement"""
        self.calc_forc_temps()

    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.parent.DeclareOut('Force_no', self.parent.sd)
        Force_no = AFFE_CHAR_MECA_F(MODELE=self.MODELE, FORCE_NODALE=self.Force_Nodale,);

    def calc_forc_temps(self):
        """Calcul de l'effort sismique dans le domaine temporel"""

        __linst = DEFI_LIST_REEL(DEBUT=0.,
                    INTERVALLE=_F(JUSQU_A= self.tmax, PAS=self.dt),)
        # Rq. Même UL que le fichier effort sismique de MISS
        unit = self.param['UNITE_RESU_FORC']
        nbmod = self.ncols
        __fdep = CALC_FONCTION( FFT=_F(FONCTION= self.fonc_depl, METHODE='COMPLET'))

        if self.param['NOM_CMP'] == 'DX':
            ndeca = 0
        elif self.param['NOM_CMP'] == 'DY':
            ndeca = nbmod
        elif self.param['NOM_CMP'] == 'DZ':
            ndeca = 2*nbmod
            
        if self.param['VARI'] == 'OUI':
           MODELE  =  self.param['MODELE']
           MATR_GENE = self.param['MATR_GENE']
           INFO = self.param['INFO']
           ISSF = self.param['ISSF']
           NOM_CMP = self.param['NOM_CMP']
           PRECISION = self.param['PRECISION']
           PAS=self.df 
           INTERF = self.param['INTERF']
           MATR_COHE = self.param['MATR_COHE']
           UNITE_RESU_IMPE = self.param['UNITE_RESU_IMPE']
           TYPE=self.param['TYPE']
           FMAX=self.fmax
           FINI=0.0
           imod=1
           FSIST=force_iss_vari(self,imod,MATR_GENE,NOM_CMP,ISSF,INFO,unit,
            UNITE_RESU_IMPE,PRECISION,INTERF,MATR_COHE,TYPE,FINI,PAS,FMAX)

        for ino in range(0, self.nbno):
           no = self.List_Noeu_Fictif[ino]
           if self.param['VARI'] == 'NON':
             fx = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+1)
             fy = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+2)
             fz = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+3)
             frx = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+4)
             fry = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+5)
             frz = self.calc_forc_comp_temps(unit, __linst, __fdep, ndeca+6*ino+6)
           elif self.param['VARI'] == 'OUI':
             fx = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+1)
             fy = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+2)
             fz = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+3)
             frx = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+4)
             fry = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+5)
             frz = self.calc_forc_vari_temps(FSIST, __linst, __fdep, 6*ino+6)
             
           self.Force_Nodale.append(_F(NOEUD=no,
                                  FX= fx,
                                  FY= fy,
                                  FZ= fz,
                                  MX= frx,
                                  MY= fry,
                                  MZ= frz,),);

        DETRUIRE(CONCEPT=_F(NOM=__fdep))

    def calc_forc_comp_temps(self, unit, linst, fdepl, indice):
        """???"""

        __lfreq = DEFI_LIST_REEL(DEBUT=0.0,
                       INTERVALLE=_F(JUSQU_A=self.fmax, PAS=self.df,),)

        if self.param['FREQ_MAX'] is None :
            __FILTRE = DEFI_FONCTION(NOM_PARA='FREQ',
                       VALE_C=(0., 1., 0., self.fmax, 1., 0.),
                       INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',);

        else:
            fcoup = self.param['FREQ_MAX']
            fcou2 = fcoup + self.df
            __FILTRE = DEFI_FONCTION(NOM_PARA='FREQ',
                       VALE_C=(0., 1., 0., fcoup, 1., 0., fcou2, 0., 0.),
                       INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',);

        __fHr = LIRE_FONCTION(UNITE=unit, NOM_PARA='FREQ',
                       INDIC_PARA=[1, 1], INDIC_RESU=[indice, 2],
                         )

        __fHi = LIRE_FONCTION(UNITE=unit, NOM_PARA='FREQ',
                       INDIC_PARA=[1, 1], INDIC_RESU=[indice, 3],
                         )

        __fH = CALC_FONCTION(COMB_C=(
                       _F(FONCTION=__fHr, COEF_C=1+0j),
                       _F(FONCTION=__fHi, COEF_C=0-1j),
                             ),
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                         )

        __fT0 = CALC_FONCTION(MULT=(
                                    _F(FONCTION=fdepl,),
                                    _F(FONCTION=__fH,),
                                    _F(FONCTION=__FILTRE,)
                                    ),
                              LIST_PARA=__lfreq, NOM_PARA='FREQ',
                              PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                         )

        __fTT = CALC_FONCTION( FFT=_F(FONCTION=__fT0, METHODE='COMPLET',
                            SYME='NON'))
                        
        __fTC = DEFI_CONSTANTE(VALE=__fTT(0))
        
        __fT1 = CALC_FONCTION(COMB=(
                                    _F(FONCTION=__fTT,COEF=1.0,),
                                    _F(FONCTION=__fTC,COEF=-1.0,)
                                    ),
                              LIST_PARA=linst, NOM_PARA='INST',
                              PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                         )

        DETRUIRE(CONCEPT=_F(NOM=(__fHr, __fHi, __fH, __fT0, __fTT, __fTC, __lfreq)))
        return __fT1
        
    def calc_forc_vari_temps(self, FSIST, linst, fdepl, indice):
        """???"""

        __lfreq = DEFI_LIST_REEL(DEBUT=0.0,
                       INTERVALLE=_F(JUSQU_A=self.fmax, PAS=self.df,),)
        NB_FREQ=self.nb_freq

        if self.param['FREQ_MAX'] is None :
            __FILTRE = DEFI_FONCTION(NOM_PARA='FREQ',
                       VALE_C=(0., 1., 0., self.fmax, 1., 0.),
                       INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',);
        else:
            fcoup = self.param['FREQ_MAX']
            fcou2 = fcoup + self.df
            __FILTRE = DEFI_FONCTION(NOM_PARA='FREQ',
                       VALE_C=(0., 1., 0., fcoup, 1., 0., fcou2, 0., 0.),
                       INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',);

        Vale = []
        for k in range(0,NB_FREQ):
           freqk=k*self.df
           Vale.append(freqk)
           Vale.append(NP.real(FSIST[k,indice-1]).tolist())
           Vale.append(NP.imag(FSIST[k,indice-1]).tolist())

        __fH=DEFI_FONCTION(NOM_PARA='FREQ', VALE_C=Vale,
                    PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',);

        __fT0 = CALC_FONCTION(MULT=(
                                    _F(FONCTION=fdepl,),
                                    _F(FONCTION=__fH,),
                                    _F(FONCTION=__FILTRE,)
                                    ),
                              LIST_PARA=__lfreq, NOM_PARA='FREQ',
                              PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                         )
                         
        __fTT = CALC_FONCTION( FFT=_F(FONCTION=__fT0, METHODE='COMPLET',
                            SYME='NON'))

        __fTC = DEFI_CONSTANTE(VALE=__fTT(0))
        
        __fT1 = CALC_FONCTION(COMB=(
                                    _F(FONCTION=__fTT,COEF=1.0,),
                                    _F(FONCTION=__fTC,COEF=-1.0,)
                                    ),
                              LIST_PARA=linst, NOM_PARA='INST',
                              PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                         )

        DETRUIRE(CONCEPT=_F(NOM=(__fH, __fT0, __fTT, __fTC, __lfreq)))
        return __fT1

class ListPost(list):
    """Définit une liste de post-traitement à enchainer"""

    def set_filename_callback(self, callback):
        """Enregistre le callback nommant les fichiers"""
        for post in self:
            post.set_filename_callback(callback)

    def run(self):
        """Lance les post-traitements"""
        for post in self:
            post.run()

def PostMissFactory(type_post, parent, param):
    """create the list of instances of a subclass of PostMiss"""
    post = ListPost()
    if type_post == 'HARM_GENE':
        post.append(PostMissHarm(parent, param))
    elif type_post == 'TRAN_GENE':
        post.append(PostMissTran(parent, param))
    elif type_post == 'TABLE':
        post.append(PostMissTabl(parent, param))
    elif type_post in ('FICHIER', 'TABLE_CONTROL'):
        post.append(PostMissFichier(parent, param))
    elif type_post == 'FICHIER_TEMPS':
        post.append(PostMissFichierTemps(parent, param))
    elif type_post == 'CHARGE':
        post.append(PostMissChar(parent, param))
    else:
        raise NotImplementedError(type_post)
    if type_post == 'TABLE_CONTROL':
        post.append(PostMissControl(parent, param))
    return post

def info_freq(param):
    """Emet un message sur les fréquences utilisées"""
    if param['LIST_FREQ']:
        UTMESS('I', 'MISS0_14', valk=repr(param['LIST_FREQ']), vali=len(param['LIST_FREQ']))
    else:
        nbfmiss = int((param['FREQ_MAX'] - param['FREQ_MIN']) / param['FREQ_PAS']) + 1
        UTMESS('I', 'MISS0_13', valr=(param['FREQ_MIN'], param['FREQ_MAX'],
                                      param['FREQ_PAS']),
                                vali=nbfmiss)
