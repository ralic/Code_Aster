#@ MODIF miss_post Miss  DATE 17/04/2012   AUTEUR GREFFET N.GREFFET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE  YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION  EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY  WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM  IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# RESPONSABLE COURTOIS M.COURTOIS

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
    FORMULE, CALC_FONC_INTERP, CREA_TABLE,
)

from Utilitai.Table import Table
from Utilitai.Utmess import UTMESS
from Utilitai.utils import set_debug, _print, _printDBG


# correspondance
FKEY = {
    'DX' : 'FONC_X',
    'DY' : 'FONC_Y',
    'DZ' : 'FONC_Z',
}


class POST_MISS(object):
    """Définition d'un post-traitement MISS3D."""

    def __init__(self, parent, parameters):
        """Initialisations"""
        self.parent = parent
        self.param = parameters
        self.verbose = parameters['INFO'] >= 2
        self.debug = self.verbose
        # liste de concepts intermédiaires à supprimer à la fin
        self._to_delete = []
        self.initco()
        if self.debug:
            set_debug(True)
        self.list_freq_DLH = None
        self.methode_fft = None


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


    def execute(self):
        """Lance le post-traitement"""
        raise NotImplementedError('must be defined in a derivated class')


    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        raise NotImplementedError('must be defined in a derivated class')


    def concepts_communs(self):
        """Construction des concepts partagés entre
        les différentes étapes du post-traitement"""
        __nddlg = NUME_DDL_GENE(BASE=self.param['BASE_MODALE'],
                                STOCKAGE= 'PLEIN')
        self.nddlgen = __nddlg
        __rigigen = PROJ_MATR_BASE(BASE=self.param['BASE_MODALE'],
                                   NUME_DDL_GENE=self.nddlgen,
                                   MATR_ASSE=self.param['MATR_RIGI'])
        self.rigigen = __rigigen
        __massgen = PROJ_MATR_BASE(BASE=self.param['BASE_MODALE'],
                                   NUME_DDL_GENE=self.nddlgen,
                                   MATR_ASSE=self.param['MATR_MASS'])
        self.massgen = __massgen
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
        self.acce_x = self.acce_y = self.acce_z = None
        self.xff = self.yff = self.zff = None
        if len(self._to_delete) > 0:
            DETRUIRE(CONCEPT=_F(NOM=tuple(self._to_delete),),)


    def check_datafile_exist(self):
        """Vérifie l'existence des fichiers."""
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_IMPE'])
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_FORC'])



class POST_MISS_TRAN(POST_MISS):
    """Post-traitement de type 1, sortie tran_gene"""

    def __init__(self, *args, **kwargs):
        """Initialisation."""
        super(POST_MISS_TRAN, self).__init__(*args, **kwargs)
        self.methode_fft = 'PROL_ZERO'
        self._suppr_matr = True
        self.excit_harmo = []


    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_TRAN, self).argument()
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
        #XXX on pensait qu'il fallait utiliser PROL_ZERO mais miss01a
        #    devient alors NOOK. A clarifier/vérifier en passant
        #    d'autres tests avec ce post-traitement.
        resugene = REST_SPEC_TEMP(RESU_GENE = self.dyge,
                                  METHODE = 'TRONCATURE',
                                  SYMETRIE = 'NON',
                                  TOUT_CHAM = 'OUI')
        self.initco()


    def initco(self):
        """Ajoute les concepts"""
        super(POST_MISS_TRAN, self).initco()
        self.dyge = None


    def boucle_dlh(self):
        """Exécution des DYNA_LINE_HARM"""
        first = True
        for freq in self.list_freq_DLH:
            opts = {}
            _printDBG("Calcul pour la fréquence %.2f Hz" % freq)
            __impe = LIRE_IMPE_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
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
                                    NOM_CMP='DX',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE = __fosx,
                            FONC_MULT_C = self.xff))
        if self.acce_y:
            __fosy = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    NOM_CMP='DY',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE = __fosy,
                            FONC_MULT_C = self.yff))
        if self.acce_z:
            __fosz = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    NOM_CMP='DZ',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            excit.append(_F(VECT_ASSE = __fosz,
                            FONC_MULT_C = self.zff))
        if len(self.excit_harmo) > 0:
            excit.extend( self.excit_harmo )
        dyge = self.dyna_line_harm(MODELE=self.param['MODELE'],
                                   MATR_MASS=self.massgen,
                                   MATR_RIGI=rigtot,
                                   FREQ=freq,
                                   AMOR_MODAL=_F(
                                   AMOR_REDUIT=self.param['AMOR_REDUIT'],),
                                   EXCIT=excit,
                                   **opts)
        #DETRUIRE(CONCEPT=_F(NOM=(__fosx, __fosy, __fosz),),)
        return dyge


    def dyna_line_harm(self, **kwargs):
        """Execution de DYNA_LINE_HARM. Produit un concept temporaire."""
        __dyge = DYNA_LINE_HARM(**kwargs)
        return __dyge



class POST_MISS_HARM(POST_MISS_TRAN):
    """Post-traitement de type 1, sortie harm_gene"""

    def __init__(self, *args, **kwargs):
        """Initialisation."""
        super(POST_MISS_HARM, self).__init__(*args, **kwargs)
        self.methode_fft = 'COMPLET'
        self._suppr_matr = False


    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_HARM, self).argument()
        self.parent.DeclareOut('trangene', self.parent.sd)
        self.suppr_acce_fft()


    def concepts_communs(self):
        """Construction des concepts spécifiques au cas HARMO."""
        super(POST_MISS_HARM, self).concepts_communs()
        for excit_i in self.excit_kw:
            dExc = excit_i.cree_dict_valeurs(excit_i.mc_liste)
            for mc in dExc.keys():
                if dExc[mc] is None:
                    del dExc[mc]
            if dExc.get('VECT_ASSE') is not None:
                __excit = PROJ_VECT_BASE(BASE=self.param['BASE_MODALE'],
                                         NUME_DDL_GENE=self.nddlgen,
                                         VECT_ASSE=dExc['VECT_ASSE'])
                dExc['VECT_ASSE'] = __excit
            self.excit_harmo.append(dExc)


    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.initco()


    def dyna_line_harm(self, **kwargs):
        """Execution de DYNA_LINE_HARM. Produit le concept définitif."""
        trangene = DYNA_LINE_HARM(**kwargs)
        return trangene



class POST_MISS_TAB(POST_MISS):
    """Post-traitement de type 2"""

    def __init__(self, *args, **kwargs):
        """Initialisation."""
        super(POST_MISS_TAB, self).__init__(*args, **kwargs)
        self.methode_fft = 'COMPLET'
        # pour la construction de la table
        self._tkeys = ('GROUP_NO', 'NOM_CHAM', 'NOM_PARA')
        self._torder = list(self._tkeys) + ['FONC_X', 'FONC_Y', 'FONC_Z']
        # pour stocker la correspondance clé_primaire : numéro_ligne
        self._tline = {}


    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_TAB, self).argument()
        # s'assurer que les unités logiques sont libérées (rewind)
        self.check_datafile_exist()
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_IMPE'])
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_FORC'])
        # table de stockage des fonctions résultats
        self.tab = Table()


    def execute(self):
        """Lance le post-traitement"""
        self.concepts_communs()
        self.boucle_dlh()
        self.recombinaison()


    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        from pprint import pformat
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
        super(POST_MISS_TAB, self).initco()
        self.dyge_x = self.dyge_y = self.dyge_z = None


    def boucle_dlh(self):
        """Exécution des DYNA_LINE_HARM dans les 3 directions (chargement unitaire)"""
        first = True
        for freq in self.list_freq_DLH:
            opts = {}
            _printDBG("Calcul pour la fréquence %.2f Hz" % freq)
            __impe = LIRE_IMPE_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
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
                                NOM_CMP=dir,
                                NOM_CHAM='ACCE',
                                UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                FREQ_EXTR=freq,)
        __dyge = DYNA_LINE_HARM(MODELE=self.param['MODELE'],
                                MATR_MASS=self.massgen,
                                MATR_RIGI=rigtot,
                                FREQ=freq,
                                AMOR_MODAL=_F(
                                AMOR_REDUIT=self.param['AMOR_REDUIT'],),
                                EXCIT=_F(VECT_ASSE=__fosx,
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
        if self.param['LIST_FREQ']:
            opts['LIST_FREQ'] = self.param['LIST_FREQ']
        _spec = CALC_FONCTION(SPEC_OSCI=_F(FONCTION=_reptemp,
                                           NORME=self.param['NORME'],
                                           AMOR_REDUIT=self.param['AMOR_SPEC_OSCI'],
                                           **opts),)
        DETRUIRE(CONCEPT=_F(NOM=tuple(to_del),),)
        self.add_line(gno, cham, 'INST', **{ FKEY[dir] : _reptemp.nom })
        self.add_line(gno, cham, 'FREQ', **{ FKEY[dir] : _repfreq.nom })
        self.add_line(gno, cham, 'SPEC_OSCI', **{ FKEY[dir] : _spec.nom })


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



class POST_MISS_FICHIER(POST_MISS):
    """Pas de post-traitement, car on ne sort que les fichiers."""

    def argument(self):
        """Vérification des arguments d'entrée."""
        # fréquences du calcul Miss
        info_freq(self.param)


    def execute(self):
        """Lance le post-traitement"""


    def sortie(self):
        """Prépare et produit les concepts de sortie."""


class POST_MISS_FICHIER_TEMPS(POST_MISS_FICHIER):
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
        self.Mg = 0 
        self.Kg = 0 
        self.Cg = 0 

        self.Z_temps = NP.zeros((self.nrows, self.ncols, self.L_points), float) 
        self.Fs_temps = NP.zeros((self.ncols, self.L_points), float)

        # Noms des fichiers à utiliser
        self._fich_impe = osp.join(self.param['_WRKDIR'], 'impe_Laplace')
        self._fich_forc = osp.join(self.param['_WRKDIR'], 'forc_Laplace')


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
        fid = open(self._fich_impe, 'r') 

        impe_Laplace = NP.zeros((self.nrows, self.ncols, self.nbr_freq), complex) 
        k = -1 
   
        while (k < self.nbr_freq - 1):
            for n in range(0,self.nrows):
                for m in range(0,self.ncols):
                    txt = fid.readline() 
                    if( txt[0:7] == 'MATRICE' ):
                        tmp = fid.readline() 
                        k = k + 1 
                    data = fid.readline().split() 
                    impe_Laplace[n,m,k] = float(data[1]) + 1j*float(data[2]) 

        fid.close() 

        Z_Laplace = NP.zeros((self.nrows, self.ncols, self.L_points), complex) 
        Z_Laplace[:,:,0:self.nbr_freq] = NP.conj(impe_Laplace[:,:,0:self.nbr_freq]) 
        # Z_Laplace[:,:,self.nbr_freq:] = impe_Laplace[:,:,self.nbr_freq-1:1:-1] 
        Z_Laplace[:,:,self.nbr_freq:] = impe_Laplace[:,:,self.nbr_freq-2:0:-1] 

        MATR_GENE = self.param['MATR_GENE']
        if MATR_GENE:
           z_complex = NP.exp(1j*2*pi/self.L_points)  # 'pi' imported from math
           GammaZ = NP.zeros(self.L_points, complex) 
           order = 2 
           for k in range(0,self.L_points):
               for m in range(1,order+1):
                   GammaZ[k] = GammaZ[k] + 1./m * (1 - self.rho*z_complex**(-k))**m 
           s_laplace = GammaZ / self.dt 
                    
           if MATR_GENE['MATR_AMOR']:
               self.Cg = MATR_GENE['MATR_AMOR'].EXTR_MATR_GENE() 
    
           if MATR_GENE['MATR_RIGI']:
               self.Kg = MATR_GENE['MATR_RIGI'].EXTR_MATR_GENE()   
               
           if MATR_GENE['DECOMP_IMPE'] == 'PRODUIT':
               if MATR_GENE['MATR_MASS']:
                   self.Mg = MATR_GENE['MATR_MASS'].EXTR_MATR_GENE()
               Hyst = NP.imag(Z_Laplace[:,:,0]) 
               for r in range(0,self.L_points):
                   mat = self.Mg*(s_laplace[r]**2) + self.Cg*s_laplace[r] + self.Kg 
                   mat_inv = NP.linalg.inv(mat) 
                   if MATR_GENE['AMOR_HYST'] == 'DANS_MATR_AMOR':
                       Z_Laplace[:,:,r] = Z_Laplace[:,:,r] - 1j*Hyst*NP.sign(NP.imag(s_laplace[r])) 
                   Z_Laplace[:,:,r] = Z_Laplace[:,:,r]*mat_inv 
           else:
               if MATR_GENE['MATR_MASS'] or MATR_GENE['AMOR_HYST']:
                   UTMESS('A','MISS0_19')
               for r in range(0,self.L_points):
                   Z_Laplace[:,:,r] = Z_Laplace[:,:,r] - self.Cg*s_laplace[r] - self.Kg 
     
        rhoinv = 1./self.rho 
        for r in range(0,self.nrows):
            for l in range(0,self.ncols):
                fact = 1 
                x_lapl = Z_Laplace[r,l,:] 
                x_t = NP.real(ifft(x_lapl)) 
                for k in range(0,len(x_t)):
                    x_t[k] = fact * x_t[k]  # x_t[k] = rho**(-k) * x_t[k]
                    fact = fact*rhoinv 
                self.Z_temps[r,l,:] = x_t 
 

    def calc_forc_temps(self):
        """Calcul de l'effort sismique dans le domaine temporel"""
        fid = open(self._fich_forc, 'r') 

        effort_sismique = NP.zeros((3, self.ncols, self.nbr_freq), complex)  # 3 chargements : X, Y, Z
        title = fid.readline() 
        for k in range(0,self.nbr_freq):
            for n in range(0,3):
                for m in range(0,self.ncols):
                    txt = fid.readline() 
                    data = fid.readline().split()  # Output matrix: 1 ligne, 3 colonnes
                    effort_sismique[n,m,k] = float(data[1]) + 1j*float(data[2]) 
                    #effort_sismique[n,m,k] = (float(data[1]) + 1j*float(data[2]))/(-2.*1j*(pi)**0.5) 
            title = fid.readline() 
    
        fid.close() 
    
        fsism1 = NP.zeros((self.nrows, self.L_points), complex)   # Chargement 1
        effort1 = NP.squeeze(effort_sismique[0,:,0:self.nbr_freq]) 
        fsism1[:,0:self.nbr_freq] = NP.conj(effort1[:,0:self.nbr_freq]) 
        fsism1[:,self.nbr_freq:] = effort1[:,self.nbr_freq-1:1:-1] 
    
        fsism2 = NP.zeros((self.nrows, self.L_points), complex)   # Chargement 2
        effort2 = NP.squeeze(effort_sismique[1,:,0:self.nbr_freq]) 
        fsism2[:,0:self.nbr_freq] = NP.conj(effort2[:,0:self.nbr_freq]) 
        fsism2[:,self.nbr_freq:] = effort2[:,self.nbr_freq-1:1:-1] 
    
        fsism3 = NP.zeros((self.nrows, self.L_points), complex)   # Chargement 3
        effort3 = NP.squeeze(effort_sismique[2,:,0:self.nbr_freq]) 
        fsism3[:,0:self.nbr_freq] = NP.conj(effort3[:,0:self.nbr_freq]) 
        fsism3[:,self.nbr_freq:] = effort3[:,self.nbr_freq-1:1:-1] 
    
        F_sism_temps1 = NP.zeros((self.nrows, self.L_points), float) 
        F_sism_temps2 = NP.zeros((self.nrows, self.L_points), float) 
        F_sism_temps3 = NP.zeros((self.nrows, self.L_points), float)  
    
        rhoinv = 1./self.rho 
        for r in range(0,self.nrows):
           fact = 1 
           x_lapl1 = fsism1[r,:] 
           x_t1 = NP.real(ifft(x_lapl1)) 
           x_lapl2 = fsism2[r,:] 
           x_t2 = NP.real(ifft(x_lapl2)) 
           x_lapl3 = fsism3[r,:] 
           x_t3 = NP.real(ifft(x_lapl3)) 
           for k in range(0,len(x_t1)): 
              x_t1[k] = fact * x_t1[k]  # x_t1[k] = rho**(-k) * x_t1[k] 
              x_t2[k] = fact * x_t2[k]  # x_t2[k] = rho**(-k) * x_t2[k] 
              x_t3[k] = fact * x_t3[k]  # x_t3[k] = rho**(-k) * x_t3[k] 
              fact = fact * rhoinv 
           F_sism_temps1[r,:] = x_t1 
           F_sism_temps2[r,:] = x_t2 
           F_sism_temps3[r,:] = x_t3 

            
        # Convolution avec le chargement impose
        deplx = NP.zeros(self.L_points, float) 
        deply = NP.zeros(self.L_points, float) 
        deplz = NP.zeros(self.L_points, float) 
    
        if self.param['EXCIT_SOL']['CHAM_X']:
              deplx = self.calc_depl('CHAM_X') 
             
        if self.param['EXCIT_SOL']['CHAM_Y']:
              deply = self.calc_depl('CHAM_Y') 
    
        if self.param['EXCIT_SOL']['CHAM_Z']:
              deplz = self.calc_depl('CHAM_Z') 
              
        ext = ['1x','1y','1z','2x','2y','2z','3x','3y','3z']
        file = osp.join(self.param['_WRKDIR'], 'filtre_inverse_')
        fname = file + ext[0]

        ## Calcul du nombre de frequences
        fid = open(fname, 'r')
        tmp = fid.readline()
        data = fid.readline().split()
        freq = []
        while data[0] != 'FIN':
            freq.append(data[0])
            data = fid.readline().split()
        fid.close()
        
        ## Lecture filtre inverse
        filler = NP.frompyfunc(lambda x: list(), 1, 1)
        filtre_inverse = filler(NP.empty((len(freq),len(ext)), dtype=NP.object))
        for file_nbr in range(0,len(ext)):
            fname = file + ext[file_nbr]
            fid = open(fname, 'r')
            tmp = fid.readline()
            data = fid.readline().split()
            for k in range(0,len(freq)):
                filtre_inverse[k][file_nbr].append(data[1] + '+' + data[2] + 'j')
                data = fid.readline().split()
            fid.close()
        
        ## FFT des chargements imposes
        #freq = [float(x) for x in freq_txt]
        deplx_freq = fft(deplx,len(freq))
        deply_freq = fft(deply,len(freq))
        deplz_freq = fft(deplz,len(freq))
        depl = NP.array([deplx_freq, deply_freq, deplz_freq])       
        
        ## Calcul des chargements SV, SH, P en frequence
        filtre = NP.zeros((3,3,len(freq)),complex)
        char_SV_SH_P = NP.zeros((3,len(freq)))
        to_stringv = NP.vectorize(lambda st : eval(str(st)[2:len(str(st))-2]))
        for k in range(0,len(freq)):
            filtre[0,:,k] = to_stringv(filtre_inverse[k][0:3])  # Onde SV
            filtre[1,:,k] = to_stringv(filtre_inverse[k][3:6])  # Onde SH
            filtre[2,:,k] = to_stringv(filtre_inverse[k][6:9])  # Onde P
            char_SV_SH_P[:,k] = NP.dot(filtre[:,:,k],depl[:,k])
        
        ## Calcul des evolutions SV, SH, P en temps
        onde_SV = NP.real(ifft(char_SV_SH_P[0,:]))
        onde_SH = NP.real(ifft(char_SV_SH_P[1,:]))
        onde_P = NP.real(ifft(char_SV_SH_P[2,:]))
        
        tmp_SV = list(onde_SV)
        tmp_SH = list(onde_SH)
        tmp_P = list(onde_P)
        tmp_SV.insert(0,0.0)
        tmp_SH.insert(0,0.0)
        tmp_P.insert(0,0.0)
        onde_SV = NP.array(tmp_SV)
        onde_SH = NP.array(tmp_SH)
        onde_P = NP.array(tmp_P)

        ## Calcul de l'effort sismique equivalent en temps                
        for n in range(0,self.L_points):
          for k in range(0,n):
            self.Fs_temps[:,n]=self.Fs_temps[:,n]+onde_SV[n-k]*F_sism_temps1[:,k]
            self.Fs_temps[:,n]=self.Fs_temps[:,n]+onde_SH[n-k]*F_sism_temps2[:,k]+onde_P[n-k]*F_sism_temps3[:,k]
   
    
    def ecri_impe(self, Z_temps):
        """Ecriture des 3 types d'impédance dans les 3 fichiers de sortie"""
        for n in range(0,self.L_points):
           # Symmetric impedance
           Z_temps[:,:,n] = (Z_temps[:,:,n] + Z_temps[:,:,n].transpose())/2. 

        if self.param['MATR_GENE']:
           if self.param['MATR_GENE']['MATR_MASS']:
             Z_temps_M = NP.zeros((self.nrows, self.ncols, self.L_points), float) 
             for n in range(0, self.L_points):
                Z_temps_M[:,:,n] = NP.dot(Z_temps[:,:,n],self.Mg) 
             self.impr_impe(Z_temps_M,self.param['UNITE_RESU_MASS']) 
   
           if self.param['MATR_GENE']['MATR_AMOR']:
             Z_temps_C = NP.zeros((self.nrows, self.ncols, self.L_points), float) 
             for n in range(0, self.L_points):
                Z_temps_C[:,:,n] = NP.dot(Z_temps[:,:,n],self.Cg) 
             self.impr_impe(Z_temps_C,self.param['UNITE_RESU_AMOR'])  
     
           if self.param['MATR_GENE']['MATR_RIGI']:
             Z_temps_K = NP.zeros((self.nrows, self.ncols, self.L_points), float) 
             for n in range(0, self.L_points):
                Z_temps_K[:,:,n] = NP.dot(Z_temps[:,:,n],self.Kg) 
             self.impr_impe(Z_temps_K,self.param['UNITE_RESU_RIGI']) 
        else:
           self.impr_impe(Z_temps,self.param['UNITE_RESU_RIGI']) 


    def impr_impe(self, Zdt, unite_type_impe):
        """Ecriture d'une impédance quelconque dans le fichier de sortie en argument"""
        fname = os.path.join( self.param['_WRKDIR'], self.param['PROJET'] + '.' + str(unite_type_impe) )
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
            for l in range(0,self.nrows):
                for c in range(0,self.ncols,nb_colonne):  
                    txt.append(fmt_ligne % tuple(Zdt[l,c:c+nb_colonne,n]))
        fid.write(os.linesep.join(txt))
        fid.close()
        shutil.copyfile(fname, os.path.join( self.param['_INIDIR'], 'fort' + '.' + str(unite_type_impe) ))


    def ecri_forc(self, fs_temps):
        """Ecriture de l'effort sismique dans le fichier de sortie"""
        fname = os.path.join( self.param['_WRKDIR'], self.param['PROJET'] + '.' + str(self.param['EXCIT_SOL']['UNITE_RESU_FORC']) )
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
        for n in range(0,self.L_points):
            txt.append('%s' % str(n*self.dt))
            for mode in range(0,self.param['NB_MODE'], nb_colonne):
                txt.append(fmt_ligne % tuple(fs_temps[mode:mode+nb_colonne,n]))   
        fid.write(os.linesep.join(txt))
        fid.close()
        shutil.copyfile(fname, os.path.join( self.param['_INIDIR'], 'fort' + '.' + str(self.param['EXCIT_SOL']['UNITE_RESU_FORC'])))


    def cumtrapz(self, a):
        """Integration en temps (accélération -> vitesse -> déplacement)"""
        length = len(a) 
        b = NP.zeros(length, float) 
        for k in range(0, length - 1):
           b[k+1] = NP.add(a[k], a[k+1])/2. 
        c = NP.add.accumulate(b) 
        return c 
        
        
    def calc_depl(self, champ_dir):
        """Lecture du fichier déplacement/vitesse/accélération pour le calcul de l'effort sismique"""
        __linst = DEFI_LIST_REEL(DEBUT=0.,INTERVALLE=_F(JUSQU_A= self.param['INST_FIN'] - self.dt,PAS=self.dt),)
        _champ = CALC_FONCTION(COMB=_F(FONCTION=self.param['EXCIT_SOL'][champ_dir], COEF=1.0,),LIST_PARA=__linst)
        
        linst = __linst.Valeurs()
        char_impo = _champ.Valeurs()[1]
        
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'ACCE'):
                  #if (EXCIT_SOL['CORRECTION'] == 1): # Baseline correction
                  #      t = NP.arange(0,N_inst*dt,dt) 
                  #      BL_coeff = NP.polyfit(t,char_impo,2)  # Baseline Estimation coefficients
                  #      Accez_BL = BL_coeff[0]*t**2 + BL_coeff[1]*t + BL_coeff[2] 
                  #      char_impo = char_impo - Accez_BL  # Baseline correction
              velo = self.dt*self.cumtrapz(char_impo) 
              depl = self.dt*self.cumtrapz(velo) 
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'VITE'):
              depl = self.dt*self.cumtrapz(char_impo) 
        if (self.param['EXCIT_SOL']['NOM_CHAM'] == 'DEPL'):
              depl = char_impo 
           
        return depl 
         

def PostMissFactory(type_post, *args, **kwargs):
    """Crée l'objet POST_MISS pour le type de post-traitement demandé."""
    if type_post == 'HARM_GENE':
        return POST_MISS_HARM(*args, **kwargs)
    elif type_post == 'TRAN_GENE':
        return POST_MISS_TRAN(*args, **kwargs)
    elif type_post == 'TABLE':
        return POST_MISS_TAB(*args, **kwargs)
    elif type_post == 'FICHIER':
        return POST_MISS_FICHIER(*args, **kwargs)
    elif type_post == 'FICHIER_TEMPS':
        return POST_MISS_FICHIER_TEMPS(*args, **kwargs)
    else:
        raise NotImplementedError, type_post


def info_freq(param):
    """Emet un message sur les fréquences utilisées"""
    if param['LIST_FREQ']:
        UTMESS('I', 'MISS0_14', valk=repr(param['LIST_FREQ']), vali=len(param['LIST_FREQ']))
    else:
        nbfmiss = int((param['FREQ_MAX'] - param['FREQ_MIN']) / param['FREQ_PAS']) + 1
        UTMESS('I', 'MISS0_13', valr=(param['FREQ_MIN'], param['FREQ_MAX'],
                                      param['FREQ_PAS']),
                                vali=nbfmiss)


