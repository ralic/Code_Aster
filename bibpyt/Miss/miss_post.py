#@ MODIF miss_post Miss  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
Module permettant le post-traitement d'un calcul MISS3D

Les concepts temporaires de calcul sont nommés "__xxxx" (soit .9000001)
automatiquement supprimés en sortie de la macro.
Les concepts dont on garde une référence dans les résultats sont nommés
avec "_xxxx" (soit _9000002).
"""

import os
import traceback
import os.path as osp

import numpy as NP

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
from Miss.miss_utils import set_debug, _print, _printDBG

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
                                   AMOR_REDUIT=self.param['AMOR_REDUIT'],
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
                                AMOR_REDUIT=self.param['AMOR_REDUIT'],
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
        if self.acce_x:
            _repx = RECU_FONCTION(RESU_GENE=self.dyge_x,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repx.convert('complex') * self.tf_xff + tfc
        if self.acce_y:
            _repy = RECU_FONCTION(RESU_GENE=self.dyge_y,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repy.convert('complex') * self.tf_yff + tfc
        if self.acce_z:
            _repz = RECU_FONCTION(RESU_GENE=self.dyge_z,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repz.convert('complex') * self.tf_zff + tfc

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
        DETRUIRE(CONCEPT=_F(NOM=(_repx, _repy, _repz),),)
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
    else:
        raise NotImplementedError, type_post


def info_freq(param):
    """Emet un message sur les fréquences utilisées"""
    if param['FREQ_MAX']:
        nbfmiss = int((param['FREQ_MAX'] - param['FREQ_MIN']) / param['FREQ_PAS']) + 1
        UTMESS('I', 'MISS0_13', valr=(param['FREQ_MIN'], param['FREQ_MAX'],
                                      param['FREQ_PAS']),
                                vali=nbfmiss)
    else:
        UTMESS('I', 'MISS0_14', valk=repr(param['LIST_FREQ']), vali=len(param['LIST_FREQ']))
