#@ MODIF miss_post Miss  DATE 01/03/2011   AUTEUR COURTOIS M.COURTOIS 
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
#TODO remplacer les assert/raise par des aster.error(id_message)

import os
import traceback
import os.path as osp

import numpy as NP

import aster
from Cata.cata import (
    _F, DETRUIRE, DEFI_FICHIER,
    NUME_DDL_GENE, PROJ_MATR_BASE, COMB_MATR_ASSE,
    LIRE_IMPE_MISS, LIRE_FORC_MISS,
    DYNA_LINE_HARM, REST_SPEC_TEMP,
    DEFI_LIST_REEL, CALC_FONCTION, RECU_FONCTION, DEFI_FONCTION,
    FORMULE, CALC_FONC_INTERP, CREA_TABLE,
)

from Utilitai.Table import Table
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
        if self.verbose:
            _print('Paramètres du calcul', self.param)
        if self.debug:
            set_debug(True)


    def argument(self):
        """Vérification des arguments d'entrée."""
        # fréquences du calcul Miss
        self.fmiss_min = self.param['FREQ_MIN']
        self.fmiss_max = self.param['FREQ_MAX']
        self.fmiss_df = self.param['FREQ_PAS']
        self.fmiss_nb = int((self.fmiss_max - self.fmiss_min) / self.fmiss_df) + 1
        _printDBG("Plage de fréquence du calcul Miss : [%.2f, %.2f] par " \
                  "pas de %.2f Hz, soit %d fréquences." \
                  % (self.fmiss_min, self.fmiss_max, self.fmiss_df, self.fmiss_nb))


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


    def initco(self):
        """Deux fonctions :
        - initialiser les attributs de stockage des concepts communs,
        - libèrer les références avant la sortie de la macro pour destruction
          propre.
        """
        self.nddlgen = None
        self.rigigen = None
        self.massgen = None
        if len(self._to_delete) > 0:
            DETRUIRE(CONCEPT=_F(NOM=tuple(self._to_delete),),)


    def check_datafile_exist(self):
        """Vérifie l'existence des fichiers."""
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_IMPE'])
        assert osp.exists('fort.%s' % self.param['UNITE_RESU_FORC'])



class POST_MISS_TRAN(POST_MISS):
    """Post-traitement de type 1, sortie harm_gene"""

    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_TRAN, self).argument()
        self.fdlh_coup = 1.e300   #XXX calcul des fréquences pour DYNA_LINE_HARM
        self.fdlh_min = 0.
        self.fdlh_max = self.fmiss_max
        self.fdlh_df = self.fmiss_df
        self.fdlh_nb = int(self.fdlh_max / self.fdlh_df) + 1
        #XXX max=1 dans le capy
        self.dir_x = self.param['ACCE_X'] is not None
        self.dir_y = self.param['ACCE_Y'] is not None
        self.dir_z = self.param['ACCE_Z'] is not None
        # s'assurer que les unités logiques sont libérées (rewind)
        self.check_datafile_exist()
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_IMPE'])
        DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_RESU_FORC'])


    def execute(self):
        """Lance le post-traitement"""
        self.concepts_communs()
        self.boucle_dlh()


    def sortie(self):
        """Prépare et produit les concepts de sortie."""
        self.parent.DeclareOut('resugene', self.parent.sd)
        self._to_delete.append(self.dyge)
        resugene = REST_SPEC_TEMP(RESU_GENE = self.dyge,
                                  METHODE = 'PROL_ZERO',
                                  SYMETRIE = 'NON',
                                  TOUT_CHAM = 'OUI')
        self.initco()


    def concepts_communs(self):
        """Construction des concepts partagés entre
        les différentes étapes du post-traitement"""
        super(POST_MISS_TRAN, self).concepts_communs()
        # fft
        if self.dir_x:
            _xff = CALC_FONCTION(FFT=_F(FONCTION=self.param['ACCE_X'], METHODE='COMPLET',),)
            self.xff = _xff
            self._to_delete.append(_xff)
        if self.dir_y:
            _yff = CALC_FONCTION(FFT=_F(FONCTION=self.param['ACCE_Y'], METHODE='COMPLET',),)
            self.yff = _yff
            self._to_delete.append(_yff)
        if self.dir_z:
            _zff = CALC_FONCTION(FFT=_F(FONCTION=self.param['ACCE_Z'], METHODE='COMPLET',),)
            self.zff = _zff
            self._to_delete.append(_zff)


    def initco(self):
        """Ajoute les concepts"""
        super(POST_MISS_TRAN, self).initco()
        self.dyge = self.xff = self.yff = self.zff = None


    def boucle_dlh(self):
        """Exécution des DYNA_LINE_HARM"""
        for ifreq in range(self.fdlh_nb):
            freq = self.fdlh_min + ifreq * self.fdlh_df
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
            if ifreq > 0:
                opts = { 'RESULTAT' : self.dyge, 'reuse' : self.dyge }
            self.dyge = self.iteration_dlh(_rito, freq, opts)

            DETRUIRE(CONCEPT=_F(NOM=__impe,),)
            self._to_delete.append(_rito)


    def iteration_dlh(self, rigtot, freq, opts):
        """Calculs à une fréquence donnée."""
        excit = []
        if self.dir_x:
            __fosx = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    NOM_CMP='DX',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            if freq > self.fdlh_coup:
                excit.append(_F(VECT_ASSE = __fosx,
                                COEF_MULT_C = 0. + 0j))
            else:
                excit.append(_F(VECT_ASSE = __fosx,
                                FONC_MULT_C = self.xff))
        if self.dir_y:
            __fosy = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    NOM_CMP='DY',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            if freq > self.fdlh_coup:
                excit.append(_F(VECT_ASSE = __fosy,
                                COEF_MULT_C = 0. + 0j))
            else:
                excit.append(_F(VECT_ASSE = __fosy,
                                FONC_MULT_C = self.yff))
        if self.dir_z:
            __fosz = LIRE_FORC_MISS(BASE=self.param['BASE_MODALE'],
                                    NUME_DDL_GENE=self.nddlgen,
                                    NOM_CMP='DZ',
                                    NOM_CHAM='ACCE',
                                    UNITE_RESU_FORC=self.param['UNITE_RESU_FORC'],
                                    FREQ_EXTR=freq,)
            if freq > self.fdlh_coup:
                excit.append(_F(VECT_ASSE = __fosz,
                                COEF_MULT_C = 0. + 0j))
            else:
                excit.append(_F(VECT_ASSE = __fosz,
                                FONC_MULT_C = self.zff))
        dyge = self.dyna_line_harm(MODELE=self.param['MODELE'],
                                   MATR_MASS=self.massgen,
                                   MATR_RIGI=rigtot, 
                                   FREQ=freq,
                                   AMOR_REDUIT=self.param['AMOR_REDUIT'],
                                   EXCIT=excit,
                                   **opts)
        DETRUIRE(CONCEPT=_F(NOM=(__fosx, __fosy, __fosz),),)
        return dyge


    def dyna_line_harm(self, **kwargs):
        """Execution de DYNA_LINE_HARM. Produit un concept temporaire."""
        __dyge = DYNA_LINE_HARM(**kwargs)
        return __dyge



class POST_MISS_HARM(POST_MISS_TRAN):
    """Post-traitement de type 1, sortie harm_gene"""
    #XXX fréquence max, pas...
    
    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_HARM, self).argument()
        self.parent.DeclareOut('trangene', self.parent.sd)


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
        # pour la construction de la table
        self._tkeys = ('NUME_CAS', 'GROUP_NO', 'NOM_CHAM', 'NOM_PARA')
        self._torder = list(self._tkeys) + ['FONC_X', 'FONC_Y', 'FONC_Z']
        # pour stocker la correspondance clé_primaire : numéro_ligne
        self._tline = {}


    def argument(self):
        """Vérification des arguments d'entrée."""
        super(POST_MISS_TAB, self).argument()
        # nb calcul
        self.nbcalc = len(self.param['INST_FIN'])
        self.dir_x = self.param['ACCE_X'] is not None
        self.dir_y = self.param['ACCE_Y'] is not None
        self.dir_z = self.param['ACCE_Z'] is not None
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
        self.boucle_sur_transitoires()


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
        """Exécution des DYNA_LINE_HARM dans les 3 directions"""
        for ifreq in range(self.fmiss_nb):
            freq = self.fmiss_min + ifreq * self.fmiss_df
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
            if self.dir_x:
                if ifreq > 0:
                    opts = { 'RESULTAT' : self.dyge_x, 'reuse' : self.dyge_x }
                self.dyge_x = self.iteration_dlh('DX', _rito, freq, opts)
            
            if self.dir_y:
                if ifreq > 0:
                    opts = { 'RESULTAT' : self.dyge_y, 'reuse' : self.dyge_y }
                self.dyge_y = self.iteration_dlh('DY', _rito, freq, opts)
            
            if self.dir_z:
                if ifreq > 0:
                    opts = { 'RESULTAT' : self.dyge_z, 'reuse' : self.dyge_z }
                self.dyge_z = self.iteration_dlh('DZ', _rito, freq, opts)
            
            DETRUIRE(CONCEPT=_F(NOM=__impe,),)
            self._to_delete.append(_rito)


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


    def boucle_sur_transitoires(self):
        """Boucle sur les calculs transitoires"""
        acce_x = self.param['ACCE_X'] or [None,]*self.nbcalc
        acce_y = self.param['ACCE_Y'] or [None,]*self.nbcalc
        acce_z = self.param['ACCE_Z'] or [None,]*self.nbcalc
        vars = zip(acce_x, acce_y, acce_z, self.param['INST_FIN'], self.param['PAS_INST'])
        
        ical = -1
        for acx, acy, acz, tfin, dt in vars:
            ical += 1
            _printDBG("Calcul #%d" % (ical + 1))
            fmax = 0.5 / dt
            df = 1. / tfin
            tfm2 = tfin - dt
            lfreq = NP.arange(0., fmax + df, df)
            __linst = DEFI_LIST_REEL(DEBUT=0.,
                                     INTERVALLE=_F(JUSQU_A=tfm2, PAS=dt),)
            _xff = _yff = _zff = None
            if acx:
                __acx = CALC_FONCTION(COMB=_F(FONCTION=acx, COEF=1.0,),
                                      LIST_PARA=__linst)
                _xff = CALC_FONCTION(FFT=_F(FONCTION=__acx, METHODE='COMPLET',),)
            if acy:
                __acy = CALC_FONCTION(COMB=_F(FONCTION=acy, COEF=1.0,),
                                      LIST_PARA=__linst)
                _yff = CALC_FONCTION(FFT=_F(FONCTION=__acy, METHODE='COMPLET',),)
            if acz:
                __acz = CALC_FONCTION(COMB=_F(FONCTION=acz, COEF=1.0,),
                                      LIST_PARA=__linst)
                _zff = CALC_FONCTION(FFT=_F(FONCTION=__acz, METHODE='COMPLET',),)
            # stockage des accéléro et leur fft
            self.add_line(ical, '', 'ACCE', 'INST',
                          FONC_X=acx.nom, FONC_Y=acy.nom, FONC_Z=acz.nom)
            self.add_line(ical, '', 'ACCE', 'FREQ',
                          FONC_X=_xff.nom, FONC_Y=_yff.nom, FONC_Z=_zff.nom)

            for gno in self.param['GROUP_NO']:
                for cham in ('DEPL', 'VITE', 'ACCE'):
                    if acx:
                        self.gen_funct(ical, gno, cham, 'DX', _xff, _yff, _zff, lfreq)
                    if acy:
                        self.gen_funct(ical, gno, cham, 'DY', _xff, _yff, _zff, lfreq)
                    if acz:
                        self.gen_funct(ical, gno, cham, 'DZ', _xff, _yff, _zff, lfreq)

            DETRUIRE(CONCEPT=_F(NOM=(__linst, __acx, __acy, __acz),),)


    def gen_funct(self, ical, gno, cham, dir, xff, yff, zff, lfreq):
        """Calcul la réponse en un noeud particulier dans la direction 'dir'
        sur les fréquences 'lfreq'."""
        _printDBG("Calcul #%d de la réponse en %s, direction %s" % (ical + 1, gno, dir))
        tfc = 0.
        if self.dir_x:
            _repx = RECU_FONCTION(RESU_GENE=self.dyge_x,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repx.convert('complex') * xff.convert('complex') + tfc
        if self.dir_y:
            _repy = RECU_FONCTION(RESU_GENE=self.dyge_y,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repy.convert('complex') * yff.convert('complex') + tfc
        if self.dir_z:
            _repz = RECU_FONCTION(RESU_GENE=self.dyge_z,
                                   NOM_CHAM=cham,
                                   NOM_CMP=dir,
                                   GROUP_NO=gno,
                                   INTERPOL='LIN',
                                   PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)
            tfc = _repz.convert('complex') * zff.convert('complex') + tfc

        tffr = tfc.evalfonc(lfreq)
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
        self.add_line(ical, gno, cham, 'INST', **{ FKEY[dir] : _reptemp.nom })
        self.add_line(ical, gno, cham, 'FREQ', **{ FKEY[dir] : _repfreq.nom })
        self.add_line(ical, gno, cham, 'SPEC_OSCI', **{ FKEY[dir] : _spec.nom })


    def add_line(self, nume_cas, gno, cham, para, **kwargs):
        """Pour simplifier l'ajout d'une ligne dans la table.
        Arguments optionnels supportés : fonc_x, fonc_y, fonc_z."""
        primkey = (nume_cas + 1, gno, cham, para)
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

