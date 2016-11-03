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

import aster_core
import aster
from numpy import linalg
import numpy as NP
from math import pi, sqrt
import os
from Utilitai.Utmess import  UTMESS
from Cata.cata import ( _F, DETRUIRE, LIRE_IMPE_MISS, LIRE_FORC_MISS, 
                        CREA_CHAMP, COMB_MATR_ASSE, DYNA_VIBRA)

def calc_miss_vari(self):
    """Compute SSI analysis with spatial variability"""

    from Utilitai.signal_correlation_utils import (CALC_COHE)
    NB_FREQ = 1 + int((self.FMAX - self.FREQ_INIT) / self.FREQ_PAS)
    if self.case == 'TRANS':
        RESU = [None]*len(self.list_NOM_CMP)
        list_NOM_CMP = self.list_NOM_CMP
    else:
        RESU = [None]*1
        list_NOM_CMP = [self.NOM_CMP]
    self.NOM_CMP = None

# BOUCLE SUR LES FREQUENCES : Matrice de cohérence
    PVEC= [None]*NB_FREQ
    nbm = [None]*NB_FREQ
    for k in range(NB_FREQ):
        freqk = self.FREQ_INIT + self.FREQ_PAS * k
        if self.INFO == 2:
            aster.affiche('MESSAGE', 'FREQUENCE DE CALCUL: ' + str(freqk)) 
        # COHERENCE  
        COHE = CALC_COHE(freqk*2.*pi, **self.cohe_params)
        # EIGENVALUE DECOMP 
        PVEC[k], nbm[k] = compute_POD(COHE, self.calc_params['PRECISION'], self.INFO)

# RECUPERATION DES MODES MECA (STATIQUES)
    nbmodd = self.mat_gene_params['NBMODD']        
    nbmods = self.mat_gene_params['NBMODS']
    GROUP_NO_INTER =  self.interf_params['GROUP_NO_INTERF']
    
# BOUCLE SUR LES DIRECTIONS
    for i_cmp, nom_cmp in enumerate(list_NOM_CMP):
        dict_modes = compute_mecmode(nom_cmp, GROUP_NO_INTER,
                                     self.mat_gene_params['BASE'], nbmods, nbmodd)
        dict_modes['nbmods'] = nbmods
        dict_modes['nbno'] = self.interf_params['NBNO']
        dict_modes['NOM_CMP'] = nom_cmp
    # BOUCLE SUR LES FREQUENCES
        for k in range(NB_FREQ):
            dict_modes['nbpod'] = nbm[k]
          # CALCUL ISS VARI    
            if self.interf_params['MODE_INTERF'] != 'QUELCONQUE' :
                RESU[i_cmp] = self.compute_freqk( k, RESU[i_cmp], PVEC[k], dict_modes)
            else:  #MODE_INTERF =='QUELCONQUE'
                RESU[i_cmp] = compute_freqk_quelconque(self, k, RESU[i_cmp], PVEC[k], dict_modes)
    return RESU
#--------------------------------------------------------------------------------



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#   ROUTINES
#--------------------------------------------------------------------------------
def compute_POD(COHE, PRECISION, INFO):
      """compute POD"""
     # EIGENVALUE DECOMP
       #nbno = self.interf_params['NBNO']
     # On desactive temporairement les FPE 
      aster_core.matfpe(-1)
      eig, vec = linalg.eig(COHE)
      vec = NP.transpose(vec)   # les vecteurs sont en colonne dans numpy
      aster_core.matfpe(1)
      eig = eig.real
      vec = vec.real
      # on rearrange selon un ordre decroissant
      eig = NP.where(eig < 1.E-10, 0.0, eig)
      order = (NP.argsort(eig)[::-1])
      eig = NP.take(eig, order)
      vec = NP.take(vec, order, 0)
      # Nombre de modes POD a retenir
      etot = NP.sum(eig**2)
      ener = 0.0
      nbme = 0
      nbno = len(eig)
      while nbme < nbno:
         ener = eig[nbme]**2 + ener
         prec = ener / etot
         nbme = nbme+1
         if INFO == 2:
            texte = 'VALEUR PROPRE ' + str(nbme) + ' : ' + str(eig[nbme-1])
            aster.affiche('MESSAGE', texte)
         if prec > PRECISION :
            break
      if INFO == 2:
         aster.affiche('MESSAGE','NOMBRE DE MODES POD RETENUS : ' + str(nbme))
         aster.affiche('MESSAGE','PRECISION (ENERGIE RETENUE) : ' + str(prec))     
      PVEC = NP.zeros((nbme, nbno))
      for k1 in range(nbme):
         PVEC[k1, 0:nbno] = NP.sqrt(eig[k1]) * vec[k1]
      return PVEC, nbme  

#---------------------------------------------------------------------
# RECUPERATION DES MODES MECA (STATIQUES)
#  boucle sur les modes statiques
def compute_mecmode(NOM_CMP, GROUP_NO_INTER, resultat, nbmods, nbmodd):
    dict_modes = {}
    dict_modes['NUME_MODE'] = range(nbmods)
    dict_modes['MCMP'] = []
    dict_modes['som'] = []
    dict_modes['maxm'] = []
    for mods in range(nbmods):
        nmo = nbmodd + mods + 1
        __CHAM = CREA_CHAMP( TYPE_CHAM = 'NOEU_DEPL_R',
             OPERATION = 'EXTR',
             NUME_ORDRE = nmo,
             RESULTAT = resultat,
             NOM_CHAM = 'DEPL');
        MCMP = __CHAM.EXTR_COMP(NOM_CMP,[GROUP_NO_INTER]).valeurs 
        #on recupere la composante COMP (dx,dy,dz) des modes  
        MCMP2 = __CHAM.EXTR_COMP(' ',[GROUP_NO_INTER],0).valeurs
        if mods == 0:
            NCMP2 =__CHAM.EXTR_COMP(' ',[GROUP_NO_INTER], topo=1).comp
            nddi = len(MCMP2)
            dict_modes['NCMP2'] = NCMP2
            dict_modes['nddi'] = nddi
            PHI = NP.zeros((nddi, nbmods))
        PHI[:, mods] = MCMP2
        som = NP.sum(MCMP)
        max1 = NP.max(MCMP)
        min1 = NP.min(MCMP)
        maxm = NP.max([abs(max1), abs(min1)])
        dict_modes['som'].append(som)
        dict_modes['maxm'].append(maxm)
        dict_modes['MCMP'].append(MCMP)
        DETRUIRE(CONCEPT = _F(NOM = (__CHAM)), INFO=1)
    dict_modes['PHI'] = PHI
    return dict_modes


#---------------------------------------------------------------------
def compute_force_vari(self, dict_modes, VEC, *KRS ):
    """compute seismic force with variability"""
    # CALCUL DE XO PAR PROJECTION------------- 
    #  CAS 1: MODES DE CORPS RIGIDE
    if self.interf_params['MODE_INTERF'] == 'CORP_RIGI':
        XO = NP.zeros(dict_modes['nbmods'])
        for mods in range(dict_modes['nbmods']):
            MCMP = dict_modes['MCMP'][mods]
            som = dict_modes['som'][mods]
            maxm = dict_modes['maxm'][mods]
            #modes de translation
            if mods+1 <= 3:
                  if abs(som) < 10.E-6:
                     XO[mods] = 0.0
                  else :
                     fact = 1. / som
                     XO[mods] = fact * abs(NP.inner(MCMP,VEC))
            #modes de rotation
            else:  
                if maxm < 10.E-6:
                    if som < 10.E-6:
                        XO[mods] = 0.0
                    else :
                        UTMESS('F','ALGORITH6_86')
                else :
                    fact = 1. / dict_modes['nbno']
                    XO[mods] = 1./ (maxm**2.) * fact * abs(NP.inner(MCMP, VEC))
        FS = NP.dot(KRS, NP.array(XO))
    # CAS 2: MODES EF
    elif self.interf_params['MODE_INTERF'] == 'TOUT':
        XO = NP.zeros(dict_modes['nbmods'])
        for mods in range(dict_modes['nbmods']):
            MCMP = dict_modes['MCMP'][mods]
            som = dict_modes['som'][mods]
            maxm = dict_modes['maxm'][mods]
            if abs(som) < 10.E-6:
                  if maxm < 10.E-6:
                     XO.append(0.0)
                  else:
                     UTMESS('F','UTILITAI5_89')
            else:
                  fact = 1. / som
                  XO.append(fact * abs(NP.inner(MCMP, VEC)))
        FS = NP.dot(KRS, NP.array(XO))
    return FS
    # CAS 3: QUELCONQUE -> ROUTINE compute_corr_vari

#------------------------------------------------------------------------------
def compute_corr_vari(dict_modes, VEC, KRS, FS0):
        PHI = dict_modes['PHI']
        NCMP2 = dict_modes['NCMP2']
        PHIT = NP.transpose(PHI)
        PPHI = NP.dot(PHIT, PHI)
        U0 = NP.dot(linalg.inv(KRS), FS0)
        XI = NP.dot(PHI, U0)
        XPI = XI
        SI0 = 0.0
        for k1 in range(dict_modes['nbpod']):
            XOe = abs(NP.sum(VEC[k1])) / dict_modes['nbno']
            SI0 = SI0 + XOe**2
        SI = sqrt(SI0)
        for idd in range(0, dict_modes['nddi']):
            if NCMP2[idd][0:2] == dict_modes['NOM_CMP']:
              XPI[idd] = SI * XI[idd]
        QPI = NP.dot(PHIT, XPI)
        U0 = NP.dot(linalg.inv(PPHI), QPI)       
        FS = NP.dot(KRS, U0)
        return FS


def compute_freqk_quelconque(self, k, RESU, VEC, dict_modes):
    """ compute response for freqk - quelconque (trans and spec case)"""
    nbmodt = self.mat_gene_params['NBMODT']   
    nbmodd = self.mat_gene_params['NBMODD']   
    freqk = self.FREQ_INIT + self.FREQ_PAS * k
    __impe = LIRE_IMPE_MISS(
                     BASE = self.mat_gene_params['BASE'],
                     TYPE = self.calc_params['TYPE'],
                     NUME_DDL_GENE = self.mat_gene_params['NUME_DDL'],
                     UNITE_RESU_IMPE = self.calc_params['UNITE_RESU_IMPE'],
                     ISSF = self.calc_params['ISSF'],
                     FREQ_EXTR = freqk,);
    __fosi = LIRE_FORC_MISS(
                      BASE =self.mat_gene_params['BASE'],
                      NUME_DDL_GENE = self.mat_gene_params['NUME_DDL'],
                      NOM_CMP = dict_modes['NOM_CMP'],
                      NOM_CHAM = 'DEPL',
                      UNITE_RESU_FORC = self.calc_params['UNITE_RESU_FORC'],
                      ISSF = self.calc_params['ISSF'],
                      FREQ_EXTR = freqk,);
    __rito = COMB_MATR_ASSE(COMB_C = (
                      _F(MATR_ASSE = __impe, COEF_C = 1.0 + 0.j,),
                      _F(MATR_ASSE = self.mat_gene_params['MATR_RIGI'],
                            COEF_C = 1.0 + 0.j,),),
                      SANS_CMP = 'LAGR', )
  
    MIMPE = __impe.EXTR_MATR_GENE()
    #  extraction de la partie modes interface
    KRS = MIMPE[nbmodd:nbmodt, nbmodd:nbmodt] 
    FSISM = __fosi.EXTR_VECT_GENE_C()  
    FS0 = FSISM[nbmodd:nbmodt][:] 
    FS = compute_corr_vari(dict_modes, VEC, KRS, FS0)
    FSISM[nbmodd:nbmodt][:] = FS
    __fosi.RECU_VECT_GENE_C(FSISM)      
    if self.mat_gene_params['MATR_AMOR'] is not None :
        __dyge = DYNA_VIBRA(
                 TYPE_CALCUL = 'HARM', BASE_CALCUL = 'GENE',
                 MATR_MASS = self.mat_gene_params['MATR_MASS'],
                 MATR_RIGI = __rito,
                 FREQ = freqk,
                 MATR_AMOR = self.mat_gene_params['MATR_AMOR'],
                 EXCIT = _F(VECT_ASSE_GENE = __fosi,
                            COEF_MULT = 1.0,), )
    else :
        __dyge = DYNA_VIBRA(
                 TYPE_CALCUL = 'HARM', BASE_CALCUL = 'GENE',
                 MATR_MASS = self.mat_gene_params['MATR_MASS'],
                 MATR_RIGI = __rito,
                 FREQ = freqk,
                 EXCIT = _F(VECT_ASSE_GENE = __fosi,
                             COEF_MULT = 1.0,),  )
    #  recuperer le vecteur modal depl calcule par dyge
    RS = NP.array(__dyge.sdj.DEPL.get())
    DETRUIRE(CONCEPT = _F(NOM=(__dyge)), INFO=1)
    VECRES = self.append_Vec(RS, k, RESU)
    if k > 0:
        DETRUIRE(CONCEPT = _F(NOM = (__impe, __fosi, __rito)), INFO=1)
    return VECRES
