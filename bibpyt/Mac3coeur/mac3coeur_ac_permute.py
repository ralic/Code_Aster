# coding=utf-8

#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# person_in_charge: romeo.fernandes at edf.fr

import os
import aster
from Cata.cata import *

# ------------------------------------------------------------------------------
def mac3coeur_ac_permute(self, **args):
   """Methode corps de la macro MACRO_AC_PERMUTE"""
   from Accas import _F

   ier = 0
   nompro = 'MACRO_AC_PERMUTE'

   ### On importe les definitions des commandes a utiliser dans la macro
   EXTR_RESU  = self.get_cmd('EXTR_RESU')
   CREA_CHAMP = self.get_cmd('CREA_CHAMP')
   CREA_RESU  = self.get_cmd('CREA_RESU')
   
   POS_INIT  = self['POS_INIT']
   POS_FIN   = self['POS_FIN']
   RESU_INI  = self['RESU_INI']
   RESU_FIN  = self['RESU_FIN']
   INSTANT   = self['INSTANT']
   MA_INI    = self['MAILLAGE_INIT']
   MA_FIN    = self['MAILLAGE_FINAL']
   MO_FIN    = self['MODELE_FINAL']
   VECT      = self['TRAN']
   
   # La macro compte pour 1 dans l'execution des commandes
   self.set_icmd(1)
   
   _tini = RESU_FIN.LIST_PARA()['INST'][-1]
   __CHDEP  = CREA_CHAMP(TYPE_CHAM = 'NOEU_DEPL_R',
                         OPERATION = 'EXTR',
                         PRECISION =  1.0E-10,
                         RESULTAT  =  RESU_FIN,
                         NOM_CHAM  = 'DEPL',
                         INST      =  _tini,);

   __ASSDEP = CREA_CHAMP(TYPE_CHAM = 'NOEU_DEPL_R',
                         MODELE    =  MO_FIN,
                         OPERATION = 'ASSE',
                         ASSE=( _F(TOUT     = 'OUI',
                                   CHAM_GD  =  __CHDEP,
                                   CUMUL    = 'NON',
                                   COEF_R   =  0.0),),);

   __RESU_F = CREA_RESU(OPERATION = 'AFFE',
                        TYPE_RESU = 'EVOL_NOLI',
                        NOM_CHAM  = 'DEPL',
                        AFFE      = _F(CHAM_GD = __ASSDEP,
                                       INST    = 0.0,
                                       MODELE  = MO_FIN,));

   __CHSIE  = CREA_CHAMP(TYPE_CHAM = 'ELGA_SIEF_R',
                         OPERATION = 'EXTR',
                         PRECISION =  1.0E-10,
                         RESULTAT  =  RESU_FIN,
                         NOM_CHAM  = 'SIEF_ELGA',
                         INST      =  _tini,);

   __ASSSIE = CREA_CHAMP(TYPE_CHAM = 'ELGA_SIEF_R',
                       MODELE    =  MO_FIN,
                       OPERATION = 'ASSE',
                       ASSE=( _F(TOUT     = 'OUI',
                                 CHAM_GD  =  __CHSIE,
                                 CUMUL    = 'NON',
                                 COEF_R   =  0.0),),);

   CREA_RESU( reuse = __RESU_F,
              OPERATION = 'AFFE',
              TYPE_RESU = 'EVOL_NOLI',
              NOM_CHAM  = 'SIEF_ELGA',
              AFFE      = _F(CHAM_GD = __ASSSIE,
                             INST    = 0.0,
                             MODELE  = MO_FIN,));

   __CHVAR  = CREA_CHAMP(TYPE_CHAM = 'ELGA_VARI_R',
                         OPERATION = 'EXTR',
                         PRECISION =  1.0E-10,
                         RESULTAT  =  RESU_FIN,
                         NOM_CHAM  = 'VARI_ELGA',
                         INST      =  _tini,);

   __ASSVAR = CREA_CHAMP(TYPE_CHAM = 'ELGA_VARI_R',
                         MODELE    =  MO_FIN,
                         OPERATION = 'ASSE',
                         ASSE=( _F(TOUT     = 'OUI',
                                   CHAM_GD  =  __CHVAR,
                                   CUMUL    = 'NON',
                                   COEF_R   =  0.0),),);

   CREA_RESU( reuse = __RESU_F,
              OPERATION = 'AFFE',
              TYPE_RESU = 'EVOL_NOLI',
              NOM_CHAM  = 'VARI_ELGA',
              AFFE        = _F(CHAM_GD = __ASSVAR,
                             INST    = 0.0,
                             MODELE  = MO_FIN,));

   CREA_RESU( reuse          =  __RESU_F,
              OPERATION      = 'PERM_CHAM',
              TYPE_RESU      = 'EVOL_NOLI',
              RESU_INIT      =  RESU_INI ,
              INST_INIT      =  INSTANT,
              MAILLAGE_INIT  =  MA_INI,
              NOM_CHAM       = 'DEPL',
              RESU_FINAL     =  __RESU_F,
              MAILLAGE_FINAL =  MA_FIN,
              PERM_CHAM      = ( _F( GROUP_MA_INIT  = 'CR_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'CR_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'TG_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'TG_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'ES_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'ES_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'EI_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'EI_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'DI_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'DI_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_B',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_B',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_T',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_T',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_M',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_M',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GT_'+ POS_INIT +'_E',
                                     GROUP_MA_FINAL = 'GT_'+ POS_FIN  +'_E',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GT_'+ POS_INIT +'_M',
                                     GROUP_MA_FINAL = 'GT_'+ POS_FIN  +'_M',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'MNT_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'MNT_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),))

   CREA_RESU( reuse          =  __RESU_F,
              OPERATION      = 'PERM_CHAM',
              TYPE_RESU      = 'EVOL_NOLI',
              RESU_INIT      =  RESU_INI ,
              INST_INIT      =  INSTANT,
              MAILLAGE_INIT  =  MA_INI,
              NOM_CHAM       = 'VARI_ELGA',
              RESU_FINAL     =  __RESU_F,
              MAILLAGE_FINAL =  MA_FIN,
              PERM_CHAM      = ( _F( GROUP_MA_INIT  = 'CR_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'CR_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'TG_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'TG_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'ES_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'ES_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'EI_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'EI_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'DI_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'DI_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_B',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_B',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_T',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_T',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GC_'+ POS_INIT +'_M',
                                     GROUP_MA_FINAL = 'GC_'+ POS_FIN  +'_M',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GT_'+ POS_INIT +'_E',
                                     GROUP_MA_FINAL = 'GT_'+ POS_FIN  +'_E',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'GT_'+ POS_INIT +'_M',
                                     GROUP_MA_FINAL = 'GT_'+ POS_FIN  +'_M',
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),
                                 _F( GROUP_MA_INIT  = 'MNT_'+ POS_INIT,
                                     GROUP_MA_FINAL = 'MNT_'+ POS_FIN,
                                     TRAN           = VECT,
                                     PRECISION      = 1.E-10),))

   self.DeclareOut('sortie', self.sd)
   sortie = EXTR_RESU(RESULTAT=__RESU_F,ARCHIVAGE=_F(INST=0.0),)
   
   return ier

MACRO_AC_PERMUTE=MACRO(nom="MACRO_AC_PERMUTE",
                       op=mac3coeur_ac_permute,
                       sd_prod=evol_noli,
                       fr="PERMUTATION DES ASSEMBLAGES",
                POS_INIT       = SIMP(statut='o',typ='TXM',),
                POS_FIN        = SIMP(statut='o',typ='TXM',),
                RESU_INI       = SIMP(statut='o',typ=evol_noli),
                RESU_FIN       = SIMP(statut='o',typ=evol_noli),
                INSTANT        = SIMP(statut='o',typ='R',validators=NoRepeat(),max=1),
                MAILLAGE_INIT  = SIMP(statut='o',typ=maillage_sdaster,),
                MAILLAGE_FINAL = SIMP(statut='o',typ=maillage_sdaster,),
                MODELE_FINAL   = SIMP(statut='o',typ=modele_sdaster),
                TRAN           = SIMP(statut='o',typ='R',min=3,max=3),
                );
