#@ MODIF test_compor_ops Macro  DATE 01/02/2011   AUTEUR PROIX J-M.PROIX 
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
from Cata.cata import *

#              MACRO "TEST_THERMOPLASTIQUE"
#           ----------------------------

import aster
import numpy as N

def test_compor_ops(self,OPTION,MATER,ALPHA,YOUNG,LIST_MATER,TEMP_INIT,TEMP_FIN,
                             NEWTON,CONVERGENCE,COMP_INCR,D_SIGM_EPSI,C_PRAG,
                             NB_VARI,VARI_TEST,INST_FIN,**args):
 # sule l'option "THER", c'est à dire le teste thermomecanique est programme a ce jour
 # ajouter l'option MECA (tests comp001,002), l'option HYDR, etc..
  from Accas import _F
  
  ier=0
  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)
  
  # Le concept sortant (de type fonction) est nomme U dans 
  # le contexte de la macro

  self.DeclareOut('U',self.sd)
  
  # On importe les definitions des commandes a utiliser dans la macro
  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
  SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
  TEST_TABLE     = self.get_cmd('TEST_TABLE')
  DETRUIRE       = self.get_cmd('DETRUIRE')
  DEFI_MATERIAU  = self.get_cmd('DEFI_MATERIAU')
  DEFI_CONSTANTE = self.get_cmd('DEFI_CONSTANTE')
  
  epsi = 1.E-10
  
  NCAL=len(LIST_MATER)
  
  _LINST0=DEFI_LIST_REEL(DEBUT=0.,
                      INTERVALLE=(_F(JUSQU_A=INST_FIN,
                                     NOMBRE=NCAL,),
                                     ),
                      );

  _LINST=DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=_LINST0,
                                     METHODE='MANUEL',
                                    # PAS_MINI=1.0E-12
                                     ),
                         ECHEC=_F(SUBD_PAS=10),
                       # ADAPTATION=_F(EVENEMENT='SEUIL'),
                        );
  _TIMP=DEFI_FONCTION(NOM_PARA='INST',  NOM_RESU='TEMP',
                     VALE=(  0. , TEMP_INIT, INST_FIN , TEMP_FIN)
                     )

  _zero=DEFI_CONSTANTE(VALE=0.)

  motscles={}
  motscles['COMP_INCR']   = COMP_INCR.List_F()
  motscles['CONVERGENCE'] = CONVERGENCE.List_F()  
  motscles['NEWTON']      = NEWTON.List_F()
  
  _U=SIMU_POINT_MAT(MATER=MATER,
                   SUPPORT='ELEMENT',
                     AFFE_VARC=(
                     _F(  NOM_VARC='TEMP',
                          VALE_FONC=_TIMP,
                          VALE_REF=TEMP_INIT),
                          ),
                   INCREMENT=_F(LIST_INST=_LINST,),
                   EPSI_IMPOSE=_F(EPXX=_zero,),
                   **motscles);

  SXM = 0.
  EXM = 0.
  time = 0.

  RESU   = [None]*(NCAL)
  if (NB_VARI > 0):
     Vim    = N.zeros(NB_VARI)

  for i in range(NCAL):
          
      timem = time 
      
      time = timem + INST_FIN/NCAL
 
      Ti = TEMP_INIT + time/INST_FIN * (TEMP_FIN - TEMP_INIT)
 
      Tm = TEMP_INIT + timem/INST_FIN * (TEMP_FIN - TEMP_INIT)
 
      # deformation mecanique imposee correspondant a la deformation thermique du premier calcul
 
      _epsimp =DEFI_CONSTANTE(VALE=-ALPHA(Ti)*(Ti - TEMP_INIT));

      # variation des coef du COMPORtement avec la temperature
      # correction eventuelle des valeurs initiales du temps ti
 
      if i > 0 :
      
         SXM = SXM *(YOUNG(Ti)/YOUNG(Tm))
         # cas particuliers
         if COMP_INCR.List_F()[0]['RELATION'] == 'VMIS_CINE_LINE' :
             Vim[0:5] = Vim[0:5]*D_SIGM_EPSI(Ti)/D_SIGM_EPSI(Tm)
         if COMP_INCR.List_F()[0]['RELATION']== 'VMIS_ECMI_LINE' :
             Vim[2:7] = Vim[2:7]*C_PRAG(Ti)/C_PRAG(Tm)
         if COMP_INCR.List_F()[0]['RELATION']== 'VMIS_ECMI_TRAC' :
             Vim[2:7] = Vim[2:7]*C_PRAG(Ti)/C_PRAG(Tm)
             

      _list0 = DEFI_LIST_REEL(DEBUT=timem,
                  INTERVALLE=(_F(JUSQU_A=time,NOMBRE=1,),),);
                  
      _list=DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=_list0,
                                     METHODE='MANUEL',
                                    # PAS_MINI=1.0E-12
                                     ),
                         ECHEC=_F(SUBD_PAS=10),
                       # ADAPTATION=_F(EVENEMENT='SEUIL'),
                        );
      if (NB_VARI > 0):
          RESU[i]=SIMU_POINT_MAT(
                             MATER=LIST_MATER[i],
                             SUPPORT='ELEMENT',
                             INCREMENT=_F(LIST_INST = _list, ),
                             EPSI_IMPOSE=_F(EPXX=_epsimp),
                             SIGM_INIT=_F(SIXX=SXM),
                             VARI_INIT=_F(VALE=[Vim[j] for j in range(NB_VARI)]),
                             EPSI_INIT=_F(EPXX=EXM, EPYY=0.,EPZZ=0.,EPXY=0.,EPXZ=0.,EPYZ=0.),
                             **motscles);
 
      else :
          RESU[i]=SIMU_POINT_MAT(
                             MATER=LIST_MATER[i],
                             SUPPORT='ELEMENT',
                             INCREMENT=_F(LIST_INST = _list, ),
                             EPSI_IMPOSE=_F(EPXX=_epsimp),
                             SIGM_INIT=_F(SIXX=SXM),
                             EPSI_INIT=_F(EPXX=EXM, EPYY=0.,EPZZ=0.,EPXY=0.,EPXZ=0.,EPYZ=0.),
                             **motscles);
 
      # recuperation des valeurs initiales du futur pas de temps dans la table resultat
 
      EXM = RESU[i]['EPXX',1]
 
      SXM = RESU[i]['SIXX',1]

      if (NB_VARI > 0):
         for j in range(NB_VARI):
            Vim[j] = RESU[i]['V'+str(j+1),1]
 
      DETRUIRE ( CONCEPT =  _F (NOM =_epsimp),INFO=1);
      DETRUIRE ( CONCEPT =  _F (NOM =_list),INFO=1);
 
      TEST_TABLE(TABLE=RESU[i],
                 NOM_PARA='VMIS',VALE=_U['VMIS',i+1],
                 FILTRE=_F(NOM_PARA='INST',VALE=time),
                 REFERENCE='AUTRE_ASTER',);

      TEST_TABLE(TABLE=RESU[i],
                 NOM_PARA='TRACE',VALE=_U['TRACE',i+1],
                 FILTRE=_F(NOM_PARA='INST',VALE=time),
                 REFERENCE='AUTRE_ASTER',);
      if (NB_VARI > 0):
         if VARI_TEST <> None  :
            for j in range(len(VARI_TEST)):
               nomvari=VARI_TEST[j]
               if abs(_U[nomvari,i+1]) > epsi :
                  TEST_TABLE(TABLE=RESU[i],
                     NOM_PARA=nomvari,VALE=_U[nomvari,i+1],
                     FILTRE=_F(NOM_PARA='INST',VALE=time),
                     REFERENCE='AUTRE_ASTER',);
         else  :
            for j in range(NB_VARI):
               nomvari='V'+str(j+1)
               if abs(_U[nomvari,i+1]) > epsi :
                  TEST_TABLE(TABLE=RESU[i],
                     NOM_PARA=nomvari,VALE=_U[nomvari,i+1],
                     FILTRE=_F(NOM_PARA='INST',VALE=time),
                     REFERENCE='AUTRE_ASTER',);
  for i in range(NCAL):
      DETRUIRE ( CONCEPT =  _F (NOM =RESU[i]),INFO=1)
      
  return ier

