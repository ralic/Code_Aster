#@ MODIF macro_rota_globale Outils  DATE 06/09/2004   AUTEUR MCOURTOI M.COURTOIS 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
from Utilitai.extract import EXTRACT

# ===========================================================================
#           CORPS DE LA MACRO "MACR_ROTA_GLOBALE"
#           -------------------------------------
# USAGE :
#  RESULTAT : resultat
#  GROUP_NO_ORIG : un groupe d un noeud definissant l entree du coude (A)
#  GROUP_NO_EXTR : un groupe d un noeud definissant la sortie du coude (B)
# 
#              /
#           A /
#            (______
#            B
#  
# ===========================================================================
# script PYTHON : rotation globale sur une tuyauterie


def macr_rota_globale_ops(self,RESULTAT,GROUP_NO_ORIG,GROUP_NO_EXTR,**args):


  """
     Ecriture de la macro MACR_ROTA_GLOBALE
  """
  import os
  from Accas import _F
  from Noyau.N_utils import AsType
  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  POST_RELEVE_T  = self.get_cmd('POST_RELEVE_T')
  DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
  DEFI_FONCTION  = self.get_cmd('DEFI_FONCTION')

  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  # Le concept sortant (de type fonction) est nomme ROTGD dans 
  # le contexte de la macro

  self.DeclareOut('ROTGD',self.sd)

  # Commandes de la macro
  
  __ROTAB=POST_RELEVE_T(ACTION=_F(INTITULE='__ROTAB',
                                GROUP_NO=GROUP_NO_ORIG,
                                RESULTAT=RESULTAT,
                                NOM_CHAM='DEPL',
                                NOM_CMP=('DRX','DRY','DRZ',),
                                OPERATION='EXTRACTION',),
                                )

  __ROTAC=POST_RELEVE_T(ACTION=_F(INTITULE='__ROTAC',
                                GROUP_NO=GROUP_NO_EXTR,
                                RESULTAT=RESULTAT,
                                NOM_CHAM='DEPL',
                                NOM_CMP=('DRX','DRY','DRZ',),
                                OPERATION='EXTRACTION',),);
  
  __DRXC  = EXTRACT(__ROTAC,'INST','DRX')
  __DRYC  = EXTRACT(__ROTAC,'INST','DRY')
  __DRZC  = EXTRACT(__ROTAC,'INST','DRZ')
  __DRXB  = EXTRACT(__ROTAB,'INST','DRX')
  __DRYB  = EXTRACT(__ROTAB,'INST','DRY')
  __DRZB  = EXTRACT(__ROTAB,'INST','DRZ')
  __DRXBC = __DRXC-__DRXB
  __DRYBC = __DRYC-__DRYB
  __DRZBC = __DRZC-__DRZB
  __ROTG  = __DRXBC*__DRXBC
  __ROTG  = __ROTG+__DRYBC*__DRYBC
  __ROTG  = __ROTG+__DRZBC*__DRZBC
  __ROTG  = __ROTG**(0.5)
  
  __livalr = []
  __livali = []
  for i in range(len(__ROTG)):
    __livalr.append(__ROTG[i,1])
    __livali.append(__DRXC[i,0])

  print __livalr
  print __livali

  __LROTG = DEFI_LIST_REEL(VALE=__livalr)
  __LINST = DEFI_LIST_REEL(VALE=__livali)

  ROTGD = DEFI_FONCTION(NOM_PARA='INST',
                        VALE_PARA=__LINST,
                        VALE_FONC=__LROTG,
                       )

  return ier



# ===========================================================================
#           CATALOGUE DE LA MACRO "MACR_ROTA_GLOBALE"
#           -----------------------------------------
# USAGE :
#  RESULTAT : resultat
#  GROUP_NO_ORIG : un groupe d un noeud definissant l entree du coude (A)
#  GROUP_NO_EXTR : un groupe d un noeud definissant la sortie du coude (B)
# 
#              /
#           A /
#            (______
#            B
#  
# ===========================================================================

# -*- coding: iso-8859-1 -*-


MACR_ROTA_GLOBALE=MACRO(nom="MACR_ROTA_GLOBALE",op=macr_rota_globale_ops,sd_prod=fonction_sdaster,
                       docu="",reentrant='n',
                       fr="calcul de la rotation globale dans un coude.",
         RESULTAT        =SIMP(statut='o',typ=(evol_elas,evol_noli,evol_ther) ),
         GROUP_NO_ORIG   =SIMP(statut='o',typ=grno,max=1),
         GROUP_NO_EXTR   =SIMP(statut='o',typ=grno,max=1),
)  ;



