#@ MODIF macro_miss_3d_ops Macro  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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



def macro_miss_3d_ops(self,UNITE_IMPR_ASTER,UNITE_OPTI_MISS,
                           UNITE_MODELE_SOL,UNITE_RESU_IMPE,
                           PROJET,REPERTOIRE,OPTION,**args):
  """
     Ecriture de la macro MACRO_MISS_3D
  """
  import types
  from Accas import _F

  ier=0
  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  DEFI_FICHIER  =self.get_cmd('DEFI_FICHIER')
  EXEC_LOGICIEL =self.get_cmd('EXEC_LOGICIEL')
  # La macro compte pour 1 dans la numerotation des commandes
  #self.icmd=1
  self.set_icmd(1)

  iunit = DEFI_FICHIER(ACTION='LIBERER',UNITE=UNITE_IMPR_ASTER)

  import aster 
  loc_fic=aster.repout()
  miss3d=loc_fic+'miss3d'

  if OPTION['TOUT']!=None:
      MODUL2='COMPLET'
  elif OPTION['MODULE']=='MISS_IMPE':
      MODUL2='CALC_IMPE'
  elif OPTION['MODULE']=='MISS_EVOL':
      MODUL2='MISS_PTAS'
  elif OPTION['MODULE']=='PRE_MISS':
      MODUL2='GTASTER'

  ETUDE = PROJET
  BASE  = REPERTOIRE
  paste = 'fort.'+str(UNITE_IMPR_ASTER)
  popti = 'fort.'+str(UNITE_OPTI_MISS)
  pdsol = 'fort.'+str(UNITE_MODELE_SOL)
  primp = 'fort.'+str(UNITE_RESU_IMPE)

  EXEC_LOGICIEL(
                LOGICIEL=miss3d,
                ARGUMENT=(_F(NOM_PARA=MODUL2),
                          _F(NOM_PARA=ETUDE),
                          _F(NOM_PARA=BASE),
                          _F(NOM_PARA=paste),
                          _F(NOM_PARA=popti),
                          _F(NOM_PARA=pdsol),
                          _F(NOM_PARA=primp)    ),
                )

  return ier
