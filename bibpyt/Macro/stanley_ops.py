#@ MODIF stanley_ops Macro  DATE 14/09/2004   AUTEUR MCOURTOI M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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



def stanley_ops(self,RESULTAT,MODELE,CHAM_MATER,CARA_ELEM,**args):

  """
     Importation et lancement de Stanley
  """

  import os,string
  import aster
  from Accas import _F
  from Noyau.N_utils import AsType
  ier=0

  # La macro compte pour 1 dans la numerotation des commandes
  self.icmd=1

  import Stanley
  from Stanley import stanley

  if (RESULTAT and MODELE and CHAM_MATER):
    _MAIL = aster.getvectjev( string.ljust(MODELE.nom,8) + '.MODELE    .NOMA        ' )
    _MAIL = string.strip(_MAIL[0])
    MAILLAGE = self.jdc.g_context[_MAIL]
    if CARA_ELEM:
      stanley.STANLEY(RESULTAT,MAILLAGE,MODELE,CHAM_MATER,CARA_ELEM)
    else:
      stanley.STANLEY(RESULTAT,MAILLAGE,MODELE,CHAM_MATER,None)
  else:

    stanley.PRE_STANLEY()

  return ier
