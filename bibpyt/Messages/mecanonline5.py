#@ MODIF mecanonline5 Messages  DATE 19/05/2008   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

cata_msg = {


21 : _("""
  -> Critère de convergence est lache !
  -> Risque & Conseil : La valeur de RESI_GLOB_RELA est supérieure à 10-4.
     Cela peut nuire à la qualité de la solution. Vous ne vérifiez pas l'équilibre de 
     manière rigoureuse.
"""),

44 : _("""
Pour la prédiction de type 'DEPL_CALCULE', il faut obligatoirement:
 - ITER_GLOB_MAXI = 0
 - ARRET = 'NON'
"""),

45 : _("""
Il faut préciser un concept EVOL_NOLI en prédiction de type 'DEPL_CALCULE'
"""),

46 : _("""
  -> La définition des paramètres RHO_MIN et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MIN à RHO_EXCL.
  -> Risque & Conseil :
     RHO_MIN ne doit pas etre compris entre -RHO_EXCL et RHO_EXCL

"""),

47 : _("""
  -> La définition des paramètres RHO_MAX et RHO_EXCL est contradictoire.
     On choisit de prendre RHO_MAX à -RHO_EXCL.
  -> Risque & Conseil :
     RHO_MAX ne doit pas etre compris entre -RHO_EXCL et RHO_EXCL

"""),

}
