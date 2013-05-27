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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

97 : _(u"""
 La loi de comportement ELAS_HYPER ne peut pas être utilisée avec le modèle %(k1)s.
 Modèles disponibles: 3D, 3D_SI, C_PLAN, D_PLAN
"""),

98 : _(u"""
 Le coefficient de Poisson ne permet pas de calculer le module de compressibilité car il provoquerait une division par zéro.
 Si vous voulez un matériau quasi incompressible:
  - Renseignez directement le module de compressibilité (paramètre K dans DEFI_MATERIAU/ELAS_HYPER)
  - Choisissez un coefficient de Poisson proche de 0.5 mais pas strictement égal (par exemple 0.499)
"""),

}
