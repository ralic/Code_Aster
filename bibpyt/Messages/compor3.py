# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={


2 : _(u"""
 Dans le KIT_DDI, on ne peut pas coupler GRANGER avec %(k1)s.
"""),

3 : _(u"""
 Dans le KIT_DDI, on ne peut pas coupler BETON_UMLV_FP avec %(k1)s.
"""),

4 : _(u"""
 Dans le KIT_DDI, on ne peut pas coupler GLRC avec %(k1)s.
"""),

6 : _(u"""
 La loi de fluage %(k1)s n'est pas autorisée dans le couplage fluage/fissuration (KIT_DDI).
"""),

83 : _(u"""
 Vous utilisez le modèle BETON_UMLV_FP avec un modèle d'endommagement.
 Attention, la mise à jour des contraintes sera faite suivant les déformations totales et non pas suivant un schéma incrémental.
"""),

91 : _(u"""
   La loi métallurgique META_LEMA_ANI n'est utilisable qu'avec le zirconium.
"""),
}
