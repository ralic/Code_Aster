#@ MODIF fatigue1 Messages  DATE 16/07/2007   AUTEUR ANGLES J.ANGLES 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={
1: _("""
 Aucun élément du modèle ne sait calculer l'option
 de fatigue multiaxiale, ('PFACY_R').
 Il se peut que la modélisation affectée au groupe de mailles
 sur lequel vous faites un calcul de fatigue ne soit pas "3D".

 Le critère de fatigue que vous utilisez n'est utilisable qu'en 3D.

"""),

2: _("""
 La modélisation affectée au groupe de mailles sur lequel vous
 faites un calcul de fatigue n'est problament pas "3D".
 La composante %(i1)d du tenseur des contraintes n'existe pas.

 Le critère de fatigue que vous utilisez n'est utilisable qu'en 3D.

"""),

3: _("""
 La modélisation affectée au groupe de mailles sur lequel vous
 faites un calcul de fatigue n'est problament pas "3D".
 La composante %(i1)d du tenseur des déformations n'existe pas.

 Le critère de fatigue que vous utilisez n'est utilisable qu'en 3D.

"""),


}
