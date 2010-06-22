#@ MODIF modelisa10 Messages  DATE 21/06/2010   AUTEUR PROIX J-M.PROIX 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
#            CONFIGURATION MANAGEMENT OF EDF VERSION

def _(x) : return x

cata_msg = {

1 : _("""
Le vecteur definissant l'axe de rotation a une composante non nulle suivant Ox ou Oz,
ce qui induit un chargement non axisymetrique. Avec une modelisation AXIS ou AXIS_FOURIER,
l'axe de rotation doit etre dirige suivant Oy.
"""),

2 : _("""
Les coordonnees du centre de rotation ont au moins une composante non nulle, ce qui induit
un chargement non axisymetrique. Avec une modelisation AXIS ou AXIS_FOURIER,
le centre de rotation doit etre confondu avec l'origine.
"""),

3 : _("""
Le vecteur definissant l'axe de rotation a une composante non nulle suivant Ox ou Oy,
ce qui induit des forces centrifuges hors plan. Avec une modelisation C_PLAN ou D_PLAN,
l'axe de rotation doit être dirige suivant Oz.
"""),


4 : _("""
Les mailles affectées à la modélisation TUYAU ne semblent pas former des lignes continues.
Il y a probablement un probleme dans le maillage (superposition d'éléments par exemple).
Pour obtenir le détail des mailles affectées, utilisez INFO=2. 
"""),

}
