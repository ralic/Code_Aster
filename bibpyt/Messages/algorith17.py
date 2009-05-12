#@ MODIF algorith17 Messages  DATE 12/05/2009   AUTEUR DESROCHES X.DESROCHES 
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
 Solveur linéaire MUMPS distribué, routine CRESOL.
 Le processeur de rang %(i1)d ne s'est vu attribuer aucune maille physique
 du modèle!
"""),

2: _("""
         Comportement %(k1)s non implanté pour l'élément d'interface
"""),
3: _("""
        il manque le déplacement de référence DEPL_REFE  
"""), 
4: _("""
        La formulation n'est ni en contrainte nette ni en bishop  
"""), 
5 : _("""
  Le champ posttraité est un cham_elem, le calcul de moyenne ne fonctionne que
 sur les cham_no. Pour les cham_elem utiliser POST_ELEM mot-clé INTEGRALE.
"""), 
}
