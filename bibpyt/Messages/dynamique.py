#@ MODIF dynamique Messages  DATE 13/02/2007   AUTEUR GREFFET N.GREFFET 
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
 schema inconnu
"""),

2: _("""
 la liste d'instants fournie ne respecte pas la condition de stabilite.
"""),

3: _("""
 la condition de stabilite n'a pas pu etre calculee pour tous les elements. elle peut etre trop grande.
"""),

4: _("""
  -> La condition de stabilité n'a pu etre calculée pour aucun élément.
  -> Risque & Conseil :
     Vous prenez le risque de sortir du cadre de la stabilité conditionnelle du schéma de temps explicite. Vérifiez bien
     que vos éléments finis ont une taille et un matériau (module de Young) compatibles avec le respect de la condition 
     de Courant vis-à-vis du pas de temps que vous avez imposé (temps de propagation des ondes dans la maille, voir 
     documentation). Si c'est le cas, lever l'arret fatal en utilisant l'option "STOP_CFL", à vos risques et périls 
     (risques de résultats faux).
"""),

5: _("""
 Pas de temps maximal (condition CFL) pour le schéma des différences centrées : %(r1)g s, sur la maille : %(k1)s
"""),

6: _("""
  Pas de temps maximal (condition CFL) pour le schéma de Tchamwa-Wilgosz : %(r1)g s, sur la maille : %(k1)s
"""),

7: _("""
 Pas de temps maximal (condition CFL) pour le schéma des différences centrées : %(r1)12.5E s
"""),

8: _("""
  Pas de temps maximal (condition CFL) pour le schéma de Tchamwa-Wilgosz : %(r1)12.5E s
"""),

}
