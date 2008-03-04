#@ MODIF dynamique Messages  DATE 21/02/2008   AUTEUR ANDRIAM H.ANDRIAMBOLOLONA 
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
 Pas de temps maximal (condition CFL) pour le schéma des différences centrées : %(r1)g s
"""),

8: _("""
  Pas de temps maximal (condition CFL) pour le schéma de Tchamwa-Wilgosz : %(r1)g s
"""),

9: _("""
   Arret par manque de temps CPU au numéro d'instant : %(i1)d
      - Temps moyen par pas de temps : %(r1)f
      - Temps cpu restant            : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

10: _("""
   Arret par manque de temps CPU au groupe de pas de temps : %(i1)d
                                 au "petit" pas de temps   : %(i2)d
      - Temps moyen par "petit" pas : %(r1)f
      - Temps cpu restant           : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

11: _("""
   Arret par manque de temps CPU après le calcul de %(i1)d pas.
      - Dernier instant archivé : %(r1)f
      - Numéro d'ordre correspondant : %(i2)d
      - Temps moyen pour les %(i3)d pas de temps : %(r2)f
      - Temps cpu restant            : %(r3)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

12: _("""
 Le pas de temps est trop grand : %(r1)f
 le pas de temps maximal est    : %(r2)f
 
 Avec le pas de temps maximal, le nombre de pas de calcul est %(i1)d
"""),

13: _("""
   Arret par manque de temps CPU à la fréquence : %(i1)d
      - Temps moyen par pas fréquence : %(r1)f
      - Temps cpu restant             : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

14: _("""
   La matrice est presque singulière à la fréquence : %(r1)f
   Cette fréquence est probablement une fréquence propre du système.
"""),

}
