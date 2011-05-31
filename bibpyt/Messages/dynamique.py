#@ MODIF dynamique Messages  DATE 31/05/2011   AUTEUR NISTOR I.NISTOR 
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
# RESPONSABLE DELMAS J.DELMAS

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
 Dans l\'intervalle : %(i2)d
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

15 : _("""
 Pas de temps maximal (mot-clé PAS_MAXI) demandé : %(r1)f plus petit que
 le pas de temps initial demandé par l'utilisateur (mot-clé PAS) : %(r2)f
 Il faut s'assurer que PAS est bien inférieur ou égal à PAS_MAXI 
"""), 

16 : _("""
 Pas de temps maximal calculé pour le schéma ADAPT : %(r1)f
 
 Risque & Conseil : la méthode de calcul automatique de ce pas maximal semble être prise en défaut.
 On recommande donc de définir explicitement cette valeur avec le mot-clé PAS_MAXI (sous INCREMENT).
"""), 

17 : _("""
 Pas de temps maximal (mot-clé PAS_MAXI) demandé trop grand :   %(r1)f
 Pas de temps necessaire pour le calcul: %(r2)f
 Risques de problemes de precision 
"""), 

18 : _("""
 Le nombre maximal de sous-division du pas : %(i1)d est atteint à l'instant : %(r1)f 
 Le pas de temps vaut alors : %(r2)f
 On continue cependant la résolution en passant au pas suivant.
 
 Risque & Conseil : la solution calculée risque d'être imprécise.
 Il faudrait relancer la calcul en autorisant le schéma ADAPT à utiliser un pas de temps plus petit.
 Pour cela on peut jouer sur au moins un des trois paramètres suivants :
 - diminuer le pas de temps initial (mot-clé PAS),
 - augmenter le nombre maximal de sous-découpage du pas (mot-clé NMAX_ITER_PAS),
 - augmenter le facteur de division du pas (mot-clé COEF_DIVI_PAS)
"""), 

}
