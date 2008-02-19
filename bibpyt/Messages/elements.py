#@ MODIF elements Messages  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _("""
 arret du code
"""),

2 : _("""
 ABS(RAYON2-RAYON1).GT.EPSI
"""),

5 : _("""
 problème de maillage TUYAU
 pour une maille definie par les noeuds N1 N2 N3  le noeud N3 doit etre le noeud milieu
"""),

6 : _("""
  GENE_TUYAU
  il faut donner un vecteur non colinéaire au tuyau
"""),

7 : _("""
 angle du coude trop grand
"""),

8 : _("""
  mailler plus fin
"""),

9 : _("""
 il faut renseigner le coefficient E_N  dans les cas des déformations planes et de l'axisymétrie
 on ne regarde donc que le cas des contraintes planes.
"""),

10 : _("""
 Subroutine CHPVER :
 le champ  %(k1)s n'a pas le bon type :
   type autorisé  :%(k2)s
   type du champ  :%(k3)s
"""),

11 : _("""
 La modélisation utilisée n'est pas traitée.
"""),

12 : _("""
 nombre de couches obligatoirement supérieur à 0
"""),

13 : _("""
 nombre de couches limité a 10 pour les coques 3d
"""),

14 : _("""
 le type d'élément :  %(k1)s n'est pas prevu.
"""),

15 : _("""
 la nature du matériau  %(k1)s  n'est pas traitée
 seules sont considérées les natures : ELAS, ELAS_ISTR, ELAS_ORTH .
"""),

16 : _("""
 type de maille inconnu
"""),

17 : _("""
 noeuds confondus pour un élément
"""),

18 : _("""
 le nombre de noeuds d'un tuyau est différent de 3 ou 4
"""),

20 : _("""
 aucun type d'éléments ne correspond au type demandé
"""),

21 : _("""
 prédicteur ELAS hors champs
"""),

24 : _("""
 derivatives of "mp" not defined
"""),

25 : _("""
 on passe en mécanisme 2
"""),

26 : _("""
 chargement en mécanisme 2 trop important
 à vérifier
"""),

27 : _("""
 on poursuit en mécanisme 2
"""),

28 : _("""
 décharge négative sans passer par meca 1 diminuer le pas de temps
"""),

29 : _("""
 on revient en mécanisme 1
"""),

30 : _("""
 pas de retour dans meca 1 trop important
 diminuer le pas de temps
"""),

31 : _("""
 type d'élément  %(k1)s  incompatible avec  %(k2)s
"""),

32 : _("""
 le comportement %(k1)s est inattendu
"""),

33 : _("""
 la convergence locale de la loi GLRC_DM n'est pas atteinte en 1000 itérations :
 XM1 vaut %(r1)f
 XM2 vaut %(r2)f
 YM1 vaut %(r3)f
 YM2 vaut %(r4)f
"""),

34 : _("""
 élément non traité  %(k1)s
"""),

35 : _("""
 pas d'excentrement avec STAT_NON_LINE
 maille  : %(k1)s
"""),

36 : _("""
 nombre de couches négatif ou nul :  %(k1)s
"""),

37 : _("""
 Subroutine CHPVER :
 le champ  %(k1)s n'a pas la bonne grandeur :
   grandeur autorisée  :%(k2)s
   grandeur du champ   :%(k3)s
"""),

38 : _("""
 Le phénomène sensible %(k1)s choisi ne correspond pas au phénomène %(k2)s dont il est issu 
"""),

39 : _("""
 l'axe de référence est normal à un élément de plaque anisotrope
"""),

40 : _("""
  -> L'axe de référence pour le calcul du repère local est normal à un
     au moins un élément de plaque.
  -> Risque & Conseil :
     Il faut modifier l'axe de référence (axe X par défaut) en utilisant
     ANGL_REP ou VECTEUR.

"""),

41 : _("""
 impossibilité :
 vous avez un materiau de type "ELAS_COQUE" et vous n'avez pas défini la raideur de membrane,
 ni sous la forme "MEMB_L", ni sous la forme "M_LLLL".
"""),

42 : _("""
 comportement matériau non admis
"""),

43 : _("""
 impossibilité :
 vous avez un materiau de type "ELAS_COQUE" et le determinant de la sous-matrice de Hooke relative au cisaillement est nul.
"""),

45 : _("""
 L'un des coefficients materiaux N_VP et M_VP de la loi de comportement VENDO_CHAB 
  est négatif. Or ce n'est physiquement pas possible.
"""),

46 : _("""
 nombre de couches négatif ou nul
"""),

47 : _("""
 pas d excentrement avec FORC_NODA
 maille  : %(k1)s
"""),

48 : _("""
 impossibilité, la surface de l'élément est nulle.
"""),

49 : _("""
 l'axe de référence est normal à un élément de plaque
 calcul option impossible
 orienter ces mailles
"""),

50 : _("""
 comportement elastique inexistant
"""),

51 : _("""
  -> Le type de comportement %(k1)s n'est pas prévu pour le calcul de
     SIGM_ELNO_DEPL. Les seuls comportements autorisés sont :
     ELAS, ELAS_COQUE, ou ELAS_ORTH
  -> Risque & Conseil :
     Pour les autres comportements, utiliser SIEF_ELNO_ELGA (efforts)
     ou SIGM_ELNO_COQU (contraintes en un point de l'épaisseur).
"""),

53 : _("""
 problème: temperature sur la maille: %(k1)s : il manque la composante "TEMP"
"""),

55 : _("""
 ELREFA inconnu:  %(k1)s
"""),

58 : _("""
 la nature du matériau  %(k1)s  nécessite la définition du coefficient  B_ENDOGE dans DEFI_MATERIAU.
"""),

62 : _("""
 GROUP_MA :  %(k1)s  inconnu dans le maillage
"""),

64 : _("""
  le LIAISON_*** de  %(k1)s  implique les noeuds physiques  %(k2)s  et  %(k3)s et traverse l'interface
"""),

65 : _("""
  le LIAISON_*** de  %(k1)s  implique le noeud physique  %(k2)s et touche l'interface
"""),

66 : _("""
 si NOEUD_ORIG : donner un GROUP_MA ou une liste de mailles
 on ne réordonne pas les GROUP_NO et les listes de noeuds.
"""),

67 : _("""
 le GROUP_NO :  %(k1)s n'existe pas.
"""),

68 : _("""
 le noeud origine  %(k1)s ne fait pas partie du chemin
"""),

69 : _("""
 le noeud origine  %(k1)s  n'est pas une extremité
"""),

70 : _("""
 pas d'origine définie par NOEUD_ORIG ou GROUP_NO_ORIG
"""),

71 : _("""
 échec dans la recherche du noeud origine
"""),

72 : _("""
 GROUP_NO orienté : noeud origine =  %(k1)s
"""),

73 : _("""
 le GROUP_MA :  %(k1)s n'existe pas.
"""),

74 : _("""
 si le fond est une courbe fermée MAILLE_ORIG ou GROUP_MA_ORIG doit accompagner NOEUD_ORIG
"""),

75 : _("""
 le noeud_orig n'appartient pas a la maille_orig
"""),

76 : _("""
 la maille_orig %(k1)s  n'appartient pas au fond de fissure
"""),

77 : _("""
 le noeud extremité  %(k1)s  n'est pas le dernier noeud
"""),

78 : _("""
 GROUP_NO orienté : noeud extremité =  %(k1)s
"""),

83 : _("""
 le type des mailles des lèvres doit etre quadrangle ou triangle
"""),

84 : _("""
  %(k1)s CHAM_NO inexistant
"""),

87 : _("""
 bad definition of MP1 and MP2
"""),

90 : _("""
Erreur de programmation :
   L'attribut NBSIGM n'est pas défini pour cette modélisation.
Solution :
   Il faut modifier la catalogue phenomene_modelisation__.cata pour ajouter NBSIGM pour cette modélisation.
"""),

94 : _("""
 arret sur erreur(s) utilisateur.
"""),

}
