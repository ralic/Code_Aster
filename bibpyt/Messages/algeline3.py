#@ MODIF algeline3 Messages  DATE 29/03/2011   AUTEUR BOITEAU O.BOITEAU 
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
 Le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_MAILLE.
"""),

2: _("""
 Le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_GROUP_MA.
"""),

3: _("""
 Le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_POI1.
"""),

4: _("""
 Le mot-clé MAILLAGE est obligatoire avec le mot-cle REPERE.
"""),

5: _("""
 Sous le mot-cle "NOM_ORIG" du mot-clé facteur "REPERE",
 on ne peut donner que les mots "CDG" ou "TORSION".
"""),

6: _("""
 Maille non créée  %(k1)s
"""),

7: _("""
 Le groupe de mailles '%(k1)s' existe déjà.

 Conseil :
    Si vous souhaitez utiliser un nom de groupe existant, il suffit
    de le détruire avec DEFI_GROUP / DETR_GROUP_MA.
"""),

8: _("""
 Le mot-cle MAILLAGE est obligatoire avec le mot-clé DETR_GROUP_MA.
"""),

9: _("""
 Mode non compatible.
"""),

10: _("""
 Masses effectives unitaires non calculées par NORM_MODE
"""),

11: _("""
 L'extraction des modes a échoué.
 La structure de données mode_meca est vide ou aucun mode ne remplit le critère d'extraction.
 Conseils & solution :
   Vérifiez le résultat de votre calcul modal et/ou modifiez votre filtre d'extraction"
"""),

12: _("""
 Le nombre de noeuds sur le contour est insuffisant pour déterminer correctement
 les ordres de coque.
"""),

13: _("""
 L'azimut n'est pas défini pour un des noeuds de la coque.
"""),

14: _("""
 ordre de coque nul pour l'un des modes pris en compte pour le couplage.
 Le modèle de résolution ne supporte pas une telle valeur.
"""),

15: _("""
 détermination du DRMAX et du déphasage pour le mode  %(k1)s  :
 le déterminant du système issu du moindre carré est nul
"""),

16: _("""
 détermination du déphasage pour le mode  %(k1)s  :
 THETA0 indéfini
"""),

17: _("""
 Pivot nul dans la résolution du système complexe
"""),

18: _("""
 Annulation du numérateur dans l'expression d un coefficient donnant
 la solution du problème fluide instationnaire pour UMOY = 0
"""),

19: _("""
 Détermination des valeurs propres de l'opérateur différentiel :
 existence d'une racine double
"""),

20: _("""
 La %(k1)s ème valeur propre est trop petite.
"""),

21: _("""
 La MATR_ASSE  %(k1)s  n'est pas stockée "morse" :
 le GCPC est donc impossible.
"""),

22: _("""
 Conflit : une matrice stockée morse ne peut avoir qu'un bloc
"""),

23: _("""
Problème :
  Le préconditionnement LDLT_INC d'une matrice complexe n'est pas implémenté
Conseils & solution :
  Il faut choisir un autre solveur que GCPC
"""),

24: _("""
 Résolution LDLT : erreur de programmation.
"""),

25: _("""
 Erreur à l'appel de METIS
"""),

26: _("""
 Problème d'affichage FETI dans PREML1
"""),

27: _("""
 Solveur interne LDLT interdit pour l'instant avec FETI
"""),

28: _("""
 Solveur interne MUMPS interdit pour l'instant avec FETI
"""),

29: _("""
 Solveur interne gcpc pour l'instant proscrit  avec feti
"""),

30: _("""
 Matrices A et B incompatibles pour l'opération *
"""),

31: _("""
 La section de la poutre doit etre constante.
"""),

32: _("""
 Structure non tubulaire
"""),

33: _("""
 On ne traite pas ce type de CHAM_ELEM, ICOEF différent de 1
"""),

34: _("""
 Le CHAM_NO :  %(k1)s  n'existe pas
"""),

37: _("""
  GCPC n"est pas prevu pour une matrice complexe
"""),

38: _("""
 Pas de matrice de préconditionnement : on s'arrete
"""),

40: _("""
 Erreur : LMAT est nul
"""),

41: _("""
La matrice possède des ddls imposés éliminés: il faut un VCINE
"""),

42: _("""
La matrice et le vecteur cinématique ne contiennent pas des valeurs de meme type
"""),

44: _("""
La methode de resolution:  %(k1)s  est inconnue. on attend ldlt,gcpc, mult_fro ou feti
"""),

45: _("""
 methode de bathe et wilson : convergence non atteinte
"""),

46: _("""
Recherche de corps rigide : pour l'instant proscrite avec matrice non-symetrique
"""),

47: _("""
Recherche de corps rigide : pour l'instant proscrite avec matrice complexe
"""),





49: _("""
Attention : plus de six modes de corps rigides detectés
"""),

50: _("""
Attention  %(k1)s .VALF existe déjà
"""),

51: _("""
Le tableau B est insuffisamment dimensionné pour l'opération *
"""),

53: _("""
Toutes les fréquences sont des fréquences de corps rigide
"""),

54: _("""
Calcul des NUME_MODE : matrice non inversible pour la fréquence considérée
"""),

55: _("""
Problème à la résolution du système réduit.
"""),

56: _("""
Valeur propre infinie trouvée
"""),

57: _("""
Méthode QR : problème de convergence
"""),

58: _("""
Il y a des valeurs propres très proches
"""),

59: _("""
Erreur d'utilisation :
  Le solveur MULT_FRONT est interdit ici car les ddls de la matrice ne sont pas
  portés par les noeuds d'un maillage.
  Peut-etre s'agit-il d'une matrice généralisée ?

Conseil :
  Il faut changer de solveur
"""),

60: _("""
La matrice : %(k1)s a une numérotation incohérente avec le NUME_DDL.
"""),

61: _("""
Le concept MODE "%(k1)s" a été créé avec les matrices
 MATR_A:  %(k2)s
 MATR_B:  %(k3)s
 MATR_C:  %(k4)s
 et non avec celles passées en arguments.
"""),

62: _("""
Le concept MODE "%(k1)s" a été créé avec les matrices
 MATR_A:  %(k2)s
 MATR_B:  %(k3)s
 et non avec celles passées en arguments.
"""),

63: _("""
Le système à résoudre n'a pas de DDL actif.
"""),

64: _("""
On trouve plus de 9999 valeurs propres dans la bande demandée
"""),

66: _("""
  -> La borne minimale de la bande de fréquences est une valeur propre !
     Malgré la stratégie de décalage, la matrice de raideur est numériquement
     singulière (modes de corps rigide).
  -> Risque & Conseil :
     Augmenter (ou diminuer) la fréquence (ou la charge critique dans le cas du calcul de
     flambement) qui définit la borne minimale de la bande de fréquence.
"""),

67: _("""
La matrice de raideur est numeriquement singulière (malgré la stratégie de decalage) :
la borne maximale de la bande est une valeur propre.
On poursuit tout de meme.
"""),

68: _("""
  -> La matrice de raideur est singulière malgre la strategie de décalage
(structure avec des modes de corps rigide).

  -> Risque & Conseil :
  Pour passer avec l'option 'PLUS_PETITE', augmenter la valeur de SEUIL_FREQ
  (0.1 par exemple) ou de PREC_SHIFT (0.5 par exemple). 
  En fait, il est plutot conseille d'utiliser l'option 'BANDE'. Ici avec une borne minimale
  de la bande de fréquence légèrement négative (ou positive).
  A defaut, si vous n'arrivez pas a capter tous les modes rigides, essayer la methode
  TRI_DIAG avec OPTION='MODE_RIGIDE'.
"""),

69: _("""
Option  %(k1)s non reconnue.
"""),

70: _("""
Le type des valeurs varie d'un mode à l'autre, récupération impossible.
"""),

71: _("""
Le nombre d'équations est variable d'un mode à l'autre, récupération impossible.
"""),

72: _("""
Probleme interne ARPACK
"""),

73: _("""
Problème taille WORKD/L -> augmenter DIM_SOUS_ESPACE
"""),

74: _("""
Problème interne LAPACK
"""),

75: _("""
Probleme construction vecteur initial --> si possible diminuer nmax_freq
"""),

76: _("""
Probleme interne LAPACK, routine FLAHQR (forme de SCHUR)
"""),

77: _("""
Probleme interne LAPACK, routine FTREVC (vecteurs propres)
"""),

78: _("""
Aucune valeur propre à la précision requise
 --> augmenter PREC_SOREN ou NMAX_ITER_SOREN ou augmenter DIM_SOUS_ESPACE
"""),

79: _("""
La position modale d'une des fréquences est négative ou nulle
votre système matriciel est surement fortement singulier
(ceci correspond généralement à un problème dans la modélisation).
"""),

80: _("""
MODE à créer avant appel à VPSTOR
"""),

81: _("""
"%(k1)s" argument du mot cle "OPTION" pour le calcul des fréquences est invalide.
"""),

82: _("""
Pour l'option "BANDE" il faut exactement 2 fréquences.
"""),

83: _("""
Fréquence min. supérieure ou égale à la fréquence max.
"""),

84: _("""
Pour l'option "CENTRE" il faut exactement une fréquence.
"""),

85: _("""
Pour les options  "PLUS_PETITE" et "TOUT" les frequences de "FREQ" sont ignorées.
"""),

86: _("""
Pour l'option  "BANDE" il faut exactement 2 charges critiques.
"""),

87: _("""
Charge crit. min. plus  grande ou egale a la charge crit. max.
"""),

88: _("""
Pour l'option  "CENTRE" il faut exactement une charge critique.
"""),

89: _("""
Pour l'option  "PLUS_PETITE" et "TOUT" les charges critiques de "CHAR_CRIT" sont ignorées.
"""),

90: _("""
Objet .REFE/.REFA/.CELK inexistant.
"""),

91: _("""
CHAM_NO non FETI
"""),

92: _("""
Liste de CHAM_NO à concaténer hétérogène
"""),

93: _("""
Les CHAM_NO  %(k1)s  et  %(k2)s  sont de type inconnu  %(k3)s
"""),

94: _("""
Le CHAM_NO  %(k1)s  de type  %(k2)s  ne peut etre copié dans le CHAM_NO  %(k3)s  de type  %(k4)s
"""),

95: _("""
Champ à représentation constante : cas non traité.
"""),

96: _("""
CHOUT non feti
"""),

97: _("""
Type de tri inconnu
"""),

98: _("""
Problème interne LAPACK, routine DLAHQR (forme de SCHUR)
"""),

99: _("""
Problème interne LAPACK, routine DTREVC (vecteurs propres)
"""),
}
