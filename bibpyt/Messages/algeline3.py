#@ MODIF algeline3 Messages  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
 le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_MAILLE.
"""),

2: _("""
 le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_GROUP_MA.
"""),

3: _("""
 le mot-clé MAILLAGE est obligatoire avec le mot-cle CREA_POI1.
"""),

4: _("""
 le mot-clé MAILLAGE est obligatoire avec le mot-cle REPERE.
"""),

5: _("""
 sous le mot-cle "NOM_ORIG" du mot-clé facteur "REPERE",
 on ne peut donner que les mots "CDG" ou "TORSION".
"""),

6: _("""
 maille non créée  %(k1)s
"""),

7: _("""
  le GROUP_MA :  %(k1)s  existe déjà.
"""),

8: _("""
 le mot-cle MAILLAGE est obligatoire avec le mot-clé DETR_GROUP_MA.
"""),

9: _("""
 mode non compatible.
"""),

10: _("""
 masses effectives unitaires non calculées par NORM_MODE
"""),

11: _("""
 structure résultat vide
"""),

12: _("""
 nombre de noeuds sur le contour insuffisant pour déterminer correctement
 les ordres de coque
"""),

13: _("""
 azimut indéfini pour un des noeuds de la coque
"""),

14: _("""
 ordre de coque nul pour l'un des modes pris en compte pour le couplage.
 le modèle de résolution ne supporte pas une telle valeur.
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
 pivot nul dans la résolution du système complexe
"""),

18: _("""
 annulation du numérateur dans l'expression d un coefficient donnant
 la solution du problème fluide instationnaire pour UMOY = 0
"""),

19: _("""
 détermination des valeurs propres de l'opérateur différentiel :
 existence d'une racine double
"""),

20: _("""
 la  %(k1)s ème valeur propre est trop petite
"""),

21: _("""
 la MATR_ASSE  %(k1)s  n'est pas stockée "morse" :
 le GCPC est donc impossible.
"""),

22: _("""
 conflit : une matrice stockée morse ne peut avoir qu'un bloc
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
  erreur a l'appel de METIS
"""),

26: _("""
 pb d'affichage FETI dans PREML1
"""),

27: _("""
 solveur interne LDLT interdit pour l'instant avec FETI
"""),

28: _("""
 solveur interne MUMPS interdit pour l'instant avec FETI
"""),

29: _("""
 solveur interne gcpc pour l'instant proscrit  avec feti
"""),

30: _("""
 matrices A et B incompatibles pour l'opération *
"""),

31: _("""
 la section de la poutre doit etre constante.
"""),

32: _("""
 structure non tubulaire
"""),

33: _("""
 on ne traite pas ce type de CHAM_ELEM, ICOEF différent de 1
"""),

34: _("""
 le CHAM_NO :  %(k1)s  n'existe pas
"""),

37: _("""
  GCPC n"est pas prevu pour une matrice complexe
"""),

38: _("""
 pas de matrice de préconditionnement : on s'arrete
"""),

39: _("""
  le CHAM_NO : %(k1)s  n'existe pas
  Reprise impossible ==> initialisation par le vecteur nul
"""),

40: _("""
 erreur : LMAT est nul
"""),

41: _("""
 la matrice possède des ddls imposés éliminés: il faut un VCINE
"""),

42: _("""
  la matrice et le vecteur cinématique ne contiennent pas des valeurs de meme type
"""),

43: _("""
 la matrice et le second membre ne contiennent pas des valeurs de meme type
"""),

44: _("""
  la methode de resolution:  %(k1)s  est inconnue. on attend ldlt,gcpc, mult_fro ou feti
"""),

45: _("""
 methode de bathe et wilson : convergence non atteinte
"""),

46: _("""
 recherche de corps rigide : pour l'instant proscrite avec matrice non-symetrique
"""),

47: _("""
 recherche de corps rigide : pour l'instant proscrite avec matrice complexe
"""),

48: _("""
 NOM_NUME_DDL  %(k1)s  non trouvé
"""),

49: _("""
 attention : plus de six modes de corps rigides detectés
"""),

50: _("""
 ! attention  %(k1)s .valf existe deja !
"""),

51: _("""
 le tableau B est insuffisamment dimensionné pour l'opération *
"""),

53: _("""
 toutes les fréquences sont des fréquences de corps rigide
"""),

54: _("""
 calcul des NUME_MODE : matrice non inversible pour la fréquence considérée
"""),

55: _("""
 problème à la résolution du système réduit.
"""),

56: _("""
 valeur propre infinie trouvée
"""),

57: _("""
 methode QR : problème de convergence
"""),

58: _("""
 il y a des valeurs propres très proches
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
 la matrice : %(k1)s a une numérotation incohérente avec le NUME_DDL.
"""),

61: _("""
 le concept MODE " %(k1)s " a été créé avec les matrices
 MATR_A:  %(k2)s
 MATR_B:  %(k3)s
 MATR_C:  %(k4)s
 et non avec celles passées en arguments.
"""),

62: _("""
 le concept MODE " %(k1)s " a été créé avec les matrices
 MATR_A:  %(k2)s
 MATR_B:  %(k3)s
 et non avec celles passées en arguments.
"""),

63: _("""
 le système à résoudre n'a pas de DDL actif.
"""),

64: _("""
 on trouve plus de 9999 valeurs propres dans la bande demandée
"""),

65: _("""
 la matrice de raideur est numériquement singulière (malgré la stratégie de décalage) :
 la valeur de décalage est une valeur propre ou la matrice est non inversible.
"""),

66: _("""
  -> La borne minimale de la bande de fréquences est une valeur propre !
     Malgré la stratégie de décalage, la matrice de raideur est numériquement
     singulière.
  -> Risque & Conseil :
     Augmenter (ou diminuer) la fréquence (ou la charge critique dans le cas du calcul de
     flambement) qui définit la borne minimale de la bande de fréquence.
"""),

67: _("""
 la matrice de raideur est numeriquement singulière (malgré la stratégie de decalage) :
 la borne maximale de la bande est une valeur propre.
 n poursuit tout de meme.
"""),

68: _("""
 la matrice de raideur est singulière malgre la strategie de décalage
 (structure avec des modes de corps solide).
"""),

69: _("""
 option  %(k1)s non reconnue.
"""),

70: _("""
 type des valeurs variable d'un mode à l'autre, récuperation impossible.
"""),

71: _("""
 nombre d'équations variable d'un mode à l'autre, récuperation impossible.
"""),

72: _("""
 probleme interne ARPACK
"""),

73: _("""
 problème taille WORKD/L -> augmenter DIM_SOUS_ESPACE
"""),

74: _("""
 problème interne LAPACK
"""),

75: _("""
 probleme construction vecteur initial --> si possible diminuer nmax_freq
"""),

76: _("""
 probleme interne LAPACK, routine FLAHQR (forme de SCHUR)
"""),

77: _("""
 probleme interne LAPACK, routine FTREVC (vecteurs propres)
"""),

78: _("""
 aucune valeur propre à la précision requise
 --> augmenter PREC_SOREN ou NMAX_ITER_SOREN ou augmenter DIM_SOUS_ESPACE
"""),

79: _("""
 la position modale d'une des fréquences est négative ou nulle
 votre système matriciel est surement fortement singulier
 (ceci correspond généralement à un problème dans la modélisation).
"""),

80: _("""
 MODE à créer avant appel à VPSTOR
"""),

81: _("""
 " %(k1)s "  argument du mot cle "OPTION" pour le calcul des fréquences est invalide.
"""),

82: _("""
 pour l'option "BANDE" il faut exactement 2 fréquences.
"""),

83: _("""
 fréquence min. supérieure ou égale à la fréquence max.
"""),

84: _("""
 pour l'option "CENTRE" il faut exactement une fréquence.
"""),

85: _("""
 pour les options  "PLUS_PETITE" et "TOUT" les frequences de "FREQ" sont ignorées.
"""),

86: _("""
 pour l'option  "BANDE" il faut exactement 2 charges critiques.
"""),

87: _("""
 charge crit. min. plus  grande ou egale a la charge crit. max.
"""),

88: _("""
 pour l'option  "CENTRE" il faut exactement une charge critique.
"""),

89: _("""
 pour l'option  "PLUS_PETITE" et "TOUT" les charges critiques de "CHAR_CRIT" sont ignorées.
"""),

90: _("""
 objet .REFE/.REFA/.CELK inexistant.
"""),

91: _("""
 CHAM_NO non FETI
"""),

92: _("""
 liste de CHAM_NO à concaténer hétérogène
"""),

93: _("""
 les CHAM_NO  %(k1)s  et  %(k2)s  sont de type inconnu  %(k3)s
"""),

94: _("""
 le CHAM_NO  %(k1)s  de type  %(k2)s  ne peut etre copié dans le CHAM_NO  %(k3)s  de type  %(k4)s
"""),

95: _("""
 champ à représentation constante : cas non traité.
"""),

96: _("""
 CHOUT non feti
"""),

97: _("""
 type de tri inconnu
"""),

98: _("""
 probleme interne LAPACK, routine DLAHQR (forme de SCHUR)
"""),

99: _("""
 probleme interne LAPACK, routine DTREVC (vecteurs propres)
"""),
}
