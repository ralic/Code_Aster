# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

1: _(u"""
 Le mot-clé MAILLAGE est obligatoire avec le mot-clé CREA_MAILLE.
"""),

2: _(u"""
 Le mot-clé MAILLAGE est obligatoire avec le mot-clé CREA_GROUP_MA.
"""),

3: _(u"""
 Le mot-clé MAILLAGE est obligatoire avec le mot-clé CREA_POI1.
"""),

4: _(u"""
 Le mot-clé MAILLAGE est obligatoire avec le mot-clé REPERE.
"""),

5: _(u"""
 Sous le mot-clé "NOM_ORIG" du mot-clé facteur "REPERE",
 on ne peut donner que les mots "CDG" ou "TORSION".
"""),

6: _(u"""
 Maille non créée  %(k1)s
"""),

7: _(u"""
 Le groupe de mailles '%(k1)s' existe déjà.

 Conseil :
    Si vous souhaitez utiliser un nom de groupe existant, il suffit
    de le détruire avec DEFI_GROUP / DETR_GROUP_MA.
"""),

8: _(u"""
 Le mot-clé MAILLAGE est obligatoire avec le mot-clé DETR_GROUP_MA.
"""),

9: _(u"""
 Mode non compatible.
"""),

10: _(u"""
 Masses effectives unitaires non calculées par NORM_MODE
"""),

11: _(u"""
 L'extraction des modes a échoué.
 La structure de données MODE_MECA est vide ou aucun mode ne remplit le critère d'extraction.
 Conseils & solution :
   Vérifiez le résultat de votre calcul modal et/ou modifiez votre filtre d'extraction"
"""),

12: _(u"""
 Le nombre de noeuds sur le contour est insuffisant pour déterminer correctement
 les ordres de coque.
"""),

13: _(u"""
 L'azimut n'est pas défini pour un des noeuds de la coque.
"""),

14: _(u"""
 ordre de coque nul pour l'un des modes pris en compte pour le couplage.
 Le modèle de résolution ne supporte pas une telle valeur.
"""),

15: _(u"""
 détermination du DRMAX et du déphasage pour le mode  %(k1)s  :
 le déterminant du système issu du moindre carré est nul
"""),

16: _(u"""
 détermination du déphasage pour le mode  %(k1)s  :
 THETA0 indéfini
"""),

17: _(u"""
 Pivot nul dans la résolution du système complexe
"""),

18: _(u"""
 Annulation du numérateur dans l'expression d un coefficient donnant
 la solution du problème fluide instationnaire pour UMOY = 0
"""),

19: _(u"""
 Détermination des valeurs propres de l'opérateur différentiel :
 existence d'une racine double
"""),

20: _(u"""
 La %(k1)s ème valeur propre est trop petite.
"""),

21: _(u"""
 La MATR_ASSE  %(k1)s  n'est pas stockée "morse" :
 le GCPC est donc impossible.
"""),

22: _(u"""
 Conflit : une matrice stockée morse ne peut avoir qu'un bloc
"""),

23: _(u"""
Problème :
  Le préconditionnement LDLT_INC d'une matrice complexe n'est pas implémenté
Conseils & solution :
  Il faut choisir un autre solveur que GCPC
"""),

24: _(u"""
 Résolution LDLT : erreur de programmation.
"""),

26: _(u"""
 Problème d'affichage FETI dans PREML1
"""),

27: _(u"""
 Solveur interne LDLT interdit pour l'instant avec FETI
"""),

28: _(u"""
 Solveur interne MUMPS interdit pour l'instant avec FETI
"""),

29: _(u"""
 Solveur interne GCPC pour l'instant proscrit  avec FETI
"""),

30: _(u"""
 Matrices A et B incompatibles pour l'opération *
"""),

31: _(u"""
 La section de la poutre doit être constante.
"""),

32: _(u"""
 Structure non tubulaire
"""),

33: _(u"""
 On ne traite pas ce type de CHAM_ELEM, ICOEF différent de 1
"""),

34: _(u"""
 Le CHAM_NO :  %(k1)s  n'existe pas
"""),

35: _(u"""
MULT_FRONT factorise une Matrice Généralisée.
On a détecté l'existence d'au moins une liaison entre degré de liberté.
On ne renumérote pas car les degrés de liberté sont a priori compris entre  Lagrange1 et Lagrange2 .

Conseil :
  En cas d'arrêt ultérieur avec MATRICE singulière, il faudra changer de SOLVEUR (MUMPS par exemple).
 """),

37: _(u"""
  GCPC n"est pas prévu pour une matrice complexe
"""),

38: _(u"""
 Pas de matrice de préconditionnement : on s'arrête
"""),

40: _(u"""
 Erreur : LMAT est nul
"""),

41: _(u"""
La matrice possède des ddls imposés éliminés: il faut un VCINE
"""),

42: _(u"""
La matrice et le vecteur cinématique ne contiennent pas des valeurs de même type
"""),

43: _(u"""
Attention :
  La pile des matrices frontales a une longueur (%(i1)d) qui, en octets, sera supérieure à l'entier maximum pour cette machine (%(i2)d).
  Vous aurez un problème dans une allocation ultérieure.
Conseil :
  Utilisez une machine 64 bits. Si vous y êtes déjà votre étude est vraiment trop volumineuse !
"""),


44: _(u"""
La méthode de résolution:  %(k1)s  est inconnue. on attend LDLT,GCPC, MULT_FRONT ou FETI
"""),

45: _(u"""
 méthode de BATHE et WILSON : convergence non atteinte
"""),

46: _(u"""
La matrice %(k1)s est non symétrique.
Pour l'instant, la recherche des modes de corps rigide n'a pas été développée
pour une matrice non symétrique.
"""),

47: _(u"""
La matrice %(k1)s est complexe.
Pour l'instant, la recherche des modes de corps rigide n'a pas été développée
pour une matrice complexe.
"""),

48: _(u"""
Cet opérateur a besoin du "procédé de STURM" pour tester la validité de modes propres ou
pour nourrir un algorithme de recherche de modes propres (dichotomie...). Or celui-ci
ne fonctionne, pour l'instant, que sur des matrices réelles et symétriques.
  --> La matrice utilisée ici, %(k1)s ne répond pas a ces critères !
"""),

49: _(u"""
Attention : plus de six modes de corps rigide ont été détectés.

--> Conseil :
Si vous pensez avoir une seule structure dans le modèle, cela peut provenir de noeud(s) orphelin(s). Dans ce cas, vérifiez le maillage.
"""),

50: _(u"""
Attention  %(k1)s .VALF existe déjà
"""),

51: _(u"""
Le tableau B est insuffisamment dimensionné pour l'opération *
"""),

52: _(u"""
Attention :
  Le bloc %(i1)d a une longueur (%(i2)d) qui, en octets, sera supérieure à l'entier maximum pour cette machine (%(i3)d).
  Vous aurez un problème dans une allocation ultérieure.
Conseil :
  Utilisez une machine 64 bits. Si vous y êtes déjà votre étude est vraiment trop volumineuse.
"""),

53: _(u"""
Toutes les fréquences sont des fréquences de corps rigide
"""),

54: _(u"""
Calcul des NUME_MODE : matrice non inversible pour la fréquence considérée
"""),

55: _(u"""
Problème à la résolution du système réduit.
"""),

56: _(u"""
Valeur propre infinie trouvée
"""),

57: _(u"""
Méthode QR : problème de convergence
"""),

58: _(u"""
Il y a des valeurs propres très proches
"""),

60: _(u"""
La matrice : %(k1)s a une numérotation incohérente avec le NUME_DDL.
"""),

61: _(u"""
Le concept "%(k1)s" a été créé avec les matrices
 MATR_RIGI (ou MATR_A) :                   %(k2)s
 MATR_MASS (ou MATR_RIGI_GEOM ou MATR_B) : %(k3)s
 MATR_AMOR (ou MATR_C) :                   %(k4)s
 et non avec celles passées en arguments.
"""),

62: _(u"""
Le concept "%(k1)s" a été créé avec les matrices
 MATR_RIGI (ou MATR_A) :                   %(k2)s
 MATR_MASS (ou MATR_RIGI_GEOM ou MATR_B) : %(k3)s
 et non avec celles passées en arguments.
"""),

63: _(u"""
Le système à résoudre n'a pas de DDL actif.

Conseil :
vérifier que les DDL ne sont pas tous encastrés.
"""),

64: _(u"""
On trouve plus de 9999 valeurs propres dans la bande demandée
"""),






69: _(u"""
Option  %(k1)s non reconnue.
"""),

70: _(u"""
Le type des valeurs varie d'un mode à l'autre, récupération impossible.
"""),

71: _(u"""
Le nombre d'équations est variable d'un mode à l'autre, récupération impossible.
"""),

72: _(u"""
Problème interne ARPACK
"""),

73: _(u"""
Problème taille WORKD/L -> augmenter DIM_SOUS_ESPACE
"""),

74: _(u"""
Problème interne LAPACK
"""),

75: _(u"""
Problème de construction du vecteur initial.

Conseil :
si possible, diminuer NMAX_FREQ (ou NMAX_CHAR_CRIT selon le type d'étude).
"""),

76: _(u"""
Problème interne LAPACK, routine FLAHQR (forme de SCHUR)
"""),

77: _(u"""
Problème interne LAPACK, routine FTREVC (vecteurs propres)
"""),

78: _(u"""
Aucune valeur propre à la précision requise.

Conseils :
augmenter PREC_SOREN ou NMAX_ITER_SOREN
ou augmenter DIM_SOUS_ESPACE.
"""),

79: _(u"""
La position modale d'une des fréquences est négative ou nulle
votre système matriciel est sûrement fortement singulier
(ceci correspond généralement à un problème dans la modélisation).
"""),

80: _(u"""
MODE à créer avant appel à VPSTOR
"""),

81: _(u"""
  Le shift=%(r1)g
  utilisé pour construire la matrice dynamique coïncide avec une valeur propre !
  Avec l'option 'CENTRE', ce shift vaut %(k1)s,
  Avec l'option 'BANDE', c'est le milieu de la bande sélectionnée,
  Avec l'option 'PLUS_PETITE' ou 'TOUT', il prend la valeur 0.
  
  Malgré la stratégie de décalage du shift, cette matrice dynamique reste
  numériquement singulière.
  
  -> Risque :
  Cette matrice étant abondamment utilisée pour résoudre des systèmes linéaires
  à chaque itération du processus modal, cette quasi singularité peut fausser les résultats
  (mauvais conditionnement matriciel).

  -> Conseils :
  La structure analysée présente probablement des modes de corps rigide.
  
    * si aucun mode de corps rigide n'était attendu :
  Vous pouvez modifier les paramètres du solveur linéaire (par exemple METHODE ou NPREC),
  ou ceux de l'algorithme de décalage (PREC_SHIFT, NMAX_ITER_SHIFT et %(k2)s)
  pour vérifier qu'il s'agit bien d'une singularité et non d'un problème numérique ponctuel.
  Si c'est une singularité, vérifiez la mise en donnée du problème :
  conditions aux limites, maillage (présence de noeuds / mailles orphelin(e)s), unités, ...

   * si ces modes étaient attendus et que vous ne voulez pas les calculer :
  Utilisez l'option 'BANDE' avec une borne inférieure suffisamment positive (par exemple 1.e-1).
   * si ces modes étaient attendus et que vous voulez les calculer :
  - utilisez l'option 'BANDE' avec une borne inférieure légèrement négative (par exemple -1.e-1).
  - utilisez la méthode 'TRI_DIAG' avec OPTION='MODE_RIGIDE'.
"""),

82: _(u"""
  Cette borne minimale de la bande de recherche est une valeur propre !
"""),

83: _(u"""
  Cette borne maximale de la bande de recherche est une valeur propre !
"""),

84: _(u"""
  Malgré la stratégie de décalage, la matrice dynamique reste numériquement
  singulière.
  
  -> Risque :
  Le test de Sturm qui sert à évaluer le nombre de modes présents dans l'intervalle
  peut être faussé.

  -> Conseils :
  Vous pouvez modifier les paramètres du solveur linéaire (par exemple METHODE ou NPREC),
  ou ceux de l'algorithme de décalage (PREC_SHIFT, NMAX_ITER_SHIFT et %(k1)s) pour
  vérifiez qu'il s'agit bien d'une singularité et non d'un problème numérique ponctuel.
  
  S'il ne s'agit pas d'un test de vérification ('VERIFICATION A POSTERIORI DES MODES'),
  vous pouvez aussi relancer un autre calcul en décalant les bornes de l'intervalle de
  recherche pour éviter cette fréquence.
"""),

85: _(u"""
  La borne inférieure de l'intervalle a été décalée plusieurs fois car elle est trop proche
  d'une valeur propre. En raison de ces décalages, elle est devenue plus grande que la borne
  supérieure !

  -> Conseils :
  Relancez votre calcul en espaçant suffisamment les bornes de l'intervalle (en tenant compte
  des valeurs des paramètres de décalage NMAX_ITER_SHIFT et PREC_SHIFT).
"""),



90: _(u"""
Objet .REFE/.REFA/.CELK inexistant.
"""),

91: _(u"""
CHAM_NO non FETI
"""),

92: _(u"""
Liste de CHAM_NO à concaténer hétérogène
"""),

93: _(u"""
Les CHAM_NO  %(k1)s  et  %(k2)s  sont de type inconnu  %(k3)s
"""),

94: _(u"""
Le CHAM_NO  %(k1)s  de type  %(k2)s  ne peut être copié dans le CHAM_NO  %(k3)s  de type  %(k4)s
"""),

95: _(u"""
Champ à représentation constante : cas non traité.
"""),

96: _(u"""
CHOUT non FETI
"""),

97: _(u"""
Type de tri inconnu
"""),

98: _(u"""
Problème interne LAPACK, routine DLAHQR (forme de SCHUR)
"""),

99: _(u"""
Problème interne LAPACK, routine DTREVC (vecteurs propres)
"""),
}
