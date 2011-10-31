#@ MODIF algeline2 Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _(u"""
 L'argument de "BLOC_DEBUT" doit être strictement positif,
 il est pris à 1
"""),

2 : _(u"""
 Calcul des modes en eau au repos :
 une des valeurs propres de la matrice n'est pas réelle
"""),

3 : _(u"""
 Calcul des modes en eau au repos :
 une des valeurs propres obtenues est nulle
"""),

4 : _(u"""
 Erreur sur la recherche des multiplicateurs de Lagrange
"""),

5 : _(u"""
 mot-clé facteur incorrect
"""),

6 : _(u"""
 Type de matrice " %(k1)s " inconnu.
"""),

7 : _(u"""
 On ne traite pas cette option
"""),

8 : _(u"""
 L'argument de "BLOC_FIN" est plus grand que le nombre de blocs de la matrice,
 il est ramené à cette valeur
"""),

9 : _(u"""
 Les matrices à combiner ne sont pas construites sur le même maillage.
"""),

10 : _(u"""
 Erreur de programmation :
 On cherche à combiner 2 matrices qui n'ont pas les mêmes charges cinématiques.
 Noms des 2 matrices :
    %(k1)s
    %(k2)s

 Solution :
    1) émettre une fiche d'anomalie / évolution
    2) En attendant : ne pas utiliser de charges cinématiques :
       remplacer AFFE_CHAR_CINE par AFFE_CHAR_MECA
"""),

11 : _(u"""
 Les matrices "%(k1)s"  et  "%(k2)s"  n'ont pas la même structure.
"""),

12 : _(u"""
 Résolution système linéaire méthode de Crout.
 Attention: une dimension nulle ou nmax.lt.dmax(1,n)
"""),

13 : _(u"""
 Résolution système linéaire méthode de Crout.
 Attention: une dimension négative ou nulle
"""),

14 : _(u"""
 Résolution système linéaire méthode de Crout.
 Attention: les dimensions des tableaux ne sont pas correctes
"""),

15 : _(u"""
 Pas de charge critique  dans l'intervalle demandé
"""),

16 : _(u"""
  %(k1)s charges critiques  dans l'intervalle demandé
"""),

17 : _(u"""
 Au moins une fréquence calculée extérieure à la bande demandée
"""),

18 : _(u"""
 Les matrices " %(k1)s " et " %(k2)s " n'ont pas le même domaine de définition
"""),

19 : _(u"""
 Problèmes a l'allocation des descripteurs de la matrice " %(k1)s "
"""),

20 : _(u"""
 L'argument de "BLOC_DEBUT" est plus grand que le nombre de blocs de la matrice
"""),

21 : _(u"""
 L'argument de "BLOC_FIN" doit être strictement positif
"""),





28 : _(u"""
 les "MATR_ASSE" %(k1)s "  et  " %(k2)s "  ne sont pas combinables.
"""),

29 : _(u"""
 la valeur d'entrée 'min' est supérieure ou égale à la valeur d'entrée 'SUP'
"""),

30 : _(u"""
 les matrices  " %(k1)s "  et  " %(k2)s "  n'ont pas le même domaine de définition.
"""),

31 : _(u"""
 trop de réajustement de la borne minimale.
"""),

32 : _(u"""
 trop de réajustements de la borne maximale.
"""),

33 : _(u"""
 type de mode inconnu:  %(k1)s
"""),

34 : _(u"""
 il n'est pas permis de modifier un objet père
"""),

35 : _(u"""
 mode non calculé à partir de matrices assemblées
"""),

36 : _(u"""
 normalisation impossible, le point n'est pas présent dans le modèle.
"""),

37 : _(u"""
 normalisation impossible, la composante n'est pas présente dans le modèle.
"""),

38 : _(u"""
 il manque des paramètres entiers
"""),

39 : _(u"""
 il manque des paramètres réels
"""),

40 : _(u"""
 manque des paramètres caractères
"""),

41 : _(u"""
 normalisation impossible,  aucune composante n'est présente dans le modèle.
"""),

42 : _(u"""
 normalisation impossible, le noeud n'est pas présent dans le modèle.
"""),

43 : _(u"""
 on ne tient pas compte du mot-clé facteur "MODE_SIGNE" pour des "MODE_MECA_C"
"""),

44 : _(u"""
 " %(k1)s "  type de mode non traité
"""),

45 : _(u"""
 calcul de flambement et absence du mot-clé CHAR_CRIT ne sont pas compatibles
"""),

46 : _(u"""
 calcul de flambement et matrice d'amortissement ne sont pas compatibles
"""),

47 : _(u"""
 le nombre de fréquences demandées est incorrect.
"""),

48 : _(u"""
 NMAX_ITER_AJUSTE ou NMAX_ITER_SEPARE est négatif
"""),

49 : _(u"""
 NMAX_ITER est négatif
"""),

50 : _(u"""
 PREC_AJUSTE ou PREC_SEPARE est irréaliste
"""),

51 : _(u"""
 PREC est irréaliste (inférieure a 1.e-70)
"""),

52 : _(u"""
 pas de valeur donnée, séparation impossible
"""),

53 : _(u"""
 une seule valeur donnée, séparation impossible
"""),

54 : _(u"""
 la suite des valeurs données n'est pas croissante
"""),

55 : _(u"""
 mot-clé AMOR_REDUIT impossible pour cas généralisé.
"""),

56 : _(u"""
 mot-clé AMOR_REDUIT impossible si option différente de PROCHE
"""),

57 : _(u"""
 nombre différent d'arguments entre les mots-clés AMOR_REDUIT et FREQ
"""),

58 : _(u"""
 les matrices " %(k1)s " et  " %(k2)s "  sont incompatibles entre elles
"""),

59 : _(u"""
 présence de fréquences négatives dans les données.
"""),

60 : _(u"""
  trop de réajustement d'une borne de l'intervalle de recherche.
"""),

61 : _(u"""
 erreur trop de réajustement d'une borne de l'intervalle de recherche.
"""),

62 : _(u"""
 pas de valeurs propres dans la bande de calcul,  le concept ne peut être créé dans ces conditions.
"""),

63 : _(u"""
 " %(k1)s "   option inconnue.
"""),

64 : _(u"""
 le nombre PARAM_ORTHO_SOREN n'est pas valide.
"""),

65 : _(u"""
 détection des modes de corps rigide n'est utilisée qu'avec TRI_DIAG
"""),

66 : _(u"""
 option bande non autorisée pour un problème avec amortissement
"""),

67 : _(u"""
 approche imaginaire ou complexe et fréquence nulle incompatible
"""),

68 : _(u"""
  option modes de corps rigide non utilisée avec amortissement
"""),

69 : _(u"""
 pour le problème généralisé ou quadratique complexe on utilise seulement
 METHODE='SORENSEN' ou 'QZ'
"""),

70 : _(u"""
 problème complexe et fréquence nulle incompatible
"""),

71 : _(u"""
 calcul quadratique par la méthode de SORENSEN et fréquence nulle incompatible
"""),

72 : _(u"""
 la dimension du sous espace de travail est inférieure au nombre de modes rigides
"""),

73 : _(u"""
 Attention : pour l'instant, il n'y a pas de vérification de type STURM
 (comptage du bon nombre des valeurs propres calculées) lorsqu'on est
 dans le plan complexe :
            modal généralisé avec K complexe,
            modal généralisé avec K et/ou M non symétrique(s),
            modal quadratique.
"""),

74 : _(u"""
  erreur de vérification
"""),

75 : _(u"""
  le problème traité étant quadratique, on double l'espace de recherche
"""),

76 : _(u"""
 3 ou 6 valeurs pour le mot-clé "DIRECTION"
"""),

77 : _(u"""
 pour le mot-clé facteur  "PSEUDO_MODE", il faut donner la matrice de masse.
"""),

78 : _(u"""
 la direction est nulle.
"""),

79 : _(u"""
 Les NUME_DDL associés aux matrices MATR_A et MATR_B sont différents.
"""),

80 : _(u"""
 bases modales BASE_1 et BASE_2 avec numérotations incompatibles
"""),

81 : _(u"""
 bases modales et matrice MATR_ASSE avec numérotations incompatibles
"""),

82 : _(u"""
 nombre de modes et d amortissements différents
"""),

83 : _(u"""
 nombre de modes et d amortissements de CONNORS différents
"""),

85 : _(u"""
 inversion valeur min <=> valeur max
"""),

86 : _(u"""
 type de matrice inconnu
"""),

87 : _(u"""
  pas de produit car le champ aux noeuds  %(k1)s  existe déjà.
"""),

88 : _(u"""
  Problème de programmation :
    La matrice globale %(k1)s n'existe pas.
    Elle est nécessaire pour déterminer les degrés de liberté bloqués par AFFE_CHAR_CINE.

  Solution (pour l'utilisateur) :
    1) Ne pas utiliser de charges cinématiques (AFFE_CHAR_CINE)
    2) Émettre une fiche d'anomalie.

  Solution (pour le programmeur) :
    La matrice globale a été détruite abusivement.
    Instrumenter la routine de destruction pour déterminer la routine coupable.
"""),

89 : _(u"""
 le mot-clé MAILLAGE est obligatoire avec le mot-clé CREA_FISS.
"""),

90 : _(u"""
 le mot-clé MAILLAGE est obligatoire avec le mot-clé LINE_QUAD.
"""),

91 : _(u"""
 CREA_MAILLAGE : l'option LINE_QUAD ne traite pas les macro-commandes mailles
"""),

92 : _(u"""
 CREA_MAILLAGE : l'option LINE_QUAD ne traite pas les ABSC_CURV
"""),

93 : _(u"""
 le mot-clé MAILLAGE est obligatoire avec le mot-clé QUAD_LINE.
"""),

94 : _(u"""
 CREA_MAILLAGE : l'option QUAD_LINE ne traite pas les macro-commandes mailles
"""),

95 : _(u"""
 CREA_MAILLAGE : l'option QUAD_LINE ne traite pas les ABSC_CURV
"""),

96 : _(u"""
 le mot-clé MAILLAGE est obligatoire avec le mot-clé MODI_MAILLE.
"""),

97 : _(u"""
 une seule occurrence de "QUAD_TRIA3"
"""),

98 : _(u"""
 le mot-clé MAILLAGE est obligatoire avec le mot-clé COQU_VOLU.
"""),

99 : _(u"""
 pas de maille a modifier
"""),

}
