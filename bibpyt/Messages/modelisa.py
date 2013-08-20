# coding=utf-8
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

cata_msg = {

1 : _(u"""
 l'option de calcul d'une abscisse curviligne sur un groupe de mailles
 n'est pas implantée
"""),

2 : _(u"""
 il est possible de définir une abscisse curviligne uniquement
 pour des mailles de type: POI1 ou SEG2
"""),

3 : _(u"""
 point non trouvé parmi les SEG2
"""),

4 : _(u"""
 mauvaise définition pour l'abscisse curviligne
 détection de plusieurs chemins.
"""),

6 : _(u"""
 méthode AU-YANG : la géométrie doit être cylindrique
"""),

7 : _(u"""
 BARRE : une erreur a été détectée lors de l'affectation des valeurs dans le tampon
"""),

8 : _(u"""
 Vous affectez des caractéristiques de type %(k1)s à la maille %(k2)s qui est pas de ce type.

 Conseil :
   Vérifier le résultat de la commande AFFE_MODELE pour la maille %(k2)s.
"""),

10 : _(u"""
 la norme de l'axe AXE définie sous le mot clé facteur GRILLE ou MEMBRANE est nul.
"""),

11 : _(u"""
 L'axe AXE est colinéaire à la normale de l'élément. On ne peut pas définir
 l'orientation des armatures.
"""),

12 : _(u"""
 orientation : une erreur a été détectée lors de l'affectation des orientations
"""),

13 : _(u"""
 erreur(s) dans les données.
"""),

14 : _(u"""
 POUTRE : une erreur a été détectée lors de l'affectation des valeurs dans le tampon
"""),

15 : _(u"""
 poutre : une  erreur a été détectée lors des vérifications des valeurs entrées
"""),

16 : _(u"""
 vous fournissez deux caractéristiques élémentaires. Il est obligatoire de fournir une caractéristique
 relative à l'amortissement et une caractéristique relative à la rigidité
"""),

17 : _(u"""
 caractéristique  %(k1)s  non admise actuellement
"""),

18 : _(u"""
 le noeud  %(k1)s  non modélisé par un discret
"""),

19 : _(u"""
 pas de noeuds du Radier modélisés par des discrets
"""),

20 : _(u"""
 le discret  %(k1)s  n'a pas le bon nombre de noeuds.
"""),

21 : _(u"""
 le noeud  %(k1)s  extrémité d'un des discrets n'existe pas dans la surface donnée par GROUP_MA.
"""),

22 : _(u"""
 La température de référence doit être comprise entre %(r1)f et %(r2)f.
"""),

23 : _(u"""
 AFFE_CARA_ELEM :
 La caractéristique %(k1)s, coefficient de cisaillement, pour les poutres doit toujours
être >=1.0
   Valeur donnée : %(r1)f
"""),

24 : _(u"""
  GENE_TUYAU : préciser un seul noeud par tuyau
"""),

25 : _(u"""
 ORIENTATION : GENE_TUYAU
 le noeud doit être une des extrémités
"""),

26 : _(u"""
  Il y a un problème lors de l'affectation du mot clé MODI_METRIQUE sur la maille %(k1)s
"""),

27 : _(u"""
 on ne peut pas mélanger des tuyaux à 3 et 4 noeuds pour le moment
"""),

28 : _(u"""
 ORIENTATION : GENE_TUYAU
 un seul noeud doit être affecté
"""),

29 : _(u"""
 vous ne pouvez affecter des valeurs de type "POUTRE" au modèle  %(k1)s
 qui ne contient pas un seul élément poutre
"""),

30 : _(u"""
 vous ne pouvez affecter des valeurs de type "COQUE" au modèle  %(k1)s
 qui ne contient pas un seul élément coque
"""),

31 : _(u"""
 vous ne pouvez affecter des valeurs de type "DISCRET" au modèle  %(k1)s
 qui ne contient pas un seul élément discret
"""),

32 : _(u"""
 vous ne pouvez affecter des valeurs de type "ORIENTATION" au modèle  %(k1)s
 qui ne contient ni élément poutre ni élément DISCRET ni élément BARRE
"""),

33 : _(u"""
 vous ne pouvez affecter des valeurs de type "CABLE" au modèle  %(k1)s
 qui ne contient pas un seul élément CABLE
"""),

34 : _(u"""
 vous ne pouvez affecter des valeurs de type "BARRE" au modèle  %(k1)s
 qui ne contient pas un seul élément BARRE
"""),

35 : _(u"""
 vous ne pouvez affecter des valeurs de type "MASSIF" au modèle  %(k1)s
 qui ne contient pas un seul élément thermique ou mécanique
"""),

36 : _(u"""
 vous ne pouvez affecter des valeurs de type "GRILLE" au modèle  %(k1)s
 qui ne contient pas un seul élément GRILLE
"""),

37 : _(u"""
 impossible d'affecter des caractéristiques à des noeuds de ce modèle
 car aucun noeud ne supporte un élément
"""),

38 : _(u"""
 la maille  %(k1)s  n'a pas été affectée par des caractéristiques de poutre.
"""),

39 : _(u"""
 la maille  %(k1)s  n'a pas été affectée par une matrice (DISCRET).
"""),

40 : _(u"""
 la maille  %(k1)s  n'a pas été affectée par des caractéristiques de câble.
"""),

41 : _(u"""
 la maille  %(k1)s  n'a pas été affectée par des caractéristiques de barre.
"""),

42 : _(u"""
 la maille  %(k1)s  n'a pas été affectée par des caractéristiques de grille.
"""),

43 : _(u"""
 le noeud  %(k1)s  n'a pas été affecté par une matrice.
"""),

44 : _(u"""
 BARRE :
 occurrence :  %(k1)s
 "CARA"    :  %(k2)s
 arguments maximums pour une section " %(k3)s "
"""),

45 : _(u"""
 BARRE :
 occurrence  %(k1)s
 "CARA"   :  4
 arguments maximums pour une section " %(k2)s "
"""),

46 : _(u"""
 BARRE :
 occurrence  %(k1)s
 section " %(k2)s
 argument "h" incompatible avec "HY" ou "HZ"
"""),

47 : _(u"""
 barre :
 occurrence  %(k1)s
 section " %(k2)s
 argument "HY" ou "HZ" incompatible avec "h"
"""),

48 : _(u"""
 barre :
 occurrence  %(k1)s
 section " %(k2)s  argument "EP" incompatible avec "EPY" ou "EPZ"
"""),

49 : _(u"""
 barre :
 occurrence  %(k1)s
 section " %(k2)s
 argument "EPY" ou "EPZ" incompatible avec "EP"
"""),

50 : _(u"""
 barre :
 occurrence  %(k1)s
 "CARA" : nombre de valeurs entrées incorrect
 il en faut  %(k2)s
"""),

51 : _(u"""
 barre :
 occurrence  %(k1)s
 section " %(k2)s
 valeur  %(k3)s  de "VALE" non admise (valeur test interne)
"""),

52 : _(u"""
 CABLE :
 occurrence 1
 le mot clé "section" est obligatoire.
"""),

53 : _(u"""
 coque : occurrence 1
 le mot clé "EPAIS" ou "EPAIS_FO" est obligatoire.
"""),

54 : _(u"""
 coque : avec un excentrement, la prise en compte des termes d'inertie de rotation est obligatoire.
"""),

55 : _(u"""
 vous ne pouvez affecter des valeurs de type "MEMBRANE" au modèle  %(k1)s
 qui ne contient pas un seul élément MEMBRANE
"""),

56 : _(u"""
 impossibilité, la maille  %(k1)s  doit être une maille de type  %(k2)s , et elle est de type :  %(k3)s  pour la caractéristique  %(k4)s
"""),

57 : _(u"""
 orientation :
 occurrence 1
 le mot clé "VALE" est obligatoire
"""),

58 : _(u"""
 orientation :
 occurrence 1
 le mot clé "CARA" est obligatoire
"""),

59 : _(u"""
 orientation :
 occurrence  %(k1)s
 présence de "VALE" obligatoire si "CARA" est présent
"""),

60 : _(u"""
 orientation :
 occurrence  %(k1)s
 val :  %(k2)s
 nombre de valeurs entrées incorrect
"""),

61 : _(u"""
 DEFI_ARC:
 le rayon de courbure doit être positif.
"""),

62 : _(u"""
 DEFI_ARC:
 il faut 3 réels pour définir le centre de courbure.
"""),

63 : _(u"""
 DEFI_ARC:
 il faut 3 réels pour définir le point de concours des tangentes.
"""),

64 : _(u"""
 DEFI_ARC:
 le coefficient de flexibilité doit être positif.
"""),

65 : _(u"""
 DEFI_ARC: l'indice de contrainte doit être positif.
"""),

66 : _(u"""
 poutre :
 occurrence  %(k1)s
 section "cercle", VARI_SECT "constant" la caractéristique "r" est obligatoire
"""),





69 : _(u"""
 occurrence  %(k1)s de "barre" (maille  %(k2)s ) écrasement d un type de géométrie de section par un autre
"""),

70 : _(u"""
 barre :
 maille  %(k1)s
 section GENERALE
 il manque la caractéristique  %(k2)s
"""),

71 : _(u"""
 barre :
 maille  %(k1)s
 section GENERALE
 la valeur de  %(k2)s  doit être  strictement positive.
"""),

72 : _(u"""
 barre :
 maille  %(k1)s
 section rectangle
 il manque  la caractéristique  %(k2)s
"""),

73 : _(u"""
 barre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  doit être  strictement positive.
"""),

74 : _(u"""
 barre :
 maille  %(k1)s
 section cercle
 il manque  la caractéristique  %(k2)s
"""),

75 : _(u"""
 barre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit être  strictement positive.
"""),

76 : _(u"""
 barre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit être positive.
"""),

77 : _(u"""
 poutre :
 maille  %(k1)s
 section GENERALE
 il manque la caractéristique  %(k2)s
"""),

78 : _(u"""
 poutre :
 maille  %(k1)s
 section GENERALE
 élément poutre : il manque la caractéristique  %(k2)s
"""),

79 : _(u"""
 poutre :
 maille  %(k1)s
 section rectangle
 il manque  la caractéristique  %(k2)s
"""),

80 : _(u"""
 poutre :
 maille  %(k1)s
 section cercle
 il manque la caractéristique  %(k2)s
"""),

81 : _(u"""
 poutre :
 maille  %(k1)s
 section générale
 la valeur de  %(k2)s  doit être strictement positive
"""),

82 : _(u"""
 poutre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  doit être strictement positive
"""),

83 : _(u"""
 poutre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit être strictement positive
"""),

84 : _(u"""
 poutre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  ne doit pas dépasser  %(k3)s /2
"""),

85 : _(u"""
 poutre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  ne doit pas dépasser celle de  %(k3)s
"""),

86 : _(u"""
 section CIRCULAIRE/RECTANGULAIRE non supportée par POUTRE/TUYAU/FAISCEAU
"""),

87 : _(u"""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s  sur la maille  %(k2)s
 qui n est pas un SEG2
"""),

88 : _(u"""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s sur la maille  %(k2)s
 de longueur nulle
"""),

89 : _(u"""
 orientation :
 pas d affectation d orientation du type  %(k1)s  sur le noeud  %(k2)s
"""),

90 : _(u"""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s  sur la maille  %(k2)s
 de longueur non nulle
"""),

91 : _(u"""
 orientation :
 pas d affectation d orientation du type  %(k1)s  sur la maille  %(k2)s
 qui n est pas SEG2
"""),

92 : _(u"""
 occurrence  %(k1)s de "poutre" (maille  %(k2)s )
 écrasement d'un type de variation de section par un autre
"""),

93 : _(u"""
 occurrence  %(k1)s de "poutre" (maille  %(k2)s )
 écrasement d'un type de géométrie de section par un autre
"""),

94 : _(u"""
 le DESCRIPTEUR_GRANDEUR des déplacements ne tient pas sur dix entiers codés
"""),




}
