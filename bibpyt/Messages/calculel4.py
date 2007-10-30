#@ MODIF calculel4 Messages  DATE 29/10/2007   AUTEUR PELLET J.PELLET 
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
 option non opérationnelle:
 seule l'option COURONNE est à utiliser dans le cas où
 on emploie le mot clé THETA_3D
"""),

2: _("""
 il faut donner 3 composantes de la direction
 la 3ème nulle
"""),

3: _("""
 il faut donner la direction de propagation en 2D
 la direction par defaut n'existe plus
"""),

4: _("""
 option non opérationnelle:
 seule l'option COURONNE est à utiliser dans le cas où
 on emploie le mot clé THETA_2D
"""),

5: _("""
 option inexistante:
 seule l'option BANDE est à utiliser dans le cas ou on emploie le mot clé THETA_BANDE .
"""),

6: _("""
 la normale n'est pas orthogonale à la tangente à l'origine
"""),

7: _("""
 la normale n'est pas orthogonale à la tangente à l'extremite
"""),

8: _("""
 le resultat  %(k1)s  n'existe pas
"""),

9: _("""
 creation de la table  %(k1)s
"""),

13: _("""
 probleme a la recuperation d'un champ
"""),

16: _("""
 dans le cas d'une SD RESULTAT de type DYNA_TRANS,
 le mot-cle EXCIT est obligatoire.
"""),

17: _("""
 pour un resultat de type MODE_MECA,
 l option de calcul doit etre K_G_MODA.
"""),

19: _("""
 le mot cle 'FISSURE' est obligatoire avec l'option  %(k1)s
"""),

20: _("""
 le mot cle 'FOND_FISS' est obligatoire avec l'option  %(k1)s
"""),

21: _("""
 mot cle FOND_FISS obligatoire pour l option  %(k1)s
"""),

22: _("""
 champ THETA calcule automatiquement
"""),

23: _("""
 pour l option  %(k1)s (3d local) utiliser le mot cle THETA_LAGR
"""),

24: _("""
 il faut donner 3 composantes de la direction %(k1)s
"""),

25: _("""
 mot cle PROPAGATION utilisé seulement pour le calcul de G avec propagation lagrangienne
"""),

26: _("""
 cette combinaison de lissage n'est pas programmee pour l'option :  %(k1)s .
"""),

27: _("""
 le degré des polynomes de legendre doit etre inferieur ou egal au nombre de noeuds du fond de fissure avec la methode theta-lagrange
"""),

28: _("""
 le lissage de G doit etre de type LEGENDRE si le lissage de THETA est de type LEGENDRE
"""),

29: _("""
 l'option  %(k1)s  n'est pas permise avec le lissage 'lagrange_regu'
"""),

30: _("""
 si la methode 'lagrange_regu' est utilisee pour le lissage, alors le lissage de g et de theta doivent etre de type 'lagrange_regu'.
"""),

31: _("""
 champ theta obligatoire avec  %(k1)s . utiliser le mot cle theta_lagr.
"""),

32: _("""
 fond obligatoire avec option calc_k_g
"""),

36: _("""
 l usage des polynomes de legendre dans le cas d un fond de fissure clos est interdit.
"""),

37: _("""
 acces impossible au deplacement
"""),

38: _("""
 mot-clef <bornes> obligatoire avec l option  %(k1)s  !
"""),

39: _("""
 acces impossible au mode propre
"""),

40: _("""
 option non dispo actuellement
"""),

41: _("""
 erreur_01
"""),

42: _("""
 erreur_02
"""),

43: _("""
 le nom_para n'existe pas
"""),

44: _("""
 0 ligne trouvee pour le nom_para
"""),

45: _("""
 plusieurs lignes trouvees
"""),

46: _("""
 code retour de "tbliva" inconnu
"""),

47: _("""
 type_resu inconnu:  %(k1)s
"""),

48: _("""
 erreur calcul alpha0 :champ depl elastique non trouve
"""),

49: _("""
 erreur : le champ depl elastique n'existe pas
"""),

50: _("""
 erreur: le champ sief_elga_depl n'existe pas
"""),

51: _("""
 methode zac : accommodation et chargement non radial --> methode non appliquable
"""),

53: _("""
 longueurs des modes locaux incompatibles entre eux.
"""),

54: _("""
 aucuns noeuds sur lesquels projeter.
"""),

55: _("""
 pas de mailles a projeter.
"""),

56: _("""
  %(k1)s  pas trouve.
"""),

57: _("""
 il n'y a pas de mailles a projeter.
"""),

58: _("""
 les maillages a projeter sont ponctuels.
"""),

59: _("""
 maillages 1 differents.
"""),

60: _("""
 maillages 2 differents.
"""),

61: _("""
 probleme dans l'examen de  %(k1)s
"""),

62: _("""
 aucun numero d'ordre dans  %(k1)s
"""),

63: _("""
 On n'a pas pu projeter le champ %(k1)s de la sd_resultat %(k2)s
 vers la sd_resultat %(k3)s pour le numéro d'ordre %(i1)d
"""),

64: _("""
 Aucun champ projete.
"""),

65: _("""
  maillages non identiques :  %(k1)s  et  %(k2)s
"""),

66: _("""
 pas de chmate
"""),

67: _("""
 erreur dans etanca pour le probleme primal
"""),

68: _("""
 erreur dans etenca pour le probleme dual
"""),

69: _("""
 On ne trouve pas la variable de commande :  %(k1)s  pour la maille:  %(k2)s
"""),

79: _("""
 La grandeur :  %(k1)s  n'existe pas dans le catalogue des grandeurs.
"""),

80: _("""
 le nom de la grandeur  %(k1)s  ne respecte pas le format xxxx_c
"""),

81: _("""
 probleme dans le catalogue des grandeurs simples, la grandeur complexe %(k1)s  ne possede pas le
 meme nombre de composantes que son homologue réelle %(k2)s
"""),

82: _("""
 probleme dans le catalogue des grandeurs simples, la grandeur %(k1)s  ne possede pas les memes champs que son homologue reelle %(k2)s
"""),

83: _("""
 erreur: le calcul des contraintes ne fonctionne que pour le phenomene mecanique
"""),

84: _("""
 erreur numeros des noeuds bords
"""),

85: _("""
 erreur: les elements supportes sont tria3 ou tria6
"""),

86: _("""
 erreur: les elements supportes sont quad4 ou quad8 ou quad9
"""),

87: _("""
 maillage mixte tria-quad non supporte pour l estimateur zz2
"""),

88: _("""
 erreur: les mailles supportees sont tria ou quad
"""),

89: _("""
 erreur: un element du maillage possede tous ses sommets sur une frontiere. il faut au moins un sommet interne.
Pour pouvoir utiliser ZZ2 il faut remailler le coin de telle facon que tous les trg aient au moins un sommet interieur.
"""),

91: _("""
 on ne trouve pas de routine te0npq npq doit etre compris entre 1 et 600 ici : npq = %(k1)s
"""),

92: _("""
  relation :  %(k1)s  non implantee sur les poulies
"""),

93: _("""
  deformation :  %(k1)s  non implantee sur les poulies
"""),

94: _("""
 l'attribut:  %(k1)s  n'existe pas pour le type:  %(k2)s
"""),

95: _("""
 erreur de programmation :
 on ne trouve pas dans les arguments de la routine calcul de champ a associer au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

96: _("""
 erreur de programmation :
 on n'a pas pu extraire toutes les cmps voulues du champ global associe au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

97: _("""
 TOUT = OUI obligatoire avec  %(k1)s
"""),

98: _("""
 on n'a pas pu récupérer le paramètre THETA dans le résultat  %(k1)s
 valeur prise pour THETA: 0.57
"""),

99: _("""
 récupération d'une valeur de THETA illicite dans le résultat  %(k1)s
 valeur prise pour THETA: 1.
"""),
}
