#@ MODIF modelisa Messages  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
 l'option de calcul d'une abscisse curviligne sur un groupe de mailles
 n'est pas implantée
"""),

2 : _("""
 il est possible de définir une abscisse curviligne uniquement
 pour des mailles de type: POI1 ou SEG2
"""),

3 : _("""
 point non trouvé parmi les SEG2
"""),

4 : _("""
 mauvaise définition pour l'abscisse curviligne
 détection de plusieurs chemins. 
"""),

5 : _("""
  le groupe de maille que vous donnez ne correspond pas
  au modèle de structure que vous étudiez  
"""),

6 : _("""
 methode AU-YANG : la géometrie doit etre cylindrique
"""),

7 : _("""
 BARRE : une erreur a été détectée lors de l'affectation des valeurs dans le tampon
"""),

8 : _("""
 on interdit d'avoir sur un maillage 2D des éléments discrets 2D et 3D
"""),

9 : _("""
 on interdit d'avoir sur un maillage 3D des éléments discrets 2D
"""),

10 : _("""
 la norme de l'axe définie sous le mot clé facteur GRILLE/AXE est nul.
"""),

11 : _("""
 noeud confondu avec l'origine
"""),

12 : _("""
 orientation : une erreur a ete détectée lors de l'affectation des orientations
"""),

13 : _("""
 erreur(s) dans les données.
"""),

14 : _("""
 POUTRE : une erreur a été détectée lors de l'affectation des valeurs dans le tampon
"""),

15 : _("""
 poutre : une  erreur a ete detectee lors des verifications des valeurs entrees
"""),

16 : _("""
 vous fournissez deux caractéristiques élémentaires. Il est obligatoire de fournir une caractéristique
 relative à l'amortissement et une caractéristique relative à la rigidité
"""),

17 : _("""
 caractéristique  %(k1)s  non admise actuellement
"""),

18 : _("""
 le noeud  %(k1)s  non modelisé par un discret
"""),

19 : _("""
 pas de noeuds du Radier modélisés par des discrets
"""),

20 : _("""
 le discret  %(k1)s  n'a pas le bon nombre de noeuds.
"""),

21 : _("""
 le noeud  %(k1)s  éxtremité d'un des discrets n'existe pas dans la surface donnée par GROUP_MA.
"""),









24 : _("""
  GENE_TUYAU : préciser un seul noeud par tuyau
"""),

25 : _("""
 ORIENTATION : GENE_TUYAU
 le noeud doit etre une des extremités
"""),

26 : _("""
  Il y a un probleme lors de l'affectation du mot clé MODI_METRIQUE sur la maille %(k1)s 
"""),

27 : _("""
 on ne peut pas mélanger des tuyaux à 3 et 4 noeuds pour le moment
"""),

28 : _("""
 ORIENTATION : GENE_TUYAU
 un seul noeud doit etre affecté
"""),

29 : _("""
 vous ne pouvez affecter des valeurs de type "POUTRE" au modèle  %(k1)s
 qui ne contient pas un seul élément poutre
"""),

30 : _("""
 vous ne pouvez affecter des valeurs de type "COQUE" au modèle  %(k1)s
 qui ne contient pas un seul élément coque
"""),

31 : _("""
 vous ne pouvez affecter des valeurs de type "DISCRET" au modèle  %(k1)s
 qui ne contient pas un seul élément discret
"""),

32 : _("""
 vous ne pouvez affecter des valeurs de type "ORIENTATION" au modèle  %(k1)s
 qui ne contient ni element poutre ni élément DISCRET ni élément BARRE
"""),

33 : _("""
 vous ne pouvez affecter des valeurs de type "CABLE" au modèle  %(k1)s
 qui ne contient pas un seul élément CABLE
"""),

34 : _("""
 vous ne pouvez affecter des valeurs de type "BARRE" au modèle  %(k1)s
 qui ne contient pas un seul élément BARRE
"""),

35 : _("""
 vous ne pouvez affecter des valeurs de type "MASSIF" au modèle  %(k1)s
 qui ne contient pas un seul élément thermique ou mécanique
"""),

36 : _("""
 vous ne pouvez affecter des valeurs de type "GRILLE" au modèle  %(k1)s
 qui ne contient pas un seul élément GRILLE
"""),

37 : _("""
 impossible d'affecter des caractéristiques à des noeuds de ce modèle
 car aucun noeud ne supporte un élément
"""),

38 : _("""
 la maille  %(k1)s  n'a pas été affectée par des caractéristiques de poutre.
"""),

39 : _("""
 la maille  %(k1)s  n'a pas ete afféctée par une matrice (DISCRET).
"""),

40 : _("""
 la maille  %(k1)s  n'a pas ete affectée par des caractéristiques de cable.
"""),

41 : _("""
 la maille  %(k1)s  n'a pas ete affectée par des caractéristiques de barre.
"""),

42 : _("""
 la maille  %(k1)s  n'a pas ete affectée par des caractéristiques de grille.
"""),

43 : _("""
 le noeud  %(k1)s  n'a pas ete affecté par une matrice.
"""),

44 : _("""
 BARRE :
 occurence :  %(k1)s
 "CARA"    :  %(k2)s
 arguments maximums pour une section " %(k3)s "
"""),

45 : _("""
 BARRE :
 occurence  %(k1)s
 "cara"   :  4
 arguments maximums pour une section " %(k2)s "
"""),

46 : _("""
 BARRE :
 occurence  %(k1)s
 section " %(k2)s
 argument "h" incompatible avec "hy" ou "hz"
"""),

47 : _("""
 barre :
 occurence  %(k1)s
 section " %(k2)s
 argument "hy" ou "hz" incompatible avec "h" 
"""),

48 : _("""
 barre :
 occurence  %(k1)s
 section " %(k2)s  argument "ep" incompatible avec "epy" ou "epz"
"""),

49 : _("""
 barre :
 occurence  %(k1)s
 section " %(k2)s
 argument "epy" ou "epz" incompatible avec "ep"
"""),

50 : _("""
 barre :
 occurence  %(k1)s
 "cara" : nombre de valeurs entrees incorrect
 il en faut  %(k2)s 
"""),

51 : _("""
 barre :
 occurence  %(k1)s
 section " %(k2)s
 valeur  %(k3)s  de "vale" non admise (valeur test interne)
"""),

52 : _("""
 cable :
 occurence 1
 le mot cle "section" est obligatoire.
"""),

53 : _("""
 coque :
 occurence 1
 le mot cle "epais" est obligatoire.
"""),

54 : _("""
 coque : avec un excentrement, la prise en compte des termes d'inertie de rotation est obligatoire.
"""),

55 : _("""
 absence d elements discrets dans le modele
"""),

56 : _("""
 impossibilite, la maille  %(k1)s  doit etre une maille de type  %(k2)s , et elle est de type :  %(k3)s  pour la caracteristique  %(k4)s 
"""),

57 : _("""
 orientation :
 occurence 1
 le mot cle "vale" est obligatoire
"""),

58 : _("""
 orientation :
 occurence 1
 le mot cle "cara" est obligatoire
"""),

59 : _("""
 orientation :
 occurence  %(k1)s
 presence de "vale" obligatoire si "cara" est present
"""),

60 : _("""
 orientation :
 occurence  %(k1)s
 val :  %(k2)s
 nombre de valeurs entrees incorrect
"""),

61 : _("""
 defi_arc:
 le rayon de courbure doit etre positif.
"""),

62 : _("""
 defi_arc:
 il faut 3 reels pour definir le centre de courbure.
"""),

63 : _("""
 defi_arc:
 il faut 3 reels pour definir le point de concours des tangentes.
"""),

64 : _("""
 defi_arc:
 le coefficient de flexibilite doit etre positif.
"""),

65 : _("""
 defi_arc: l'indice de contrainte doit etre positif.
"""),

66 : _("""
 poutre :
 occurence  %(k1)s
 section "cercle", vari_sect "constant" la caracteristique "r" est obligatoire
"""),







69 : _("""
 occurence  %(k1)s de "barre" (maille  %(k2)s ) ecrasement d un type de geometrie de section par un autre
"""),

70 : _("""
 barre :
 maille  %(k1)s
 section generale
 il manque la caracteristique  %(k2)s 
"""),

71 : _("""
 barre :
 maille  %(k1)s
 section generale
 la valeur de  %(k2)s  doit etre  strictement positive.
"""),

72 : _("""
 barre :
 maille  %(k1)s
 section rectangle
 il manque  la caracteristique  %(k2)s 
"""),

73 : _("""
 barre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  doit etre  strictement positive.
"""),

74 : _("""
 barre :
 maille  %(k1)s
 section cercle
 il manque  la caracteristique  %(k2)s 
"""),

75 : _("""
 barre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit etre  strictement positive.
"""),

76 : _("""
 barre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit etre positive.
"""),

77 : _("""
 poutre :
 maille  %(k1)s
 section generale
 il manque la caracteristique  %(k2)s 
"""),

78 : _("""
 poutre :
 maille  %(k1)s
 section generale
 element poutre de timoshenko : il manque la caracteristique  %(k2)s 
"""),

79 : _("""
 poutre :
 maille  %(k1)s
 section rectangle
 il manque  la caracteristique  %(k2)s 
"""),

80 : _("""
 poutre :
 maille  %(k1)s
 section cercle
 il manque la caracteristique  %(k2)s 
"""),

81 : _("""
 poutre :
 maille  %(k1)s
 section générale
 la valeur de  %(k2)s  doit etre strictement positive
"""),

82 : _("""
 poutre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  doit etre strictement positive
"""),

83 : _("""
 poutre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  doit etre strictement positive
"""),

84 : _("""
 poutre :
 maille  %(k1)s
 section rectangle
 la valeur de  %(k2)s  ne doit pas dépasser  %(k3)s /2
"""),

85 : _("""
 poutre :
 maille  %(k1)s
 section cercle
 la valeur de  %(k2)s  ne doit pas dépasser celle de  %(k3)s
"""),

86 : _("""
 section CIRCULAIRE/RECTANGULAIRE non supportée par POUTRE/TUYAU/FAISCEAU
"""),

87 : _("""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s  sur la maille  %(k2)s
 qui n est pas un SEG2
"""),

88 : _("""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s sur la maille  %(k2)s
 de longueur nulle
"""),

89 : _("""
 orientation :
 pas d affectation d orientation du type  %(k1)s  sur le noeud  %(k2)s 
"""),

90 : _("""
 orientation :
 pas d'affectation d'orientation du type  %(k1)s  sur la maille  %(k2)s
 de longueur non nulle
"""),

91 : _("""
 orientation :
 pas d affectation d orientation du type  %(k1)s  sur la maille  %(k2)s
 qui n est pas SEG2
"""),

92 : _("""
 occurence  %(k1)s de "poutre" (maille  %(k2)s )
 écrasement d'un type de variation de section par un autre
"""),

93 : _("""
 occurence  %(k1)s de "poutre" (maille  %(k2)s )
 écrasement d'un type de géometrie de section par un autre
"""),

94 : _("""
 le DESCRIPTEUR_GRANDEUR des déplacements ne tient pas sur dix entiers codés
"""),

95 : _("""
 la carte :  %(k1)s  n'existe pas
"""),

97 : _("""
 tous les coefficients sont nuls
"""),

98 : _("""
 type de coefficient inconnu: %(k1)s 
"""),

}
