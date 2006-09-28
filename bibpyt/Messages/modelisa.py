#@ MODIF modelisa Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 l option de calcul d une abscisse curviligne sur un groupe de mailles n est pas implantee
"""),

2: _("""
 il est possible de definir une abscisse curviligne uniquement pour des mailles de type: poi1 ou seg2
"""),

3: _("""
 point non trouve parmi les seg2
"""),

4: _("""
 mauvaise definition pour l abs_curv. detection de plusieurs chemins. 
"""),

5: _("""
  le groupe de maille que    vous donnez ne correspond    pas au modele de structure que vous etudiez  
"""),

6: _("""
 methode au-yang : la geometrie doit etre cylindrique
"""),

7: _("""
 barre : une erreur a ete detectee lors de l affectation des valeurs dans le tampon
"""),

8: _("""
 on interdit d'avoir sur un maillage 2d des elements discrets 2d et 3d .
"""),

9: _("""
 on interdit d'avoir sur un maillage 3d des elements discrets 2d .
"""),

10: _("""
 axe_z nul
"""),

11: _("""
 noeud confondu avec l'origine
"""),

12: _("""
 orientation : une  erreur a ete detectee lors de l affectation des orientations
"""),

13: _("""
 erreur(s) dans les donnees.
"""),

14: _("""
 poutre : une erreur a ete detectee lors de l affectation des valeurs dans le tampon
"""),

15: _("""
 poutre : une  erreur a ete detectee lors des verifications des valeurs entrees
"""),

16: _("""
 si 2 caracteristiques 1 amor et 1 rigi obligatoires
"""),

17: _("""
 caracteristique  %(k1)s  non admise actuellement
"""),

18: _("""
 le noeud  %(k1)s  non modelise par un discret
"""),

19: _("""
 pas de noeuds du radier modelises par des discrets
"""),

20: _("""
 le discret  %(k1)s  n a pas le bon nombre de noeuds.
"""),

21: _("""
 le noeud  %(k1)s  extremite d un des discrets n existe pas dans la surface donnee par group_ma.
"""),

22: _("""
 nbext1.ne.nbext2
"""),

23: _("""
 nbext1.ne.nbpart
"""),

24: _("""
  gene_tuyau preciser un seul noeud par tuyau
"""),

25: _("""
 orientation : gene_tuyau le noeud doit etre une des extremites
"""),

26: _("""
  pb nmmt 
"""),

27: _("""
 on ne peut pas melanger des tuyaux a 3 et 4 noeuds pour le moment
"""),

28: _("""
 orientation : gene_tuyau un seul noeud doit etre efecte
"""),

29: _("""
 vous ne pouvez affecter des valeurs de type "poutre" au modele  %(k1)s  qui ne contient pas un seul element poutre !
"""),

30: _("""
 vous ne pouvez affecter des valeurs de type "coque" au modele  %(k1)s  qui ne contient pas un seul element coque !
"""),

31: _("""
 vous ne pouvez affectee des valeurs de type "discret" au modele  %(k1)s  qui ne contient pas un seul element discret !
"""),

32: _("""
 vous ne pouvez affecter des valeurs de type "orientation" au modele  %(k1)s  qui ne contient ni element poutre ni element discret ni element barre !
"""),

33: _("""
 vous ne pouvez affecter des valeurs de type "cable" au modele  %(k1)s  qui ne contient pas un seul element cable !
"""),

34: _("""
 vous ne pouvez affecter des valeurs de type "barre" au modele  %(k1)s  qui ne contient pas un seul element barre !
"""),

35: _("""
 vous ne pouvez affecter des valeurs de type "massif" au modele  %(k1)s  qui ne contient pas un seul element thermique ou mecanique !
"""),

36: _("""
 vous ne pouvez affecter des valeurs de type "grille" au modele  %(k1)s  qui ne contient pas un seul element grille
"""),

37: _("""
 impossible d"affecter des caracteristiques a des noeuds de ce modele car aucun noeud ne supporte un element
"""),

38: _("""
 la maille  %(k1)s  n'a pas ete affectee par des caracteristiques de poutre.
"""),

39: _("""
 la maille  %(k1)s  n'a pas ete affectee par une matrice (discret).
"""),

40: _("""
 la maille  %(k1)s  n'a pas ete affectee par des caracteristiques de cable.
"""),

41: _("""
 la maille  %(k1)s  n'a pas ete affectee par des caracteristiques de barre.
"""),

42: _("""
 la maille  %(k1)s  n'a pas ete affectee par des caracteristiques de grille.
"""),

43: _("""
 le noeud  %(k1)s  n'a pas ete affecte par une matrice.
"""),

44: _("""
 barre : occurence  %(k1)s  : "cara" :  %(k2)s  arguments maxi pour une section " %(k3)s "
"""),

45: _("""
 barre : occurence  %(k1)s  : "cara" :  4  arguments maxi pour une section " %(k2)s "
"""),

46: _("""
 barre : occurence  %(k1)s  : section " %(k2)s  argument "h" incompatible avec "hy" ou "hz"
"""),

47: _("""
 barre : occurence  %(k1)s  : section " %(k2)s  argument "hy" ou "hz" incompatible avec "h" 
"""),

48: _("""
 barre : occurence  %(k1)s  : section " %(k2)s  argument "ep" incompatible avec "epy" ou "epz"
"""),

49: _("""
 barre : occurence  %(k1)s  : section " %(k2)s  argument "epy" ou "epz" incompatible avec "ep"
"""),

50: _("""
 barre : occurence  %(k1)s  : "cara" : nombre de valeurs entrees incorrect : il en faut  %(k2)s 
"""),

51: _("""
 barre : occurence  %(k1)s  : section " %(k2)s  : valeur  %(k3)s  de "vale" non admise (valeur test interne)
"""),

52: _("""
 cable : occurence 1 : le mot cle "section" est obligatoire.
"""),

53: _("""
 coque : occurence 1 : le mot cle "epais" est obligatoire.
"""),

54: _("""
 coque : avec un excentrement, la prise en compte des termes d'inertie de rotation est obligatoire.
"""),

55: _("""
 absence d elements discrets dans le modele
"""),

56: _("""
 impossibilite, la maille  %(k1)s  doit etre une maille de type  %(k2)s , et elle est de type :  %(k3)s  pour la caracteristique  %(k4)s 
"""),

57: _("""
 orientation : occurence 1 : le mot cle "vale" est obligatoire
"""),

58: _("""
 orientation : occurence 1 : le mot cle "cara" est obligatoire
"""),

59: _("""
 orientation : occurence  %(k1)s  : presence de "vale" obligatoire si "cara" est present
"""),

60: _("""
 orientation : occurence  %(k1)s  : val :  %(k2)s  : nombre de valeurs entrees incorrect
"""),

61: _("""
 defi_arc: le rayon de courbure doit etre positif.
"""),

62: _("""
 defi_arc: il faut 3 reels pour definir le centre de courbure.
"""),

63: _("""
 defi_arc: il faut 3 reels pour definir le point de concours des tangentes.
"""),

64: _("""
 defi_arc: le coefficient de flexibilite doit etre positif.
"""),

65: _("""
 defi_arc: l'indice de contrainte doit etre positif.
"""),

66: _("""
 poutre : occurence  %(k1)s  : section "cercle", vari_sect "constant" la caracteristique "r" est obligatoire
"""),

67: _("""
 erreur de programmation
"""),

68: _("""
 mauvais calcul de nbgrel
"""),

69: _("""
 occurence  %(k1)s de "barre" (maille  %(k2)s ) ecrasement d un type de geometrie de section par un autre
"""),

70: _("""
 barre : maille  %(k1)s  : section generale : il manque la caracteristique  %(k2)s 
"""),

71: _("""
 barre : maille  %(k1)s  : section generale : la valeur de  %(k2)s  doit etre  strictement positive.
"""),

72: _("""
 barre : maille  %(k1)s  : section rectangle : il manque  la caracteristique  %(k2)s 
"""),

73: _("""
 barre : maille  %(k1)s  : section rectangle : la valeur de  %(k2)s  doit etre  strictement positive.
"""),

74: _("""
 barre : maille  %(k1)s  : section cercle : il manque  la caracteristique  %(k2)s 
"""),

75: _("""
 barre : maille  %(k1)s  : section cercle : la valeur de  %(k2)s  doit etre  strictement positive.
"""),

76: _("""
 barre : maille  %(k1)s  : section cercle : la valeur de  %(k2)s  doit etre positive.
"""),

77: _("""
 poutre : maille  %(k1)s  : section generale : il manque la caracteristique  %(k2)s 
"""),

78: _("""
 poutre : maille  %(k1)s  : section generale : element poutre de timoshenko : il manque la caracteristique  %(k2)s 
"""),

79: _("""
 poutre : maille  %(k1)s  : section rectangle : il manque  la caracteristique  %(k2)s 
"""),

80: _("""
 poutre : maille  %(k1)s  : section cercle :  il manque  la caracteristique  %(k2)s 
"""),

81: _("""
 poutre : maille  %(k1)s  : section generale : la valeur de  %(k2)s  doit etre  strictement positive
"""),

82: _("""
 poutre : maille  %(k1)s  : section rectangle : la valeur de  %(k2)s  doit etre strictement positive
"""),

83: _("""
 poutre : maille  %(k1)s  : section cercle :  la valeur de  %(k2)s  doit etre strictement positive
"""),

84: _("""
 poutre : maille  %(k1)s  : section rectangle : la valeur de  %(k2)s  ne doit pas depasser  %(k3)s /2  !!! m enfin quoi !
"""),

85: _("""
 poutre : maille  %(k1)s  : section cercle :  la valeur de  %(k2)s  ne doit pas depasser celle de  %(k3)s  !!! aarg !!
"""),

86: _("""
 section circulaire/rectangulaire non supportee par poutre/tuyau/faisceau
"""),

87: _("""
 orientation : pas d affectation d orientation du type  %(k1)s  sur la maille  %(k2)s  qui n est pas un seg2
"""),

88: _("""
 orientation : pas d affectation d orientation du type  %(k1)s sur la maille  %(k2)s  de longueur nulle
"""),

89: _("""
 orientation : pas d affectation d orientation du type  %(k1)s  sur le noeud  %(k2)s 
"""),

90: _("""
 orientation : pas d affectation d orientation du type  %(k1)s  sur la maille  %(k2)s  de longueur non nulle
"""),

91: _("""
 orientation : pas d affectation d orientation du type  %(k1)s  sur la maille  %(k2)s  qui n est pas seg2
"""),

92: _("""
 occurence  %(k1)s de "poutre" (maille  %(k2)s ) ecrasement d un type de variation de section par un autre
"""),

93: _("""
 occurence  %(k1)s de "poutre" (maille  %(k2)s ) ecrasement d un type de geometrie de section par un autre
"""),

94: _("""
 le descripteur_grandeur des deplacements ne tient pas sur dix entiers codes
"""),

95: _("""
 la carte :  %(k1)s  n'existe pas
"""),

96: _("""
  %(k1)s 
"""),

97: _("""
 tous les coef. sont nuls
"""),

98: _("""
 type de coef. inconnu: %(k1)s 
"""),

99: _("""
 les coef. de l equation sont tres petits ou tres grands et on ne peut la normaliser.
"""),
}
