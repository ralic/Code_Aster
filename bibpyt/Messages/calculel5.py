#@ MODIF calculel5 Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
 ! attention numeros d'ordre non contigus !
"""),

2: _("""
 pour les options de thermique, il y a encore a travailler !!
"""),

4: _("""
 !! probleme creation cham_elem nul dans alchml !!
"""),

5: _("""
 nb_ec trop grand
"""),

6: _("""
 cumul impossible avec i
"""),

7: _("""
 cumul impossible avec kn
"""),

8: _("""
 il faut donner "maille"
"""),

10: _("""
 nbnocp est trop grand, contacter l'assistance
"""),

11: _("""
 le parametre est a valeurs de type  " %(k1)s "  et la valeur de reference de type  " %(k2)s ".
"""),

12: _("""
 "type_test" inconnu
"""),

13: _("""
 le champ  %(k1)s  est a valeurs de type  " %(k2)s "  et la valeur de reference de type  " %(k3)s ".
"""),

14: _("""
 le champ  %(k1)s  est de type inconnu.
"""),

15: _("""
 ! type d'element  %(k1)s inconnu !
"""),

16: _("""
 ! nbnv: typelem inconnu !
"""),

17: _("""
 ! jac(ipg): div par zero !
"""),

18: _("""
 le group_no  %(k1)s  n'existe pas.
"""),

19: _("""
 le group_ma  %(k1)s  n'existe pas.
"""),

20: _("""
 le group_no  %(k1)s  contient  %(k2)s  noeuds
"""),

21: _("""
 le group_ma  %(k1)s  contient  %(k2)s  mailles
"""),

22: _("""
 ! jac(1): div par zero !
"""),

23: _("""
 ! jac(2): div par zero !
"""),

24: _("""
 ! hf: div par zero !
"""),

25: _("""
 ! calcul naret 2d: typelem inconnu !
"""),

26: _("""
 ! calcul nsomm 2d: typelem inconnu !
"""),

27: _("""
 ! pas de lumpe en 3d p2: hexa20_d --> face8_d !
"""),

28: _("""
 ! pas de lumpe en 3d p2: hexa27 --> face9_d !
"""),

29: _("""
 ! pas de lumpe en 3d p2: penta15_d --> face6/8_d !
"""),

30: _("""
 ! pas de lumpe en 3d p2: tetra10_d --> face6_d !
"""),

31: _("""
 ! calcul naret/nsomm 3d: typelem inconnu !
"""),

32: _("""
 ! l'objet chval des segments est inexistant !
"""),

33: _("""
 ! l'objet chval2 des segments est inexistant !
"""),

34: _("""
 ! p2 obligeatoire avec terme source non nul !
"""),

35: _("""
 la cmp:  %(k1)s  est en double.
"""),

36: _("""
 la cmp:  %(k1)s  n'est pas une cmp de  %(k2)s
"""),

37: _("""
 programme  %(k1)s
"""),

38: _("""
  il faut definir un champ de vitesse
"""),

39: _("""
 la grandeur pour la variable:  %(k1)s  doit etre:  %(k2)s  mais elle est:  %(k3)s
"""),

40: _("""
 nombre de sous-points incoherent avec etat initial
"""),

41: _("""
 pas de variables internes initiales pour la maille  %(k1)s
"""),

42: _("""
 comportements incompatibles :  %(k1)s  et  %(k2)s  pour la maille  %(k3)s
"""),

43: _("""
 erreur pgmeur dans zechlo : type_scalaire: %(k1)s  non autorise(r ou c),
"""),

44: _("""
 ! le champ doit etre un cham_elem !
"""),

45: _("""
 ! longueurs des modes locaux imcompatibles entre eux !
"""),

46: _("""
 ! terme normalisation global nul !
"""),

47: _("""
 le champ doit etre un cham_elem
"""),

48: _("""
 PROJ_CHAMP (ou LAISON_MAIL) :
 Le noeud : %(k1)s      est projeté sur une maille un peu distante (%(k2)s).
    distance à la maille  =   %(r1)g
    diamètre de la maille =   %(r2)g
"""),

49: _("""
 LIAISON_MAIL :
 La relation linéaire destinée à éliminer le noeud esclave %(k1)s est une tautologie
 car la maille maitre en vis à vis de ce noeud possède ce meme noeud dans sa connectivité.
 On ne l'écrit donc pas.
"""),

50: _("""
 Présence de coques orthotropes, les mots clés ANGL_REP ou VECTEUR
 du mot clé facteur REPE_COQUE ne sont pas traités.
"""),

51: _("""
 Le repère de post-traitement a été défini dans la commande AFFE_CARA_ELEM mot clé facteur COQUE.
 Il est conseillé de définir ce repère à l'aide du mot clé ANGL_REP ou VECTEUR du mot clé 
 facteur REPE_COQUE de la commande CALC_ELEM.
"""),

52: _("""
 Présence de GRILLE dans la modélisation, les mots clés ANGL_REP ou VECTEUR
 du mot clé facteur REPE_COQUE ne sont pas traités.
"""),

53: _("""
 La super_maille %(k1)s n'existe pas dans le maillage %(k2)s.
"""),

54: _("""
 La maille %(k1)s doit etre une maille de peau de type QUAD ou TRIA
 car on est en 3D et elle est de type %(k2)s.
"""),

55: _("""
 L'un des mots-cles ANGL_REP ou VECTEUR est à fournir pour l'option ARCO_ELNO_SIGM.
"""),

56: _("""
 La combinaison 'fonction multiplicatrice' et 'chargement de type fonction' n'est pas autorisée car 
 votre chargement %(k1)s contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction' 
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP).
 On poursuit sans tenir compte de la fonction multiplicatrice.
"""),

57: _("""
 La combinaison de chargements de meme type n'est pas autorisée car l'un des chargements 
 contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction' 
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP) 
"""),

58: _("""
 La combinaison de chargements de type 'déformation initiale' n'a aucun sens physique.'
"""),

59: _("""
 La combinaison de chargements de type 'pesanteur' n'a aucun sens physique.'
"""),

60: _("""
 La combinaison de chargements de type 'rotation' est déconseillée.
 Veuillez plutot utiliser un chargement de type 'force interne'.
"""),

61: _("""
 TOUT = 'OUI' obligatoire avec l'option %(k1)s , pas de calcul de champ d'erreur.
"""),

62: _("""
 Le champ de contraintes n'a pas été calculé sur tout le modèle.
 On ne calcule pas l'option %(k1)s pour le numero d'ordre %(k2)s
"""),

63: _("""
 Il faut au moins 2 numéros d'ordre pour traiter l'option %(k1)s
"""),

64: _("""
 les champs ne sont pas de la meme grandeur:  type du cham_no  %(k1)s 
   type du cham_no_affe  %(k2)s 
"""),

65: _("""
 composante non definie dans  la grandeur.  composante:  %(k1)s 
"""),

66: _("""
 
 le nombre de composantes affectees n'est pas egal  au nombre de composantes a affecter
 occurence de affe numero %(i1)d 
 nbre de cmp affectees :  %(i2)d 
 nbre de cmp a affecter :  %(i3)d 
"""),

67: _("""
 erreurs donneesle group_ma  %(k1)s 
  n'a pas le meme nombre de mailles  que le group_ma  %(k2)s 
"""),

68: _("""
 erreurs donneesle group_ma  %(k1)s 
  n'a pas les memes types de maille  que le group_ma  %(k2)s 
"""),

69: _("""
 erreurs donnees : la maille  %(k1)s  du maillage  %(k2)s 
  n'est pas la translation de la  maille  %(k3)s 
  du maillage  %(k4)s 
    vecteur translation :  %(r1)f %(r2)f %(r3)f
"""),

70: _("""
 l'instant  de calcul  %(r1)f  n'existe pas dans  %(k1)s 
"""),

71: _("""
 plusieurs numeros d'ordre trouves pour l'instant  %(r1)f 
"""),

72: _("""
 cette commande est reentrante :   sd resultat en sortie     %(k1)s 
    sd resultat "resu_final"  %(k2)s 
"""),

73: _("""
 la sd resultat en sortie  %(k1)s 
  doit contenir qu'un seul nume_ordre %(k2)s 
"""),

74: _("""
 manque le champ  %(k1)s  dans la sd resultat  %(k2)s 
  pour le nume_ordre  %(i1)d 
"""),

75: _("""
 manque le champ  %(k1)s  dans la sd resultat  %(k2)s 
  pour le nume_ordre  %(i1)d 
"""),

76: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa  :  %(k3)s 
    famille :  %(k4)s 
"""),

77: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa  :  %(k3)s 
    famille :  %(k4)s 
"""),

78: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa :  %(k3)s 
"""),

79: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa  :  %(k3)s 
    famille :  %(k4)s 
"""),

80: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa  :  %(k3)s 
    famille :  %(k4)s 
"""),

81: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa  :  %(k3)s 
    famille :  %(k4)s 
"""),

82: _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s 
    elrefa :  %(k3)s 
"""),

83: _("""
 ecla_pg : champ vide nom_cham:  %(k1)s  nume_ordre :  %(i1)d 
"""),

84: _("""
 elrefe mal programme nom local cherche (nomte elrefe famille)  %(k1)s 
 parmi les existants  %(k2)s 
"""),

85: _("""
 pb liste de mailles carte : %(k1)s  numero entite : %(i1)d 
  position ds liste : %(i2)d 
  numero de maille  : %(i3)d 
"""),

86: _("""
 pb liste de mailles carte : %(k1)s  numero entite : %(i1)d 
  position ds liste : %(i2)d 
  numero de maille  : %(i3)d 
"""),

87: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

88: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

89: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

90: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

91: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

92: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

93: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

94: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

95: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

96: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

97: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

98: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

99: _("""
 famille non disponible    maille de reference  %(k1)s 
"""),

}
