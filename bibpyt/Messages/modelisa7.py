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

cata_msg={

1: _(u"""
 pas fréquentiel négatif ou nul
"""),

2: _(u"""
  la méthode est de CORCOS. on a  besoin des vecteurs directeurs VECT_X et VECT_Y de la plaque
"""),

3: _(u"""
  la méthode est de AU-YANG
  on a besoin du  vecteur de l'axe VECT_X et de l'origine ORIG_AXE du cylindre
"""),

4: _(u"""
 le type de spectre est incompatible avec la configuration étudiée
"""),

5: _(u"""
 problème rencontré lors de l'interpolation d'un interspectre
"""),

6: _(u"""
 nombre de noeuds insuffisant sur le maillage
"""),

7: _(u"""
 l'intégrale double pour le calcul de la longueur de corrélation ne converge pas.
 JM,IM  = %(i1)d , %(i2)d
 valeur finale = %(r1)f
 valeur au pas précédent = %(r2)f
 erreur relative = %(r3)f
"""),

8: _(u"""
 la liste de noms doit être de même longueur que la liste de GROUP_MA
"""),

9: _(u"""
 le GROUP_NO :  %(k1)s  existe déjà, on ne le crée donc pas.
"""),

10: _(u"""
 le nom  %(k1)s  existe déjà
"""),

11: _(u"""
 le groupe  %(k1)s  existe déjà
"""),

12: _(u"""
 Vous avez demandé l'affectation d'un modèle sur un %(k1)s,
 or le maillage %(k2)s n'en contient aucun.
 L'affectation du modèle n'est donc pas possible.
"""),

13: _(u"""
 L'interpolation par spline cubique du câble numéro %(i1)d renseigné dans
 DEFI_CABLE_BP a échoué.
 Le nombre de changements de signe de la dérivée seconde en X,Y ou Z de
 la trajectoire du câble par rapport à l'abscisse curviligne est : %(i2)d.
 Alors que le nombre de variations de la dérivée première est : %(i3)d.

 Ceci est certainement dû à des irrégularités dans le maillage.

 L'interpolation est donc faite par une méthode discrète.
"""),

15: _(u"""
 FAISCEAU_AXIAL : il y a plus de types de grilles que de grilles
"""),

16: _(u"""
 FAISCEAU_AXIAL : il faut autant d'arguments pour les opérandes <TYPE_GRILLE> et <COOR_GRILLE>
"""),

17: _(u"""
 FAISCEAU_AXIAL, opérande <TYPE_GRILLE> : détection d'une valeur illicite
"""),

18: _(u"""
 FAISCEAU_AXIAL : il faut autant d'arguments pour les opérandes <LONG_TYPG>, <LARG_TYPG>, <EPAI_TYPG>, <RUGO_TYPG>, <COEF_TRAI_TYPG> et <COEF_DPOR_TYPG>
"""),

19: _(u"""
 <FAISCEAU_TRANS> le mot clé <COUPLAGE> doit être renseigné au moins une fois sous l'une des occurrence du mot-clé facteur <FAISCEAU_TRANS>
"""),

20: _(u"""
 <FAISCEAU_TRANS> : si couplage <TYPE_PAS> , <TYPE_RESEAU> et <PAS> mots-clés obligatoires dans au moins l une des occurrences du mot-clé facteur
"""),

21: _(u"""
 FAISCEAU_TRANS : si pas de couplage <COEF_MASS_AJOU> mot-clé obligatoire dans au moins l une des occurrences du mot clé facteur <FAISCEAU_TRANS>
"""),

22: _(u"""
 <FAISCEAU_TRANS> : le mot-clé <CARA_ELEM> doit être renseigne au moins une fois dans l une des occurrences du mot-clé facteur <FAISCEAU_TRANS>
"""),

23: _(u"""
 <FAISCEAU_TRANS> : le mot-clé <PROF_RHO_F_INT> doit être renseigne au moins une fois dans l une des occurrences du mot-clé facteur <FAISCEAU_TRANS>
"""),

24: _(u"""
 <FAISCEAU_TRANS> : le mot-clé <PROF_RHO_F_EXT> doit être renseigne au moins une fois dans l une des occurrences du mot-clé facteur <FAISCEAU_TRANS>
"""),

25: _(u"""
 <FAISCEAU_TRANS> : le mot-clé <NOM_CMP> doit être renseigne au moins une fois dans l une des occurrences du mot-clé facteur <FAISCEAU_TRANS>
"""),

26: _(u"""
 grappe : si prise en compte du couplage, les mots-clés <grappe_2>, <noeud>, <CARA_ELEM>, <MODELE> et <RHO_FLUI> doivent être renseignes
"""),

27: _(u"""
 FAISCEAU_AXIAL : plusieurs occurrences pour le mot-clé facteur => faisceau équivalent => mots-clés <RAYON_TUBE> et <COOR_TUBE> obligatoires a chaque occurrence
"""),

28: _(u"""
 FAISCEAU_AXIAL : on attend un nombre pair d arguments pour le mot-clé <COOR_TUBE>. il faut fournir deux coordonnées pour définir la position de chacun des tubes du faisceau réel
"""),

29: _(u"""
 FAISCEAU_AXIAL : il faut trois composantes pour <VECT_X>
"""),

30: _(u"""
 FAISCEAU_AXIAL : le vecteur directeur du faisceau doit être l un des vecteurs unitaires de la base liée au repère global
"""),

31: _(u"""
 FAISCEAU_AXIAL : il faut 4 données pour le mot-clé <pesanteur> : la norme du vecteur et ses composantes dans le repère global, dans cet ordre
"""),

32: _(u"""
 FAISCEAU_AXIAL : il faut 3 ou 4 données pour le mot-clé <CARA_PAROI> : 3 pour une enceinte circulaire : <YC>,<ZC>,<r>. 4 pour une enceinte rectangulaire : <YC>,<ZC>,<HY>,<HZ>
"""),

33: _(u"""
 FAISCEAU_AXIAL : pour définir une enceinte, il faut autant d arguments pour les mots-clés <CARA_PAROI> et <VALE_PAROI>
"""),

34: _(u"""
 FAISCEAU_AXIAL : mot-clé <CARA_PAROI>. données incohérentes pour une enceinte circulaire
"""),

35: _(u"""
 FAISCEAU_AXIAL : valeur inacceptable pour le rayon de l enceinte circulaire
"""),

36: _(u"""
 FAISCEAU_AXIAL : mot-clé <CARA_PAROI>. données incohérentes pour une enceinte rectangulaire
"""),

37: _(u"""
 FAISCEAU_AXIAL : valeur(s) inacceptable(s) pour l une ou(et) l autre des dimensions de l enceinte rectangulaire
"""),

38: _(u"""
 FAISCEAU_AXIAL : le mot-clé <ANGL_VRIL> est obligatoire quand on définit une enceinte rectangulaire
"""),

39: _(u"""
 FAISCEAU_AXIAL : le mot-clé <VECT_X> est obligatoire si il n y a qu'une seule occurrence pour le mot-clé facteur. sinon, il doit apparaître dans au moins une des occurrences
"""),

40: _(u"""
 FAISCEAU_AXIAL : le mot-clé <PROF_RHO_FLUI> est obligatoire si il n y a qu'une seule occurrence pour le mot-clé facteur. sinon, il doit apparaître dans au moins une des occurrences
"""),

41: _(u"""
 FAISCEAU_AXIAL : le mot-clé <PROF_VISC_CINE> est obligatoire si il n y a qu'une seule occurrence pour le mot-clé facteur. sinon, il doit apparaître dans au moins une des occurrences
"""),

42: _(u"""
 FAISCEAU_AXIAL : le mot-clé <RUGO_TUBE> est obligatoire si il n y a qu'une seule occurrence pour le mot-clé facteur. sinon, il doit apparaître dans au moins une des occurrences
"""),

43: _(u"""
 FAISCEAU_AXIAL : les mots-clés <CARA_PAROI> et <VALE_PAROI> sont obligatoires si il n y a qu'une seule occurrence pour le mot-clé facteur. 
Sinon, ils doivent apparaître ensemble dans au moins une des occurrences. le mot-clé <ANGL_VRIL> doit également être présent si l'on définit une enceinte rectangulaire
"""),

44: _(u"""
 COQUE_COAX : il faut trois composantes pour <VECT_X>
"""),

45: _(u"""
 COQUE_COAX : l axe de révolution des coques doit avoir pour vecteur directeur l un des vecteurs unitaires de la base liée au repère global
"""),

46: _(u"""
 caractérisation de la topologie de la structure béton : le groupe de mailles associe ne doit contenir que des mailles 2d ou que des mailles 3d
"""),

47: _(u"""
 récupération du matériau béton : les caractéristiques matérielles n ont pas été affectées a la maille no %(k1)s  appartenant au groupe de mailles associée a la structure béton
"""),

48: _(u"""
 récupération des caractéristiques du matériau béton : absence de relation de comportement de type <BPEL_BETON> ou <ETCC_BETON>
"""),

49: _(u"""
 le calcul de la tension est fait selon BPEL. il ne peut y avoir qu'un seule jeu de données. vérifiez la cohérence du paramètre PERT_FLUA  dans les DEFI_MATERIAU
"""),

51: _(u"""
 le calcul de la tension est fait selon BPEL. il ne peut y avoir qu'un seule jeu de données. vérifiez la cohérence du paramètre PERT_RETR dans les DEFI_MATERIAU
"""),

52: _(u"""
 récupération des caractéristique du matériau béton, relation de comportement <BPEL_BETON> : au moins un paramètre indéfini
"""),

53: _(u"""
 récupération des caractéristiques du matériau béton, relation de comportement <BPEL_BETON> : au moins une valeur de paramètre invalide
"""),

54: _(u"""
 caractérisations de la topologie du câble no %(k1)s  : on a trouve une maille d un type non acceptable
"""),

55: _(u"""
 caractérisation de la topologie du câble no %(k1)s  : il existe plus de deux chemins possibles au départ du noeud  %(k2)s
"""),

56: _(u"""
 caractérisation de la topologie du câble no %(k1)s  : il n existe aucun chemin possible au départ du noeud  %(k2)s
"""),

57: _(u"""
 caractérisation de la topologie du câble no %(k1)s  : deux chemins continus possibles de  %(k2)s  a  %(k3)s  : ambiguïté
"""),

58: _(u"""
 caractérisation de la topologie du câble no %(k1)s  : aucun chemin continu valide
"""),

59: _(u"""
 interpolation de la trajectoire du câble no %(k1)s  : deux noeuds sont géométriquement confondus
"""),

60: _(u"""
 interpolation de la trajectoire du câble no %(k1)s  : détection d un point de rebroussement
"""),

61: _(u"""
Erreur utilisateur :
 Vous devez fournir le mot clé MAILLAGE pour un champ aux noeuds ou une carte.
"""),

62: _(u"""
Erreur utilisateur :
 vous devez fournir les mots clés MODELE et OPTION pour un champ élémentaire
"""),

63: _(u"""
 occurrence  %(k1)s  de  %(k2)s : impossible d affecter les valeurs demandées sur le(la) %(k3)s  qui n a pas été affecte(e) par un élément
"""),

64: _(u"""
 occurrence  %(k1)s  de  %(k2)s : impossible d affecter les valeurs demandées sur le(la)  %(k3)s  qui ne supporte pas un élément du bon type
"""),

65: _(u"""
 occurrence  %(k1)s  de  %(k2)s  : le(la) %(k3)s  ne supporte pas un élément compatible avec la caractéristique  %(k4)s
"""),

66: _(u"""
  %(k1)s  item attendu en début de ligne
"""),

67: _(u"""
  La table des fonctions de forme fournie pour le mode no. %(i1)d n'est pas valide.
"""),

68: _(u"""
  La table fournie pour le mode no. %(i1)d n'est pas une table_fonction.
"""),

69: _(u"""
  Les fonctions de forme pour le mode no. %(i1)d ne sont pas définies à partir de l'abscisse 0.
"""),

70: _(u"""
  Les fonctions de forme pour le mode no. %(i1)d ne sont pas définies sur le même intervalle.
"""),

71: _(u"""
 Les discrétisations des fonctions de forme pour les différents modes ne sont pas cohérentes.
 Le domaine de définition 0,L doivent être communs à toutes les fonctions.
"""),

72: _(u"""
 Les discrétisations des fonctions de forme pour les différents modes ne sont pas cohérentes.
 Le nombre de points de discrétisation doivent être communs à toutes les fonctions.
"""),

75: _(u"""
 le GROUP_NO  %(k1)s  ne fait pas partie du maillage :  %(k2)s
"""),

76: _(u"""
 le noeud  %(k1)s  ne fait pas partie du maillage :  %(k2)s
"""),

77: _(u"""
 le GROUP_MA  %(k1)s  ne fait pas partie du maillage :  %(k2)s
"""),

79: _(u"""
 le type  %(k1)s d'objets a vérifier n'est pas correct : il ne peut être égal qu'a GROUP_NO ou noeud ou GROUP_MA ou maille
"""),

80: _(u"""
 défaut de planéité
 l angle entre les normales aux mailles: %(k1)s  et  %(k2)s  est supérieur à ANGL_MAX.
"""),

81: _(u"""
  %(k1)s  un identificateur est attendu : " %(k2)s " n'en est pas un
"""),

82: _(u"""
  %(k1)s  un identificateur dépasse 8 caractères
"""),

83: _(u"""
  %(k1)s  le mot clé FIN n'est pas attendu
"""),

84: _(u"""
  %(k1)s  le mot clé FINSF n est pas attendu
"""),

85: _(u"""
  %(k1)s  un nombre est attendu
"""),

97: _(u"""
  -> Le GROUP_MA %(k1)s du maillage %(k2)s se retrouve vide du fait
     de l'élimination des mailles servant au collage.
     Il n'est donc pas recréé dans le maillage assemblé.
"""),

}
