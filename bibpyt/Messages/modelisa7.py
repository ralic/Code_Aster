#@ MODIF modelisa7 Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg={

1: _(u"""
 pas frequentiel negatif ou nul
"""),

2: _(u"""
  la method est de corcos. on a  besoin des vecteurs directeurs vect_x et vect_y de la plaque
"""),

3: _(u"""
  la methode est de AU-YANG
  on a besoin du  vecteur de l'axe VECT_X et de l'origine ORIG_AXE du cylindre
"""),

4: _(u"""
 le type de spectre est incompatible avec la configuration etudiée
"""),

5: _(u"""
 probleme rencontré lors de l'interpolation d'un interspectre
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
 la liste de noms doit etre de meme longueur que la liste de GROUP_MA
"""),

9: _(u"""
 le GROUP_NO :  %(k1)s  existe deja, on ne le crée donc pas.
"""),

10: _(u"""
 le nom  %(k1)s  existe deja
"""),

11: _(u"""
 le groupe  %(k1)s  existe deja
"""),

12: _(u"""
 Vous avez demandé l'affectation d'un modèle sur un %(k1)s,
 or le maillage %(k2)s n'en contient aucun.
 L'affectation du modèle n'est donc pas possible.
"""),

15: _(u"""
 FAISCEAU_AXIAL : il y a plus de types de grilles que de grilles
"""),

16: _(u"""
 FAISCEAU_AXIAL : il faut autant d'arguments pour les operandes <TYPE_GRILLE> et <COOR_GRILLE>
"""),

17: _(u"""
 FAISCEAU_AXIAL, operande <TYPE_GRILLE> : detection d'une valeur illicite
"""),

18: _(u"""
 faisceau_axial : il faut autant d'arguments pour les operandes <LONG_TYPG>, <LARG_TYPG>, <EPAI_TYPG>, <RUGO_TYPG>, <COEF_TRAI_TYPG> et <COEF_DPOR_TYPG>
"""),

19: _(u"""
 <FAISCEAU_TRANS> le mot cle <COUPLAGE> doit etre renseigné au moins une fois sous l'une des occurence du mot-cle facteur <FAISCEAU_TRANS>
"""),

20: _(u"""
 <FAISCEAU_TRANS> : si couplage <TYPE_PAS> , <TYPE_RESEAU> et <PAS> mots-cles obligatoires dans au moins l une des occurences du mot-cle facteur
"""),

21: _(u"""
 FAISCEAU_TRANS : si pas de couplage <coef_mass_ajou> mot-cle obligatoire dans au moins l une des occurences du mot cle facteur <faisceau_trans>
"""),

22: _(u"""
 <FAISCEAU_TRANS> : le mot-cle <CARA_ELEM> doit etre renseigne au moins une fois dans l une des occurences du mot-cle facteur <faisceau_trans>
"""),

23: _(u"""
 <faisceau_trans> : le mot-cle <prof_rho_f_int> doit etre renseigne au moins une fois dans l une des occurences du mot-cle facteur <faisceau_trans>
"""),

24: _(u"""
 <faisceau_trans> : le mot-cle <prof_rho_f_ext> doit etre renseigne au moins une fois dans l une des occurences du mot-cle facteur <faisceau_trans>
"""),

25: _(u"""
 <faisceau_trans> : le mot-cle <nom_cmp> doit etre renseigne au moins une fois dans l une des occurences du mot-cle facteur <faisceau_trans>
"""),

26: _(u"""
 grappe : si prise en compte du couplage, les mots-cles <grappe_2>, <noeud>, <cara_elem>, <modele> et <rho_flui> doivent etre renseignes
"""),

27: _(u"""
 faisceau_axial : plusieurs occurences pour le mot-cle facteur => faisceau equivalent => mots-cles <rayon_tube> et <coor_tube> obligatoires a chaque occurence
"""),

28: _(u"""
 faisceau_axial : on attend un nombre pair d arguments pour le mot-cle <coor_tube>. il faut fournir deux coordonnees pour definir la position de chacun des tubes du faisceau reel
"""),

29: _(u"""
 faisceau_axial : il faut trois composantes pour <vect_x>
"""),

30: _(u"""
 faisceau_axial : le vecteur directeur du faisceau doit etre l un des vecteurs unitaires de la base liee au repere global
"""),

31: _(u"""
 faisceau_axial : il faut 4 donnees pour le mot-cle <pesanteur> : la norme du vecteur et ses composantes dans le repere global, dans cet ordre
"""),

32: _(u"""
 faisceau_axial : il faut 3 ou 4 donnees pour le mot-cle <cara_paroi> : 3 pour une enceinte circulaire : <yc>,<zc>,<r>. 4 pour une enceinte rectangulaire : <yc>,<zc>,<hy>,<hz>
"""),

33: _(u"""
 faisceau_axial : pour definir une enceinte, il faut autant d arguments pour les mots-cles <cara_paroi> et <vale_paroi>
"""),

34: _(u"""
 faisceau_axial : mot-cle <cara_paroi>. donnees incoherentes pour une enceinte circulaire
"""),

35: _(u"""
 faisceau_axial : valeur inacceptable pour le rayon de l enceinte circulaire
"""),

36: _(u"""
 faisceau_axial : mot-cle <cara_paroi>. donnees incoherentes pour une enceinte rectangulaire
"""),

37: _(u"""
 faisceau_axial : valeur(s) inacceptable(s) pour l une ou(et) l autre des dimensions de l enceinte rectangulaire
"""),

38: _(u"""
 faisceau_axial : le mot-cle <angl_vril> est obligatoire quand on definit une enceinte rectangulaire
"""),

39: _(u"""
 faisceau_axial : le mot-cle <vect_x> est obligatoire si il n y a qu une seule occurence pour le mot-cle facteur. sinon, il doit apparaitre dans au moins une des occurences
"""),

40: _(u"""
 faisceau_axial : le mot-cle <prof_rho_flui> est obligatoire si il n y a qu une seule occurence pour le mot-cle facteur. sinon, il doit apparaitre dans au moins une des occurences
"""),

41: _(u"""
 faisceau_axial : le mot-cle <prof_visc_cine> est obligatoire si il n y a qu une seule occurence pour le mot-cle facteur. sinon, il doit apparaitre dans au moins une des occurences
"""),

42: _(u"""
 faisceau_axial : le mot-cle <rugo_tube> est obligatoire si il n y a qu une seule occurence pour le mot-cle facteur. sinon, il doit apparaitre dans au moins une des occurences
"""),

43: _(u"""
 faisceau_axial : les mots-cles <cara_paroi> et <vale_paroi> sont obligatoires si il n y a qu une seule occurence pour le mot-cle facteur. sinon, ils doivent apparaitre ensemble dans au moins une des occurences. le mot-cle <angl_vril> doit egalement etre present si l on definit une enceinte rectangulaire
"""),

44: _(u"""
 coque_coax : il faut trois composantes pour <vect_x>
"""),

45: _(u"""
 coque_coax : l axe de revolution des coques doit avoir pour vecteur directeur l un des vecteurs unitaires de la base liee au repere global
"""),

46: _(u"""
 caracterisation de la topologie de la structure beton : le groupe de mailles associe ne doit contenir que des mailles 2d ou que des mailles 3d
"""),

47: _(u"""
 recuperation du materiau beton : les caracteristiques materielles n ont pas ete affectees a la maille no %(k1)s  appartenant au groupe de mailles                                 associe a la structure beton
"""),

48: _(u"""
 recuperation des caracteristiques du materiau beton : absence de relation de comportement de type <bpel_beton>
"""),

49: _(u"""
 le calcul de la tension est fait selon bpel. il ne peut y avoir qu un seule jeu de donnees. verifiez la coherence du parametre pert_flua  dans les defi_materiau
"""),

50: _(u"""
 le calcul de la tension est fait selon bpel. il ne peut y avoir qu un seul jeu de donnees. verifiez la coherence du parametre pert_flua dans les defi_materiau
"""),

51: _(u"""
 le calcul de la tension est fait selon bpel. il ne peut y avoir qu un seule jeu de donnees. verifiez la coherence du parametre pert_retr dans les defi_materiau
"""),

52: _(u"""
 recuperation des caracteristiques du materiau beton, relation de comportement <bpel_beton> : au moins un parametre indefini
"""),

53: _(u"""
 recuperation des caracteristiques du materiau beton, relation de comportement <bpel_beton> : au moins une valeur de parametre invalide
"""),

54: _(u"""
 caracterisation de la topologie du cable no %(k1)s  : on a trouve une maille d un type non acceptable
"""),

55: _(u"""
 caracterisation de la topologie du cable no %(k1)s  : il existe plus de deux chemins possibles au depart du noeud  %(k2)s
"""),

56: _(u"""
 caracterisation de la topologie du cable no %(k1)s  : il n existe aucun chemin possible au depart du noeud  %(k2)s
"""),

57: _(u"""
 caracterisation de la topologie du cable no %(k1)s  : deux chemins continus possibles de  %(k2)s  a  %(k3)s  : ambiguite
"""),

58: _(u"""
 caracterisation de la topologie du cable no %(k1)s  : aucun chemin continu valide
"""),

59: _(u"""
 interpolation de la trajectoire du cable no %(k1)s  : deux noeuds sont geometriquement confondus
"""),

60: _(u"""
 interpolation de la trajectoire du cable no %(k1)s  : detection d un point de rebroussement
"""),

61: _(u"""
Erreur utilisateur :
 Vous devez fournir le mot cle MAILLAGE pour un champ aux noeuds ou une carte.
"""),

62: _(u"""
Erreur utilisateur :
 vous devez fournir les mots cles MODELE et OPTION pour un champ élementaire
"""),

63: _(u"""
 occurence  %(k1)s  de  %(k2)s : impossible d affecter les valeurs demandees sur le(la) %(k3)s  qui n a pas ete affecte(e) par un element
"""),

64: _(u"""
 occurence  %(k1)s  de  %(k2)s : impossible d affecter les valeurs demandees sur le(la)  %(k3)s  qui ne supporte pas un element du bon type
"""),

65: _(u"""
 occurence  %(k1)s  de  %(k2)s  : le(la) %(k3)s  ne supporte pas un element compatible avec la caracteristique  %(k4)s
"""),

66: _(u"""
  %(k1)s  item attendu en debut de ligne
"""),

67: _(u"""
 dicretisation insuffisante pour une des fonctions
"""),

68: _(u"""
 les fonctions de forme doivent etre definies sur un intervalle commun 0,2l ou l est la longueur excitee. la discretisation doit etre strictement croissante avec une exception pour le parametre l qui doit etre repete.
"""),

69: _(u"""
 discretisation des fonctions de forme : un seul parametre doit etre repete, correspondant a la longueur excitee l
"""),

70: _(u"""
 discretisation des fonctions de forme : le parametre correspondant a la longueur excitee l doit etre repete.
"""),

71: _(u"""
 absence de discretisation sur 0,l ou l,2l pour une des fonctions.
"""),

72: _(u"""
 discretisation des fonctions de forme : le parametre repete doit correspondre a la longueur excitee l
"""),

73: _(u"""
 les discretisations des fonctions de forme ne sont pas coherentes. le domaine de definition 0,2l et la longueur excitee l qui en est deduite doivent etre communs a toutes les fonctions.
"""),

74: _(u"""
 les discrétisations des fonctions de forme ne sont pas cohérentes
 les nombres de points sur 0,l et sur l,2l doivent etre communs à toutes les fonctions.
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
 le type  %(k1)s d'objets a verifier n'est pas correct : il ne peut etre egal qu'a group_no ou noeud ou group_ma ou maille
"""),

80: _(u"""
 défaut de planéité
 l angle entre les normales aux mailles: %(k1)s  et  %(k2)s  est supérieur à ANGL_MAX.
"""),

81: _(u"""
  %(k1)s  un identificateur est attendu : " %(k2)s " n'en est pas un
"""),

82: _(u"""
  %(k1)s  un identificateur depasse 8 caractères
"""),

83: _(u"""
  %(k1)s  le mot cle FIN n'est pas attendu
"""),

84: _(u"""
  %(k1)s  le mot cle FINSF n est pas attendu
"""),

85: _(u"""
  %(k1)s  un nombre est attendu
"""),

86: _(u"""
 la maille de nom :  %(k1)s  n'est pas de type SEGMENT
 elle ne sera pas affectée par  %(k2)s
"""),

87: _(u"""
 la maille de nom :  %(k1)s  n'est pas de type TRIA ou QUAD
 elle ne sera pas affectee par  %(k2)s
"""),

88: _(u"""
  -> Erreur dans les mailles du mot-clé facteur %(k1)s :
     aucune maille n'est du bon type. Elles sont toutes ignorées.
"""),

89: _(u"""
 la maille de numéro :  %(k1)s  n'est pas de type SEGMENT
 elle ne sera pas affectée par  %(k2)s
"""),

90: _(u"""
 la maille de numéro :  %(k1)s  n'est pas de type TRIA ou QUAD
 elle ne sera pas affectée par  %(k2)s
"""),

91: _(u"""
 erreur dans les noms de maille du GROUP_MA:  %(k1)s  du mot-cle facteur  %(k2)s
 aucune maille n'est du bon type
"""),

92: _(u"""
 la maille de nom :  %(k1)s  n'est pas une maille 3d, elle ne sera pas affectée par  %(k2)s
"""),

93: _(u"""
 la maille de numéro :  %(k1)s  n'est pas une maille 3d, elle ne sera pas affectee par  %(k2)s
"""),

97: _(u"""
  -> Le group_ma %(k1)s du maillage %(k2)s se retrouve vide du fait
     de l'élimination des mailles servant au collage.
     Il n'est donc pas recréé dans le maillage assemblé.
"""),

99: _(u"""
 Aucun groupe de fibres n'a de comportement.
"""),

}
