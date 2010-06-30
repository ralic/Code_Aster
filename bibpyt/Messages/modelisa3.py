#@ MODIF modelisa3 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={

1: _("""
 on doit donner un cham_no apres le mot cle cham_no derriere le mot facteur liaison_chamno .
"""),

2: _("""
 il faut definir la valeur du second membre de la relation lineaire apres le mot cle coef_impo derriere le mot facteur liaison_chamno .
"""),

3: _("""
 la premiere liste de noeuds dont on doit faire le vis a vis n'existe pas.
"""),

4: _("""
 la seconde liste de noeuds dont on doit faire le vis a vis n'existe pas.
"""),

5: _("""
 la premiere liste de noeuds dont on doit faire le vis a vis est vide.
"""),

6: _("""
 la seconde liste de noeuds dont on doit faire le vis a vis est vide.
"""),

7: _("""
 impossibilite de faire le vis a vis des 2 listes de noeuds, elles n'ont pas le meme nombre de noeuds apres elimination des doublons.
"""),

8: _("""
 Erreur utilisateur dans CREA_MAILLAGE / QUAD_LINE :
  Vous avez demandé de transformer des mailles quadratiques en mailles linéaires.
  Mais il n'y a aucune maille quadratique à transformer.
"""),

9: _("""
 le mot cle "tran" sous le mot cle facteur %(k1)s  n"admet que 3 valeurs
"""),

10: _("""
 le mot cle "angl_naut" sous le mot cle facteur %(k1)s  n"admet que 3 valeurs
"""),

11: _("""
 le mot cle "centre" sous le mot cle facteur %(k1)s  n"admet que 3 valeurs
"""),

12: _("""
  Mot clé LIAISON_GROUP : les mots clés %(k1)s et %(k2)s à mettre 
  en vis-à-vis n'ont pas le meme nombre de noeuds.
  
   - Nombre de noeuds présent sous le mot clé %(k1)s: %(i1)d
   - Nombre de noeuds présent sous le mot clé %(k2)s: %(i2)d

"""),

13: _("""
 il n'y a aucun groupe de noeuds ni aucun noeud defini apres le mot facteur  %(k1)s 
"""),

14: _("""
 le mot cle "tran" sous le mot cle facteur  %(k1)s  n'admet que  %(k2)s  valeurs
"""),

15: _("""
 le mot cle "angl_naut" sous le mot cle facteur  %(k1)s  n'admet que  %(k2)s  valeurs
"""),

16: _("""
 le mot cle "centre" sous le mot cle facteur  %(k1)s  n'admet que  %(k2)s  valeurs
"""),

17: _("""
 attention, la liste des noeuds est reduite a un seul terme et l'on ne fait aucun traitement
"""),

18: _("""
 la table "cara_geom" n'existe pas dans le maillage
"""),

19: _("""
 mailles mal orientees
"""),

20: _("""
 pour les seg en 3d, il faut renseigner vect_orie_pou
"""),

21: _("""
 pas de normale pour les tria en 2d
"""),

22: _("""
 pas de normale pour les quad en 2d
"""),

23: _("""
 il est impossible de                    calculer la tangente de la maille  %(k1)s . des noeuds doivent etre confondus.
"""),

24: _("""
 il est impossible de                    calculer la normale de la maille  %(k1)s . des noeuds doivent etre confondus.
"""),

25: _("""
 impossible de calculer la normale d un segment en 3d
"""),

26: _("""
 il est impossible de                    calculer la normale de la maille  %(k1)s . des aretes doivent etre confondues.
"""),

27: _("""
 type d element inconnu
"""),

28: _("""
 la norme du vecteur normal ( ou tangentiel) moyenne est presque nulle. les facettes concourantes  au noeud  %(k1)s  ne definissent pas une normale moyenne fiable . il y a un probleme dans la definition de vos mailles de bord .
"""),

29: _("""
 L'angle forme par le vecteur normal courant a 1 face et le vecteur normal moyen, au noeud  %(k1)s , est superieur a 10 degres et vaut  %(k2)s  degres.
"""),

30: _("""
Erreur d'utilisation :
 La norme du vecteur normal (moyenne des normales des éléments concourants) est presque nulle.
 Les facettes concourantes au noeud  %(k1)s ne definissent pas une normale fiable.
 Il y a un probleme dans la definition des mailles de bord .

Suggestion :
 Avez-vous pensé à réorienter les mailles de bord avec l'operateur MODI_MAILLAGE
"""),

31: _("""
 l'angle forme par le vecteur normal courant a 1 face et le vecteur normal moyenne, au noeud  %(k1)s , est superieur a 10 degres et vaut  %(k2)s  degres.
"""),

32: _("""
 Erreur utilisateur dans CREA_MAILLAGE/MODI_MAILLE :
  Vous avez demandé la transformation de type %(k1)s.
  Mais il n'y a aucune maille quadratique à transformer.
"""),

35: _("""
 probleme pour determiner le rang de la composante <n> de la grandeur <sief_r>
"""),

36: _("""
 le concept  %(k1)s  n est pas un concept de type cabl_precont
"""),

37: _("""
 erreur a l appel de la routine etenca pour extension de la carte  %(k1)s 
"""),






44: _("""
 Erreur utilisateur dans CREA_MAILLAGE / LINE_QUAD :
  Vous avez demandé de transformer des mailles linéaires en mailles quadratiques.
  Mais il n'y a aucune maille linéaire à transformer.
"""),

60: _("""
 on ne donne le mot facteur "convection" qu"une fois au maximum
"""),

61: _("""
 le type s est interdit en 3d
"""),

62: _("""
 les types sv ou sh sont interdits en 2d
"""),

63: _("""
 donner un vecteur non nul
"""),

64: _("""
 nombre d occurence du mot cle "sour_calculee"  superieur a 1
"""),






66: _("""
 la dimension du maillage ne correspond pas a la dimension des elements
"""),

67: _("""
 on doit utiliser obligatoirement le mot-cle dist pour definir la demi-largeur de bande. 
"""),

68: _("""
 on doit donner une distance strictement positive pour definir la bande. 
"""),

69: _("""
 on doit utiliser obligatoirement le mot-cle angl_naut ou le mot-cle vect_normale pour l'option bande de crea_group_ma. ce mot-cle permet de definir la direction perpendiculaire au plan milieu de la bande. 
"""),

70: _("""
 pour l'option bande de crea_group_ma, il faut  definir les 3 composantes du vecteur perpendiculaire au plan milieu de la  bande quand on est en 3d.
"""),

71: _("""
 pour l'option bande de crea_group_ma, il faut  definir les 2 composantes du vecteur perpendiculaire au plan milieu de la  bande quand on est en 2d.
"""),

72: _("""
 erreur dans la donnee du vecteur normal au plan milieu de la  bande : ce vecteur est nul.
"""),

73: _("""
 l'option cylindre de crea_group_ma n'est utilisable qu'en 3d.
"""),

74: _("""
 on doit utiliser obligatoirement le mot-cle rayon pour definir le rayon du cylindre. 
"""),

75: _("""
 on doit donner un rayon strictement positif pour definir la cylindre. 
"""),

76: _("""
 on doit utiliser obligatoirement le mot-cle angl_naut ou le mot-cle vect_normale pour l'option cylindre de crea_group_ma
"""),

77: _("""
 pour l'option cylindre de crea_group_ma, il faut  definir les 3 composantes du vecteur orientant l'axe du cylindre quand on utilise le mot cle vect_normale.
"""),

78: _("""
 pour l'option cylindre de crea_group_ma, il faut definir les 2 angles nautiques quand on utilise le mot cle "angl_naut".
"""),

79: _("""
 erreur dans la donnee du vecteur orientant l'axe du cylindre,ce vecteur est nul.
"""),

80: _("""
 on doit utiliser obligatoirement le mot-cle angl_naut ou le mot-cle vect_normale pour l'option face_normale de crea_group_ma
"""),

81: _("""
 erreur dans la donnee du vecteur normal selon lequel on veut selectionner des mailles : ce vecteur est nul.
"""),

82: _("""
 on doit utiliser obligatoirement le mot-cle rayon pour definir le rayon de la sphere. 
"""),

83: _("""
 on doit donner un rayon strictement positif pour definir la sphere. 
"""),

84: _("""
 l'option env_cylindre de crea_group_no n'est utilisable qu'en 3d.
"""),

85: _("""
 on doit utiliser obligatoirement le mot-cle angl_naut ou le mot-cle vect_normale pour l'option env_cylindre de crea_group_no
"""),

86: _("""
 pour l'option env_cylindre de crea_group_no, il faut definir les 3 composantes du vecteur orientant l'axe du cylindre quand on utilise le mot cle "vect_normale".
"""),

87: _("""
 pour l'option env_cylindre de crea_group_no, il faut definir les 2 angles nautiques quand on utilise le mot cle "angl_naut".
"""),

88: _("""
 le mot-cle precision est obligatoire apres le mot-cle env_cyli pour definir la tolerance (i.e. la distance du point a l'enveloppe du cylindre) acceptee pour declarer l'appartenance du point a cette enveloppe.
"""),

89: _("""
 on doit donner une demi-epaisseur strictement positive definir l'enveloppe du cylindre. 
"""),

90: _("""
 le mot-cle precision est obligatoire apres le mot-cle env_sphe pour definir la tolerance (i.e. la distance du point a l'enveloppe de la sphere) acceptee pour declarer l'appartenance du point a cette enveloppe.
"""),

91: _("""
 on doit donner une demi-epaisseur strictement positive definir l'enveloppe de la sphere. 
"""),

92: _("""
 erreur dans la donnee du vecteur orientant l'axe d'un segment ,ce vecteur est nul.
"""),

93: _("""
 on doit utiliser obligatoirement le mot-cle angl_naut ou le mot-cle vect_normale pour l'option plan de crea_group_no. ce mot-cle permet de definir la direction  perpendiculaire au plan ou a la droite. 
"""),

94: _("""
 pour l'option plan de crea_group_no, il faut  definir les 3 composantes du vecteur perpendiculaire au plan.
"""),

95: _("""
 pour l'option plan de crea_group_no, il faut  definir les 2 composantes du vecteur perpendiculaire a la droite.
"""),

96: _("""
 erreur dans la donnee du vecteur normal au plan ou a la droite : ce vecteur est nul.
"""),

97: _("""
 le mot-cle precision est obligatoire apres le mot-cle plan  pour definir la tolerance (i.e. la distance du point au plan ou a la droite) acceptee pour declarer l'appartenance du point a ce plan ou a cette droite.
"""),

98: _("""
 on doit donner une tolerance strictement positive pour verifier l'appartenance d'un noeud au plan ou a la droite. 
"""),

99: _("""
 il manque l'ensemble des noeuds que l'on veut ordonner, mots cles "noeud" et/ou "group_no"
"""),
}
