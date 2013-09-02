# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
 on doit donner un CHAM_NO après le mot clé CHAM_NO derrière le mot facteur LIAISON_CHAMNO .
"""),

2: _(u"""
 il faut définir la valeur du second membre de la relation linéaire après le mot clé COEF_IMPO derrière le mot facteur LIAISON_CHAMNO .
"""),

8: _(u"""
 Erreur utilisateur dans CREA_MAILLAGE / QUAD_LINE :
  Vous avez demandé de transformer des mailles quadratiques en mailles linéaires.
  Mais il n'y a aucune maille quadratique à transformer.
"""),

9: _(u"""
 le mot clé "TRAN" sous le mot clé facteur %(k1)s  n"admet que 3 valeurs
"""),

10: _(u"""
 le mot clé "ANGL_NAUT" sous le mot clé facteur %(k1)s  n"admet que 3 valeurs
"""),

11: _(u"""
 le mot clé "centre" sous le mot clé facteur %(k1)s  n"admet que 3 valeurs
"""),

12: _(u"""
  Mot clé LIAISON_GROUP : les mots clés %(k1)s et %(k2)s à mettre
  en vis-à-vis n'ont pas le même nombre de noeuds.

   - Nombre de noeuds présent sous le mot clé %(k1)s: %(i1)d
   - Nombre de noeuds présent sous le mot clé %(k2)s: %(i2)d

"""),

13: _(u"""
 Il n'y a aucun groupe de noeuds ni aucun noeud défini après le mot facteur  %(k1)s
"""),



18: _(u"""
 la table "CARA_GEOM" n'existe pas dans le maillage
"""),

19: _(u"""
 mailles mal orientées
"""),

20: _(u"""
 pour les segments en 3d, il faut renseigner VECT_ORIE_pou
"""),

21: _(u"""
 pas de normale pour les tria en 2d
"""),

22: _(u"""
 pas de normale pour les quadrangles en 2d
"""),





32: _(u"""
 Alarme utilisateur dans CREA_MAILLAGE/MODI_MAILLE :
  Occurrence du mot clé facteur MODI_MAILLE : %(i1)d.
  Vous avez demandé la transformation de type %(k1)s.
  Mais il n'y a aucune maille à transformer.
"""),


37: _(u"""
 erreur a l appel de la routine etenca pour extension de la carte  %(k1)s
"""),

38: _(u"""
 Le concept issu de DEFI_CABLE_BP donné en entrée de AFFE_CHAR_MECA/RELA_CINE_BP
 a son mot-clé ADHERENT égal à 'NON'.
 Il est interdit de renseigner SIGM_BPEL = 'OUI' dans ce cas.
"""),

39: _(u"""
 Cas ADHERENT = 'NON' :
 Attention le profil de tension calculé dans DEFI_CABLE_BP ne sera pas utilisé si vous poursuivez le calcul avec CALC_PRECONT.
 Les paramètres des lois BPEL_**** ou ETCC_**** ne sont donc pas pris en compte lors de la mise en tension.
 Les coefficients de frottement considérés sont ceux de la loi CABLE_GAINE_FROT.

"""),

44: _(u"""
 Erreur utilisateur dans CREA_MAILLAGE / LINE_QUAD :
  Vous avez demandé de transformer des mailles linéaires en mailles quadratiques.
  Mais il n'y a aucune maille linéaire à transformer.
"""),

60: _(u"""
 on ne donne le mot facteur "CONVECTION" qu'une fois au maximum
"""),




64: _(u"""
 nombre d occurrence du mot clé "SOUR_CALCULEE"  supérieur a 1
"""),


66: _(u"""
 la dimension du maillage ne correspond pas à la dimension des éléments
"""),

67: _(u"""
 on doit utiliser obligatoirement le mot-clé DIST pour définir la demi largeur de bande.
"""),

68: _(u"""
 on doit donner une distance strictement positive pour définir la bande.
"""),

69: _(u"""
 on doit utiliser obligatoirement le mot-clé ANGL_NAUT ou le mot-clé VECT_NORMALE pour l'option bande de CREA_GROUP_MA. ce mot-clé permet de définir la direction perpendiculaire au plan milieu de la bande.
"""),

70: _(u"""
 pour l'option bande de CREA_GROUP_MA, il faut  définir les 3 composantes du vecteur perpendiculaire au plan milieu de la  bande quand on est en 3d.
"""),

71: _(u"""
 pour l'option bande de CREA_GROUP_MA, il faut  définir les 2 composantes du vecteur perpendiculaire au plan milieu de la  bande quand on est en 2d.
"""),

72: _(u"""
 erreur dans la donnée du vecteur normal au plan milieu de la  bande : ce vecteur est nul.
"""),

73: _(u"""
 l'option cylindre de CREA_GROUP_MA n'est utilisable qu'en 3d.
"""),

74: _(u"""
 on doit utiliser obligatoirement le mot-clé rayon pour définir le rayon du cylindre.
"""),

75: _(u"""
 on doit donner un rayon strictement positif pour définir la cylindre.
"""),

76: _(u"""
 on doit utiliser obligatoirement le mot-clé ANGL_NAUT ou le mot-clé VECT_NORMALE pour l'option cylindre de CREA_GROUP_MA
"""),

77: _(u"""
 pour l'option cylindre de CREA_GROUP_MA, il faut  définir les 3 composantes du vecteur orientant l'axe du cylindre quand on utilise le mot-clé VECT_NORMALE.
"""),

78: _(u"""
 pour l'option cylindre de CREA_GROUP_MA, il faut définir les 2 angles nautiques quand on utilise le mot-clé "ANGL_NAUT".
"""),

79: _(u"""
 erreur dans la donnée du vecteur orientant l'axe du cylindre,ce vecteur est nul.
"""),

80: _(u"""
 on doit utiliser obligatoirement le mots-clés ANGL_NAUT ou le mot-clé VECT_NORMALE pour l'option FACE_NORMALE de CREA_GROUP_MA
"""),

81: _(u"""
 erreur dans la donnée du vecteur normal selon lequel on veut sélectionner des mailles : ce vecteur est nul.
"""),

82: _(u"""
 on doit utiliser obligatoirement le mot-clé rayon pour définir le rayon de la sphère.
"""),

83: _(u"""
 on doit donner un rayon strictement positif pour définir la sphère.
"""),

84: _(u"""
 l'option ENV_CYLINDRE de CREA_GROUP_NO n'est utilisable qu'en 3d.
"""),

85: _(u"""
 on doit utiliser obligatoirement le mot-clé ANGL_NAUT ou le mot-clé VECT_NORMALE pour l'option ENV_CYLINDRE de CREA_GROUP_NO
"""),

86: _(u"""
 pour l'option ENV_CYLINDRE de CREA_GROUP_NO, il faut définir les 3 composantes du vecteur orientant l'axe du cylindre quand on utilise le mot clé "VECT_NORMALE".
"""),

87: _(u"""
 pour l'option ENV_CYLINDRE de CREA_GROUP_NO, il faut définir les 2 angles nautiques quand on utilise le mot clé "ANGL_NAUT".
"""),

88: _(u"""
 le mot-clé précision est obligatoire après le mot-clé ENV_CYLINDRE pour définir la tolérance (i.e. la distance du point a l'enveloppe du cylindre) acceptée pour déclarer l'appartenance du point a cette enveloppe.
"""),

89: _(u"""
 on doit donner une demi épaisseur strictement positive définir l'enveloppe du cylindre.
"""),

90: _(u"""
 le mot-clé précision est obligatoire après le mot-clé ENV_SPHERE pour définir la tolérance (i.e. la distance du point a l'enveloppe de la sphère) acceptée pour déclarer l'appartenance du point a cette enveloppe.
"""),

91: _(u"""
 on doit donner une demi épaisseur strictement positive définir l'enveloppe de la sphère.
"""),

92: _(u"""
 erreur dans la donnée du vecteur orientant l'axe d'un segment ,ce vecteur est nul.
"""),

93: _(u"""
 on doit utiliser obligatoirement le mot-clé ANGL_NAUT ou le mot-clé VECT_NORMALE pour l'option plan de CREA_GROUP_NO. ce mot-clé permet de définir la direction  perpendiculaire au plan ou a la droite.
"""),

94: _(u"""
 pour l'option plan de CREA_GROUP_NO, il faut  définir les 3 composantes du vecteur perpendiculaire au plan.
"""),

95: _(u"""
 pour l'option plan de CREA_GROUP_NO, il faut  définir les 2 composantes du vecteur perpendiculaire a la droite.
"""),

96: _(u"""
 erreur dans la donnée du vecteur normal au plan ou a la droite : ce vecteur est nul.
"""),

97: _(u"""
 le mot-clé précision est obligatoire après le mot-clé plan  pour définir la tolérance (i.e. la distance du point au plan ou a la droite) acceptée pour déclarer l'appartenance du point a ce plan ou a cette droite.
"""),

98: _(u"""
 on doit donner une tolérance strictement positive pour vérifier l'appartenance d'un noeud au plan ou a la droite.
"""),

99: _(u"""
 il manque l'ensemble des noeuds que l'on veut ordonner, mots clés "noeud" et/ou "GROUP_NO"
"""),
}
