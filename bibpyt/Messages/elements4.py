#@ MODIF elements4 Messages  DATE 29/05/2012   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""
  erreur dans le calcul de PRES_F
"""),

2 : _(u"""
 pour l'option INDIC_ENER, les seules relations admises sont "VMIS_ISOT_LINE" et "VMIS_ISOT_TRAC" .
"""),

3 : _(u"""
 pour l'option INDIC_SEUIL, les seules relations admises sont "VMIS_ISOT_LINE", "VMIS_ISOT_TRAC"  et "VMIS_CINE_LINE" .
"""),

15 : _(u"""
  déformation :  %(k1)s non implantée sur les éléments "POU_D_TGM" : utiliser PETIT ou GROT_GDEP
"""),

16 : _(u"""
 option "VARI_ELNO" impossible actuellement
"""),

31 : _(u"""
 dfdi mal dimensionnée
"""),

32 : _(u"""
 vous utilisez le mot clé LIAISON_ELEM avec l'option COQ_POU: l'épaisseur des éléments de bord de coque n'a pas été affectée.
"""),

33 : _(u"""
 l'épaisseur des éléments de bord de coque est négative ou nulle.
"""),

34 : _(u"""
 le jacobien est nul.
"""),

38 : _(u"""
 option  %(k1)s  non active pour un élément de type  %(k2)s
"""),

39 : _(u"""
 option  %(k1)s  : incompatibilité des deux champs d entrée
"""),

40 : _(u"""
 le nombre de ddl est trop grand
"""),

41 : _(u"""
 le nombre de ddl est faux
"""),

42 : _(u"""
 nom de type élément inattendu
"""),

43 : _(u"""
 comportement. élastique inexistant
"""),

44 : _(u"""
 l'option " %(k1)s " est interdite pour les tuyaux
"""),

45 : _(u"""
 l'option " %(k1)s " en repère local est interdite pour les tuyaux : utiliser le repère global
"""),

46 : _(u"""
 le nombre de couches et de secteurs doivent être supérieurs a 0
"""),

48 : _(u"""
 champ  %(k1)s  non traité, on abandonne
"""),

49 : _(u"""
 l'option " %(k1)s " est non prévue
"""),

51 : _(u"""
  NUME_SECT incorrect
"""),

53 : _(u"""
 ep/r > 0.2 MODI_METRIQUE pas adapté
"""),

54 : _(u"""
 ep/r > 0.2 MODI_METRIQUE=non pas adapté
"""),

56 : _(u"""
 famille inexistante  %(k1)s
"""),

57 : _(u"""
 indn = 1 (intégration normale) ou indn = 0 (intégration réduite) obligatoirement.
"""),

58 : _(u"""
  le code " %(k1)s " est non prévu. code doit être = "gl" ou "lg"
"""),

59 : _(u"""
Pour l'option %(k1)s, vous ne pouvez affecter qu'un seul matériau qui ne doit avoir
qu'un seul comportement : ELAS. Commande DEFI_MATERIAU / ELAS.
Conseil :
   Définir un seul matériau avec un seul comportement : ELAS.
"""),

61 : _(u"""
 préconditions non remplies
"""),

62 : _(u"""
  erreur: élément non 2d
"""),

63 : _(u"""
  l'option %(k1)s n'est pas disponible pour le comportement %(k2)s
"""),

64 : _(u"""
Pour l'option %(k1)s votre matériau doit avoir un seul comportement : ELAS.
Commande DEFI_MATERIAU / ELAS.
Votre matériau a %(k2)s comme comportement possible.
Conseil :
   Définir un matériau avec un seul comportement : ELAS.
"""),

65 : _(u"""
  Comportement inattendu : %(k1)s.
"""),

67: _(u"""
Le module de Young est nul.
"""),

69 : _(u"""
 Problème récupération donnée matériau dans THM_LIQU %(k1)s
"""),

70 : _(u"""
 Problème récupération donnée matériau dans THM_INIT %(k1)s
"""),

71 : _(u"""
 Problème récupération données matériau dans ELAS %(k1)s
"""),

72 : _(u"""
   RCVALA ne trouve pas nu, qui est nécessaire pour l'élément MECA_HEXS8
"""),

73 : _(u"""
   élément MECA_HEXS8:COMP_ELAS non implanté, utiliser COMP_INCR RELATION='ELAS'
"""),

74 : _(u"""
  Attention l'élément MECA_HEXS8 ne fonctionne correctement que sur les parallélépipèdes.
  Sur les éléments quelconques on peut obtenir des résultats faux.
"""),

76 : _(u"""
 la maille du modèle de numéro:  %(i1)d n appartient à aucun sous-domaine !
"""),

78 : _(u"""
 Problème récupération donnée matériau dans THM_DIFFU %(k1)s
"""),

79 : _(u"""
 la loi de comportement n'existe pas pour la modélisation DKTG :  %(k1)s
"""),

80 : _(u"""
  L'élément de plaque QUAD4 défini sur la maille : %(k1)s
  n'est pas plan et peut conduire a des résultats faux
  Distance au plan :  %(r1)f
"""),

81 : _(u"""
 Il manque le paramètre  %(k1)s pour la maille  %(k2)s
"""),

84 : _(u"""
 famille non disponible élément de référence  %(k1)s
 famille  %(k2)s
"""),

88 : _(u"""
 ELREFE non disponible élément de référence  %(k1)s
"""),

90 : _(u"""
 ELREFE mal programme maille  %(k1)s  type  %(k2)s  nombre noeuds  %(i1)d
 nombre noeuds pour le passage Gauss noeuds  %(i2)d
"""),

91 : _(u"""
 Le calcul de cet estimateur ne tient pas compte d'éventuelles
 conditions limites non linéaires
"""),

92 : _(u"""
Erreur utilisateur :
 Vous essayez d'appliquer une pression (comme fonction) non nulle sur un élément de coque.
 (AFFE_CHAR_MECA_F/PRES_REP/PRES) pour la maille  %(k1)s
 La programmation ne le permet pas.

Conseil :
 Pour appliquer une telle pression, il faut utiliser AFFE_CHAR_MECA_F/FORCE_COQUE/PRES
"""),

}
