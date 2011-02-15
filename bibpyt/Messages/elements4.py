#@ MODIF elements4 Messages  DATE 15/02/2011   AUTEUR FLEJOU J-L.FLEJOU 
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

def _(x) : return x

cata_msg = {

1 : _("""
  erreur dans le calcul de pres_f
"""),

2 : _("""
 pour l'option INDIC_ENER, les seules relations admises sont "vmis_isot_line" et "vmis_isot_trac" .
"""),

3 : _("""
 pour l'option INDIC_SEUIL, les seules relations admises sont "vmis_isot_line", "vmis_isot_trac"  et "vmis_cine_line" .
"""),

15 : _("""
  deformation :  %(k1)s non implantée sur les éléments "pou_d_tgm" : utiliser PETIT ou GROT_GDEP
"""),

16 : _("""
 option "VARI_ELNO" impossible actuellement
"""),

31 : _("""
 dfdi mal dimensionnée
"""),

32 : _("""
 vous utilisez le mot clé liaison_elem avec l'option coq_pou: l'épaisseur des éléments de bord de coque n'a pas été affectée.
"""),

33 : _("""
 l'epaisseur des éléments de bord de coque est negative ou nulle.
"""),

34 : _("""
 le jacobien est nul.
"""),

35 : _("""
 matns() sous-dimensionné
"""),

36 : _("""
 pr() sous-dimensionne
"""),

38 : _("""
 option  %(k1)s  non active pour un élément de type  %(k2)s
"""),

39 : _("""
 option  %(k1)s  : incompatibilité des deux champs d entrée
"""),

40 : _("""
 le nombre de ddl est trop grand
"""),

41 : _("""
 le nombre de ddl est faux
"""),

42 : _("""
 nom de type élément inattendu
"""),

43 : _("""
 comp. elastique inexistant
"""),

44 : _("""
 l'option " %(k1)s " est interdite pour les tuyaux
"""),

45 : _("""
 l'option " %(k1)s " en repère local est interdite pour les tuyaux : utiliser le repère global
"""),

46 : _("""
 le nombre de couches et de secteurs doivent etre supérieurs a 0
"""),

47 : _("""
 composante  %(k1)s  non traitée, on abandonne
"""),

48 : _("""
 champ  %(k1)s  non traité, on abandonne
"""),

49 : _("""
 l'option " %(k1)s " est non prévue
"""),

51 : _("""
  nume_sect incorrect
"""),

52 : _("""
 mauvaise option
"""),

53 : _("""
 ep/r > 0.2 modi_metrique pas adapté
"""),

54 : _("""
 ep/r > 0.2 modi_metrique=non pas adapté
"""),

56 : _("""
 famille inexistante  %(k1)s
"""),

57 : _("""
 indn = 1 (intégration normale) ou indn = 0 (integration réduite) obligatoirement.
"""),

58 : _("""
  le code " %(k1)s " est non prévu. code doit etre = "gl" ou "lg"
"""),

59 : _("""
Pour l'option %(k1)s, vous ne pouvez affecter qu'un seul matériau qui ne doit avoir
qu'un seul comportement : ELAS. Commande DEFI_MATERIAU / ELAS.
Conseil :
   Définir un seul matériau avec un seul comportement : ELAS.
"""),

61 : _("""
 préconditions non remplies
"""),

62 : _("""
  erreur: élément non 2d
"""),

63 : _("""
  l'option %(k1)s n'est pas disponible pour le comportement %(k2)s
"""),

64 : _("""
Pour l'option %(k1)s votre matériau doit avoir un seul comportement : ELAS.
Commande DEFI_MATERIAU / ELAS.
Votre matériau a %(k2)s comme comportement possible.
Conseil :
   Définir un matériau avec un seul comportement : ELAS.
"""),

65 : _("""
  Comportement inattendu : %(k1)s.
"""),

67: _("""
Le module d'Young est nul.
"""),

69 : _("""
 pb récuperation donnée matériau dans thm_liqu %(k1)s
"""),

70 : _("""
 pb récupération donnée matériau dans thm_init %(k1)s
"""),

71 : _("""
 pb récupération données matériau dans elas %(k1)s
"""),

72 : _("""
   rcvala ne trouve pas nu, qui est nécessaire pour l'élément MECA_HEXS8
"""),

73 : _("""
   élément MECA_HEXS8:COMP_ELAS non implanté, utiliser COMP_INCR RELATION='ELAS'
"""),

74 : _("""
  Attention l'élément MECA_HEXS8 ne fonctionne correctement que sur les parallélépipèdes.
  Sur les elements quelconques on peut obtenir des résultats faux.
"""),

76 : _("""
 la maille du modèle de numéro:  %(i1)d n appartient à aucun sous-domaine !
"""),

77 : _("""
 numero de couche  %(i1)d
 trop grand par rapport au nombre de couches autorisé pour la maille  %(k1)s
"""),

78 : _("""
 pb recuperation donnée matériau dans thm_diffu %(k1)s
"""),

79 : _("""
 la loi de comportement n'existe pas pour la modélisation dktg :  %(k1)s
"""),

80 : _("""
  L'élément de plaque QUAD4 défini sur la maille : %(k1)s
  n'est pas plan et peut conduire a des résultats faux
  Distance au plan :  %(r1)f
"""),

81 : _("""
 Il manque le paramètre  %(k1)s pour la maille  %(k2)s
"""),

84 : _("""
 famille non disponible élément de référence  %(k1)s
 famille  %(k2)s
"""),

88 : _("""
 ELREFE non disponible élément de référence  %(k1)s
"""),

90 : _("""
 ELREFE mal programme maille  %(k1)s  type  %(k2)s  nb noeuds  %(i1)d
 nb noeuds pour le gano  %(i2)d
"""),

91 : _("""
 Le calcul de cet estimateur ne tient pas compte d'éventuelles
 conditions limites non linéaires
"""),

92 : _("""
Erreur utilisateur :
 Vous essayez d'appliquer une pression (comme fonction) non nulle sur un élément de coque.
 (AFFE_CHAR_MECA_F/PRES_REP/PRES) pour la maille  %(k1)s
 La programmation ne le permet pas.

Conseil :
 Pour appliquer une telle presssion, il faut utiliser AFFE_CHAR_MECA_F/FORCE_COQUE/PRES
"""),

}
