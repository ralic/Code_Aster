# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    4 : _(u"""
 L'option SIPM_ELNO n'est pas autorisée pour les sections de poutre de type GENERALE
 pour les éléments de poutre autres que multifibre.
 En effet, dans ce cas il est impossible de connaître les contraintes extrêmes dans la section.

 Conseil :

 Vous pouvez utiliser des poutres multifibres pour calculer cette option.
"""),
    14 : _(u"""
  Vous utilisez un élément de type multifibre <%(k1)s>.
  Il faut que sous COMPORTEMENT le mot clef RELATION='MULTIFIBRE'.
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
MODI_METRIQUE pas adapté
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    54 : _(u"""
MODI_METRIQUE pas adapté
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    56 : _(u"""
 famille inexistante  %(k1)s
"""),

    57 : _(u"""
Intégration normale ou intégration réduite obligatoirement.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    58 : _(u"""
  le code " %(k1)s " est non prévu
Ce message est un message d'erreur développeur.
Contactez le support technique.
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
On ne trouve pas le coefficient de Poisson
"""),

    73 : _(u"""
   élément: élasticité non linéaire non programmée.
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
"""),

    81 : _(u"""
 Il manque le paramètre  %(k1)s pour la maille  %(k2)s
"""),

    82 : _(u"""
  Distance au plan :  %(r1)f
"""),

    84 : _(u"""
 famille non disponible élément de référence  %(k1)s
 famille  %(k2)s
"""),

    88 : _(u"""
élément de référence  %(k1)s non disponible
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    90 : _(u"""
 élément mal programmé maille  %(k1)s  type  %(k2)s  nombre noeuds  %(i1)d
 nombre noeuds pour le passage Gauss noeuds  %(i2)d
"""),

    91 : _(u"""
 Le calcul de cet estimateur ne tient pas compte d'éventuelles
 conditions limites non linéaires
"""),

    92 : _(u"""
 Vous essayez d'appliquer un chargement de pression fonction (et non nul) sur un élément de coque (avec le mot-clé facteur PRES_REP) pour la maille %(k1)s.

 Pour cette modélisation, il faut utiliser le mot-clé FORCE_COQUE.
"""),

    93 : _(u"""
 Vous essayez d'appliquer un chargement de pression suiveur (et non nul) sur un élément 'DKT' (avec le mot-clé facteur PRES_REP et le mot-clé TYPE_CHARGE='SUIV') pour la maille %(k1)s.
 Cette fonctionnalité n'est pas disponible.

 Conseil :
    - remplacez la modélisation 'DKT' par la modélisation 'COQUE_3D'.
"""),

}
