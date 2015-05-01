# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

    1  : _(u"""
   Aucun champ de déplacement ni de vitesse n'est fourni
   pour le calcul de l'option %(k1)s.
"""),

    2  : _(u"""
   La réactualisation de la géométrie pour le chargement de type suiveur n'existe
   pas pour le type d'élément %(k2)s.
   Option concernée : %(k1)s
"""),

    27 : _(u"""
 pas d'intersection trouvé
"""),

    29 : _(u"""
 élément faisceau homogénéisé non prévu
"""),

    31 : _(u"""
  ELREFE non prévu
"""),

    32 : _(u"""
 comportement non trouve: %(k1)s
"""),

    33 : _(u"""
 pas de dilatation thermique orthotrope pour coque_3d
"""),

    34 : _(u"""
 les vecteurs sont au nombre de 1 ou 2
"""),

    38 : _(u"""
  ->  L'option ANGL_AXE n'est pas prise en compte en 2D mais seulement
      en 3D.
  -> Risque & Conseil :
     Ce mot clé utilisé dans l'opérateur AFFE_CARA_ELEM (MASSIF), permet
     de définir des axes locaux pour lesquels on utilise une propriété de
     symétrie de révolution, ou d'isotropie transverse. En 2D, on peut définir
     un repère d'orthotropie via ANGL_REP.
"""),

    39 : _(u"""
 La loi %(k1)s n'existe pas pour une utilisation avec des poutres multifibres.
"""),

    40 : _(u"""
 Poutres multifibres. Pas d'intégration possible sur les poutre de type %(i1)d.
"""),


    42 : _(u"""
 " %(k1)s "    nom d'élément inconnu.
"""),

    43 : _(u"""
 noeuds confondus pour la maille:  %(k1)s
"""),

    44 : _(u"""
  option de matrice de masse  %(k1)s  inconnue
"""),

    45 : _(u"""
 on n'a pas trouvé de variable interne correspondante a la déformation plastique équivalente cumulée
"""),

    46 : _(u"""
 on ne traite pas les moments
"""),

    47 : _(u"""
 l'option " %(k1)s " est inconnue
"""),

    48 : _(u"""
 type de poutre inconnu
"""),

    49 : _(u"""
 charge répartie variable non admise sur un élément courbe.
"""),

    50 : _(u"""
 charge répartie variable non admise sur un élément variable.
"""),

    51 : _(u"""
 on ne peut pas imposer de charges réparties suiveuses de type vitesse de vent sur les poutres courbes.
"""),

    52 : _(u"""
 on ne peut pas imposer de charges réparties suiveuses sur les poutres courbes.
"""),

    53 : _(u"""
 un champ de vitesse de vent est imposé sans donner un Cx dépendant de la vitesse sur une des poutres.
"""),

    54 : _(u"""
 le module de cisaillement G est nul mais pas le module de Young E
"""),

    55 : _(u"""
 section circulaire uniquement
"""),

    57 : _(u"""
       La modélisation T3G ne permet pas de bien prendre en compte l'excentrement à cause de son interpolation de la flèche.
"""),

    58 : _(u"""
 Échec de convergence dans l'inversion du système par Newton-Raphson.
"""),

    59 : _(u"""
 Les moments répartis ne sont autorisés que sur les poutres droites à section constante.
"""),

    61 : _(u"""
 " %(k1)s "   nom d'option non reconnue
"""),

    62 : _(u"""
 ! Problème RCVALA rhocp !
"""),

    63 : _(u"""
 On ne trouve pas le comportement %(k1)s dans le matériau fourni.
"""),

    64 : _(u"""
 Le matériau contient le comportement thermique %(k1)s alors que l'on attend le comportement %(k2)s.
"""),

    65 : _(u"""
 Le matériau contient le comportement thermique %(k1)s alors que l'on attend un des comportements suivants :
 - THER_NL
 - THER_HYDR
"""),

    66 : _(u"""
 On ne trouve pas l'un des comportements suivants dans le matériau :
 - THER_NL
 - THER_HYDR
"""),

    72 : _(u"""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC') est déconseillée pour les éléments de type plaque.
     Les grandes rotations ne sont pas modélisées correctement.
  -> Risque & Conseil :
     En présence de grands déplacements et grandes rotations, il est préférable
     d'utiliser pour les modélisations type COQUE_3D ou  DKTG  DEFORMATION='GROT_GDEP'
"""),

    73 : _(u"""
 Seule la loi de comportement ELAS est autorisée avec la déformation GROT_GDEP en modélisation DKT.
"""),

    74 : _(u"""
  %(k1)s  non implante.
"""),

    75 : _(u"""
  Les matériaux de coque homogénéisées (ELAS_COQUE) sont interdits en non-linéaire.
"""),

    77 : _(u"""
 option :  %(k1)s  interdite
"""),


    81 : _(u"""
 éléments de poutre section variable affine :seul une section rectangle plein est disponible.
"""),

    82 : _(u"""
 éléments de poutre section variable homothétique : l'aire initiale est nulle.
"""),

    84 : _(u"""
 éléments de poutre l'option " %(k1)s " est inconnue
"""),

    90 : _(u"""
 le seul comportement élastique valide est ELAS
"""),

}
