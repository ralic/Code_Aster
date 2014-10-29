# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

    3: _(u"""
POSI_ANGUL POSI_CURV_LONGI est obligatoire.
"""),

    4: _(u"""
Il faut renseigner : ANGLE, R_CINTR et POSI_ANGUL ou ANGLE, R_CINTR et POSI_CURV_LONGI.
"""),

    5: _(u"""
ANGL_COUDE et ANGL_SOUS_EPAI sont inutiles dans ce cas.
"""),

    6: _(u"""
 ASCSEP valeur hors domaine
 sous-épaisseur numéro : %(i1)d
 taille axe circonférentiel : %(r1)f
 bord plaque : %(r2)f
"""),

    7: _(u"""
 ASCSEP cas de symétrie :
 la sous-épaisseur doit être dans la section médiane du coude !
"""),


    9: _(u"""
 Valeur hors domaine :
 sous-épaisseur numéro :%(i1)d
 abscisse curviligne circonférentielle :%(r1)f
 bord plaque :%(r2)f
"""),

    10: _(u"""
 Valeur hors domaine :
 sous-épaisseur numéro :%(i1)d
 abscisse curviligne longitudinale  :%(r1)f
 bord plaque :%(r2)f
"""),

    11: _(u"""
 valeur hors domaine :
 sous-épaisseur numéro :%(i1)d
 bord inférieur  :%(r1)f
 bord plaque :%(r2)f
"""),


    13: _(u"""
 Les quart et demi structure ne peuvent être réalisées
 sur un modèle comportant une transition d'épaisseur.
"""),

    14: _(u"""
 Les deux embouts doivent être de même longueur pour les cas de symétrie.
"""),

    15: _(u"""
 Longueur d'embout P1 inférieure a la longueur d'amortissement = %(r1)f
"""),

    16: _(u"""
 Longueur d'embout P2 inférieure à la longueur d'amortissement = %(r1)f
"""),

    18: _(u"""
 Le nombre d'éléments dans l'épaisseur du coude n'est pas paramétrable pour
 un coude avec fissure.
 Le mot-clé NB_ELEM_EPAIS est ignoré.
"""),

    19: _(u"""
 Pour les fissures non axisymétriques, la longueur doit être spécifiée.
"""),

    20: _(u"""
 La fissure est axisymétrique : on ne tient pas compte de la longueur spécifiée.
"""),

    21: _(u"""
 Avec une transition d'épaisseur,la fissure doit obligatoirement être transverse.
"""),

    23: _(u"""
 L'orientation de la fissure doit être transverse (orientation : 90.) pour modéliser
 un quart ou une demi structure.
"""),

    24: _(u"""
 La fissure est axisymétrique : son orientation doit être transverse (ORIEN : 90.)
"""),

    25: _(u"""
 Il ne peut pas y avoir plusieurs sous-épaisseurs en même temps
 qu'une transition d'épaisseur:
 si une seule sous-épaisseur, alors utiliser SOUS_EPAIS_COUDE.
"""),

    26: _(u"""
 Avec une transition d'épaisseur,il doit obligatoirement y avoir un défaut,
 soit une fissure  soit une sous-épaisseur.
"""),

    27: _(u"""
 Ne modéliser qu'une seule sous-épaisseur pour un quart ou demi coude.
"""),

    28: _(u"""
 Vous ne pouvez déclarer la sous-épaisseur comme axisymétrique et donner
 une taille d'axe circonférentiel.
"""),

    29: _(u"""
 Vous devez donner une taille d'axe circonférentiel pour une sous-épaisseur
 de type elliptique.
"""),

    30: _(u"""
 Valeur hors domaine de validité :
 sous-épaisseur numéro :%(i1)d
 abscisse curviligne longitudinale :%(r1)f
 valeur maximale autorisée :%(r2)f
"""),

    31: _(u"""
 Valeur hors domaine de validité :
 sous-épaisseur numéro :%(i1)d
 position angulaire centre sous-épaisseur :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

    32: _(u"""
 Valeur hors domaine de validité :
 sous-épaisseur numéro :%(i1)d
 abscisse curviligne circonférentielle :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

    33: _(u"""
 Le centre d'une sous-épaisseur axisymétrique est imposé en intrados (pi*RM).
"""),

    34: _(u"""
 Le centre d'une sous-épaisseur axisymétrique est imposé en intrados.
 L'azimut est fixé à 180 degrés.
"""),

    35: _(u"""
 Le nombre d'éléments dans l'épaisseur du coude n'est pas paramétrable pour
 la version 2 de la procédure de plaque avec sous-épaisseur :
 mot-clé NB_ELEM_EPAIS ignoré.
"""),

    36: _(u"""
 Valeur hors domaine de validité :
 sur-épaisseur :%(i1)d
 valeur limite autorisée (RM-EP1/2) :%(r1)f
"""),

    37: _(u"""
 Valeur hors domaine de validité :
 le rayon de cintrage : %(r1)f
 doit être supérieur a (RM+EP1/2) :%(r2)f
"""),

    38: _(u"""
 Valeur hors domaine de validité (5,50)
 rapport RM/EP1 : %(r1)f
"""),

    39: _(u"""
 Valeur hors domaine de validité :
 abscisse curviligne centre fissure :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

    40: _(u"""
 Valeur hors domaine de validité :
 nombre de tranches :%(i1)d
"""),

    41: _(u"""
 Valeur hors domaine de validité :
 position angulaire  centre fissure : %(r1)f
 POSI_ANGUL doit être >= 0 et <= %(r2)f
"""),

    42: _(u"""
 Valeur hors domaine de validité :
 début transition d'épaisseur :%(r1)f
 valeur minimale autorisée :%(r2)f
 valeur maximale autorisée :%(r3)f
"""),

    43: _(u"""
 Valeur hors domaine de validité :
 angle de transition TETA1 : %(r1)f
 valeur minimale autorisée : 0.
 valeur maximale autorisée : 30.
"""),

    44: _(u"""
 Valeur hors domaine de validité :
 épaisseur avant la transition :%(r1)f
 valeur minimale autorisée : 12
 valeur maximale autorisée : 80
"""),

    45: _(u"""
 Valeur hors domaine de validité :
 épaisseur après la transition :%(r1)f
 valeur minimale autorisée : 20
 valeur maximale autorisée : 110
"""),

    46: _(u"""
 L'épaisseur avant la transition doit être inférieure
 à celle après la transition.
"""),

    47: _(u"""
 Valeur hors domaine de validité :
 fin transition d'épaisseur :%(r1)f
 valeur limite autorisée :%(r2)f
"""),

    48: _(u"""
 Valeur hors domaine de validité :
 diamètre extérieur du tube avant transition:%(r1)f
 valeur minimum autorisée : 112.
 valeur maximum autorisée : 880.
"""),

    49: _(u"""
 Valeur hors domaine de validité :
 angle de transition TETA2: %(r1)f
 valeur minimum autorisée : 0.
 valeur maximum autorisée : 45.
"""),

    50: _(u"""
 Valeur hors domaine de validité :
 épaisseur avant 1ère transition:%(r1)f
 valeur minimum autorisée : 7.
 valeur maximum autorisée : 35.
"""),

    51: _(u"""
 Valeur hors domaine de validité :
 épaisseur avant 2ème transition:%(r1)f
 valeur minimum autorisée : 15.
 valeur maximum autorisée : 40.
"""),

    52: _(u"""
 Valeur hors domaine de validité :
 épaisseur intermédiaire:%(r1)f
 valeur minimum autorisée : 15.
 valeur maximum autorisée : 40.
"""),

    53: _(u"""
 Valeur hors domaine de validité.
 L'épaisseur avant la transition doit être inférieure
 à l'épaisseur intermédiaire.
"""),

    54: _(u"""
 Valeur hors domaine de validité.
 L'épaisseur après la transition doit être inférieure
 à l'épaisseur intermédiaire.
"""),

    55: _(u"""
 Valeur hors domaine de validité :
 fin transition d'épaisseur:%(r1)f
 valeur limite autorisée :%(r2)f
"""),

    56: _(u"""
 Valeur hors domaine de validité :
 diamètre extérieur du tube avant transition:%(r1)f
 valeur minimum autorisée : 77.
 valeur maximum autorisée : 355.
"""),

    57: _(u"""
Seuls GIBI98 et GIBI2000 sont appelables.
"""),

    58: _(u"""
Une interpénétration des lèvres est détectée pour le numéro d'ordre %(i1)d : sur les
%(i2)d noeuds de chaque lèvre, %(i3)d noeuds s'interpénètrent.
-> Risque et Conseil :
Le contact n'est pas pris en compte dans le calcul. Le taux de restitution de l'énergie G
est donc positif y compris là où la fissure tend à se refermer, ce qui peut conduire à
des résultats trop pénalisants.
Pour prendre en compte le contact entre les lèvres, il faut lancer le calcul hors macro-commande.
"""),

}
