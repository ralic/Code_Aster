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

    5 : _(u"""
La force normale est nulle.
"""),

    6 : _(u"""
La somme des impacts écrouissage est inférieure à la somme des glissements.
"""),

    7 : _(u"""
NOM_CAS n'est pas une variable d'accès d'un résultat de type EVOL_THER.
"""),

    8 : _(u"""
NUME_MODE n'est pas une variable d'accès d'un résultat de type EVOL_THER.
"""),

    9 : _(u"""
NUME_MODE n'est pas une variable d'accès d'un résultat de type MULT_ELAS.
"""),

    10 : _(u"""
INST n'est pas une variable d'accès d'un résultat de type MULT_ELAS.
"""),

    11 : _(u"""
NOM_CAS n'est pas une variable d'accès d'un résultat de type FOURIER_ELAS.
"""),

    12 : _(u"""
INST n'est pas une variable d'accès d'un résultat de type FOURIER_ELAS.
"""),

    13 : _(u"""
NOM_CAS n'est pas une variable d'accès d'un résultat de type FOURIER_THER.
"""),

    14 : _(u"""
INST n'est pas une variable d'accès d'un résultat de type FOURIER_THER.
"""),

    15 : _(u"""
Le mot-clef RESU_INIT est obligatoire.
"""),

    16 : _(u"""
Le mot-clef MAILLAGE_INIT est obligatoire.
"""),

    17 : _(u"""
Le mot-clef RESU_FINAL est obligatoire.
"""),

    18 : _(u"""
Le mot-clef MAILLAGE_FINAL est obligatoire.
"""),

    24 : _(u"""
Absence de potentiel permanent.
"""),

    25 : _(u"""
Le modèle fluide n'est pas thermique.
"""),

    26 : _(u"""
Le modèle interface n'est pas thermique.
"""),

    27 : _(u"""
Le modèle fluide est incompatible avec le calcul de masse ajoutée.
Utilisez les modélisations PLAN ou 3D ou AXIS.
"""),

    29 : _(u"""
Le nombre d'amortissement modaux est différent du nombre de modes dynamiques.
"""),

    30 : _(u"""
Il n y a pas le même nombre de modes retenus  dans l'excitation modale et
dans la base modale
"""),

    31 : _(u"""
Il faut autant d'indices en i et j.
"""),

    32 : _(u"""
Avec SOUR_PRESS et SOUR_FORCE, il faut deux points par degré de liberté d'application
"""),

    33 : _(u"""
Mauvais accord entre le nombre d'appuis et le nombre de valeur dans le mot-clé NUME_ORDRE_I
"""),

    34 : _(u"""
Il faut autant de noms de composantes que de noms de noeuds.
"""),

    35 : _(u"""
Précisez le mode statique.
"""),

    36 : _(u"""
Le mode statique n'est pas nécessaire.
"""),

    37 : _(u"""
La fréquence minimale doit être plus petite que la fréquence maximale.
"""),

    73 : _(u"""
Le paramètre matériau taille limite D10 n'est pas défini.
"""),

    79 : _(u"""
Pas d'interpolation possible.
"""),

    82 : _(u"""
Erreur de la direction de glissement.
 Angle ALPHA: %(k1)s
 Angle BETA : %(k2)s
"""),

    83 : _(u"""
Arrêt par manque de temps CPU.
"""),

    86 : _(u"""
La perturbation est trop petite, calcul impossible.
"""),

    87 : _(u"""
Champ déjà existant
Le champ %(k1)s à l'instant %(r1)g est remplacé par le champ %(k2)s à l'instant %(r2)g avec la précision %(r3)g.
"""),

    88 : _(u"""
Arrêt débordement assemblage : ligne.
"""),

    90 : _(u"""
Arrêt débordement assemblage : colonne.
"""),

    92 : _(u"""
Arrêt pour nombre de sous-structures invalide :
 Il en faut au minimum : %(i1)d
 Vous en avez défini   : %(i2)d
"""),

    93 : _(u"""
Arrêt pour nombre de noms de sous-structures invalide :
 Il en faut exactement : %(i1)d
 Vous en avez défini   : %(i2)d
"""),

    94 : _(u"""
Arrêt pour nombre de MACR_ELEM invalide :
 Sous-structure %(k1)s
 Il en faut exactement : %(i2)d
 Vous en avez défini   : %(i1)d
"""),

    95 : _(u"""
Arrêt pour nombre d'angles nautiques invalide :
 Sous-structure %(k1)s
 Il en faut exactement : %(i2)d
 Vous en avez défini   : %(i1)d
"""),

    96 : _(u"""
Arrêt pour nombre de translations invalide :
 Sous-structure %(k1)s
 Il en faut exactement : %(i2)d
 Vous en avez défini   : %(i1)d
"""),

    97 : _(u"""
Arrêt pour nombre de liaisons définies invalide :
 Il en faut exactement : %(i2)d
 Vous en avez défini   : %(i1)d
"""),

    98 : _(u"""
Arrêt pour nombre de mots-clés invalide :
 Numéro liaison : %(i1)d
 Mot-clé        : %(k1)s
 Il en faut exactement : %(i3)d
 Vous en avez défini   : %(i2)d
"""),

    99 : _(u"""
Arrêt pour sous-structure indéfinie :
 Numéro liaison    : %(i1)d
 Nom sous-structure: %(k1)s
"""),

}
