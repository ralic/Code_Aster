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
Le type de résultat DYNA_TRANS ne supporte pas les données complexes.
"""),

    2 : _(u"""
Le type de résultat DYNA_HARMO ne supporte pas les données réelles.
"""),

    3 : _(u"""
On ne traite pas les déformations complexes.
"""),

    4 : _(u"""
Le nombre de DATASET de type 58 est supérieur au produit du nombre de noeuds par le nombre de composantes.
"""),

    5 : _(u"""
Erreur lors de la lecture du fichier IDEAS.
"""),

    6 : _(u"""
Seules les données de type déplacement, vitesse, accélération, déformation
 ou contrainte sont traitées.
"""),

    9 : _(u"""
On ne traite pas la redéfinition des orientations pour les champs de contrainte.
"""),

    10 : _(u"""
On ne traite pas la redéfinition des orientations pour les champs de déformation.
"""),

    11 : _(u"""
La condition GAMMA/KSI <= 1 n'est pas respectée.
"""),

    12 : _(u"""
Incohérence des relations SIGMA_C, SIGMA_P1, M_PIC, A_PIC, A_E et M_E.
"""),

    16 : _(u"""
Le profil de la matrice n'est sûrement pas plein.
On continue pour vérifier.
"""),

    17 : _(u"""
Le profil de la matrice n'est sûrement pas plein.
On continue pour vérifier.
"""),

    18 : _(u"""
Le profil de la matrice n'est pas plein.
On arrête tout.
"""),

    19 : _(u"""
Le déterminant de la matrice à inverser est nul.
"""),

    23 : _(u"""
Le pas de temps minimal a été atteint. Le calcul s'arrête.
"""),

    24 : _(u"""
Données erronées.
"""),

    26 : _(u"""
Dispositif anti-sismique :  la distance entre les noeuds 1 et 2 est nulle.
"""),

    27 : _(u"""
Le noeud  %(k1)s  n'est pas un noeud du maillage %(k2)s .
"""),

    28 : _(u"""
On n'a pas trouvé le ddl DX pour le noeud  %(k1)s .
"""),

    29 : _(u"""
On n'a pas trouvé le ddl DY pour le noeud  %(k1)s .
"""),

    30 : _(u"""
On n'a pas trouvé le ddl DZ pour le noeud  %(k1)s .
"""),

    31 : _(u"""
 calcul non-linéaire par sous-structuration :
 le mot-clé SOUS_STRUC_1 est obligatoire
"""),

    32 : _(u"""
 argument du mot-clé "%(k1)s" n'est pas un nom de sous-structure
"""),

    35 : _(u"""
  obstacle BI_CERC_INT : DIST_2 doit être supérieur ou égal a DIST_1
"""),

    36 : _(u"""
 calcul non-linéaire par sous-structuration :
 pas de dispositif anti-sismique ou de flambage possible
"""),

    37 : _(u"""
 La sous-structuration en présence de multiappui n'est pas développée.
"""),

    38 : _(u"""
 conflit entre choc et flambage au même lieu de choc :
 le calcul sera de type flambage
"""),

    39 : _(u"""
 argument du mot-clé "REPERE" inconnu
"""),

    40 : _(u"""
 les rigidités de chocs doivent être strictement positives
"""),

    41 : _(u"""
 incohérence dans les données de la loi de flambage :
 les caractéristiques introduites peuvent conduire à
 un écrasement résiduel négatif
"""),

    42 : _(u"""
 les bases utilisées pour la projection sont différentes.
"""),

    43 : _(u"""
 les bases utilisées n'ont pas le même nombre de vecteurs.
"""),

    46 : _(u"""
 on n'a pas pu trouver les déplacements initiaux
"""),

    47 : _(u"""
 on n'a pas pu trouver les vitesses initiales
"""),

    48 : _(u"""
 on n'a pas pu trouver les variables internes initiales :
 reprise choc avec flambage
"""),

    54 : _(u"""
 le calcul de la réponse temporelle n'est pas possible pour le type
 de structure étudiée.
"""),

    55 : _(u"""
 le couplage fluide-structure n'a pas été pris en compte en amont.
"""),

    57 : _(u"""
Le GROUP_NO %(k1)s ne doit pas contenir plus d'un noeud.
"""),

    64 : _(u"""
 l'argument du mot-clé "SOUS_STRUC" n'est pas un nom de sous-structure
"""),

    66 : _(u"""
 le taux de souplesse négligée est supérieur au seuil.
"""),

    76 : _(u"""
 NUME_ORDRE plus grand que le nombre de modes de la base
"""),

    79 : _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    82 : _(u"""
Vous réalisez une poursuite d'un calcul avec DYNA_VIBRA.
Le nombre de dispositif de choc est différent.
    Avant      : %(i1)d
    Maintenant : %(i2)d

Conseil :
   Vérifier vos données concernant les dispositifs de choc.
"""),

    83 : _(u"""
Vous réalisez une poursuite d'un calcul avec DYNA_VIBRA.
La nature, les noeuds du des non-linéarités localisées sont différents.

              Avant     Maintenant
    Nature    %(k1)8s   %(k2)8s
    Noeud1    %(k3)8s   %(k4)8s
    Noeud2    %(k5)8s   %(k6)8s

Conseil :
   Vérifier vos données concernant les dispositifs de choc.
"""),

}
