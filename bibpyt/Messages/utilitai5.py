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

cata_msg = {

    1 : _(u"""
 Le fichier de nom %(k1)s associé à l'unité logique %(k2)s n'existe pas.
"""),

    3 : _(u"""
 sélection de ddl : choix < %(k1)s > inconnu
"""),

    4 : _(u"""
 argument d'appel invalide :  typf =  %(k1)s
"""),

    5 : _(u"""
 argument d'appel invalide :  ACCES =  %(k1)s
"""),

    6 : _(u"""
 argument d'appel invalide :  autor =  %(k1)s
"""),

    7 : _(u"""
 redéfinition de l'unité logique  %(k1)s  non autorisée
"""),

    8 : _(u"""
 nombre maximum d'unités logiques ouvertes atteint  %(k1)s
"""),

    9 : _(u"""
 argument d'appel invalide :  unit =  %(k1)s
"""),

    10 : _(u"""
 aucun numéro d'unité logique disponible
"""),

    11 : _(u"""
 unité logique  %(k1)s  associée au nom  %(k2)s  et au fichier  %(k3)s
"""),

    12 : _(u"""
 vous devez d'abord le fermer pour l'associer au nom  %(k1)s
"""),

    13 : _(u"""
 unité logique  %(k1)s  déjà utilisée en accès  %(k2)s  par le fichier  %(k3)s
"""),

    14 : _(u"""
 vous devez d'abord le fermer
"""),

    15 : _(u"""
 unité logique  %(k1)s  déjà utilisée en mode binaire par le fichier  %(k2)s
"""),

    16 : _(u"""
 vous devez d'abord fermer le fichier associe
"""),

    17 : _(u"""
 unité logique  %(k1)s  déjà utilisée par le fichier  %(k2)s  associée au nom  %(k3)s
"""),

    18 : _(u"""
 unité logique  %(k1)s , problème lors de l'open  %(k2)s
"""),

    19 : _(u"""
 unité logique  %(k1)s , problème lors du positionnement
"""),

    20 : _(u"""
 Problème lors de la récupération d'informations sur l'unité logique %(k1)s.
"""),

    21 : _(u"""
 Le nombre d'unités logiques ouvertes est supérieur à %(i1)d.
"""),

    22 : _(u"""
 unité logique  %(k1)s , problème lors du close de la réservation.
"""),

    23 : _(u"""
 La redéfinition de l'unité logique  %(k1)s n'est pas autorisée.
"""),

    24 : _(u"""
 Le type d'accès est inconnu "%(k1)s" pour l'unité %(k2)s.
"""),

    25 : _(u"""
 fichier non nomme, unité  %(k1)s
"""),

    26 : _(u"""
 fichier non ouvert, unité  %(k1)s
"""),

    27 : _(u"""
 rembobinage impossible, unité  %(k1)s
"""),

    28 : _(u"""
 positionnement inconnu " %(k1)s ", unité  %(k2)s
"""),

    29 : _(u"""
 les champs de type " %(k1)s " sont interdits.(a faire ...)
"""),

    30 : _(u"""
 composante  %(k1)s inexistante pour la grandeur  %(k2)s
"""),

    31 : _(u"""
 La maille '%(k1)s' n'appartient pas au maillage '%(k2)s'.
"""),

    32 : _(u"""
 Le champ '%(k1)s' n'est pas un champ par éléments aux noeuds.
"""),

    34 : _(u"""
 La maille '%(k1)s' n'est pas affectée dans le groupe d'éléments finis '%(k2)s'.
"""),

    35 : _(u"""
 La maille '%(k1)s' possède un type d'élément ignorant le champ par élément testé.
"""),

    36 : _(u"""
 Le numéro de sous-point demandé est supérieur au numéro maximum de sous-point.
"""),

    37 : _(u"""
 Le numéro de point demandé (%(i1)d) est supérieur au numéro maximum de point (%(i2)d).
"""),

    38 : _(u"""
 L'élément n'admet pas la composante '%(k1)s'.
"""),

    39 : _(u"""
 Détermination de la localisation des points de gauss.
"""),

    41 : _(u"""
 XOUS :  %(k1)s  non prévu.
"""),

    42 : _(u"""
 chaîne sch1 trop longue >24
"""),

    43 : _(u"""
 ipos hors de l intervalle (0 24)
"""),

    44 : _(u"""
 longueur totale > 24
"""),

    45 : _(u"""
 on demande un nombre de composantes négatif pour  %(k1)s
"""),

    46 : _(u"""
 on demande des composantes inconnues pour  %(k1)s
"""),

    47 : _(u"""
 mot-clef :  %(k1)s  inconnu.
"""),

    48 : _(u"""
 composante inexistante dans le champ:  %(k1)s
"""),

    49 : _(u"""
 type de champ non traite:  %(k1)s
"""),

    50 : _(u"""
    La valeur de l'argument est en dehors de l'intervalle [-1, 1].
"""),

    52 : _(u"""
 mauvaise valeur pour fonree
"""),

    53 : _(u"""
 pas de composantes
"""),

    54 : _(u"""
Erreur Programmeur / UTTCPU :
 l"argument "INDI" est non valide
"""),




    56 : _(u"""
Erreur Programmeur / UTTCPU  :
 On ne peut pas passer dans l'état %(k1)s quand on vient de l'état %(k2)s
"""),

    57 : _(u"""
Erreur Programmeur / UTTCPU  :
 L"appel a uttcpu ne peut être effectue avec la valeur  %(k1)s  pour l"argument PARA
"""),

    58 : _(u"""
 (uttrif) type de fonction non connu.
"""),

    59 : _(u"""
 il existe au moins un noeud qui n appartient pas au groupe de mailles.
"""),





    88 : _(u"""
 L'option " %(k1)s " est à recalculer
"""),

    89 : _(u"""
 Erreur de programmation : contacter l'assistance
"""),

    90 : _(u"""
 On ne trouve pas le VALE_PARA_FONC exact dans la liste de la nappe
"""),





    92 : _(u"""
 Interpolation LOG et complexe en ordonnées sont incompatibles !
"""),

    93 : _(u"""
 Vous essayez de stocker le modèle, le champ matériau ou
 des caractéristiques élémentaires dans la SD résultat.
 Ce n'est pas possible pour une SD résultat de type %(k1)s,
 on ne stocke aucun de ces trois éléments.
"""),

    94 : _(u"""
 Vous essayer de stocker la SD charge dans la SD résultat.
 Ce n'est pas possible pour une SD résultat de type %(k1)s,
 on ne stocke pas la charge.
"""),

    97 : _(u"""
 le type de champ  %(k1)s n'est pas accepté
 veuillez consulter la documentation U correspondante
"""),

    98 : _(u"""
 Pour une SD résultat de type %(k1)s, le modèle ne sera pas stocké.
"""),
}
