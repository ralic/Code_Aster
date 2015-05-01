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

    4 : _(u"""
  le nombre de noeuds sélectionnés est supérieur au nombre de noeuds du maillage. on va tronquer la liste.
"""),

    5 : _(u"""
 chaîne de caractères trop longues : imprimer moins de champs
"""),

    6 : _(u"""
 type inconnu" %(k1)s "
"""),



    9 : _(u"""
 type de base inconnu:  %(k1)s
"""),

    10 : _(u"""
 soit le fichier n'existe pas, soit c'est une mauvaise version de HDF (utilise par MED).
"""),


    31 : _(u"""
 on n'a pas trouvé le numéro d'ordre à l'adresse indiquée
"""),

    32 : _(u"""
 on n'a pas trouvé l'instant à l'adresse indiquée
"""),

    33 : _(u"""
 on n'a pas trouvé la fréquence à l'adresse indiquée
"""),

    34 : _(u"""
 on n'a pas trouvé dans le fichier UNV le type de champ
"""),

    35 : _(u"""
 on n'a pas trouvé dans le fichier UNV le nombre de composantes à lire
"""),

    36 : _(u"""
 on n'a pas trouvé dans le fichier UNV la nature du champ
 (réel ou complexe)
"""),

    37 : _(u"""
 le type de champ demandé est différent du type de champ à lire
"""),

    38 : _(u"""
 le champ demande n'est pas de même nature que le champ à lire
 (réel/complexe)
"""),

    39 : _(u"""
 le mot clé MODELE est obligatoire pour un CHAM_ELEM
"""),

    40 : _(u"""
 Problème correspondance noeud IDEAS
"""),

    41 : _(u"""
 le champ de type ELGA n'est pas supporté
"""),




    65 : _(u"""
 pour la variable d'accès "NOEUD_CMP", il faut un nombre pair de valeurs.
"""),

    66 : _(u"""
 le modèle et le maillage introduits ne sont pas cohérents
"""),

    67 : _(u"""
 il faut donner le maillage pour une impression au format "CASTEM".
"""),

    68 : _(u"""
 vous voulez imprimer sur un même fichier le maillage et un champ
 ce qui est incompatible avec le format GMSH
"""),

    69 : _(u"""
 L'impression d'un champ complexe nécessite l'utilisation du mot-clé PARTIE.
 Ce mot-clé permet de choisir la partie du champ à imprimer (réelle ou imaginaire).
"""),

    70 : _(u"""
 Vous avez demandé une impression au format ASTER sans préciser de MAILLAGE.
 Aucune impression ne sera réalisée car IMPR_RESU au format ASTER n'imprime qu'un MAILLAGE.
"""),

    73 : _(u"""
 l'impression avec sélection sur des entités topologiques n'a pas de sens au format CASTEM  : toutes les valeurs sur tout le maillage seront donc imprimées.
"""),

    74 : _(u"""
 Le maillage %(k1)s n'est pas cohérent avec le maillage %(k2)s portant le résultat %(k3)s
"""),

    75 : _(u"""
 fichier GIBI créé par SORT FORMAT non supporté dans cette version
"""),

    76 : _(u"""
 version de GIBI non supportée, la lecture peut échouer
"""),

    77 : _(u"""
 fichier GIBI erroné
"""),

    78 : _(u"""
 le fichier maillage GIBI est vide
"""),

    81 : _(u"""
 Il n'a pas été possible de récupérer d'information concernant la matrice de masse
 assemblée de la structure. Le calcul de l'option %(k1)s n'est donc pas possible.
 """),

    84 : _(u"""
 il faut autant de composantes en i et j
"""),

    85 : _(u"""
 il faut autant de composantes que de noeuds
"""),

    93 : _(u"""
 la fonction n'existe pas.
"""),

    94 : _(u"""
 il faut définir deux paramètres pour une nappe.
"""),

    95 : _(u"""
 pour le paramètre donné on n'a pas trouvé la fonction.
"""),


}
