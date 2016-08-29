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





    8: _(u"""
 La composante %(k1)s n'existe pas dans le champ de la grandeur.
"""),

    9: _(u"""
 les numéros d'ordre des vitesses donnés sous le mot-clé "NUME_ORDRE" ne sont pas valides.
"""),

    10: _(u"""
 Le mode demandé n'est pas un mode couplé.
"""),

    11: _(u"""
 problème(s) rencontre(s) lors de l'accès au RESU_GENE
"""),

    14: _(u"""
 composante généralisée NUME_CMP_GENE non trouvée
 Conseil : vérifiez que la composante généralisée demandée est bien présente dans la base modale.
"""),

    15: _(u"""
 problème(s) rencontre(s) lors de la lecture des fréquences.
"""),

    17: _(u"""
 Seul le type réel est traité.
"""),

    19: _(u"""
 on ne traite que les champs par éléments de type réel.
"""),

    20: _(u"""
 on ne traite pas ce type de champ: %(k1)s
"""),

    21: _(u"""
 "INTERP_NUME" interdit pour récupérer un paramètre en fonction d'une variable d'accès
"""),

    22: _(u"""
 aucun champ trouve pour l'accès  %(k1)s
"""),

    23: _(u"""
 le champ  %(k1)s  n'existe pas dans le RESU_GENE.
"""),

    24: _(u"""
 problème(s) rencontre(s) lors de la lecture de la discrétisation ( instants ou fréquences )
"""),

    26: _(u"""
 ACCE_MONO_APPUI est compatible uniquement avec un champ de type : ACCE
"""),

    27: _(u"""
 manque la définition d'un mot clé
"""),

    29: _(u"""
 nouvelle longueur invalide
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    30: _(u"""
 problème dans le décodage de ( %(k1)s , %(k2)s )
"""),

    31: _(u"""
 type_RESULTAT inconnu : %(k1)s
"""),

    33: _(u"""
 type scalaire inconnu :  %(k1)s
"""),

    34: _(u"""
Structure de données %(k1)s  inexistante
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    36: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    42: _(u"""
 numéro d'ordre trop grand.
"""),

    43: _(u"""
 nom de champ interdit :  %(k1)s  pour le résultat :  %(k2)s
"""),

    44: _(u"""
  pas de variables d'accès
"""),

    45: _(u"""
  pas de paramètres
"""),

    46: _(u"""
 Cet accès est interdit pour un résultat de type champ.
"""),

    47: _(u"""
 Cet accès est interdit :  %(k1)s
"""),

    49: _(u"""
 problème pour récupérer les numéros d'ordre dans la structure "résultat"  %(k1)s
"""),

    50: _(u"""
 problème pour récupérer les paramètres
"""),

    51: _(u"""
 aucun numéro d'ordre ne correspond au paramètre demande  %(k1)s
"""),

    52: _(u"""
 aucun numéro d'ordre ne correspond au champ demande  %(k1)s
"""),

    53: _(u"""
 aucun numéro d'ordre trouve. stop.
"""),

    63: _(u"""
 accès inconnu  %(k1)s
"""),

    64: _(u"""
 la table n'existe pas
"""),

    65: _(u"""
 pas de paramètres définis
"""),

    66: _(u"""
 pas de lignes définis
"""),

    67: _(u"""
 mauvais numéro de ligne
"""),

    68: _(u"""
 nom de table incorrect
"""),

    69: _(u"""
 nombre de valeur a ajoute supérieur au nombre de ligne de la table
"""),

    70: _(u"""
 numéro de ligne négatif
"""),

    71: _(u"""
 numéro de ligne supérieur au nombre de ligne de la table
"""),

    72: _(u"""
 le paramètre n existe pas
"""),

    73: _(u"""
 les types du paramètre ne correspondent pas entre eux.
"""),

    74: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    75: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    76: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    77: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    78: _(u"""
 on n a pas trouve de ligne contenant les deux paramètres.
"""),

    79: _(u"""
 table  %(k1)s  : n'existe pas
"""),

    80: _(u"""
 table  %(k1)s  : aucun paramètre n'est défini
"""),

    81: _(u"""
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    82: _(u"""
 pas de lignes sélectionnées
"""),

    83: _(u"""
 table non diagonalisable
"""),

    84: _(u"""
 impression de la table supérieure a 2000 colonnes, sélectionnez vos paramètres.
"""),

    85: _(u"""
 pagination supprimée, utiliser IMPR_TABLE
"""),

    86: _(u"""
 il faut 3 paramètres pour une impression au format "tableau"
"""),

    87: _(u"""
 on ne trie que 1 ou 2 paramètres
"""),

    89: _(u"""
 seules les %(i1)d premières lignes du titre sont conservées (%(i2)d au total).
"""),

    99: _(u"""
%(k1)s
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),
}
