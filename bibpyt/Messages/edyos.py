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


#
#  messages d'erreur pour interface Aster/edyos
#
#
#    Ce script python permet d'associer un texte aux numéros d'erreur
#    appelés dans le sous programme errcou.f
#    Ces messages d'erreur sont issus de la note HI-26/03/007A
#    "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
#    ANNEXE 1: CODES D'ERREURS  (PAGE 70)
#    FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003
#
#    Les numéros des erreurs de ce script correspondent aux numéros de la
#    référence bibliographique
cata_msg = {

    1 : _(u"""
      YACS : Émetteur inconnu
"""),

    2 : _(u"""
      YACS : Nom de variable inconnu
"""),

    3 : _(u"""
      YACS : Variable ne devant pas être lue mais écrite
"""),


    4 : _(u"""
      YACS : Type de variable inconnu
"""),

    5 : _(u"""
      YACS : Type de variable différent de celui déclaré
"""),

    6 : _(u"""
      YACS : Mode de dépendance inconnu
"""),

    7 : _(u"""
      YACS : Mode de dépendance différent de celui déclaré
"""),

    8 : _(u"""
      YACS : Requête non autorisée
"""),

    9 : _(u"""
      YACS : Type de déconnexion incorrect
"""),

    10 : _(u"""
       YACS : Directive de déconnexion incorrecte
"""),

    11 : _(u"""
       YACS : Nom de code inconnu
"""),

    12 : _(u"""
       YACS : Nom d'instance inconnue
"""),

    13 : _(u"""
      YACS : Requête en attente
"""),

    14 : _(u"""
      YACS : Message de service
"""),

    15 : _(u"""
      YACS : Nombre de valeurs transmises nul
"""),

    16 : _(u"""
       YACS : Dimension de tableau récepteur insuffisante
"""),

    17 : _(u"""
      YACS : Blocage
"""),

    18 : _(u"""
      YACS : Arrêt anormal d'une instance
"""),

    19 : _(u"""
      YACS : Coupleur absent...
"""),

    20 : _(u"""
      YACS : Variable ne figurant dans aucun lien
"""),

    21 : _(u"""
      YACS : Nombre de pas de calcul égal à zéro
"""),

    22 : _(u"""
      YACS : Machine non déclarée
"""),

    23 : _(u"""
      YACS : Erreur variable d'environnement COUPLAGE_GROUPE non positionnée
"""),

    24 : _(u"""
=      YACS : Variable d'environnement COUPLAGE_GROUPE inconnue
"""),

    25 : _(u"""
      YACS : Valeur d'information non utilisée
"""),

    26 : _(u"""
      YACS : Erreur de format dans le fichier de couplage
"""),

    27 : _(u"""
      YACS : Requête annulée à cause du passage en mode normal
"""),

    28 : _(u"""
      YACS : Coupleur en mode d'exécution normal
"""),

    29 : _(u"""
      YACS : Option inconnue
"""),

    30 : _(u"""
      YACS : Valeur d'option incorrecte
"""),

    31 : _(u"""
      YACS : Écriture d'une variable dont l'effacement est demandé
"""),

    32 : _(u"""
      YACS : Lecture d'une variable incorrectement connectée
"""),

    33 : _(u"""
      YACS : Valeur d'information non utilisée
"""),

    34 : _(u"""
      YACS : Valeur d'information non utilisée
"""),

    35 : _(u"""
      YACS : Erreur dans la chaîne de déclaration
"""),

    36 : _(u"""
      YACS : Erreur dans le lancement dynamique d'une instance
"""),

    37 : _(u"""
      YACS : Erreur de communication
"""),

    38 : _(u"""
      YACS : Valeur d'information non utilisée
"""),

    39 : _(u"""
      YACS : Mode d'exécution non défini
"""),

    40 : _(u"""
      YACS : Instance déconnectée
"""),


    41 : _(u"""
 Avertissement YACS (gravité faible) :
       Dans le SSP %(k1)s la variable %(k2)s a une valeur différente
       de celle envoyée
"""),

    42 : _(u"""
 Erreur YACS :
       Problème dans le SSP  : %(k1)s
       Pour la variable      : %(k2)s
       A l'itération numéro  : %(i1)d
"""),

    43 : _(u"""
      Attention, le nombre maximal de palier est 20
"""),

    45 : _(u"""
      Non convergence du code EDYOS
"""),

    46 : _(u"""
      Le code EDYOS n'a pas convergé
      Avec le schéma en temps d'Euler, on ne sous divise pas le pas de temps
      Le calcul s'arrête donc
      Conseil : tester le schéma en temps adaptatif
"""),

    48 : _(u"""
      Erreur de syntaxe pour le couplage avec EDYOS :
      Il faut obligatoirement renseigner COUPLAGE_EDYOS et PALIERS_EDYOS
"""),

    49 : _(u"""
      Erreur de syntaxe pour le couplage avec EDYOS :
      Pour le mot-clé PALIERS_EDYOS dans le cas où l'on utilise TYPE_EDYOS,
      il faut donner à chaque occurrence soit le GROUP_NO du palier, soit son NOEUD.

"""),

}
