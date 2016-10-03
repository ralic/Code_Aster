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

    3 : _(u"""
Le champ %(k1)s que l'on veut extraire est de type champ aux noeuds.
Vous n'avez pas précisé correctement son lieu d'extraction ou les noeuds donnés n'appartiennent pas au modèle.
"""),

    4 : _(u"""
Le champ %(k1)s que l'on veut extraire est de type champ aux points d'intégration.
Vous n'avez pas précisé correctement son lieu d'extraction ou les mailles données n'appartiennent pas au modèle.
"""),

    5 : _(u"""
Vous n'avez pas précisé le type de l'extraction pour le champ %(k1)s.
On a pris <VALE> par défaut.
"""),

    6 : _(u"""
Le champ %(k1)s que l'on veut extraire est de type champ aux points d'intégration.
Vous n'avez pas précisé le type de l'extraction.
On a pris <VALE> par défaut.
"""),

    7 : _(u"""
Le champ %(k1)s est de type <ELGA> et vous voulez extraire sa valeur (EXTR_ELGA='VALE').
Vous n'avez pas précisé l'endroit où il est doit être extrait.
Il faut donner le point d'intégration et le SOUS_POINT si c'est un élément de structure (POINT/SOUS_POINT).
"""),

    12 : _(u"""
 L'extraction doit se faire sur plus d'une composante et moins de %(i1)d composantes, or vous en avez %(i2)d.
"""),

    20 : _(u"""
 La composante %(k2)s est inconnue sur le noeud %(k1)s .
"""),

    21 : _(u"""
 La composante %(k2)s sur la maille %(k1)s sur le point d'intégration %(i1)d et le SOUS_POINT %(i2)d n'existe pas.
"""),

    22 : _(u"""
 La variable interne nommée %(k1)s n'existe pas sur les mailles concernées.
"""),

    24 : _(u"""
Erreur utilisateur commande RECU_TABLE / RESU :
  On veut désigner des variables internes en utilisant le mot clé NOM_VARI.
  Le mot clé RESU / RESULTAT est obligatoire. 
"""),

    25 : _(u"""
Erreur utilisateur commande RECU_TABLE / RESU :
  On veut désigner des variables internes en utilisant le mot clé NOM_VARI.
  Le champ concerné doit être un champ par élément de VARI_R.
  Ici, NOM_CHAM = %(k1)s 
"""),


    99: _(u"""
 Le champ %(k1)s que l'on veut extraire est incompatible avec la commande ou les fonctionnalités actives.
 On l'ignore.
"""),

}
