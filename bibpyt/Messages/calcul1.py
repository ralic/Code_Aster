# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mickael.abbas at edf.fr

# For the command CALCUL and not for CALCUL.F90 !!!

cata_msg = {

    1: _(u"""
La table que l'on tente d'enrichir dans la commande CALCUL n'est pas du type attendu.
Elle n'a pas le bon nombre de paramètres.
"""),

    2: _(u"""
La table que l'on tente d'enrichir dans la commande CALCUL n'est pas du type attendu.
Elle n'a pas les bons paramètres.
"""),

    3: _(u"""
Le nom de la table donné par le mot-clef TABLE n'est pas le même que celui de la table produite par la commande CALCUL.
"""),

    4: _(u"""
L'objet %(k1)s au numéro d'ordre %(i1)d existe déjà dans la table fournie.
On l'écrase pour le remplacer par le nouveau.
"""),

    5 : _(u"""
A cause des erreurs précédentes, le code s'arrête.
  Le champ des variables internes fourni à CALCUL n'est pas cohérent avec le comportement donné par le mot-clef COMPORTEMENT.
"""),

    7 : _(u"""
Le modèle contient des éléments qui ne supportent pas les champs de type variables internes.
"""),

    8 : _(u"""
Le modèle contient des éléments qui ne supportent pas les champs de type variables internes.
On ne peut donc pas calculer les options les réclamant.
"""),

    9 : _(u"""
Le modèle contient des éléments qui ne supportent pas les champs de type variables internes.
Or les variables de commande ont des champs métallurgiques. On ne peut pas calculer la contribution de la métallurgie.
"""),

   10 : _(u"""
Le modèle contient des éléments XFEM.
La commande CALCUL ne sait pas traiter ce cas.
"""),

   11 : _(u"""
Le modèle contient des macro-éléments statiques.
La commande CALCUL ne sait pas traiter ce cas.
"""),

   12 : _(u"""
Pour calculer des options non-linéaires dans la commande CALCUL, il faut les déplacements, les contraintes et les variables internes précédents.
"""),

   13 : _(u"""
Pour calculer l'option FORC_NODA dans la commande CALCUL, il faut les déplacements et les contraintes précédents.
"""),

}
