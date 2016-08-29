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

    1: _(u"""
Erreur utilisateur :
 Pour définir le coefficient de dilatation thermique ALPHA, vous devez utiliser uniquement une fonction. Les formules et les nappes sont interdites
 Utilisez CALC_FONC_INTERP si nécessaire.
"""),

    2: _(u"""
Erreur utilisateur :
  Dans le CHAM_MATER %(k1)s, vous avez affecté le matériau %(k2)s.
  Dans ce matériau, il existe un coefficient de dilatation thermique ALPHA
  qui est une fonction de la température.
  Pour pouvoir utiliser cette fonction, il est nécessaire de transformer
  cette fonction (changement de repère : "TEMP_DEF_ALPHA" -> "TEMP_REF").
  Pour cela, l'utilisateur doit fournir une température de référence.

Solution :
  Vérifier que les mailles affectées par le matériau %(k2)s sont bien
  toutes affectées par une température de référence
  (mot clé AFFE_VARC/NOM_VARC='TEMP',VALE_REF=...).
"""),

    3: _(u"""
Erreur utilisateur :
 Problème lors de l'interpolation de la fonction définissant le coefficient de dilatation thermique ALPHA.
 Il faut resserrer le mot clé PRECISION pour le matériau ELAS_FO.
"""),

    42: _(u"""
Erreur utilisateur :
 Le coefficient de dilatation thermique ALPHA, du matériau est une fonction de la température.
 Cette fonction (%(k1)s) n'est définie que par un point.
 TEMP_DEF_ALPHA et TEMP_REF ne sont pas identiques.
 On ne peut pas faire le changement de variable TEMP_DEF_ALPHA -> TEMP_REF.
 On s'arrête donc.

Risque & Conseil:
 Il faut définir la fonction ALPHA avec plus d'un point.
"""),

    56: _(u"""
Erreur utilisateur :
 Un des matériaux du CHAM_MATER %(k1)s contient un coefficient de dilatation ALPHA fonction de la température.
 Mais la température de référence n'est pas fournie sous AFFE_MATERIAU/AFFE_VARC/VALE_REF
"""),

}
