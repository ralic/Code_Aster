# -*- coding: utf-8 -*-
# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

    2 : _(u"""
  option %(k1)s : pour l élément  %(k2)s  il faut ajouter dans le %(k3)s
 le nombre de composante calculées du flux
"""),

    3: _(u"""
  Le MODELE doit être le même pour tous les numéros d'ordre du RESULTAT.
  Faire le post-traitement en rentrant le numéro d'ordre ou explicitement
  le nom du modèle.
"""),

    5 : _(u"""
  Pour l'option %(k1)s, le nombre de couches est limité à 1,
  or vous en avez définies %(i1)d !
  Veuillez contacter votre assistance technique.
"""),

    6 : _(u"""
  Pour ce type d'opération, il n'est pas permis d'utiliser la structure de
  données résultat existante %(k1)s derrière le mot clé reuse.
"""),

    7 : _(u"""
  Erreur développeur : le champ n'a pas été créé car aucun type élément
  ne connaît le paramètre %(k1)s de l'option %(k2)s.
"""),
}
