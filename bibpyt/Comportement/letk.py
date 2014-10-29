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
# person_in_charge: alexandre.foucault at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='LETK',
    doc="""Relation de comportement pour la modélisation élasto visco plastique des roches suivant le modèle de Laigle et Kleine, cf. [R7.01.24].
   L'opérateur relatif à la prédiction élastique est celui de l'élasticité non linéaire spécifique à la loi.""",
    num_lc=35,
    nb_vari=9,
    nom_vari=('XIP', 'GAMMAP', 'XIVP', 'GAMMAVP',
                     'INDICDIL', 'INDIVISC', 'INDIPLAS', 'DOMAINE', 'INDIC'),
    mc_mater = ('ELAS', 'LETK'),
    modelisation = ('3D', 'AXIS', 'D_PLAN', 'THM'),
    deformation = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
    nom_varc = ('TEMP'),
    algo_inte = ('NEWTON', 'NEWTON_PERT', 'SPECIFIQUE'),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = ' ',
)
