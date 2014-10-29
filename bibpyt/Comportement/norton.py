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

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='NORTON',
    doc="""Modele viscoplastique isotrope de Norton""",
    num_lc=32,
    nb_vari=7,
    nom_vari=(
        'EPSPXX', 'EPSPYY', 'EPSPZZ', 'EPSPXY', 'EPSPXZ', 'EPSPYZ', 'VIDE'),
    mc_mater = ('ELAS', 'LEMAITRE'),
    modelisation = ('3D', 'AXIS', 'D_PLAN'),
    deformation = ('PETIT', 'PETIT_REAC', 'GDEF_HYPO_ELAS', 'GDEF_LOG'),
    nom_varc = ('TEMP'),
    algo_inte = ('RUNGE_KUTTA', 'NEWTON_PERT'),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
