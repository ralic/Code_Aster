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

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='BETON_BURGER_FP',
    doc="""Comportement de fluage propre du beton selon modele de burger avec non linearite sur le fluide de Maxwell (R7.01.35)""",
    num_lc=30,
    nb_vari=21,
    nom_vari=(
        'ERSP', 'EISP', 'ERD11', 'EID11', 'ERD22', 'EID22', 'ERD33', 'EID33', 'EFD11', 'EFD22',
        'EFD33', 'ERD12', 'EID12', 'ERD23', 'EID23', 'ERD31', 'EID31', 'EFD12', 'EFD23', 'EFD31', 'EIEQM'),
    mc_mater = ('ELAS', 'BETON_BURGER_FP'),
    modelisation = ('3D', 'AXIS', 'C_PLAN', 'D_PLAN'),
    deformation = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
    nom_varc = ('TEMP', 'SECH', 'HYDR'),
    algo_inte = ('NEWTON', 'NEWTON_PERT'),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
