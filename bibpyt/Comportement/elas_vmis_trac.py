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
# person_in_charge: jean-michel.proix at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='ELAS_VMIS_TRAC',
    doc="""Elasticité non linéaire de Von Mises - Hencky à écrouissage isotrope défini
par une courbe de traction affine par morceaux""",
    num_lc=1,
    nb_vari=2,
    nom_vari=('EPSPEQ', 'INDIPLAS'),
    mc_mater = ('ELAS', 'TRACTION'),
    modelisation = ('3D', 'AXIS', 'C_PLAN', 'D_PLAN'),
    deformation = ('PETIT', 'GROT_GDEP'),
    nom_varc = ('TEMP'),
    algo_inte = 'ANALYTIQUE',
    type_matr_tang = None,
    proprietes = ('COMP_ELAS'),
)
