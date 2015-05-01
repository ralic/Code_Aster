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
# person_in_charge: jean-luc.flejou at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='DIS_BILI_ELAS',
    doc="""Relation de comportement bilineaire des elements discrets""",
    num_lc=9999,
    nb_vari=6,
    nom_vari=('DIS1', 'DIS2', 'DIS3', 'DIS4', 'DIS5', 'DIS6'),
    modelisation = ('DIS_T', 'DIS_TR', '2D_DIS_T', '2D_DIS_TR'),
    deformation = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
    nom_varc = None,
    algo_inte = ('ANALYTIQUE'),
    type_matr_tang = None,
    proprietes = None,
)
