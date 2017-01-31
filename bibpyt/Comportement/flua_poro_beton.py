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
# person_in_charge: etienne.grimal at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='FLUA_PORO_BETON',
    doc="""Loi Fluage pour le beton""",
    num_lc=65,
    nb_vari=77,
    nom_vari=(
        'HYD0', 'SSG1', 'SSG2', 'SSG3', 'SSG4', 'SSG5', 'SSG6', 'EPG1', 'EPG2', 'EPG3', 'EPG4', 'EPG5', 'EPG6', 'DG1', 'DG2', 'DG3', 'PWAT', 'PGEL', 'SSW1', 'SSW2',
        'SSW3', 'SSW4', 'SSW5', 'SSW6', 'DW1', 'DW2', 'DW3', 'DTH', 'PAS0', 'E0S', 'E1S', 'E2S', 'VE1S', 'VE2S', 'E0D1', 'E1D1', 'E2D1', 'VE11', 'VE21',
        'E0D2', 'E1D2', 'E2D2', 'VE12', 'VE22', 'E0D3', 'E1D3', 'E2D3', 'VE13', 'VE23', 'E0D4', 'E1D4', 'E2D4', 'VE14', 'VE24', 'E0D5', 'E1D5', 'E2D5', 'VE15', 'VE25',
        'E0D6', 'E1D6', 'E2D6', 'VE16', 'VE26', 'TMP1', 'MSRF', 'SET1', 'SET2', 'SET3', 'SET4', 'SET5', 'SET6', 'PHIS', 'PHID', 'EPEQ', 'DFLU', 'CCF1'),
    mc_mater = ('ELAS', 'FLUA3D'),
    modelisation = ('3D',),
    deformation = ('PETIT', 'PETIT_REAC'),
    nom_varc = ('TEMP', 'HYDR'),
    algo_inte = ('SPECIFIQUE',),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
