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
    nom='RGI_BETON',
    doc="""lois associees aux reactions de gonglements internes (RAG et RSI) dans le beton""",
    num_lc=67,
    nb_vari=26,
    nom_vari=(
        'SEE1', 'SEE2', 'SEE3', 'SEE4', 'SEE5', 'SEE6', 'HYD', 'CSH', 'ALL', 'ALF', 'SULL', 'SULF',
        'AFT', 'AFM', 'ID', 'CASH', 'CSHE', 'TMDF', 'CAL', 'VDEF', 'VTOT', 'ARAG', 'VRAG', 'NSOL', 'DTHE', 'VWE'),
    mc_mater = ('ELAS', 'RGILIN3D'),
    modelisation = ('3D',),
    deformation = ('PETIT', 'PETIT_REAC'),
    nom_varc = ('TEMP',),
    algo_inte = ('SPECIFIQUE',),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
