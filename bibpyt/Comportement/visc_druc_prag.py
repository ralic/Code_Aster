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
# person_in_charge: samuel.geniaut at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='VISC_DRUC_PRAG',
    doc="""Modele viscoplastique base sur une loi de type Drucker Prager non associee,
   pour la mecanique des roches (cf. [R7.01.22] pour plus de details).Le fluage suit la loi de Perzyna""",
    num_lc=42,
    nb_vari=4,
    nom_vari=('EPSPEQ', 'INDIPLAS', 'POS', 'NBITER'),
    mc_mater = ('ELAS', 'VISC_DRUC_PRAG'),
    modelisation = ('3D', 'AXIS', 'D_PLAN'),
    deformation = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
    nom_varc = ('TEMP',),
    algo_inte = ('SECANTE', 'BRENT',),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = ' ',
)
