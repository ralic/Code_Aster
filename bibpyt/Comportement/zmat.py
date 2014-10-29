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
# person_in_charge: jean-michel.proix at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
    nom='ZMAT',
    doc="""Comportement integre par le logiciel externe Zmat de l'Ecole des Mines de Paris""",
    num_lc=39,
    nb_vari=0,
    nom_vari=None,  # données par Zmat dans le fichier messages
    mc_mater=None,
    modelisation=('3D', 'AXIS', 'D_PLAN'),
    deformation = ('PETIT', 'PETIT_REAC', 'GROT_GDEP', 'GDEF_HYPO_ELAS'),
    nom_varc = ('TEMP', 'IRRA', 'CORR', 'HYDR', 'SECH'),
    algo_inte = ('SANS_OBJET'),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
