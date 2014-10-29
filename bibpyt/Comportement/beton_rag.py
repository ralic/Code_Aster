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
    nom='BETON_RAG',
    doc="""Loi RAG pour le beton""",
    num_lc=44,
    nb_vari=65,
    nom_vari=(
        'ERS', 'EIS', 'EID11', 'ERD11', 'EID22', 'ERD22', 'EID33', 'ERD33', 'EID12', 'ERD12',
        'EID31', 'ERD31', 'EID23', 'ERD23', 'ENDOT11', 'ENDOT22', 'ENDOT33', 'ENDOT12',
        'ENDOT13', 'ENDOT23', 'ENDOC0', 'SUT11', 'SUT22', 'SUT33', 'SUT12', 'SUT13',
        'SUT23', 'SUC', 'PW', 'PCH', 'ARAG', 'ESI', 'ESS', 'EDI11', 'EDS11', 'EDI22',
        'EDS22', 'EDI33', 'EDS33', 'EDI12', 'EDS12', 'EDI13', 'EDS13', 'EDI23', 'EDS23',
        'SEF11', 'SEF22', 'SEF33', 'SEF12', 'SEF13', 'SEF23', 'EVP11', 'EVP22', 'EVP33',
        'EVP12', 'EVP13', 'EVP23', 'BT11', 'BT22', 'BT33', 'BT12', 'BT13', 'BT23', 'BC',
        'PEFFRAG'),
    mc_mater = ('ELAS', 'BETON_RAG'),
    modelisation = ('3D', 'AXIS', 'D_PLAN'),
    deformation = ('PETIT', 'PETIT_REAC'),
    nom_varc = ('TEMP',),
    algo_inte = ('SPECIFIQUE',),
    type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
    proprietes = None,
)
