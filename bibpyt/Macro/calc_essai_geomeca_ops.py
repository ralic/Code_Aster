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

from geomec_utils import *
from geomec_essais import *


def calc_essai_geomeca_ops(self, MATER, COMPORTEMENT, CONVERGENCE, INFO,
                           ESSAI_TD,
                           ESSAI_TND,
                           ESSAI_CISA_C,
                           ESSAI_TND_C,
                           ESSAI_TD_A,
                           ESSAI_TD_NA,
                           ESSAI_OEDO_C,
                           ESSAI_ISOT_C,
                           # ESSAI_XXX,
                           **args):
    """Corps de CALC_ESSAI_GEOMECA"""

    ier = 0

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Verifs supplementaires des valeurs renseignees pr les MCF ESSAI_*
    verif_essais(COMPORTEMENT, ESSAI_TD,
                 ESSAI_TND,
                 ESSAI_CISA_C,
                 ESSAI_TND_C,
                 ESSAI_TD_A,
                 ESSAI_TD_NA,
                 ESSAI_OEDO_C,
                 ESSAI_ISOT_C,)
                           # ESSAI_XXX,)

    # ---
    # Essai 'TD'
    # ---
    if ESSAI_TD != None:

        for iocc, DicoEssai in enumerate(ESSAI_TD.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_TD.List_F()))
            essai_TD(self, str_num, DicoEssai,
                     MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'TND'
    # ---
    if ESSAI_TND != None:

        for iocc, DicoEssai in enumerate(ESSAI_TND.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_TND.List_F()))
            essai_TND(self, str_num, DicoEssai,
                      MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'CISA_C'
    # ---
    if ESSAI_CISA_C != None:

        for iocc, DicoEssai in enumerate(ESSAI_CISA_C.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_CISA_C.List_F()))
            essai_CISA_C(self, str_num, DicoEssai,
                         MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'TND_C'
    # ---
    if ESSAI_TND_C != None:

        for iocc, DicoEssai in enumerate(ESSAI_TND_C.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_TND_C.List_F()))
            essai_TND_C(self, str_num, DicoEssai,
                        MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'TD_A'
    # ---
    if ESSAI_TD_A != None:

        for iocc, DicoEssai in enumerate(ESSAI_TD_A.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_TD_A.List_F()))
            essai_TD_A(self, str_num, DicoEssai,
                       MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'TD_NA'
    # ---
    if ESSAI_TD_NA != None:

        for iocc, DicoEssai in enumerate(ESSAI_TD_NA.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_TD_NA.List_F()))
            essai_TD_NA(self, str_num, DicoEssai,
                        MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'OEDO_C'
    # ---
    if ESSAI_OEDO_C != None:

        for iocc, DicoEssai in enumerate(ESSAI_OEDO_C.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_OEDO_C.List_F()))
            essai_OEDO_C(self, str_num, DicoEssai,
                         MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'ISOT_C'
    # ---
    if ESSAI_ISOT_C != None:

        for iocc, DicoEssai in enumerate(ESSAI_ISOT_C.List_F()):
            str_num = int_2_str(iocc + 1, len(ESSAI_ISOT_C.List_F()))
            essai_ISOT_C(self, str_num, DicoEssai,
                         MATER, COMPORTEMENT, CONVERGENCE, INFO)

    # ---
    # Essai 'XXX'
    # ---
    # if ESSAI_XXX != None : ...
    return ier
