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

from cataelem.Tools.base_objects import InputParameter, OutputParameter, Option, CondCalcul
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.parameters as SP
import cataelem.Commons.attributes as AT


PCAORIE = InputParameter(phys=PHY.GEOM_R)


CARA_SECT_POUT4 = Option(
    para_in=(
        SP.PCACOQU,
        PCAORIE,
        SP.PGEOMER,
        SP.PORIGIN,
    ),
    para_out=(
        SP.PVECTU1,
        SP.PVECTU2,
    ),
    condition=(
        CondCalcul('+', ((AT.PHENO, 'ME'), (AT.BORD, '-1'),)),
        CondCalcul(
            '+', ((AT.PHENO, 'ME'), (AT.BORD, '0'), (AT.COQUE, 'OUI'),)),
    ),
)
