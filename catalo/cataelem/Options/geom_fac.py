# coding=utf-8
# person_in_charge: mickael.abbas at edf.fr


# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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


PPINTER = InputParameter(phys=PHY.N816_R)


PLONGCO = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
                         comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PGESCLO = InputParameter(phys=PHY.N816_R)


PLST = InputParameter(phys=PHY.NEUT_R)


PFISNO = InputParameter(phys=PHY.NEUT_I)


PHEA_NO = InputParameter(phys=PHY.N120_I)


PHEA_FA = InputParameter(phys=PHY.N240_I)

PBASLOR  = InputParameter(phys=PHY.NEUT_R)

PSTANO   = InputParameter(phys=PHY.N120_I)

PLSN     = InputParameter(phys=PHY.NEUT_R)

GEOM_FAC = Option(
    para_in=(
        SP.NOMFIS,
        SP.PDEPLA,
        PFISNO,
        PGESCLO,
        PHEA_FA,
        PHEA_NO,
        PLONGCO,
        PLST,
        PPINTER,
        SP.PGEOMER,
        SP.PMATERC,
        PBASLOR,
        PSTANO,
        PLSN,
    ),
    para_out=(
        SP.PNEWGEM,
        SP.PNEWGES,
    ),
    condition=(
        CondCalcul(
            '+', ((AT.PHENO, 'ME'), (AT.LXFEM, 'OUI'), (AT.CONTACT, 'OUI'),)),
    ),
)
