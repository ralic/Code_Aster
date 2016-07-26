# coding=utf-8
# person_in_charge: samuel.geniaut at edf.fr


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




PLST     = InputParameter(phys=PHY.NEUT_R)

PLSN     = InputParameter(phys=PHY.NEUT_R)

PPINTER  = InputParameter(phys=PHY.N816_R)


PAINTER  = InputParameter(phys=PHY.N1360R)


PCFACE   = InputParameter(phys=PHY.N720_I)


PLONGCO  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PBASECO  = InputParameter(phys=PHY.N2448R)


PFISNO   = InputParameter(phys=PHY.NEUT_I)


PHEA_NO  = InputParameter(phys=PHY.N120_I)


PHEA_FA  = InputParameter(phys=PHY.N240_I)


PCOHESO  = OutputParameter(phys=PHY.NEUT_R, type='ELEM')

PBASLOR  = InputParameter(phys=PHY.NEUT_R)

PSTANO = InputParameter(phys=PHY.N120_I,
                        comment=""" XFEM """)

PBASLOC  = InputParameter(phys=PHY.N480_R)

PLSNGG     = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM """)

# Attention : les champs PINDCOO, PINDMEM, PINDCOT et PCOHESO
# sont des champs a sous-points
# pour les elements de contact XFEM (xhc,xhtc,xtc)

XCVBCA = Option(
    para_in=(
           PAINTER,
           PBASECO,
        SP.PCAR_AI,
        SP.PCAR_PT,
           PCFACE,
        SP.PCOHES,
        SP.PDEPL_P,
        SP.PDONCO,
           PFISNO,
        SP.PGEOMER,
        SP.PGLISS,
        SP.PHEAVNO,
           PHEA_FA,
           PHEA_NO,
        SP.PINDCOI,
           PLONGCO,
           PLST,
        SP.PMATERC,
        SP.PMEMCON,
           PPINTER,
           PLSN,
           PBASLOR,
           PSTANO,
           PBASLOC,
           PLSNGG,
    ),
    para_out=(
           PCOHESO,
        SP.PINCOCA,
        SP.PINDCOO,
        SP.PINDMEM,
    ),
    condition=(
      CondCalcul('+', ((AT.LXFEM,'OUI'),(AT.CONTACT,'OUI'),)),
    ),
)
