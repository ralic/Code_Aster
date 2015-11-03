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




PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment="""  PCOMPOR : COMPORTEMENT """)


PPINTTO  = InputParameter(phys=PHY.N132_R)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I)


PHEA_NO  = InputParameter(phys=PHY.N120_I)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PBASLOR  = InputParameter(phys=PHY.NEUT_R)


PLSN     = InputParameter(phys=PHY.NEUT_R)


PLST     = InputParameter(phys=PHY.NEUT_R)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PPINTER  = InputParameter(phys=PHY.N816_R)


PAINTER  = InputParameter(phys=PHY.N1360R)


PCFACE   = InputParameter(phys=PHY.N720_I)


PBASECO  = InputParameter(phys=PHY.N2448R)


REFE_FORC_NODA = Option(
    para_in=(
           PAINTER,
           PBASECO,
           PBASLOR,
        SP.PCACOQU,
        SP.PCAGEPO,
        SP.PCAGNBA,
        SP.PCAMASS,
           PCAORIE,
           PCFACE,
        SP.PCINFDI,
           PCNSETO,
           PCOMPOR,
        SP.PDEPLMR,
        SP.PGEOMER,
           PHEAVTO,
           PHEA_NO,
           PLONCHA,
        SP.PLONFA,
           PLSN,
           PLST,
        SP.PMATERC,
           PNBSP_I,
           PPINTER,
           PPINTTO,
           PPMILTO,
        SP.PREFCO,
    ),
    para_out=(
        SP.PVECTUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
    ),
    comment="""  REFE_FORC_NODA : CALCUL DE LA REFERENCE DE FORCE INTERNE
        POUR LE CRITERE DE CONVERGENCE EN SUPER-RESIDU """,
)
