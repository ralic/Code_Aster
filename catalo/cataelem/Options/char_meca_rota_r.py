

# ======================================================================
# COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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




PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment=""" COMPORTMENT INFO """)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment=""" BEAMS: ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment=""" MULTIFIBER:  NOMBRE DE SOUS_POINTS """)


PPINTTO  = InputParameter(phys=PHY.N132_R,
comment=""" SPECIFIQUE X-FEM """)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment=""" XFEM - CONNECTIVITE DES SOUS-ELEMENTS """)


PHEA_NO  = InputParameter(phys=PHY.N120_I,
comment=""" SPECIFIQUE X-FEM """)


PHEAVTO  = InputParameter(phys=PHY.N512_I,
comment=""" SPECIFIQUE X-FEM """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment=""" XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS """)


PLSN     = InputParameter(phys=PHY.NEUT_R,
comment=""" SPECIFIQUE X-FEM """)


PLST     = InputParameter(phys=PHY.NEUT_R,
comment=""" SPECIFIQUE X-FEM """)


PSTANO   = InputParameter(phys=PHY.N120_I,
comment=""" SPECIFIQUE X-FEM """)


PPMILTO  = InputParameter(phys=PHY.N792_R,
comment=""" SPECIFIQUE X-FEM """)


CHAR_MECA_ROTA_R = Option(
    para_in=(
        SP.PCACOQU,
        SP.PCAGNPO,
           PCAORIE,
        SP.PCINFDI,
           PCNSETO,
           PCOMPOR,
        SP.PDEPLMR,
        SP.PDEPLPR,
        SP.PFIBRES,
        SP.PGEOMER,
           PHEAVTO,
           PHEA_NO,
           PLONCHA,
           PLSN,
           PLST,
        SP.PMATERC,
           PNBSP_I,
           PPINTTO,
           PPMILTO,
        SP.PROTATR,
           PSTANO,
    ),
    para_out=(
        SP.PVECTUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
    ),
    comment=""" SECOND MEMBRE POUR LES FORCES CENTRIFUGES (ROTATION) """,
)
