

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




PCOMPOR  = InputParameter(phys=PHY.COMPOR, container='RESU!COMPORTEMENT!N')


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  CAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU,
           ISSUE DE AFFE_CARA_ELEM MOT CLE ORIENTATION """)


PVARCPR  = InputParameter(phys=PHY.VARI_R, container='VOLA!&&CCPARA.VARI_INT_N',
comment="""  PVARCPR : TEMPERATURES DE TYPE REEL INSTANT ACTUEL """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS """)


PSTRXRR  = InputParameter(phys=PHY.STRX_R, container='RESU!STRX_ELGA!N',
comment="""  PSTRXRR : CHAMPS ELEMENTS DE STRUCTURE INSTANT ACTUEL """)


DEGE_ELNO = Option(
    para_in=(
        SP.PCACOQU,
        SP.PCAGEPO,
        SP.PCAGNPO,
           PCAORIE,
           PCOMPOR,
        SP.PDEPLAR,
        SP.PGEOMER,
        SP.PMATERC,
           PNBSP_I,
           PSTRXRR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PDEFOGR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.EFGE,'OUI'),(AT.BORD,'0'),)),
    ),
    comment=""" DEFORMATIONS GENERALISEES DANS LES ELEMENTS DE STRUCTURE : COQUES OU POUTRES
   CALCULEES AUX NOEUDS DE CHAQUE ELEMENT A PARTIR DES DEPLACEMENTS.""",
)
