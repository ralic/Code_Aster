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




PCOMPOR  = InputParameter(phys=PHY.COMPOR, container='RESU!COMPORTEMENT!N',
comment="""  PCOMPOR :  COMPORTEMENT STOCKE DANS LA SD_RESULTAT  """)


PCONTRR  = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_ELGA!N',
comment="""  PCONTRR : ETAT DE CONTRAINTE AUX POINTS DE GAUSS """)


PSTRXRR  = InputParameter(phys=PHY.STRX_R, container='RESU!STRX_ELGA!N',
comment="""  PSTRXRR : CHAMPS SPECIAL ELEMENTS DE STRUCTURE """)


PVARCPR  = InputParameter(phys=PHY.VARI_R, container='VOLA!&&CCPARA.VARI_INT_N',
comment="""  PVARCPR : VARIABLE DE COMMANDE INSTANT ACTUEL """)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  CAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PEFFORR  = OutputParameter(phys=PHY.SIEF_R, type='ELNO')


EFGE_ELNO = Option(
    para_in=(
        SP.PCAARPO,
        SP.PCACOQU,
        SP.PCADISK,
        SP.PCAGEPO,
        SP.PCAGNBA,
        SP.PCAGNPO,
           PCAORIE,
        SP.PCHDYNR,
        SP.PCINFDI,
        SP.PCOEFFC,
        SP.PCOEFFR,
           PCOMPOR,
           PCONTRR,
        SP.PDEPLAR,
        SP.PFF1D1D,
        SP.PFIBRES,
        SP.PFR1D1D,
        SP.PGEOMER,
        SP.PMATERC,
           PNBSP_I,
        SP.PNONLIN,
        SP.PPESANR,
           PSTRXRR,
        SP.PSUROPT,
        SP.PTEMPSR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PEFFORC,
           PEFFORR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.EFGE,'OUI'),(AT.BORD,'0'),)),
    ),
    comment="""  EFGE_ELNO : CALCUL DES EFFORTS GENERALISES AUX NOEUDS
           A PARTIR DES DEPLACEMENTS OU DES CONTRAINTES """,
)
