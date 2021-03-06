# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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




PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PVARCPR  = InputParameter(phys=PHY.VARI_R,
comment="""  PVARCPR : VARIABLES DE COMMANDE  """)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PCOMPOR  = InputParameter(phys=PHY.COMPOR)


PPINTTO  = InputParameter(phys=PHY.N132_R,
comment="""  PPINTTO : CHAMP SPECIFIQUE XFEM  """)


PCNSETO  = InputParameter(phys=PHY.N1280I,
comment="""  PCNSETO : CHAMP SPECIFIQUE XFEM  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I,
comment="""  PHEAVTO : CHAMP SPECIFIQUE XFEM  """)


PHEA_NO  = InputParameter(phys=PHY.N120_I)


PLONCHA  = InputParameter(phys=PHY.N120_I,
comment="""  PLONCHA : CHAMP SPECIFIQUE XFEM  """)


PBASLOR  = InputParameter(phys=PHY.NEUT_R,
comment="""  PBASLOR : CHAMP SPECIFIQUE XFEM  """)


PLSN     = InputParameter(phys=PHY.NEUT_R,
comment="""  PLSN    : CHAMP SPECIFIQUE XFEM  """)


PLST     = InputParameter(phys=PHY.NEUT_R,
comment="""  PLST    : CHAMP SPECIFIQUE XFEM  """)


PSTANO   = InputParameter(phys=PHY.N120_I,
comment="""  PSTANO  : CHAMP SPECIFIQUE XFEM  """)


PPMILTO  = InputParameter(phys=PHY.N792_R,
comment="""  PPMILTO : CHAMP SPECIFIQUE XFEM  """)


PFISNO   = InputParameter(phys=PHY.NEUT_I,
comment="""  PFISNO  : CHAMP SPECIFIQUE XFEM  """)


CHAR_MECA_TEMP_R = Option(
    para_in=(
           PBASLOR,
        SP.PCAARPO,
        SP.PCACOQU,
        SP.PCAGEPO,
        SP.PCAGNBA,
        SP.PCAGNPO,
        SP.PCAMASS,
           PCAORIE,
           PCNSETO,
           PCOMPOR,
        SP.PFIBRES,
           PFISNO,
        SP.PGEOMER,
        SP.PHARMON,
           PHEAVTO,
           PHEA_NO,
           PLONCHA,
           PLSN,
           PLST,
        SP.PMATERC,
           PNBSP_I,
           PPINTTO,
           PPMILTO,
           PSTANO,
        SP.PTEMPSR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PCONTRT,
        SP.PVECTUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.TYPMOD2, 'THM'),)),
#     Les elements d'interface ne sont pas concernes (issue24099) :
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.INTERFACE,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.FLUIDE,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'3FI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'AFI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'PFI'),)),
    ),
    comment=""" CHAR_MECA_TEMP_R (MOT-CLE : TEMP_CALCULEE): CALCUL DU SECOND MEMBRE
           ELEMENTAIRE CORRESPONDANT A UN CHAMP DE TEMPERATURE""",
)
