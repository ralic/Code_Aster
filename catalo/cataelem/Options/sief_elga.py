

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




PVARCPR  = InputParameter(phys=PHY.VARI_R, container='VOLA!&&CCPARA.VARI_INT_N',
comment="""  PVARCPR : VARIABLES DE COMMANDE  """)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PCOMPOR  = InputParameter(phys=PHY.COMPOR, container='CHMA!.COMPOR',
comment="""  PCOMPOR :  DESCRIPTION DU COMPORTEMENT DE CHAQUE GROUPE DE FIBRES
           NECESSITE DE FOURNIR LE CONCEPT PRODUIT PAR AFFE_MATERIAU  """)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PPINTTO  = InputParameter(phys=PHY.N132_R,
comment=""" XFEM - COORD. POINTS SOMMETS DES SOUS-ELEMENTS """)


PHEAVTO  = InputParameter(phys=PHY.N512_I,
comment=""" XFEM - VALEUR FONCTION HEAVISIDE SUR LES SOUS-ELEMENTS """)


PBASLOR  = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - BASE LOCALE AU FOND DE FISSURE """)


PLSN     = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - VALEURS DE LA LEVEL SET NORMALE """)


PLST     = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - VALEURS DE LA LEVEL SET TANGENTE """)


PSTANO   = InputParameter(phys=PHY.N120_I,
comment=""" XFEM - STATUT DES NOEUDS (ENRICHISSEMENT) """)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PHEA_NO  = InputParameter(phys=PHY.N120_I)


PFISNO   = InputParameter(phys=PHY.NEUT_I,
comment=""" PFISNO : CONNECTIVITE DES FISSURES ET DES DDL HEAVISIDE """)


PSTRXRR  = InputParameter(phys=PHY.STRX_R, container='RESU!STRX_ELGA!N',
comment="""  PSTRXRR : CHAMPS ELEMENTS DE STRUCTURE INSTANT ACTUEL """)


PCONTRR  = OutputParameter(phys=PHY.SIEF_R, type='ELGA')


SIEF_ELGA = Option(
    para_in=(
           PBASLOR,
        SP.PCAARPO,
        SP.PCACOQU,
        SP.PCADISK,
        SP.PCAGEPO,
        SP.PCAGNBA,
        SP.PCAGNPO,
        SP.PCAMASS,
           PCAORIE,
        SP.PCINFDI,
           PCNSETO,
           PCOMPOR,
        SP.PDEPLAR,
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
           PSTRXRR,
        SP.PTEMPSR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PCONTRC,
           PCONTRR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.ABSO,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'3FL'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'2FL'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'AXF'),)),
    ),
    comment=""" CALCUL DES CONTRAINTES ET/OU EFFORTS GENERALISES AUX POINTS DE GAUSS
   A PARTIR DES DEPLACEMENTS. LICITE EN LINEAIRE SEULEMENT. """,
)
