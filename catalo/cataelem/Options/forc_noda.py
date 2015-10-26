

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




PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment="""  PCOMPOR : COMPORTEMENT """)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PCONTMR  = InputParameter(phys=PHY.SIEF_R,
comment="""  PCONTMR : CONTRAINTES INSTANT PRECEDENT """)


PVARCPR  = InputParameter(phys=PHY.VARI_R,
comment="""  PVARCPR : TEMPERATURE INSTANT ACTUEL """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PPINTTO  = InputParameter(phys=PHY.N132_R)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PBASLOR  = InputParameter(phys=PHY.NEUT_R)


PLSN     = InputParameter(phys=PHY.NEUT_R)


PLST     = InputParameter(phys=PHY.NEUT_R)


PSTANO   = InputParameter(phys=PHY.N120_I)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PFISNO   = InputParameter(phys=PHY.NEUT_I)


PHEA_NO  = InputParameter(phys=PHY.N120_I,
comment="""  CADRE X-FEM : PPINTTO : COORDONNEES DES POINTS D INTERSECTION
                         PHEAVTO : VALEURS DE L HEAVISIDE SUR LES SS-ELTS
                         PBASLOR : BASE LOCALE AU FOND DE FISSURE
                         PLSN    : LEVEL SET NORMALE
                         PLST    : LEVEL SET TANGENTE
                         PSTANO  : STATUT DES NOEUDS (ENRICHISSEMENT) """)


FORC_NODA = Option(
    para_in=(
           PBASLOR,
        SP.PCACOQU,
        SP.PCAGEPO,
        SP.PCAGNPO,
        SP.PCAMASS,
           PCAORIE,
        SP.PCINFDI,
           PCNSETO,
           PCOMPOR,
           PCONTMR,
        SP.PDEPLMR,
        SP.PDEPLPR,
        SP.PFIBRES,
           PFISNO,
        SP.PGEOMER,
        SP.PHARMON,
           PHEAVTO,
           PHEA_NO,
        SP.PINSTMR,
        SP.PINSTPR,
           PLONCHA,
           PLSN,
           PLST,
        SP.PMATERC,
           PNBSP_I,
           PPINTTO,
           PPMILTO,
           PSTANO,
        SP.PSTRXMR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PVECTUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.ABSO,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FS2'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FSA'),)),
    ),
    comment="""  FORC_NODA : CALCUL DES FORCES NODALES EQUILIBRANT LES CONTRAINTES
       OU EFFORTS AUX POINTS D'INTEGRATION AU SENS DES TRAVAUX VIRTUELS """,
)
