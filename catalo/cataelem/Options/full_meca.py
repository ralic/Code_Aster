

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
comment=""" PARAMETRES DE CONVERGENCE POUR COMPORTEMENT """)


PCAORIE  = InputParameter(phys=PHY.CAORIE,
comment=""" ORIENTATION DES REPERES LOCAUX DES POUTRES ET TUYAUX """)


PCONTMR  = InputParameter(phys=PHY.SIEF_R,
comment=""" VECTEUR DES CONTRAINTES POUR T- """)


PVARIMR  = InputParameter(phys=PHY.VARI_R,
comment=""" VARIABLES INTERNES POUR T- """)


PVARCPR  = InputParameter(phys=PHY.VARI_R,
comment=""" VARIABLES DE COMMANDES  POUR T+ """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I,
comment=""" NOMBRE DE SOUS-POINTS (EPAISSEUR COQUES/TUYAUX) ET DE FIBRES (PMF) """)


PPINTTO  = InputParameter(phys=PHY.N132_R,
comment=""" XFEM - COORD. POINTS SOMMETS DES SOUS-ELEMENTS """)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I,
comment=""" XFEM - VALEUR FONCTION HEAVISIDE SUR LES SOUS-ELEMENTS """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PBASLOR  = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - BASE LOCALE AU FOND DE FISSURE """)


PLSN     = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - VALEURS DE LA LEVEL SET NORMALE """)


PLST     = InputParameter(phys=PHY.NEUT_R,
comment=""" XFEM - VALEURS DE LA LEVEL SET TANGENTE """)


PSTANO   = InputParameter(phys=PHY.N120_I,
comment=""" XFEM - STATUT DES NOEUDS (ENRICHISSEMENT) """)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PFISNO   = InputParameter(phys=PHY.NEUT_I,
comment=""" PFISNO : CONNECTIVITE DES FISSURES ET DES DDL HEAVISIDE """)


PHEA_NO  = InputParameter(phys=PHY.N120_I)


PCONTPR  = OutputParameter(phys=PHY.SIEF_R, type='ELGA',
comment=""" VECTEUR DES CONTRAINTES POUR T+ """)


PVARIPR  = OutputParameter(phys=PHY.VARI_R, type='ELGA',
comment=""" VARIABLES INTERNES POUR T+ """)


PCACO3D  = OutputParameter(phys=PHY.CACO3D, type='ELEM',
comment=""" COQUE_3D (ROTATION FICTIVE AUTOUR DE LA NORMALE) """)


FULL_MECA = Option(
    para_in=(
        SP.PACCKM1,
        SP.PACCPLU,
           PBASLOR,
        SP.PCACABL,
        SP.PCACOQU,
        SP.PCADISK,
        SP.PCAGEPO,
        SP.PCAGNBA,
        SP.PCAGNPO,
        SP.PCAMASS,
           PCAORIE,
        SP.PCARCRI,
        SP.PCINFDI,
           PCNSETO,
           PCOMPOR,
           PCONTMR,
        SP.PDDEPLA,
        SP.PDEPENT,
        SP.PDEPKM1,
        SP.PDEPLMR,
        SP.PDEPLPR,
        SP.PFIBRES,
           PFISNO,
        SP.PGEOMER,
        SP.PHEAVNO,
           PHEAVTO,
           PHEA_NO,
        SP.PINSTMR,
        SP.PINSTPR,
        SP.PITERAT,
           PLONCHA,
           PLSN,
           PLST,
        SP.PMATERC,
           PNBSP_I,
           PPINTTO,
           PPMILTO,
        SP.PROMK,
        SP.PROMKM1,
        SP.PSTADYN,
           PSTANO,
        SP.PSTRXMP,
        SP.PSTRXMR,
        SP.PVARCMR,
           PVARCPR,
        SP.PVARCRR,
        SP.PVARIMP,
           PVARIMR,
        SP.PVITENT,
        SP.PVITKM1,
        SP.PVITPLU,
    ),
    para_out=(
           PCACO3D,
        SP.PCODRET,
           PCONTPR,
        SP.PMATUNS,
        SP.PMATUUR,
        SP.PSTRXPR,
           PVARIPR,
        SP.PVECTUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.ABSO,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FS2'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FSA'),)),
    ),
    comment=""" MATRICE TANGENTE COHERENTE POUR MECANIQUE NON-LINEAIRE """,
)
