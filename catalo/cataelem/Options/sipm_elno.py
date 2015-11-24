# coding=utf-8
# person_in_charge: jean-luc.flejou at edf.fr


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
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU,
           ISSUE DE AFFE_CARA_ELEM MOT CLE ORIENTATION """)


PSIEFNOR = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_ELNO!N',
comment="""  PPSIEFNOR : ETAT DE CONTRAINTE AUX NOEUDS """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS """)


SIPM_ELNO = Option(
    para_in=(
        SP.PCAARPO,
        SP.PCAGEPO,
        SP.PCAGNPO,
           PCAORIE,
        SP.PCHDYNR,
        SP.PCOEFFC,
        SP.PCOEFFR,
        SP.PDEPLAR,
        SP.PFF1D1D,
        SP.PFR1D1D,
        SP.PGEOMER,
        SP.PMATERC,
           PNBSP_I,
        SP.PPESANR,
           PSIEFNOR,
        SP.PSUROPT,
        SP.PTEMPSR,
           PVARCPR,
        SP.PVARCRR,
    ),
    para_out=(
        SP.PSIMXRC,
        SP.PSIMXRR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.DIM_TOPO_MODELI,'1'),)),
    ),
    comment="""  SIPM_ELNO : CALCUL DES CONTRAINTES (COMP SIXX) MAXI ET MINI AUX NOEUDS
           DANS LA SECTION DE POUTRE A PARTIR DES EFFORTS GENERALISES.
           LICITE EN LINEAIRE SEULEMENT. """,
)
