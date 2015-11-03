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




PVARCPR  = InputParameter(phys=PHY.VARI_R)


PCOMPOR  = InputParameter(phys=PHY.COMPOR)


PCONTRR  = InputParameter(phys=PHY.SIEF_R)


PVARIPR  = InputParameter(phys=PHY.VARI_R)


CALC_G_GLOB = Option(
    para_in=(
        SP.PACCELE,
           PCOMPOR,
        SP.PCONTGR,
           PCONTRR,
        SP.PDEFOPL,
        SP.PDEPINR,
        SP.PDEPLAR,
        SP.PEPSINR,
        SP.PFR2D3D,
        SP.PFRVOLU,
        SP.PGEOMER,
        SP.PMATERC,
        SP.PPESANR,
        SP.PPRESSR,
        SP.PROTATR,
        SP.PSIGINR,
        SP.PTHETAR,
           PVARCPR,
        SP.PVARCRR,
           PVARIPR,
        SP.PVITESS,
    ),
    para_out=(
        SP.PGTHETA,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'-1'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.ABSO,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.DISCRET,'OUI'),)),
    ),
)
