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




PPINTTO  = OutputParameter(phys=PHY.N132_R, type='ELEM')


PCNSETO  = OutputParameter(phys=PHY.N1280I, type='ELEM')


PHEAVTO  = OutputParameter(phys=PHY.N512_I, type='ELEM')


PLONCHA  = OutputParameter(phys=PHY.N120_I, type='ELEM')


PPMILTO  = OutputParameter(phys=PHY.N792_R, type='ELEM')


PAINTTO  = OutputParameter(phys=PHY.N480_R, type='ELEM')


TOPOSE = Option(
    para_in=(
        SP.PFISCO,
        SP.PGEOMER,
        SP.PLEVSET,
    ),
    para_out=(
           PAINTTO,
           PCNSETO,
           PHEAVTO,
           PLONCHA,
           PPINTTO,
           PPMILTO,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.LXFEM,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'-1'),(AT.LXFEM,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),(AT.BORD,'0'),(AT.LXFEM,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),(AT.BORD,'-1'),(AT.LXFEM,'OUI'),)),
    ),
    comment=""" TOPOSE : CALCUL DU DE LA TOPOLOGIE DES
           SOUS-ELEMENTS POUR L INTEGRATION AVEC X-FEM """,
)
