
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




PNBSP_I  = InputParameter(phys=PHY.NBSP_I,
comment=""" NOMBRE DE SOUS-POINTS (EPAISSEUR COQUES/TUYAUX) ET DE FIBRES (PMF) """)


PCAORIE  = InputParameter(phys=PHY.CAORIE,
comment=""" ORIENTATION DES REPERES LOCAUX DES POUTRES ET TUYAUX """)


PPINTTO  = InputParameter(phys=PHY.N132_R)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PCOORPG  = OutputParameter(phys=PHY.GEOM_R, type='ELGA')


COOR_ELGA = Option(
    para_in=(
        SP.PCACOQU,
        SP.PCAGEPO,
           PCAORIE,
           PCNSETO,
        SP.PFIBRES,
        SP.PGEOMER,
           PLONCHA,
           PNBSP_I,
           PPINTTO,
           PPMILTO,
    ),
    para_out=(
           PCOORPG,
    ),
    condition=(
      CondCalcul('+', (('PHENO','ME'),)),
      CondCalcul('+', (('PHENO','TH'),)),
      CondCalcul('+', (('PHENO','AC'),)),
    ),
)
