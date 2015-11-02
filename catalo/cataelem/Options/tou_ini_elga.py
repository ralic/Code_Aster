
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




PDOMMAG  = OutputParameter(phys=PHY.DOMA_R, type='ELGA')


PEPSI_R  = OutputParameter(phys=PHY.EPSI_R, type='ELGA')


PGEOM_R  = OutputParameter(phys=PHY.GEOM_R, type='ELGA')


PINST_R  = OutputParameter(phys=PHY.INST_R, type='ELGA')


PNEUT_F  = OutputParameter(phys=PHY.NEUT_F, type='ELGA')


PNEUT_R  = OutputParameter(phys=PHY.NEUT_R, type='ELGA')


PPRES_R  = OutputParameter(phys=PHY.PRES_R, type='ELGA')


PSIEF_R  = OutputParameter(phys=PHY.SIEF_R, type='ELGA')


PVARI_R  = OutputParameter(phys=PHY.VARI_R, type='ELGA')


PSOUR_R  = OutputParameter(phys=PHY.SOUR_R, type='ELGA')


PDEPL_R  = OutputParameter(phys=PHY.DEPL_R, type='ELGA')


PFLUX_R  = OutputParameter(phys=PHY.FLUX_R, type='ELGA')


TOU_INI_ELGA = Option(
    para_in=(
    ),
    para_out=(
        SP.PDEPL_C,
           PDEPL_R,
           PDOMMAG,
           PEPSI_R,
        SP.PFACY_R,
           PFLUX_R,
           PGEOM_R,
           PINST_R,
           PNEUT_F,
           PNEUT_R,
           PPRES_R,
           PSIEF_R,
           PSOUR_R,
        SP.PTEMP_R,
        SP.PVALO_R,
           PVARI_R,
    ),
    condition=(
      CondCalcul('+', (('PHENO','ME'),)),
      CondCalcul('+', (('PHENO','TH'),)),
      CondCalcul('+', (('PHENO','AC'),)),
      CondCalcul('+', (('PHENO','PR'),)),
      CondCalcul('+', (('LXFEM','OUI'),)),
    ),
)
