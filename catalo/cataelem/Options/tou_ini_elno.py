# coding=utf-8
# person_in_charge: jacques.pellet at edf.fr


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




PDOMMAG  = OutputParameter(phys=PHY.DOMA_R, type='ELNO')


PEPSI_R  = OutputParameter(phys=PHY.EPSI_R, type='ELNO')


PGEOM_R  = OutputParameter(phys=PHY.GEOM_R, type='ELNO')


PINST_R  = OutputParameter(phys=PHY.INST_R, type='ELNO')


PNEUT_F  = OutputParameter(phys=PHY.NEUT_F, type='ELNO')


PNEUT_R  = OutputParameter(phys=PHY.NEUT_R, type='ELNO')


PPRES_R  = OutputParameter(phys=PHY.PRES_R, type='ELNO')


PSIEF_R  = OutputParameter(phys=PHY.SIEF_R, type='ELNO')


PVARI_R  = OutputParameter(phys=PHY.VARI_R, type='ELNO')


PSOUR_R  = OutputParameter(phys=PHY.SOUR_R, type='ELNO')


PHYDRPM  = OutputParameter(phys=PHY.HYDR_R, type='ELNO')


PFLUX_R  = OutputParameter(phys=PHY.FLUX_R, type='ELNO')


TOU_INI_ELNO = Option(
    para_in=(
    ),
    para_out=(
           PDOMMAG,
           PEPSI_R,
           PFLUX_R,
           PGEOM_R,
           PHYDRPM,
           PINST_R,
           PNEUT_F,
           PNEUT_R,
           PPRES_R,
           PSIEF_R,
           PSOUR_R,
           PVARI_R,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),)),
      CondCalcul('+', ((AT.PHENO,'AC'),)),
      CondCalcul('+', ((AT.PHENO,'PR'),)),
      CondCalcul('+', ((AT.LXFEM,'OUI'),)),
    ),
)
