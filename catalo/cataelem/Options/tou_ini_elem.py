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




PNBSP_I  = OutputParameter(phys=PHY.NBSP_I, type='ELEM')


PERREUR  = OutputParameter(phys=PHY.ERRE_R, type='ELEM')


PPRES_R  = OutputParameter(phys=PHY.PRES_R, type='ELEM')


PSOUR_R  = OutputParameter(phys=PHY.SOUR_R, type='ELEM')


PGEOM_R  = OutputParameter(phys=PHY.GEOM_R, type='ELEM')


TOU_INI_ELEM = Option(
    para_in=(
    ),
    para_out=(
        SP.PCAFI_R,
           PERREUR,
        SP.PFORC_R,
           PGEOM_R,
           PNBSP_I,
        SP.PNEU1_R,
           PPRES_R,
           PSOUR_R,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),)),
      CondCalcul('+', ((AT.PHENO,'AC'),)),
      CondCalcul('+', ((AT.PHENO,'PR'),)),
      CondCalcul('+', ((AT.LXFEM,'OUI'),)),
    ),
)
