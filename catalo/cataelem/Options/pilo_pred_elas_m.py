# coding=utf-8
# person_in_charge: kyrylo.kazymyrenko at edf.fr


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




PLST     = InputParameter(phys=PHY.NEUT_R)


PLSN     = InputParameter(phys=PHY.NEUT_R)


PPINTER  = InputParameter(phys=PHY.N816_R)


PAINTER  = InputParameter(phys=PHY.N1360R)


PCFACE   = InputParameter(phys=PHY.N720_I)


PLONGCO  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PBASECO  = InputParameter(phys=PHY.N2448R)


PCOPILO  = OutputParameter(phys=PHY.PILO_R, type='ELNO')


PILO_PRED_ELAS_M = Option(
    para_in=(
           PAINTER,
           PBASECO,
        SP.PCDTAU,
           PCFACE,
        SP.PCOHES,
        SP.PDDEPLR,
        SP.PDEPL0R,
        SP.PDEPL1R,
        SP.PDEPLMR,
        SP.PDONCO,
        SP.PGEOMER,
        SP.PINDCOI,
           PLONGCO,
           PLSN,
           PLST,
        SP.PMATERC,
           PPINTER,
    ),
    para_out=(
           PCOPILO,
    ),
    condition=(
#     Cette option est une variante de PILO_PRED_ELAS dediee a certains elements XFEM
#     Cette option ne concerne que les elements 'XHC3':
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.XFEM,'XHC3'),)),
    ),
)
