

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




PCONTMR  = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_ELGA!NM1T',
comment="""  PCONTMR : CONTRAINTES INSTANT PRECEDENT """)


PCONTPR  = InputParameter(phys=PHY.SIEF_R, container='RESU!SIEF_ELGA!N',
comment="""  PCONTPR : CONTRAINTES INSTANT ACTUEL """)


PENERDM  = InputParameter(phys=PHY.ENER_R, container='RESU!ETOT_ELEM!NM1T',
comment="""  PENERDM : ENERGIE TOTALE PAR ELEMENT INSTANT PRECEDENT """)


PENERDR  = OutputParameter(phys=PHY.ENER_R, type='ELEM',
comment="""  PENERDR : ENERGIE TOTALE PAR ELEMENT """)


ETOT_ELEM = Option(
    para_in=(
           PCONTMR,
           PCONTPR,
        SP.PDEPLM,
        SP.PDEPLR,
           PENERDM,
        SP.PGEOMER,
    ),
    para_out=(
           PENERDR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
    ),
    comment="""  ETOT_ELEM : ENERGIE TOTALE PAR ELEMENT """,
)
