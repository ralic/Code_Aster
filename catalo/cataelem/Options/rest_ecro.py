
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




PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment=""" PCOMPOR: COMPORTEMENT  """)


PVARIMR  = InputParameter(phys=PHY.VARI_R,
comment=""" PVARIMR: VARIABLES INTERNES AVANT MODIFICATION """)


PVARCPR  = InputParameter(phys=PHY.VARI_R,
comment=""" PVARCPR: VARIABLES DE COMMANDES POUR T+ """)


PVARIPR  = OutputParameter(phys=PHY.VARI_R, type='ELGA',
comment=""" PVARIPR: VARIABLES INTERNES APRES MODIFICATION """)


REST_ECRO = Option(
    para_in=(
        SP.PCARCRI,
           PCOMPOR,
        SP.PMATERC,
        SP.PTEMPSR,
        SP.PVARCMR,
           PVARCPR,
           PVARIMR,
    ),
    para_out=(
           PVARIPR,
    ),
    condition=(
      CondCalcul('+', (('PHENO','ME'),('BORD','0'),)),
    ),
    comment="""  REST_ECRO :
           RESTAURATION D'ECROUISSAGE - MODIFICATION VARIABLES INTERNES """,
)
