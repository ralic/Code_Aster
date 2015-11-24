# coding=utf-8
# person_in_charge: josselin.delmas at edf.fr


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




PCONTPR  = InputParameter(phys=PHY.SIEF_R, container='RESU!SIGM_ELGA!N',
comment=""" PCONTPR : CONTRAINTES A L INSTANT + """)


PCOMPOR  = InputParameter(phys=PHY.COMPOR, container='RESU!COMPORTEMENT!N',
comment=""" PCOMPOR : COMPORTEMENT """)


PVARIPR  = InputParameter(phys=PHY.VARI_R, container='RESU!VARI_ELGA!N',
comment=""" PVARIPR : VARIABLES INTERNES A L INSTANT + """)


INDL_ELGA = Option(
    para_in=(
           PCOMPOR,
           PCONTPR,
        SP.PMATERC,
           PVARIPR,
    ),
    para_out=(
        SP.PINDLOC,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.DIM_COOR_MODELI,'2'),(AT.BORD,'0'),)),
    ),
    comment="""  INDL_ELGA :
           INDICATEUR DE LOCALISATION AUX POINTS DE GAUSS
           PRODUIT UN CHAMP AUX POINTS DE GAUSS  """,
)
