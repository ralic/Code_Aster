
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




VERI_JACOBIEN = Option(
    para_in=(
        SP.PGEOMER,
    ),
    para_out=(
        SP.PCODRET,
    ),
    condition=(
      CondCalcul('+', (('DIM_COOR_MODELI','3'),('DIM_TOPO_MODELI','3'),('BORD','0'),)),
      CondCalcul('+', (('DIM_COOR_MODELI','2'),('DIM_TOPO_MODELI','2'),('BORD','0'),)),
      CondCalcul('-', (('INTERFACE','OUI'),)),
      CondCalcul('-', (('MODTHM','SUSHI'),)),
      CondCalcul('-', (('MODELI','2FP'),)),
    ),
    comment=""" verification que les jacobiens des differents points de Gauss
   ont tous le meme signe """,
)
