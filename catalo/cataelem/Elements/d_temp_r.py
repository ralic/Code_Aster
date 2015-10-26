

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

from cataelem.Tools.base_objects import LocatedComponents, ArrayOfComponents, SetOfNodes, ElrefeLoc
from cataelem.Tools.base_objects import Calcul, Element
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP
import cataelem.Commons.attributes as AT


MGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO', diff=True,
components=(
    ('EN1',()),
    ('EN2',('X','Y','Z',)),))


for cmp in ('TEMP','TEMP_INF','TEMP_MIL','TEMP_SUP','E1','H1',) :


      #----------------
      # Modes locaux :
      #----------------

      DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO', diff=True,
      components=(
          ('EN1',('LAGR',)),
          ('EN2',(cmp,)),))

      MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

      MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#     Attention : il faut nommer explicitement TOUS les modes locaux crees dans la boucle
#     ------------------------------------------------------------------------------------
      DDL_THER.setName('DDL_THER')
      MVECTTR.setName('MVECTTR')
      MMATTTR.setName('MMATTTR')

      name = ('D_TEMP_R_' + cmp)[:16]

      class TempClass(Element):
          """Please document this element"""
          _name = name

          meshType = MT.SEG3
          nodes = (
                  SetOfNodes('EN1', (2,3,)),
                  SetOfNodes('EN2', (1,)),
              )
          attrs = ((AT.CL_DUAL,'OUI'),)


          calculs = (
              OP.THER_BTLA_R(te=2,
                  para_in=((SP.PDDLMUR, LC.MDDLMUR), (OP.THER_BTLA_R.PLAGRAR, DDL_THER),
                           ),
                  para_out=((SP.PVECTTR, MVECTTR), ),
              ),

              OP.THER_BU_R(te=2,
                  para_in=((SP.PALPHAR, LC.MALPHAR), (OP.THER_BU_R.PDDLIMR, DDL_THER),
                           (SP.PDDLMUR, LC.MDDLMUR), ),
                  para_out=((SP.PVECTTR, MVECTTR), ),
              ),

              OP.THER_DDLI_F(te=2,
                  para_in=((SP.PDDLIMF, LC.MDDLIMF), (SP.PGEOMER, MGEOMER),
                           (SP.PTEMPSR, LC.MTEMPSR), ),
                  para_out=((SP.PVECTTR, MVECTTR), ),
              ),

              OP.THER_DDLI_R(te=2,
                  para_in=((OP.THER_DDLI_R.PDDLIMR, LC.MDDLIMR), ),
                  para_out=((SP.PVECTTR, MVECTTR), ),
              ),

              OP.THER_DDLM_R(te=2,
                  para_in=((SP.PDDLMUR, LC.MDDLMUR), ),
                  para_out=((OP.THER_DDLM_R.PMATTTR, MMATTTR), ),
              ),
          )

      exec name + " = TempClass"
      del TempClass
