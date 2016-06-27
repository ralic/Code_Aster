# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: ayaovi-dzifa.kudawoo at edf.fr

from cataelem.Tools.base_objects import LocatedComponents, ArrayOfComponents, SetOfNodes, ElrefeLoc
from cataelem.Tools.base_objects import Calcul, Element
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP


# ELEMENTARY TREATMENT OF 3D FRICTIONLESS ELEMENT WITH DEFI_CONTACT OPERATOR
# MORTAR LAC METHOD

#----------------
# Modes locaux :
#----------------

DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ','LAGS_C',)),
    ('EN2',('DX','DY','DZ',)),))
    
ECNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM', 
    components=('X[6]',))

##------------------------------------------------------------
class LACT33D(Element):
    """
      THE LACT33D CLASS ELEMENT : 
      DEFI_CONTACT / CONTINUE / MORTAR_LAC
          Slave frictionless Contact Element in 3D : elementary treatments
      Local Numerotation :
          
      Input parameters :
          
      Output parameters :          
    """
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN2', (1,2)),
            SetOfNodes('EN1', (3,)),
        )
    calculs = (

        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),    
        OP.CONT_ELEM(te=99,
            para_out=((OP.CONT_ELEM.CT_ELEM, ECNEUT_R), ),   
        ),

    )

#------------------------------------------------------------
class LACT63D(LACT33D):
    """
      THE LACT63D CLASS ELEMENT : 
      DEFI_CONTACT / CONTINUE / MORTAR_LAC
          Slave frictionless Contact Element in 3D : elementary treatments
      Local Numerotation :
          
      Input parameters :
          
      Output parameters :          
    """
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (1,2,4,5,6)),
            SetOfNodes('EN1', (3,)),
        )
    calculs = (

        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),

    )

##------------------------------------------------------------
class LACQ93D(LACT33D):
    """
      THE LACQ93D CLASS ELEMENT : 
      DEFI_CONTACT / CONTINUE / MORTAR_LAC
          Slave frictionless Contact Element in 3D : elementary treatments
      Local Numerotation :
          
      Input parameters :
          
      Output parameters :          
    """
    meshType = MT.QUAD9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8)),
            SetOfNodes('EN1', (9,)),
        )
    calculs = (

        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),

    )



#------------------------------------------------------------
class LACQ83D(LACT33D):
    """
      THE LACQ93D CLASS ELEMENT : 
      DEFI_CONTACT / CONTINUE / MORTAR_LAC
          Slave frictionless Contact Element in 3D : elementary treatments
      Local Numerotation :
          
      Input parameters :
          
      Output parameters :          
    """
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (1,2,5,6,7,8)),
            SetOfNodes('EN1', (3,4,)),
        )
    calculs = (

        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),

    )


#------------------------------------------------------------
class LACQ43D(LACT33D):
    """
      THE LACQ93D CLASS ELEMENT : 
      DEFI_CONTACT / CONTINUE / MORTAR_LAC
          Slave frictionless Contact Element in 3D : elementary treatments
      Local Numerotation :
          
      Input parameters :
          
      Output parameters :          
    """
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,)),
        )
    calculs = (

        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),

    )
