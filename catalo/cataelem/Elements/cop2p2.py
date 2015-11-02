
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
from cataelem.Tools.base_objects import Calcul, NewElement
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP

#----------------
# Modes locaux :
#----------------


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ',)),
    ('EN2',('DX','DY','DZ','LAGS_C',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class COP2P2(NewElement):
    """Please document this element"""
    meshType = MT.SEG22
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,)),
        )
    calculs = (

        OP.CHAR_MECA_CONT(te=365,
            para_in=((SP.PACCE_M, DDL_MECA), (SP.PCONFR, LC.CCONFR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PVITE_M, DDL_MECA),
                     (SP.PVITE_P, DDL_MECA), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.RIGI_CONT(te=364,
            para_in=((SP.PACCE_M, DDL_MECA), (SP.PCONFR, LC.CCONFR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PVITE_M, DDL_MECA),
                     (SP.PVITE_P, DDL_MECA), ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )


#------------------------------------------------------------
class COQ4Q4(COP2P2):
    """Please document this element"""
    meshType = MT.QUAD44
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,)),
        )


#------------------------------------------------------------
class COT3T3(COP2P2):
    """Please document this element"""
    meshType = MT.TRIA33
    nodes = (
            SetOfNodes('EN1', (4,5,6,)),
            SetOfNodes('EN2', (1,2,3,)),
        )


#------------------------------------------------------------
class COQ4T3(COP2P2):
    """Please document this element"""
    meshType = MT.QU4TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,)),
        )


#------------------------------------------------------------
class COT3Q4(COP2P2):
    """Please document this element"""
    meshType = MT.TR3QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,)),
        )


#------------------------------------------------------------
class COT6T3(COP2P2):
    """Please document this element"""
    meshType = MT.TR6TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,)),
        )


#------------------------------------------------------------
class COT3T6(COP2P2):
    """Please document this element"""
    meshType = MT.TR3TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,)),
        )


#------------------------------------------------------------
class COT6Q4(COP2P2):
    """Please document this element"""
    meshType = MT.TR6QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,)),
        )


#------------------------------------------------------------
class COQ4T6(COP2P2):
    """Please document this element"""
    meshType = MT.QU4TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class COT6Q8(COP2P2):
    """Please document this element"""
    meshType = MT.TR6QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,13,14,)),
        )


#------------------------------------------------------------
class COQ8T6(COP2P2):
    """Please document this element"""
    meshType = MT.QU8TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,)),
        )


#------------------------------------------------------------
class COT6Q9(COP2P2):
    """Please document this element"""
    meshType = MT.TR6QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,13,14,15,)),
        )


#------------------------------------------------------------
class COQ9T6(COP2P2):
    """Please document this element"""
    meshType = MT.QU9TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,)),
        )


#------------------------------------------------------------
class COQ8T3(COP2P2):
    """Please document this element"""
    meshType = MT.QU8TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,)),
        )


#------------------------------------------------------------
class COT3Q8(COP2P2):
    """Please document this element"""
    meshType = MT.TR3QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,10,11,)),
        )


#------------------------------------------------------------
class COQ8Q4(COP2P2):
    """Please document this element"""
    meshType = MT.QU8QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,)),
        )


#------------------------------------------------------------
class COQ4Q8(COP2P2):
    """Please document this element"""
    meshType = MT.QU4QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class COQ8Q9(COP2P2):
    """Please document this element"""
    meshType = MT.QU8QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,15,16,17,)),
        )


#------------------------------------------------------------
class COQ9Q8(COP2P2):
    """Please document this element"""
    meshType = MT.QU9QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,16,17,)),
        )


#------------------------------------------------------------
class COQ9Q4(COP2P2):
    """Please document this element"""
    meshType = MT.QU9QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,)),
        )


#------------------------------------------------------------
class COQ4Q9(COP2P2):
    """Please document this element"""
    meshType = MT.QU4QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,11,12,13,)),
        )


#------------------------------------------------------------
class COQ9T3(COP2P2):
    """Please document this element"""
    meshType = MT.QU9TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,)),
        )


#------------------------------------------------------------
class COT3Q9(COP2P2):
    """Please document this element"""
    meshType = MT.TR3QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class COQ8Q8(COP2P2):
    """Please document this element"""
    meshType = MT.QUAD88
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,15,16,)),
        )


#------------------------------------------------------------
class COQ9Q9(COP2P2):
    """Please document this element"""
    meshType = MT.QUAD99
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,16,17,18,)),
        )


#------------------------------------------------------------
class COT6T6(COP2P2):
    """Please document this element"""
    meshType = MT.TRIA66
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class COS2T3(COP2P2):
    """Please document this element"""
    meshType = MT.SE2TR3
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,)),
        )


#------------------------------------------------------------
class COS2T6(COP2P2):
    """Please document this element"""
    meshType = MT.SE2TR6
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,)),
        )


#------------------------------------------------------------
class COS2Q4(COP2P2):
    """Please document this element"""
    meshType = MT.SE2QU4
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,)),
        )


#------------------------------------------------------------
class COS2Q8(COP2P2):
    """Please document this element"""
    meshType = MT.SE2QU8
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class COS2Q9(COP2P2):
    """Please document this element"""
    meshType = MT.SE2QU9
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
        )


#------------------------------------------------------------
class COS3T3(COP2P2):
    """Please document this element"""
    meshType = MT.SE3TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,)),
        )


#------------------------------------------------------------
class COS3T6(COP2P2):
    """Please document this element"""
    meshType = MT.SE3TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,)),
        )


#------------------------------------------------------------
class COS3Q4(COP2P2):
    """Please document this element"""
    meshType = MT.SE3QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,)),
        )


#------------------------------------------------------------
class COS3Q8(COP2P2):
    """Please document this element"""
    meshType = MT.SE3QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class COS3Q9(COP2P2):
    """Please document this element"""
    meshType = MT.SE3QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
        )
