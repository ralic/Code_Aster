
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

#----------------
# Modes locaux :
#----------------


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ',)),
    ('EN2',('DX','DY','DZ','LAGS_C','LAGS_F[2]',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class CFP2P2(Element):
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

        OP.CHAR_MECA_FROT(te=365,
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

        OP.RIGI_FROT(te=364,
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
class CFQ4Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.QUAD44
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,)),
        )


#------------------------------------------------------------
class CFT3T3(CFP2P2):
    """Please document this element"""
    meshType = MT.TRIA33
    nodes = (
            SetOfNodes('EN1', (4,5,6,)),
            SetOfNodes('EN2', (1,2,3,)),
        )


#------------------------------------------------------------
class CFQ4T3(CFP2P2):
    """Please document this element"""
    meshType = MT.QU4TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,)),
        )


#------------------------------------------------------------
class CFT3Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.TR3QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,)),
        )


#------------------------------------------------------------
class CFT6T3(CFP2P2):
    """Please document this element"""
    meshType = MT.TR6TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,)),
        )


#------------------------------------------------------------
class CFT3T6(CFP2P2):
    """Please document this element"""
    meshType = MT.TR3TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,)),
        )


#------------------------------------------------------------
class CFT6Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.TR6QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,)),
        )


#------------------------------------------------------------
class CFQ4T6(CFP2P2):
    """Please document this element"""
    meshType = MT.QU4TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class CFT6Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.TR6QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,13,14,)),
        )


#------------------------------------------------------------
class CFQ8T6(CFP2P2):
    """Please document this element"""
    meshType = MT.QU8TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,)),
        )


#------------------------------------------------------------
class CFT6Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.TR6QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,13,14,15,)),
        )


#------------------------------------------------------------
class CFQ9T6(CFP2P2):
    """Please document this element"""
    meshType = MT.QU9TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,)),
        )


#------------------------------------------------------------
class CFQ8T3(CFP2P2):
    """Please document this element"""
    meshType = MT.QU8TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,)),
        )


#------------------------------------------------------------
class CFT3Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.TR3QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,10,11,)),
        )


#------------------------------------------------------------
class CFQ8Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.QU8QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,)),
        )


#------------------------------------------------------------
class CFQ4Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.QU4QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class CFQ8Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.QU8QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,15,16,17,)),
        )


#------------------------------------------------------------
class CFQ9Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.QU9QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,16,17,)),
        )


#------------------------------------------------------------
class CFQ9Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.QU9QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,)),
        )


#------------------------------------------------------------
class CFQ4Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.QU4QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
            SetOfNodes('EN1', (5,6,7,8,9,10,11,12,13,)),
        )


#------------------------------------------------------------
class CFQ9T3(CFP2P2):
    """Please document this element"""
    meshType = MT.QU9TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,)),
        )


#------------------------------------------------------------
class CFT3Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.TR3QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (4,5,6,7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class CFQ8Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.QUAD88
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
            SetOfNodes('EN1', (9,10,11,12,13,14,15,16,)),
        )


#------------------------------------------------------------
class CFQ9Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.QUAD99
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
            SetOfNodes('EN1', (10,11,12,13,14,15,16,17,18,)),
        )


#------------------------------------------------------------
class CFT6T6(CFP2P2):
    """Please document this element"""
    meshType = MT.TRIA66
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,5,6,)),
            SetOfNodes('EN1', (7,8,9,10,11,12,)),
        )


#------------------------------------------------------------
class CFS2T3(CFP2P2):
    """Please document this element"""
    meshType = MT.SE2TR3
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,)),
        )


#------------------------------------------------------------
class CFS2T6(CFP2P2):
    """Please document this element"""
    meshType = MT.SE2TR6
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,)),
        )


#------------------------------------------------------------
class CFS2Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.SE2QU4
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,)),
        )


#------------------------------------------------------------
class CFS2Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.SE2QU8
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class CFS2Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.SE2QU9
    nodes = (
            SetOfNodes('EN2', (1,2,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
        )


#------------------------------------------------------------
class CFS3T3(CFP2P2):
    """Please document this element"""
    meshType = MT.SE3TR3
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,)),
        )


#------------------------------------------------------------
class CFS3T6(CFP2P2):
    """Please document this element"""
    meshType = MT.SE3TR6
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,)),
        )


#------------------------------------------------------------
class CFS3Q4(CFP2P2):
    """Please document this element"""
    meshType = MT.SE3QU4
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,)),
        )


#------------------------------------------------------------
class CFS3Q8(CFP2P2):
    """Please document this element"""
    meshType = MT.SE3QU8
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
        )


#------------------------------------------------------------
class CFS3Q9(CFP2P2):
    """Please document this element"""
    meshType = MT.SE3QU9
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
            SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
        )
