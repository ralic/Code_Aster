
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
from cataelem.Tools.base_objects import Calcul, Element, AbstractElement
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
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_MECA_CONT, te=365,
    para_in=((SP.PACCE_M, DDL_MECA), (SP.PCONFR, LC.CCONFR),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PVITE_M, DDL_MECA),
             (SP.PVITE_P, DDL_MECA), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.RIGI_CONT, te=364,
    para_in=((SP.PACCE_M, DDL_MECA), (SP.PCONFR, LC.CCONFR),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PVITE_M, DDL_MECA),
             (SP.PVITE_P, DDL_MECA), ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)


#------------------------------------------------------------
COP2P2 = Element(modele=abstractElement)
ele = COP2P2
ele.meshType = MT.SEG22
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,)),
    )


#------------------------------------------------------------
COQ4Q4 = Element(modele=abstractElement)
ele = COQ4Q4
ele.meshType = MT.QUAD44
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN1', (5,6,7,8,)),
    )


#------------------------------------------------------------
COT3T3 = Element(modele=abstractElement)
ele = COT3T3
ele.meshType = MT.TRIA33
ele.nodes = (
        SetOfNodes('EN1', (4,5,6,)),
        SetOfNodes('EN2', (1,2,3,)),
    )


#------------------------------------------------------------
COQ4T3 = Element(modele=abstractElement)
ele = COQ4T3
ele.meshType = MT.QU4TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN1', (5,6,7,)),
    )


#------------------------------------------------------------
COT3Q4 = Element(modele=abstractElement)
ele = COT3Q4
ele.meshType = MT.TR3QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (4,5,6,7,)),
    )


#------------------------------------------------------------
COT6T3 = Element(modele=abstractElement)
ele = COT6T3
ele.meshType = MT.TR6TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,)),
        SetOfNodes('EN1', (7,8,9,)),
    )


#------------------------------------------------------------
COT3T6 = Element(modele=abstractElement)
ele = COT3T6
ele.meshType = MT.TR3TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (4,5,6,7,8,9,)),
    )


#------------------------------------------------------------
COT6Q4 = Element(modele=abstractElement)
ele = COT6Q4
ele.meshType = MT.TR6QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,)),
        SetOfNodes('EN1', (7,8,9,10,)),
    )


#------------------------------------------------------------
COQ4T6 = Element(modele=abstractElement)
ele = COQ4T6
ele.meshType = MT.QU4TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN1', (5,6,7,8,9,10,)),
    )


#------------------------------------------------------------
COT6Q8 = Element(modele=abstractElement)
ele = COT6Q8
ele.meshType = MT.TR6QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,)),
        SetOfNodes('EN1', (7,8,9,10,11,12,13,14,)),
    )


#------------------------------------------------------------
COQ8T6 = Element(modele=abstractElement)
ele = COQ8T6
ele.meshType = MT.QU8TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN1', (9,10,11,12,13,14,)),
    )


#------------------------------------------------------------
COT6Q9 = Element(modele=abstractElement)
ele = COT6Q9
ele.meshType = MT.TR6QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,)),
        SetOfNodes('EN1', (7,8,9,10,11,12,13,14,15,)),
    )


#------------------------------------------------------------
COQ9T6 = Element(modele=abstractElement)
ele = COQ9T6
ele.meshType = MT.QU9TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
        SetOfNodes('EN1', (10,11,12,13,14,15,)),
    )


#------------------------------------------------------------
COQ8T3 = Element(modele=abstractElement)
ele = COQ8T3
ele.meshType = MT.QU8TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN1', (9,10,11,)),
    )


#------------------------------------------------------------
COT3Q8 = Element(modele=abstractElement)
ele = COT3Q8
ele.meshType = MT.TR3QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (4,5,6,7,8,9,10,11,)),
    )


#------------------------------------------------------------
COQ8Q4 = Element(modele=abstractElement)
ele = COQ8Q4
ele.meshType = MT.QU8QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN1', (9,10,11,12,)),
    )


#------------------------------------------------------------
COQ4Q8 = Element(modele=abstractElement)
ele = COQ4Q8
ele.meshType = MT.QU4QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN1', (5,6,7,8,9,10,11,12,)),
    )


#------------------------------------------------------------
COQ8Q9 = Element(modele=abstractElement)
ele = COQ8Q9
ele.meshType = MT.QU8QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN1', (9,10,11,12,13,14,15,16,17,)),
    )


#------------------------------------------------------------
COQ9Q8 = Element(modele=abstractElement)
ele = COQ9Q8
ele.meshType = MT.QU9QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
        SetOfNodes('EN1', (10,11,12,13,14,15,16,17,)),
    )


#------------------------------------------------------------
COQ9Q4 = Element(modele=abstractElement)
ele = COQ9Q4
ele.meshType = MT.QU9QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
        SetOfNodes('EN1', (10,11,12,13,)),
    )


#------------------------------------------------------------
COQ4Q9 = Element(modele=abstractElement)
ele = COQ4Q9
ele.meshType = MT.QU4QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN1', (5,6,7,8,9,10,11,12,13,)),
    )


#------------------------------------------------------------
COQ9T3 = Element(modele=abstractElement)
ele = COQ9T3
ele.meshType = MT.QU9TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
        SetOfNodes('EN1', (10,11,12,)),
    )


#------------------------------------------------------------
COT3Q9 = Element(modele=abstractElement)
ele = COT3Q9
ele.meshType = MT.TR3QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (4,5,6,7,8,9,10,11,12,)),
    )


#------------------------------------------------------------
COQ8Q8 = Element(modele=abstractElement)
ele = COQ8Q8
ele.meshType = MT.QUAD88
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN1', (9,10,11,12,13,14,15,16,)),
    )


#------------------------------------------------------------
COQ9Q9 = Element(modele=abstractElement)
ele = COQ9Q9
ele.meshType = MT.QUAD99
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,7,8,9,)),
        SetOfNodes('EN1', (10,11,12,13,14,15,16,17,18,)),
    )


#------------------------------------------------------------
COT6T6 = Element(modele=abstractElement)
ele = COT6T6
ele.meshType = MT.TRIA66
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,5,6,)),
        SetOfNodes('EN1', (7,8,9,10,11,12,)),
    )


#------------------------------------------------------------
COS2T3 = Element(modele=abstractElement)
ele = COS2T3
ele.meshType = MT.SE2TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,5,)),
    )


#------------------------------------------------------------
COS2T6 = Element(modele=abstractElement)
ele = COS2T6
ele.meshType = MT.SE2TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,)),
    )


#------------------------------------------------------------
COS2Q4 = Element(modele=abstractElement)
ele = COS2Q4
ele.meshType = MT.SE2QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,5,6,)),
    )


#------------------------------------------------------------
COS2Q8 = Element(modele=abstractElement)
ele = COS2Q8
ele.meshType = MT.SE2QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
    )


#------------------------------------------------------------
COS2Q9 = Element(modele=abstractElement)
ele = COS2Q9
ele.meshType = MT.SE2QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
    )


#------------------------------------------------------------
COS3T3 = Element(modele=abstractElement)
ele = COS3T3
ele.meshType = MT.SE3TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (3,4,5,)),
    )


#------------------------------------------------------------
COS3T6 = Element(modele=abstractElement)
ele = COS3T6
ele.meshType = MT.SE3TR6
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,)),
    )


#------------------------------------------------------------
COS3Q4 = Element(modele=abstractElement)
ele = COS3Q4
ele.meshType = MT.SE3QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (3,4,5,6,)),
    )


#------------------------------------------------------------
COS3Q8 = Element(modele=abstractElement)
ele = COS3Q8
ele.meshType = MT.SE3QU8
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,9,10,)),
    )


#------------------------------------------------------------
COS3Q9 = Element(modele=abstractElement)
ele = COS3Q9
ele.meshType = MT.SE3QU9
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN1', (3,4,5,6,7,8,9,10,11,)),
    )
