
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
    ('EN1',('DX','DY','LAGS_C',)),
    ('EN2',('DX','DY',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class COS2S2(Element):
    """Please document this element"""
    meshType = MT.SEG22
    nodes = (
            SetOfNodes('EN1', (1,2,)),
            SetOfNodes('EN2', (3,4,)),
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
class COS3S3(COS2S2):
    """Please document this element"""
    meshType = MT.SEG33
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )


#------------------------------------------------------------
class COS2S3(COS2S2):
    """Please document this element"""
    meshType = MT.SEG23
    nodes = (
            SetOfNodes('EN1', (1,2,)),
            SetOfNodes('EN2', (3,4,5,)),
        )


#------------------------------------------------------------
class COS3S2(COS2S2):
    """Please document this element"""
    meshType = MT.SEG32
    nodes = (
            SetOfNodes('EN1', (1,2,3,)),
            SetOfNodes('EN2', (4,5,)),
        )


#------------------------------------------------------------
class COS2S2A(COS2S2):
    """Please document this element"""
    meshType = MT.SEG22
    nodes = (
            SetOfNodes('EN1', (1,2,)),
            SetOfNodes('EN2', (3,4,)),
        )


#------------------------------------------------------------
class COS3S3A(COS2S2):
    """Please document this element"""
    meshType = MT.SEG33
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )


#------------------------------------------------------------
class COS2S3A(COS2S2):
    """Please document this element"""
    meshType = MT.SEG23
    nodes = (
            SetOfNodes('EN1', (1,2,)),
            SetOfNodes('EN2', (3,4,5,)),
        )


#------------------------------------------------------------
class COS3S2A(COS2S2):
    """Please document this element"""
    meshType = MT.SEG32
    nodes = (
            SetOfNodes('EN1', (1,2,3,)),
            SetOfNodes('EN2', (4,5,)),
        )
