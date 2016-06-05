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

NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))

MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=DDL_MECA)

#---------------------------------------------------------------------------------------------------
class LACQ4Q4D(Element):
    """
      The main 3D class element for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   :
          PSNO    - Parameter of Smoothed Normal at T0
          PDEPL_P - DEPDEL (DISPL. at T+) - (DISPL. at T-)
          PDEPL_M - DISPL. at T-
          PGEOMER - INITIAL GEOMETRY 
          PCONFR  - FRICTIONAL CONTACT PARAMETERS                   
      Output parameters  :    
          PMMATUR : SYMMETRIC MATRIX (te=567)
          PVECTUR : VECTOR OF CONTACT LOAD (te=568)
      Options            : RIGI_CONT / CHAR_MECA_CONT / EXISTE_DDL
      Geometry           : QUAD44 (QUAD4 with QUAD4)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 3-4             (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43D
          QUAD4 MASTER ELEMENT : 1-2-5-6-7-8     (DX,DY,DZ) 
    """
    meshType = MT.QUAD44
    nodes = (
            SetOfNodes('EN1', (3,4,)),       
            SetOfNodes('EN2', (1,2,5,6,7,8,)),     
        )
    calculs = (
        OP.RIGI_CONT(te=567,
            para_in=((SP.PCONFR, LC.CCONFR),(SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA),(SP.PGEOMER, NGEOMER),
                     (SP.PSNO, NGEOMER), ),
            para_out=( (SP.PMATUUR, MMATUUR),
                     ),        
        ),            
        OP.CHAR_MECA_CONT(te=568,
            para_in=((SP.PCONFR, LC.CCONFR),(SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA),(SP.PGEOMER, NGEOMER),
                     (SP.PSNO, NGEOMER), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),
        OP.EXISTE_DDL(te=99,
            para_out=((OP.EXISTE_DDL.PDEPL_R, DDL_MECA), ),
        ),
    )
#---------------------------------------------------------------------------------------------------
class LACQ4Q4E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QUAD44 (QUAD4 with QUAD4)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 1-2-3-4   (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43DB
          QUAD4 MASTER ELEMENT : 5-6-7-8   (DX,DY,DZ) 
    """
    meshType = MT.QUAD44
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,)), 
        )
#---------------------------------------------------------------------------------------------------
class LACQ4T3D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4TR3 (QUAD4 with TRIA3)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 3-4         (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43D
          TRIA3 MASTER ELEMENT : 1-2-5-6-7   (DX,DY,DZ) 
    """
    meshType = MT.QU4TR3
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,)),
              
        )
#---------------------------------------------------------------------------------------------------
class LACQ4T6D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4TR6 (QUAD4 with TRIA6)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 3-4               (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43D
          TRIA6 MASTER ELEMENT : 1-2-5-6-7-8-9-10  (DX,DY,DZ) 
    """
    meshType = MT.QU4TR6
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,)),    
        )
#---------------------------------------------------------------------------------------------------
class LACQ4Q8D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4QU8 (QUAD4 with QUAD8)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 3-4                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43D
          QUAD8 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12  (DX,DY,DZ) 
    """
    meshType = MT.QU4QU8
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,)),    
        )
#---------------------------------------------------------------------------------------------------
class LACQ4Q9D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4QU9 (QUAD4 with QUAD9)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 3-4                        (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43D
          QUAD9 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12-13  (DX,DY,DZ) 
    """
    meshType = MT.QU4QU9
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,13,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ4T3E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4TR3 (QUAD4 with TRIA3)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 1-2-3-4   (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43DB
          TRIA3 MASTER ELEMENT : 5-6-7     (DX,DY,DZ) 
    """  
    meshType = MT.QU4TR3
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)), 
            SetOfNodes('EN2', (5,6,7,)),      
        )
#---------------------------------------------------------------------------------------------------
class LACQ4T6E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4TR6 (QUAD4 with TRIA6)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 1-2-3-4       (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43DB
          TRIA6 MASTER ELEMENT : 5-6-7-8-9-10  (DX,DY,DZ) 
    """
    meshType = MT.QU4TR6
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ4Q8E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4QU8 (QUAD4 with QUAD8)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 1-2-3-4             (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43DB
          QUAD8 MASTER ELEMENT : 5-6-7-8-9-10-11-12  (DX,DY,DZ) 
    """
    meshType = MT.QU4QU8
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ4Q9E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU4QU9 (QUAD4 with QUAD9)
      Local Numerotation :
          QUAD4 SLAVE  ELEMENT : 1-2-3-4                (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ43DB
          QUAD9 MASTER ELEMENT : 5-6-7-8-9-10-11-12-13  (DX,DY,DZ) 
    """
    meshType = MT.QU4QU9
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,13,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q8D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QUAD88 (QUAD8 with QUAD8)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 3-4   (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83D
          QUAD8 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12-13-14-15-16   (DX,DY,DZ) 
    """
    meshType = MT.QUAD88
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,13,14,15,16,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8T3D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8TR3 (QUAD8 with TRIA3)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 3-4                   (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83D
          TRIA3 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11   (DX,DY,DZ) 
    """
    meshType = MT.QU8TR3
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8T6D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8TR3 (QUAD8 with TRIA6)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 3-4                            (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83D
          TRIA6 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12-13-14   (DX,DY,DZ) 
    """
    meshType = MT.QU8TR6
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,13,14,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q4D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8QU4 (QUAD8 with QUAD4)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 3-4                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83D
          QUAD4 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12  (DX,DY,DZ) 
    """
    meshType = MT.QU8QU4
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q9D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8QU9 (QUAD8 with QUAD9)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 3-4                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83D
          QUAD9 MASTER ELEMENT : 1-2-5-6-7-8-9-10-11-12-13-14-15-16-17  (DX,DY,DZ) 
    """
    meshType = MT.QU8QU9
    nodes = (
            SetOfNodes('EN1', (3,4,)),
            SetOfNodes('EN2', (1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q8E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8QU8 (QUAD8 with QUAD8)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 1-2-3-4                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83DB
          QUAD8 MASTER ELEMENT : 5-6-7-8-9-10-11-12-13-14-15-16  (DX,DY,DZ) 
    """
    meshType = MT.QUAD88
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,13,14,15,16,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8T3E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8TR3 (QUAD8 with TRIA3)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 1-2-3-4         (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83DB
          TRIA3 MASTER ELEMENT : 5-6-7-8-9-10-11 (DX,DY,DZ) 
    """
    meshType = MT.QU8TR3
    nodes = (      
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8T6E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8TR6 (QUAD8 with TRIA6)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 1-2-3-4                  (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83DB
          TRIA6 MASTER ELEMENT : 5-6-7-8-9-10-11-12-13-14 (DX,DY,DZ) 
    """
    meshType = MT.QU8TR6
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,13,14,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q4E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8QU4 (QUAD8 with QUAD4)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 1-2-3-4            (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83DB
          QUAD4 MASTER ELEMENT : 5-6-7-8-9-10-11-12 (DX,DY,DZ) 
    """
    meshType = MT.QU8QU4
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ8Q9E(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU8QU9 (QUAD8 with QUAD9)
      Local Numerotation :
          QUAD8 SLAVE  ELEMENT : 1-2-3-4                           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ83DB
          QUAD9 MASTER ELEMENT : 5-6-7-8-9-10-11-12-13-14-15-16-17 (DX,DY,DZ) 
    """
    meshType = MT.QU8QU9
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,13,14,15,16,17,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT3T3D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TRIA33 (TRIA3 with TRIA3)
      Local Numerotation :
          TRIA3 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT33D
          TRIA3 MASTER ELEMENT : 1-2-4-5-6   (DX,DY,DZ) 
    """
    meshType = MT.TRIA33
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT3T6D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR3TR6 (TRIA3 with TRIA6)
      Local Numerotation :
          TRIA3 SLAVE  ELEMENT : 3                 (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT33D
          TRIA6 MASTER ELEMENT : 1-2-4-5-6-7-8-9   (DX,DY,DZ) 
    """
    meshType = MT.TR3TR6
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT3Q4D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR3QU4 (TRIA3 with QUAD4)
      Local Numerotation :
          TRIA3 SLAVE  ELEMENT : 3             (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT33D
          QUAD4 MASTER ELEMENT : 1-2-4-5-6-7   (DX,DY,DZ) 
    """
    meshType = MT.TR3QU4
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT3Q8D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR3QU8 (TRIA3 with QUAD8)
      Local Numerotation :
          TRIA3 SLAVE  ELEMENT : 3             (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT33D
          QUAD8 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10-11   (DX,DY,DZ) 
    """
    meshType = MT.TR3QU8
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10,11,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT3Q9D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR3QU9 (TRIA3 with QUAD9)
      Local Numerotation :
          TRIA3 SLAVE  ELEMENT : 3             (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT33D
          QUAD9 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10-11-12   (DX,DY,DZ) 
    """
    meshType = MT.TR3QU9
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT6T6D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TRIA66 (TRIA6 with TRIA6)
      Local Numerotation :
          TRIA6 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT63D
          TRIA6 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10-11-12   (DX,DY,DZ) 
    """
    meshType = MT.TRIA66
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT6T3D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR6TR3 (TRIA6 with TRIA3)
      Local Numerotation :
          TRIA6 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT63D
          TRIA3 MASTER ELEMENT : 1-2-4-5-6-7-8-9   (DX,DY,DZ) 
    """
    meshType = MT.TR6TR3
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT6Q4D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR6QU4 (TRIA6 with QUAD4)
      Local Numerotation :
          TRIA6 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT63D
          QUAD4 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10   (DX,DY,DZ) 
    """
    meshType = MT.TR6QU4
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10)),
        )
#---------------------------------------------------------------------------------------------------
class LACT6Q8D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR6QU8 (TRIA6 with QUAD8)
      Local Numerotation :
          TRIA6 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT63D
          QUAD8 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10-11-12-13-14   (DX,DY,DZ) 
    """
    meshType = MT.TR6QU8
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10,11,12,13,14,)),
        )
#---------------------------------------------------------------------------------------------------
class LACT6Q9D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : TR6QU9 (TRIA6 with QUAD9)
      Local Numerotation :
          TRIA6 SLAVE  ELEMENT : 3           (DX,DY,DZ,LAGS_C) - Geom. Slave: LACT63D
          QUAD9 MASTER ELEMENT : 1-2-4-5-6-7-8-9-10-11-12-13-14-15   (DX,DY,DZ) 
    """
    meshType = MT.TR6QU9
    nodes = (
            SetOfNodes('EN1', (3,)),
            SetOfNodes('EN2', (1,2,4,5,6,7,8,9,10,11,12,13,14,15,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ9Q9D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QUAD99 (QUAD9 with QUAD9)
      Local Numerotation :
          QUAD9 SLAVE  ELEMENT : 9                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ93D
          QUAD9 MASTER ELEMENT : 1-2-3-4-5-6-7-8-10-11-12-13-14-15-16-17-18  (DX,DY,DZ) 
    """
    meshType = MT.QUAD99
    nodes = (
            SetOfNodes('EN1', (9,)),
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ9T3D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU9TR3 (QUAD9 with TRIA3)
      Local Numerotation :
          QUAD9 SLAVE  ELEMENT : 9                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ93D
          TRIA3 MASTER ELEMENT : 1-2-3-4-5-6-7-8-10-11-12  (DX,DY,DZ) 
    """
    meshType = MT.QU9TR3
    nodes = (
            SetOfNodes('EN1', (9,)),
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,10,11,12,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ9T6D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU9TR6 (QUAD9 with TRIA6)
      Local Numerotation :
          QUAD9 SLAVE  ELEMENT : 9                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ93D
          TRIA6 MASTER ELEMENT : 1-2-3-4-5-6-7-8-10-11-12-13-14-15  (DX,DY,DZ) 
    """
    meshType = MT.QU9TR6
    nodes = (
            SetOfNodes('EN1', (9,)),
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,10,11,12,13,14,15,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ9Q4D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU9QU4 (QUAD9 with QUAD4)
      Local Numerotation :
          QUAD9 SLAVE  ELEMENT : 9                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ93D
          QUAD4 MASTER ELEMENT : 1-2-3-4-5-6-7-8-10-11-12-13  (DX,DY,DZ) 
    """
    meshType = MT.QU9QU4
    nodes = (
            SetOfNodes('EN1', (9,)),
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,10,11,12,13,)),
        )
#---------------------------------------------------------------------------------------------------
class LACQ9Q8D(LACQ4Q4D):
    """
      Derived from the main 3D class element LACQ4Q4D for DEFI_CONTACT/LAC - Frictionless 
      Input parameters   : same as the parent element               
      Output parameters  : same as the parent element 
      Options            : same as the parent element 
      Geometry           : QU9QU8 (QUAD9 with QUAD8)
      Local Numerotation :
          QUAD9 SLAVE  ELEMENT : 9                     (DX,DY,DZ,LAGS_C) - Geom. Slave: LACQ93D
          QUAD8 MASTER ELEMENT : 1-2-3-4-5-6-7-8-10-11-12-13  (DX,DY,DZ) 
    """
    meshType = MT.QU9QU8
    nodes = (
            SetOfNodes('EN1', (9,)),
            SetOfNodes('EN2', (1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,)),
        )
#---------------------------------------------------------------------------------------------------
