
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


ENEU1_R  = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X1',))



#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((SP.PNEU1_R, ENEU1_R), ),
)


#------------------------------------------------------------
PR_HEXA27 = Element(modele=abstractElement)
ele = PR_HEXA27
ele.meshType = MT.HEXA27
ele.elrefe=(
        ElrefeLoc(MT.H27,),
    )


#------------------------------------------------------------
PR_HEXA20 = Element(modele=abstractElement)
ele = PR_HEXA20
ele.meshType = MT.HEXA20
ele.elrefe=(
        ElrefeLoc(MT.H20,),
    )


#------------------------------------------------------------
PR_HEXA8 = Element(modele=abstractElement)
ele = PR_HEXA8
ele.meshType = MT.HEXA8
ele.elrefe=(
        ElrefeLoc(MT.HE8,),
    )


#------------------------------------------------------------
PR_PENTA18 = Element(modele=abstractElement)
ele = PR_PENTA18
ele.meshType = MT.PENTA18
ele.elrefe=(
        ElrefeLoc(MT.P18,),
    )


#------------------------------------------------------------
PR_PENTA15 = Element(modele=abstractElement)
ele = PR_PENTA15
ele.meshType = MT.PENTA15
ele.elrefe=(
        ElrefeLoc(MT.P15,),
    )


#------------------------------------------------------------
PR_PENTA6 = Element(modele=abstractElement)
ele = PR_PENTA6
ele.meshType = MT.PENTA6
ele.elrefe=(
        ElrefeLoc(MT.PE6,),
    )


#------------------------------------------------------------
PR_TETRA10 = Element(modele=abstractElement)
ele = PR_TETRA10
ele.meshType = MT.TETRA10
ele.elrefe=(
        ElrefeLoc(MT.T10,),
    )


#------------------------------------------------------------
PR_TETRA4 = Element(modele=abstractElement)
ele = PR_TETRA4
ele.meshType = MT.TETRA4
ele.elrefe=(
        ElrefeLoc(MT.TE4,),
    )


#------------------------------------------------------------
PR_PYRAM13 = Element(modele=abstractElement)
ele = PR_PYRAM13
ele.meshType = MT.PYRAM13
ele.elrefe=(
        ElrefeLoc(MT.P13,),
    )


#------------------------------------------------------------
PR_PYRAM5 = Element(modele=abstractElement)
ele = PR_PYRAM5
ele.meshType = MT.PYRAM5
ele.elrefe=(
        ElrefeLoc(MT.PY5,),
    )


#------------------------------------------------------------
PR_QUAD9 = Element(modele=abstractElement)
ele = PR_QUAD9
ele.meshType = MT.QUAD9
ele.elrefe=(
        ElrefeLoc(MT.QU9,),
    )


#------------------------------------------------------------
PR_QUAD8 = Element(modele=abstractElement)
ele = PR_QUAD8
ele.meshType = MT.QUAD8
ele.elrefe=(
        ElrefeLoc(MT.QU8,),
    )


#------------------------------------------------------------
PR_QUAD4 = Element(modele=abstractElement)
ele = PR_QUAD4
ele.meshType = MT.QUAD4
ele.elrefe=(
        ElrefeLoc(MT.QU4,),
    )


#------------------------------------------------------------
PR_TRIA7 = Element(modele=abstractElement)
ele = PR_TRIA7
ele.meshType = MT.TRIA7
ele.elrefe=(
        ElrefeLoc(MT.TR7,),
    )


#------------------------------------------------------------
PR_TRIA6 = Element(modele=abstractElement)
ele = PR_TRIA6
ele.meshType = MT.TRIA6
ele.elrefe=(
        ElrefeLoc(MT.TR6,),
    )


#------------------------------------------------------------
PR_TRIA3 = Element(modele=abstractElement)
ele = PR_TRIA3
ele.meshType = MT.TRIA3
ele.elrefe=(
        ElrefeLoc(MT.TR3,),
    )


#------------------------------------------------------------
PR_SEG4 = Element(modele=abstractElement)
ele = PR_SEG4
ele.meshType = MT.SEG4
ele.elrefe=(
        ElrefeLoc(MT.SE4,),
    )


#------------------------------------------------------------
PR_SEG3 = Element(modele=abstractElement)
ele = PR_SEG3
ele.meshType = MT.SEG3
ele.elrefe=(
        ElrefeLoc(MT.SE3,),
    )


#------------------------------------------------------------
PR_SEG2 = Element(modele=abstractElement)
ele = PR_SEG2
ele.meshType = MT.SEG2
ele.elrefe=(
        ElrefeLoc(MT.SE2,),
    )


#------------------------------------------------------------
PR_POI1 = Element(modele=abstractElement)
ele = PR_POI1
ele.meshType = MT.POI1
ele.elrefe=(
        ElrefeLoc(MT.PO1,),
    )
