
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


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))



#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.GRAD_NEUT_R, te=24,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PNEUTER, LC.N1NEUT_R),
             ),
    para_out=((OP.GRAD_NEUT_R.PGNEUTR, LC.E3NEUT_R), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)


#------------------------------------------------------------
PR_G_HEXA20 = Element(modele=abstractElement)
ele = PR_G_HEXA20
ele.meshType = MT.HEXA20
ele.elrefe=(
        ElrefeLoc(MT.H20, gauss = ('NOEU=NOEU','RIGI=FPG27',),),
    )


#------------------------------------------------------------
PR_G_HEXA8 = Element(modele=abstractElement)
ele = PR_G_HEXA8
ele.meshType = MT.HEXA8
ele.elrefe=(
        ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU','RIGI=FPG8',),),
    )


#------------------------------------------------------------
PR_G_PENTA15 = Element(modele=abstractElement)
ele = PR_G_PENTA15
ele.meshType = MT.PENTA15
ele.elrefe=(
        ElrefeLoc(MT.P15, gauss = ('NOEU=NOEU','RIGI=FPG21',),),
    )


#------------------------------------------------------------
PR_G_PENTA6 = Element(modele=abstractElement)
ele = PR_G_PENTA6
ele.meshType = MT.PENTA6
ele.elrefe=(
        ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU','RIGI=FPG6',),),
    )


#------------------------------------------------------------
PR_G_TETRA10 = Element(modele=abstractElement)
ele = PR_G_TETRA10
ele.meshType = MT.TETRA10
ele.elrefe=(
        ElrefeLoc(MT.T10, gauss = ('NOEU=NOEU','RIGI=FPG5',),),
    )


#------------------------------------------------------------
PR_G_TETRA4 = Element(modele=abstractElement)
ele = PR_G_TETRA4
ele.meshType = MT.TETRA4
ele.elrefe=(
        ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU','RIGI=FPG1',),),
    )


#------------------------------------------------------------
PR_G_PYRAM13 = Element(modele=abstractElement)
ele = PR_G_PYRAM13
ele.meshType = MT.PYRAM13
ele.elrefe=(
        ElrefeLoc(MT.P13, gauss = ('NOEU=NOEU','RIGI=FPG27',),),
    )


#------------------------------------------------------------
PR_G_PYRAM5 = Element(modele=abstractElement)
ele = PR_G_PYRAM5
ele.meshType = MT.PYRAM5
ele.elrefe=(
        ElrefeLoc(MT.PY5, gauss = ('NOEU=NOEU','RIGI=FPG5',),),
    )
