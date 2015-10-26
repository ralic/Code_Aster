
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


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))



#------------------------------------------------------------
class PR_G_HEXA20(Element):
    """Please document this element"""
    meshType = MT.HEXA20
    elrefe =(
            ElrefeLoc(MT.H20, gauss = ('NOEU=NOEU','RIGI=FPG27',),),
        )
    calculs = (

        OP.GRAD_NEUT_R(te=24,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PNEUTER, LC.N1NEUT_R),
                     ),
            para_out=((OP.GRAD_NEUT_R.PGNEUTR, LC.E3NEUT_R), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )


#------------------------------------------------------------
class PR_G_HEXA8(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA8
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU','RIGI=FPG8',),),
        )


#------------------------------------------------------------
class PR_G_PENTA15(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA15
    elrefe =(
            ElrefeLoc(MT.P15, gauss = ('NOEU=NOEU','RIGI=FPG21',),),
        )


#------------------------------------------------------------
class PR_G_PENTA6(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA6
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU','RIGI=FPG6',),),
        )


#------------------------------------------------------------
class PR_G_TETRA10(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA10
    elrefe =(
            ElrefeLoc(MT.T10, gauss = ('NOEU=NOEU','RIGI=FPG5',),),
        )


#------------------------------------------------------------
class PR_G_TETRA4(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU','RIGI=FPG1',),),
        )


#------------------------------------------------------------
class PR_G_PYRAM13(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.PYRAM13
    elrefe =(
            ElrefeLoc(MT.P13, gauss = ('NOEU=NOEU','RIGI=FPG27',),),
        )


#------------------------------------------------------------
class PR_G_PYRAM5(PR_G_HEXA20):
    """Please document this element"""
    meshType = MT.PYRAM5
    elrefe =(
            ElrefeLoc(MT.PY5, gauss = ('NOEU=NOEU','RIGI=FPG5',),),
        )
