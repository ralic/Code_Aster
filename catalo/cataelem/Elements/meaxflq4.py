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


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('PRES','PHI',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('PRES','PHI',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=NDEPLAC)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MEAXFLQ4(Element):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','FPG1=FPG1',), mater=('FPG1',),),
        )
    calculs = (

        OP.COOR_ELGA(te=479,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.MASS_INER(te=157,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=254,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.PRME_ELNO(te=420,
            para_in=((SP.PDEPLAC, NDEPLAC), ),
            para_out=((SP.PPRME_R, LC.EPRMENO), ),
        ),

        OP.RIGI_MECA(te=253,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=253,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM2D), ),
        ),


        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class MEAXFLQ8(MEAXFLQ4):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','FPG1=FPG1',), mater=('FPG1',),),
        )


#------------------------------------------------------------
class MEAXFLQ9(MEAXFLQ4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','FPG1=FPG1',), mater=('FPG1',),),
        )


#------------------------------------------------------------
class MEAXFLT3(MEAXFLQ4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','FPG1=FPG1',), mater=('FPG1',),),
        )


#------------------------------------------------------------
class MEAXFLT6(MEAXFLQ4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','FPG1=FPG1',), mater=('FPG1',),),
        )
