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


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


EINTENO  = LocatedComponents(phys=PHY.INTE_R, type='ELNO',
    components=('INTX_R','INTY_R','INTZ_R','INTX_I','INTY_I',
          'INTZ_I',))


DDL_ACOU = LocatedComponents(phys=PHY.PRES_C, type='ELNO',
    components=('PRES',))


MMATTTC  = ArrayOfComponents(phys=PHY.MPRE_C, locatedComponents=DDL_ACOU)


#------------------------------------------------------------
class ACOU_HEXA20(Element):
    """Please document this element"""
    meshType = MT.HEXA20
    elrefe =(
            ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','MASS=FPG27','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU=NOEU',),),
        )
    calculs = (

        OP.COOR_ELGA(te=488,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.INTE_ELNO(te=187,
            para_in=((SP.PFREQR, LC.CFREQR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PPRESSC, DDL_ACOU),
                     ),
            para_out=((SP.PINTER, EINTENO), ),
        ),

        OP.MASS_ACOU(te=181,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATTTC, MMATTTC), ),
        ),

        OP.PRAC_ELNO(te=189,
            para_in=((SP.PPRESSC, DDL_ACOU), ),
            para_out=((SP.PPRAC_R, LC.EPRACNO), ),
        ),

        OP.RIGI_ACOU(te=180,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PMATTTC, MMATTTC), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
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
class ACOU_HEXA27(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA27
    elrefe =(
            ElrefeLoc(MT.H27, gauss = ('RIGI=FPG27','MASS=FPG27','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ACOU_HEXA8(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA8
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','MASS=FPG8','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ACOU_PENTA15(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA15
    elrefe =(
            ElrefeLoc(MT.P15, gauss = ('RIGI=FPG21','MASS=FPG21','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ACOU_PENTA6(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA6
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','MASS=FPG6','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ACOU_TETRA10(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA10
    elrefe =(
            ElrefeLoc(MT.T10, gauss = ('RIGI=FPG15','MASS=FPG15','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ACOU_TETRA4(ACOU_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG4','MASS=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3','NOEU=NOEU',),),
        )
