
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
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


EINTENO  = LocatedComponents(phys=PHY.INTE_R, type='ELNO',
    components=('INTX_R','INTY_R','INTX_I','INTY_I',))


DDL_ACOU = LocatedComponents(phys=PHY.PRES_C, type='ELNO',
    components=('PRES',))


MMATTTC  = ArrayOfComponents(phys=PHY.MPRE_C, locatedComponents=(DDL_ACOU,DDL_ACOU))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.COOR_ELGA, te=479,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.INTE_ELNO, te=175,
    para_in=((SP.PFREQR, LC.CFREQR), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PPRESSC, DDL_ACOU),
             ),
    para_out=((SP.PINTER, EINTENO), ),
)

ele.addCalcul(OP.MASS_ACOU, te=177,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMATTTC, MMATTTC), ),
)

ele.addCalcul(OP.PRAC_ELNO, te=189,
    para_in=((SP.PPRESSC, DDL_ACOU), ),
    para_out=((SP.PPRAC_R, LC.EPRACNO), ),
)

ele.addCalcul(OP.RIGI_ACOU, te=176,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PMATTTC, MMATTTC), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)

ele.addCalcul(OP.VERI_JACOBIEN, te=328,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PCODRET, LC.ECODRET), ),
)


#------------------------------------------------------------
ACPLQU4 = Element(modele=abstractElement)
ele = ACPLQU4
ele.meshType = MT.QUAD4
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )


#------------------------------------------------------------
ACPLQU8 = Element(modele=abstractElement)
ele = ACPLQU8
ele.meshType = MT.QUAD8
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','NOEU=NOEU','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
ACPLQU9 = Element(modele=abstractElement)
ele = ACPLQU9
ele.meshType = MT.QUAD9
ele.elrefe=(
        ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','NOEU=NOEU','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
ACPLTR3 = Element(modele=abstractElement)
ele = ACPLTR3
ele.meshType = MT.TRIA3
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG1','NOEU=NOEU','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )


#------------------------------------------------------------
ACPLTR6 = Element(modele=abstractElement)
ele = ACPLTR6
ele.meshType = MT.TRIA6
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','NOEU=NOEU','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )
