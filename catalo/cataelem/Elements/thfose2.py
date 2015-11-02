
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


CCOEFHF  = LocatedComponents(phys=PHY.COEH_F, type='ELEM',
    components=('H',))


CCOEFHR  = LocatedComponents(phys=PHY.COEH_R, type='ELEM',
    components=('H',))


CFLUXNF  = LocatedComponents(phys=PHY.FLUN_F, type='ELEM',
    components=('FLUN',))


CFLUXNR  = LocatedComponents(phys=PHY.FLUN_R, type='ELEM',
    components=('FLUN',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


CT_EXTF  = LocatedComponents(phys=PHY.TEMP_F, type='ELEM',
    components=('TEMP',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_THER_FLUN_F, te=272,
    para_in=((SP.PFLUXNF, CFLUXNF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_FLUN_R, te=271,
    para_in=((SP.PFLUXNR, CFLUXNR), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_TEXT_F, te=270,
    para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             (SP.PT_EXTF, CT_EXTF), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_TEXT_R, te=269,
    para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             (SP.PT_EXTR, LC.ET_EXTR), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=478,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.RIGI_THER_COEH_F, te=268,
    para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.RIGI_THER_COEH_F.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.RIGI_THER_COEH_R, te=267,
    para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.RIGI_THER_COEH_R.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)


#------------------------------------------------------------
THFOSE2 = Element(modele=abstractElement)
ele = THFOSE2
ele.meshType = MT.SEG2
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
THFOSE3 = Element(modele=abstractElement)
ele = THFOSE3
ele.meshType = MT.SEG3
ele.elrefe=(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )
