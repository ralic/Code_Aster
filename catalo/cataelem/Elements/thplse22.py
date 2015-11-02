
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


NDEPLAR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_THER_PARO_F, te=210,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPF, LC.CHECHPF),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_PARO_R, te=209,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPR, LC.EHECHPR),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.MTAN_THER_PARO_F, te=387,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPF, LC.CHECHPF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.MTAN_THER_PARO_F.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.MTAN_THER_PARO_R, te=386,
    para_in=((SP.PDEPLAR, NDEPLAR), (SP.PGEOMER, NGEOMER),
             (SP.PHECHPR, LC.EHECHPR), (SP.PTEMPER, DDL_THER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.MTAN_THER_PARO_R.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.RESI_THER_PARO_F, te=277,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPF, LC.CHECHPF),
             (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PRESIDU, MVECTTR), ),
)

ele.addCalcul(OP.RESI_THER_PARO_R, te=275,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPR, LC.EHECHPR),
             (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PRESIDU, MVECTTR), ),
)

ele.addCalcul(OP.RIGI_THER_PARO_F, te=212,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPF, LC.CHECHPF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.RIGI_THER_PARO_F.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.RIGI_THER_PARO_R, te=211,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PHECHPR, LC.EHECHPR),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((OP.RIGI_THER_PARO_R.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)


#------------------------------------------------------------
THPLSE22 = Element(modele=abstractElement)
ele = THPLSE22
ele.meshType = MT.SEG22
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )


#------------------------------------------------------------
THPLSE33 = Element(modele=abstractElement)
ele = THPLSE33
ele.meshType = MT.SEG33
ele.elrefe=(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
THAXSE22 = Element(modele=abstractElement)
ele = THAXSE22
ele.meshType = MT.SEG22
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )


#------------------------------------------------------------
THAXSE33 = Element(modele=abstractElement)
ele = THAXSE33
ele.meshType = MT.SEG33
ele.elrefe=(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )
