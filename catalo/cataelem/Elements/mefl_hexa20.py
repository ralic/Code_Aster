
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


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('PRES','PHI',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('PRES','PHI',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.COOR_ELGA, te=488,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.FORC_NODA, te=170,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=170,
    para_in=((OP.FULL_MECA.PCOMPOR, CCOMPOR), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PCODRET, LC.ECODRET), (SP.PMATUUR, MMATUUR),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.MASS_INER, te=152,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=171,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.PAS_COURANT, te=405,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PCOURAN, LC.ECOURAN), ),
)

ele.addCalcul(OP.PRME_ELNO, te=420,
    para_in=((SP.PDEPLAC, NDEPLAC), ),
    para_out=((SP.PPRME_R, LC.EPRMENO), ),
)

ele.addCalcul(OP.RAPH_MECA, te=170,
    para_in=((OP.RAPH_MECA.PCOMPOR, CCOMPOR), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PCODRET, LC.ECODRET), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA, te=170,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_HYST, te=170,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMATUUC, MMATUUC), ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=170,
    para_in=((OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
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
MEFL_HEXA20 = Element(modele=abstractElement)
ele = MEFL_HEXA20
ele.meshType = MT.HEXA20
ele.elrefe=(
        ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
    )


#------------------------------------------------------------
MEFL_HEXA27 = Element(modele=abstractElement)
ele = MEFL_HEXA27
ele.meshType = MT.HEXA27
ele.elrefe=(
        ElrefeLoc(MT.H27, gauss = ('RIGI=FPG27','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9',),),
    )


#------------------------------------------------------------
MEFL_HEXA8 = Element(modele=abstractElement)
ele = MEFL_HEXA8
ele.meshType = MT.HEXA8
ele.elrefe=(
        ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
MEFL_PENTA15 = Element(modele=abstractElement)
ele = MEFL_PENTA15
ele.meshType = MT.PENTA15
ele.elrefe=(
        ElrefeLoc(MT.P15, gauss = ('RIGI=FPG21','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
    )


#------------------------------------------------------------
MEFL_PENTA6 = Element(modele=abstractElement)
ele = MEFL_PENTA6
ele.meshType = MT.PENTA6
ele.elrefe=(
        ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3',),),
    )


#------------------------------------------------------------
MEFL_TETRA10 = Element(modele=abstractElement)
ele = MEFL_TETRA10
ele.meshType = MT.TETRA10
ele.elrefe=(
        ElrefeLoc(MT.T10, gauss = ('RIGI=FPG15','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
    )


#------------------------------------------------------------
MEFL_TETRA4 = Element(modele=abstractElement)
ele = MEFL_TETRA4
ele.meshType = MT.TETRA4
ele.elrefe=(
        ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG4','FPG1=FPG1',), mater=('FPG1',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3',),),
    )
