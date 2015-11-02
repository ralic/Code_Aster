
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


CCACOQU  = LocatedComponents(phys=PHY.CACOQU, type='ELEM',
    components=('EP','ALPHA','BETA',))


CCOEFHF  = LocatedComponents(phys=PHY.COEH_F, type='ELEM',
    components=('H_INF','H_SUP',))


CCOEFHR  = LocatedComponents(phys=PHY.COEH_R, type='ELEM',
    components=('H_INF','H_SUP',))


CFLUXNF  = LocatedComponents(phys=PHY.FLUN_F, type='ELEM',
    components=('FLUN_INF','FLUN_SUP',))


CFLUXNR  = LocatedComponents(phys=PHY.FLUN_R, type='ELEM',
    components=('FLUN_INF','FLUN_SUP',))


EFLUXPG  = LocatedComponents(phys=PHY.FLUX_R, type='ELGA', location='RIGI',
    components=('FLUX','FLUY','FLUZ','FLUX_INF','FLUY_INF',
          'FLUZ_INF','FLUX_SUP','FLUY_SUP','FLUZ_SUP',))


EFLUXNO  = LocatedComponents(phys=PHY.FLUX_R, type='ELNO',
    components=('FLUX','FLUY','FLUZ','FLUX_INF','FLUY_INF',
          'FLUZ_INF','FLUX_SUP','FLUY_SUP','FLUZ_SUP',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


ENGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


ENBSP_I  = LocatedComponents(phys=PHY.NBSP_I, type='ELEM',
    components=('COQ_NCOU',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


CT_EXTF  = LocatedComponents(phys=PHY.TEMP_F, type='ELEM',
    components=('TEMP','TEMP_INF','TEMP_SUP',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP_MIL','TEMP_INF','TEMP_SUP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_THER_EVOL, te=110,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCOEFHF, CCOEFHF),
             (SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
             (SP.PTEMPSR, CTEMPSR), (OP.CHAR_THER_EVOL.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_FLUN_F, te=105,
    para_in=((SP.PFLUXNF, CFLUXNF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_FLUN_R, te=106,
    para_in=((SP.PFLUXNR, CFLUXNR), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_TEXT_F, te=107,
    para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             (SP.PT_EXTF, CT_EXTF), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.CHAR_THER_TEXT_R, te=108,
    para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             (SP.PT_EXTR, LC.CT_EXTR), ),
    para_out=((SP.PVECTTR, MVECTTR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=479,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.FLUX_ELGA, te=109,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.FLUX_ELGA.PNBSP_I, ENBSP_I),
             (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
             (OP.FLUX_ELGA.PVARCPR, LC.ZVARCPG), ),
    para_out=((OP.FLUX_ELGA.PFLUXPG, EFLUXPG), ),
)

ele.addCalcul(OP.FLUX_ELNO, te=4,
    para_in=((OP.FLUX_ELNO.PFLUXPG, EFLUXPG), ),
    para_out=((SP.PFLUXNO, EFLUXNO), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.MASS_THER, te=102,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
             (OP.MASS_THER.PVARCPR, LC.ZVARCPG), ),
    para_out=((OP.MASS_THER.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.RIGI_THER, te=101,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
             (OP.RIGI_THER.PVARCPR, LC.ZVARCPG), ),
    para_out=((OP.RIGI_THER.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.RIGI_THER_COEH_F, te=103,
    para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.RIGI_THER_COEH_F.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.RIGI_THER_COEH_R, te=104,
    para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((OP.RIGI_THER_COEH_R.PMATTTR, MMATTTR), ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PNBSP_I, ENBSP_I), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
             (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
             ),
)


#------------------------------------------------------------
THCOQU4 = Element(modele=abstractElement)
ele = THCOQU4
ele.meshType = MT.QUAD4
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )


#------------------------------------------------------------
THCOQU8 = Element(modele=abstractElement)
ele = THCOQU8
ele.meshType = MT.QUAD8
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )


#------------------------------------------------------------
THCOQU9 = Element(modele=abstractElement)
ele = THCOQU9
ele.meshType = MT.QUAD9
ele.elrefe=(
        ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )


#------------------------------------------------------------
THCOTR3 = Element(modele=abstractElement)
ele = THCOTR3
ele.meshType = MT.TRIA3
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG1','MASS=FPG3','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )


#------------------------------------------------------------
THCOTR6 = Element(modele=abstractElement)
ele = THCOTR6
ele.meshType = MT.TRIA6
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )


#------------------------------------------------------------
THCOTR7 = Element(modele=abstractElement)
ele = THCOTR7
ele.meshType = MT.TRIA7
ele.elrefe=(
        ElrefeLoc(MT.TR7, gauss = ('RIGI=FPG6','MASS=FPG6','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('FPG1',),),
    )
