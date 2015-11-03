# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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


CCOEFHF  = LocatedComponents(phys=PHY.COEH_F, type='ELEM',
    components=('H',))


CCOEFHR  = LocatedComponents(phys=PHY.COEH_R, type='ELEM',
    components=('H',))


NACCELR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


CFLUXNF  = LocatedComponents(phys=PHY.FLUN_F, type='ELEM',
    components=('FLUN',))


CFLUXNR  = LocatedComponents(phys=PHY.FLUN_R, type='ELEM',
    components=('FLUN',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA','KHI','R',
          'RHO',))


CT_EXTF  = LocatedComponents(phys=PHY.TEMP_F, type='ELEM',
    components=('TEMP',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
class THAXSL2(Element):
    """Please document this element"""
    meshType = MT.SEG2
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG4','FPG1=FPG1',), mater=('FPG1',),),
        )
    calculs = (

        OP.CHAR_THER_ACCE_R(te=315,
            para_in=((SP.PACCELR, NACCELR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_ACCE_X(te=315,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_ACCE_Y(te=315,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_FLUNL(te=274,
            para_in=((SP.PFLUXNL, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_FLUN_F(te=75,
            para_in=((SP.PFLUXNF, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_FLUN_R(te=74,
            para_in=((SP.PFLUXNR, CFLUXNR), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_FLUTNL(te=506,
            para_in=((SP.PFLUXNL, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.CHAR_THER_RAYO_F(te=73,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONF, LC.CRAYONF),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_RAYO_R(te=72,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONR, LC.CRAYONR),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TEXT_F(te=73,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PT_EXTF, CT_EXTF), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TEXT_R(te=72,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PT_EXTR, LC.ET_EXTR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.FLUX_FLUI_X(te=316,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.FLUX_FLUI_X.PMATTTR, MMATTTR), ),
        ),

        OP.FLUX_FLUI_Y(te=317,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.FLUX_FLUI_Y.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_COEF_F(te=250,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.MTAN_THER_COEF_F.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_COEF_R(te=249,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.MTAN_THER_COEF_R.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_FLUXNL(te=251,
            para_in=((SP.PFLUXNL, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.MTAN_THER_FLUXNL.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_RAYO_F(te=250,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONF, LC.CRAYONF),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.MTAN_THER_RAYO_F.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_RAYO_R(te=249,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONR, LC.CRAYONR),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.MTAN_THER_RAYO_R.PMATTTR, MMATTTR), ),
        ),

        OP.RESI_THER_COEF_F(te=137,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_COEF_R(te=136,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_COEH_F(te=307,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_COEH_R(te=305,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_FLUXNL(te=138,
            para_in=((SP.PFLUXNL, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_RAYO_F(te=137,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONF, LC.CRAYONF),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RESI_THER_RAYO_R(te=136,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PRAYONR, LC.CRAYONR),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RIGI_THER_COEH_F(te=71,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COEH_F.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_COEH_R(te=70,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COEH_R.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_COET_F(te=507,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COET_F.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_COET_R(te=503,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COET_R.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_FLUTNL(te=504,
            para_in=((SP.PFLUXNL, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.RIGI_THER_FLUTNL.PMATTTR, MMATTTR), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )


#------------------------------------------------------------
class THAXSL3(THAXSL2):
    """Please document this element"""
    meshType = MT.SEG3
    elrefe =(
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','FPG1=FPG1',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CHAR_THER_ACCE_R(te=-1),

        OP.CHAR_THER_ACCE_X(te=-1),

        OP.CHAR_THER_ACCE_Y(te=-1),

        OP.CHAR_THER_FLUTNL(te=-1),

        OP.FLUX_FLUI_X(te=-1),

        OP.FLUX_FLUI_Y(te=-1),

        OP.RESI_THER_COEH_F(te=-1),

        OP.RESI_THER_COEH_R(te=-1),

        OP.RIGI_THER_FLUTNL(te=-1),
    )
