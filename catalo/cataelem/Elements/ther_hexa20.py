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


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM',))


NVITESR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


CGRAINF  = LocatedComponents(phys=PHY.FLUX_F, type='ELEM',
    components=('FLUX','FLUY','FLUZ',))


CGRAINR  = LocatedComponents(phys=PHY.FLUX_R, type='ELEM',
    components=('FLUX','FLUY','FLUZ',))


EFLUXPG  = LocatedComponents(phys=PHY.FLUX_R, type='ELGA', location='RIGI',
    components=('FLUX','FLUY','FLUZ',))


EFLUXNO  = LocatedComponents(phys=PHY.FLUX_R, type='ELNO',
    components=('FLUX','FLUY','FLUZ',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA','KHI','R',
          'RHO',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


ESOURCR  = LocatedComponents(phys=PHY.SOUR_R, type='ELGA', location='RIGI',
    components=('SOUR',))


NSOURCR  = LocatedComponents(phys=PHY.SOUR_R, type='ELNO',
    components=('SOUR',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))

MMATTSR  = ArrayOfComponents(phys=PHY.MTNS_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
class THER_HEXA20(Element):
    """Please document this element"""
    meshType = MT.HEXA20
    elrefe =(
            ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','FPG1=FPG1','MASS=FPG27',), mater=('FPG1',),),
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9',),),
        )
    calculs = (

        OP.CHAR_THER_EVOL(te=61,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_THER_EVOL.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_EVOLNI(te=281,
            para_in=((OP.CHAR_THER_EVOLNI.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_THER_EVOLNI.PHYDRPM, LC.EHYDRNO), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTMPCHF, DDL_THER), (SP.PTMPCHI, DDL_THER),
                     (OP.CHAR_THER_EVOLNI.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTTI, MVECTTR), (SP.PVECTTR, MVECTTR),
                     ),
        ),

        OP.CHAR_THER_GRAI_F(te=217,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PGRAINF, CGRAINF),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_THER_GRAI_F.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_GRAI_R(te=217,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PGRAINR, CGRAINR),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_THER_GRAI_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_SOURNL(te=354,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURNL, LC.CSOURCF),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_SOUR_F(te=56,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURCF, LC.CSOURCF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_SOUR_R(te=55,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURCR, ESOURCR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TNL(te=525,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PLAGRM, LC.EGNEUT1R),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPEI, DDL_THER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PVITESR, NVITESR), ),
            para_out=((SP.PLAGRP, LC.EGNEUT1R), (SP.PRESIDU, MVECTTR),
                     (SP.PVECTTR, MVECTTR), ),
        ),

        OP.COOR_ELGA(te=488,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DURT_ELNO(te=551,
            para_in=((SP.PMATERC, LC.CMATERC), (OP.DURT_ELNO.PPHASIN, LC.EPHASNO_),
                     ),
            para_out=((SP.PDURT_R, LC.EDURTNO), ),
        ),

        OP.ERTH_ELEM(te=3,
            para_in=((SP.PCHARG, LC.CREFERK), (SP.PFLUX_M, EFLUXNO),
                     (SP.PFLUX_P, EFLUXNO), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PSOURCF, LC.CSOURCF),
                     (SP.PSOURCR, ESOURCR), (SP.PTEMP_M, DDL_THER),
                     (SP.PTEMP_P, DDL_THER), (OP.ERTH_ELEM.PVOISIN, LC.EVOISIN),
                     ),
            para_out=((OP.ERTH_ELEM.PERREUR, LC.EERREURT), ),
        ),

        OP.ERTH_ELNO(te=379,
            para_in=((OP.ERTH_ELNO.PERREUR, LC.EERREURT), ),
            para_out=((SP.PERRENO, LC.EERRENOT), ),
        ),

        OP.ETHE_ELEM(te=66,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.ETHE_ELEM.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.ETHE_ELEM.PENERDR, EENERR), ),
        ),

        OP.FLUX_ELGA(te=62,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.FLUX_ELGA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.FLUX_ELGA.PFLUXPG, EFLUXPG), ),
        ),

        OP.FLUX_ELNO(te=4,
            para_in=((OP.FLUX_ELNO.PFLUXPG, EFLUXPG), ),
            para_out=((SP.PFLUXNO, EFLUXNO), ),
        ),

        OP.INIT_MAIL_VOIS(te=99,
            para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_THER(te=54,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.MASS_THER.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.MASS_THER.PMATTTR, MMATTTR), ),
        ),

        OP.META_ELNO(te=64,
            para_in=((OP.META_ELNO.PCOMPOR, CCOMPOR), (SP.PFTRC, LC.CFTRC),
                     (SP.PMATERC, LC.CMATERC), (OP.META_ELNO.PPHASIN, LC.EPHASNO_),
                     (SP.PTEMPAR, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPIR, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PPHASNOU, LC.EPHASNO_), ),
        ),

        OP.META_INIT_ELNO(te=321,
            para_in=((OP.META_INIT_ELNO.PCOMPOR, CCOMPOR), (SP.PMATERC, LC.CMATERC),
                     (OP.META_INIT_ELNO.PPHASIN, LC.CPHASIN_), (SP.PTEMPER, DDL_THER),
                     ),
            para_out=((SP.PPHASNOU, LC.EPHASNO_), ),
        ),

        OP.MTAN_RIGI_MASS(te=279,
            para_in=((OP.MTAN_RIGI_MASS.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPEI, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTMPCHF, DDL_THER),
                     (SP.PTMPCHI, DDL_THER), (OP.MTAN_RIGI_MASS.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.MTAN_RIGI_MASS.PMATTTR, MMATTTR), ),
        ),

        OP.MTAN_THER_SOURNL(te=354,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURNL, LC.CSOURCF),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.MTAN_THER_SOURNL.PMATTTR, MMATTTR), ),
        ),

        OP.NORME_L2(te=563,
            para_in=((SP.PCALCI, LC.EMNEUT_I), (SP.PCHAMPG, EGNEUT_R),
                     (SP.PCOEFR, EMNEUT_R), (OP.NORME_L2.PCOORPG, EGGEOP_R),
                     ),
            para_out=((SP.PNORME, LC.ENORME), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.REPERE_LOCAL(te=133,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RESI_RIGI_MASS(te=283,
            para_in=((OP.RESI_RIGI_MASS.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.RESI_RIGI_MASS.PHYDRPM, LC.EHYDRNO), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTMPCHF, DDL_THER),
                     (SP.PTMPCHI, DDL_THER), (OP.RESI_RIGI_MASS.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PHYDRPP, LC.EHYDRNO), (SP.PRESIDU, MVECTTR),
                     ),
        ),

        OP.RESI_THER_SOURNL(te=354,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURNL, LC.CSOURCF),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PRESIDU, MVECTTR), ),
        ),

        OP.RIGI_THER(te=51,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_THER.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.RIGI_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_CONV_T(te=522,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PNEUK24, LC.CDECENT), (SP.PTEMPEI, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PVITESR, NVITESR),
                     ),
            para_out=((OP.RIGI_THER_CONV_T.PMATTTR, MMATTSR), ),
        ),

        OP.RIGI_THER_TRANS(te=521,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_TRANS.PMATTTR, MMATTTR), ),
        ),

        OP.SOUR_ELGA(te=319,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (OP.SOUR_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.SOUR_ELGA.PSOUR_R, ESOURCR), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PSOUR_R, LC.CSOURCR), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PFLUX_R, EFLUXPG), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
                     (OP.TOU_INI_ELGA.PNEUT_R, LC.EGNEUT1R), (OP.TOU_INI_ELGA.PSOUR_R, ESOURCR),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PFLUX_R, EFLUXNO), (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER),
                     (OP.TOU_INI_ELNO.PHYDRPM, LC.EHYDRNO), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PSOUR_R, NSOURCR), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class THER_HEXA27(THER_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA27
    elrefe =(
            ElrefeLoc(MT.H27, gauss = ('RIGI=FPG27','FPG1=FPG1','MASS=FPG27',), mater=('FPG1',),),
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9',),),
        )


#------------------------------------------------------------
class THER_HEXA8(THER_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA8
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','FPG1=FPG1','MASS=FPG8','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4',),),
        )


#------------------------------------------------------------
class THER_PENTA15(THER_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA15
    elrefe =(
            ElrefeLoc(MT.P15, gauss = ('RIGI=FPG21','FPG1=FPG1','MASS=FPG21',), mater=('FPG1',),),
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class THER_PENTA6(THER_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA6
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','FPG1=FPG1','MASS=FPG6','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class THER_PYRAM13(THER_HEXA20):
    """Please document this element"""
    meshType = MT.PYRAM13
    elrefe =(
            ElrefeLoc(MT.P13, gauss = ('RIGI=FPG27','FPG1=FPG1','MASS=FPG27',), mater=('FPG1',),),
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6',),),
        )

    calculs = (
        OP.CHAR_THER_GRAI_F(te=-1),

        OP.CHAR_THER_GRAI_R(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.ERTH_ELEM(te=-1),

        OP.ERTH_ELNO(te=-1),
    )


#------------------------------------------------------------
class THER_PYRAM5(THER_HEXA20):
    """Please document this element"""
    meshType = MT.PYRAM5
    elrefe =(
            ElrefeLoc(MT.PY5, gauss = ('RIGI=FPG5','FPG1=FPG1','MASS=FPG5','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3',),),
        )

    calculs = (
        OP.CHAR_THER_GRAI_F(te=-1),

        OP.CHAR_THER_GRAI_R(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.ERTH_ELEM(te=-1),

        OP.ERTH_ELNO(te=-1),
    )


#------------------------------------------------------------
class THER_TETRA10(THER_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA10
    elrefe =(
            ElrefeLoc(MT.T10, gauss = ('RIGI=FPG15','FPG1=FPG1','MASS=FPG15',), mater=('FPG1',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6',),),
        )


#------------------------------------------------------------
class THER_TETRA4(THER_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG4','FPG1=FPG1','MASS=FPG4','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3',),),
        )


#------------------------------------------------------------
class THER_HEXA8_D(THER_HEXA20):
    """Please document this element"""
    meshType = MT.HEXA8
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','FPG1=FPG1','MASS=FPG8','NOEU=NOEU_S',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4',),),
        )


#------------------------------------------------------------
class THER_PENTA6_D(THER_HEXA20):
    """Please document this element"""
    meshType = MT.PENTA6
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','FPG1=FPG1','MASS=FPG6','NOEU=NOEU_S',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3','NOEU=NOEU',),),
        )


#------------------------------------------------------------
class THER_TETRA4_D(THER_HEXA20):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG4','FPG1=FPG1','MASS=FPG4','NOEU=NOEU_S',), mater=('FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3',),),
        )
