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


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM',))


NVITESR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


CGRAINF  = LocatedComponents(phys=PHY.FLUX_F, type='ELEM',
    components=('FLUX','FLUY',))


CGRAINR  = LocatedComponents(phys=PHY.FLUX_R, type='ELEM',
    components=('FLUX','FLUY',))


EFLUXPG  = LocatedComponents(phys=PHY.FLUX_R, type='ELGA', location='RIGI',
    components=('FLUX','FLUY',))


EFLUXNO  = LocatedComponents(phys=PHY.FLUX_R, type='ELNO',
    components=('FLUX','FLUY',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))




EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y',))


ENGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA','KHI','R',
          'RHO',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


ECASECT  = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[9]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


ESOURCR  = LocatedComponents(phys=PHY.SOUR_R, type='ELGA', location='RIGI',
    components=('SOUR',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=DDL_THER)

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=DDL_THER)

MMATTSR  = ArrayOfComponents(phys=PHY.MTNS_R, locatedComponents=DDL_THER)


#------------------------------------------------------------
class THPLQU4(Element):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
    calculs = (

#       Les elements axisymetriques ne devraient jamais etre utilises pour CARA_XXXX

        OP.CARA_CISA(te=509,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PTEMPE1, DDL_THER),
                     (SP.PTEMPE2, DDL_THER), ),
            para_out=((SP.PCASECT, ECASECT), ),
        ),

        OP.CARA_GAUCHI(te=509,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PTEMPER, DDL_THER),
                     ),
            para_out=((SP.PCASECT, ECASECT), ),
        ),

        OP.CARA_TORSION(te=509,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PTEMPER, DDL_THER),
                     ),
            para_out=((SP.PCASECT, ECASECT), ),
        ),

        OP.CHAR_THER_EVOL(te=78,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_THER_EVOL.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_EVOLNI(te=244,
            para_in=((OP.CHAR_THER_EVOLNI.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_THER_EVOLNI.PHYDRPM, LC.EHYDRNO), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTMPCHF, DDL_THER), (SP.PTMPCHI, DDL_THER),
                     (OP.CHAR_THER_EVOLNI.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTTI, MVECTTR), (SP.PVECTTR, MVECTTR),
                     ),
        ),

        OP.CHAR_THER_GRAI_F(te=219,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PGRAINF, CGRAINF),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_THER_GRAI_F.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_GRAI_R(te=219,
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

        OP.CHAR_THER_SOUR_F(te=80,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURCF, LC.CSOURCF),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_THER_SOUR_F.PVARCPR, LC.ZVARCPG),),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_SOUR_R(te=79,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSOURCR, ESOURCR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TNL(te=505,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PLAGRM, LC.EGNEUT1R),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPEI, DDL_THER),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (SP.PVITESR, NVITESR), ),
            para_out=((SP.PLAGRP, LC.EGNEUT1R), (SP.PRESIDU, MVECTTR),
                     (SP.PVECTTR, MVECTTR), ),
        ),

        OP.COOR_ELGA(te=479,
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

        OP.ETHE_ELEM(te=220,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.ETHE_ELEM.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.ETHE_ELEM.PENERDR, EENERR), ),
        ),

        OP.FLUX_ELGA(te=69,
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

        OP.MASS_THER(te=77,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.MASS_THER.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.MASS_THER.PMATTTR, MMATTTR), ),
        ),

        OP.META_ELNO(te=67,
            para_in=((OP.META_ELNO.PCOMPOR, CCOMPOR), (SP.PFTRC, LC.CFTRC),
                     (SP.PMATERC, LC.CMATERC), (OP.META_ELNO.PPHASIN, LC.EPHASNO_),
                     (SP.PTEMPAR, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPIR, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PPHASNOU, LC.EPHASNO_), ),
        ),

        OP.META_INIT_ELNO(te=320,
            para_in=((OP.META_INIT_ELNO.PCOMPOR, CCOMPOR), (SP.PMATERC, LC.CMATERC),
                     (OP.META_INIT_ELNO.PPHASIN, LC.CPHASIN_), (SP.PTEMPER, DDL_THER),
                     ),
            para_out=((SP.PPHASNOU, LC.EPHASNO_), ),
        ),

        OP.MTAN_RIGI_MASS(te=242,
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
            para_out=((SP.PREPLO1, LC.CGEOM2D), (SP.PREPLO2, LC.CGEOM2D),
                     ),
        ),

        OP.RESI_RIGI_MASS(te=243,
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

        OP.RIGI_THER(te=76,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_THER.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.RIGI_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_CONV_T(te=502,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PNEUK24, LC.CDECENT), (SP.PTEMPEI, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PVITESR, NVITESR),
                     ),
            para_out=((OP.RIGI_THER_CONV_T.PMATTTR, MMATTSR), ),
        ),

        OP.RIGI_THER_TRANS(te=501,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPEI, DDL_THER), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_TRANS.PMATTTR, MMATTTR), ),
        ),

        OP.SOUR_ELGA(te=318,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPER, DDL_THER), (SP.PTEMPSR, CTEMPSR),
                     (OP.SOUR_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.SOUR_ELGA.PSOUR_R, ESOURCR), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PSOUR_R, LC.CSOURCR), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PFLUX_R, EFLUXPG), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSOUR_R, ESOURCR), (SP.PTEMP_R, LC.ETEMPPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PFLUX_R, EFLUXNO), (OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R),
                     (OP.TOU_INI_ELNO.PHYDRPM, LC.EHYDRNO), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PVARI_R, LC.EPHASNO_), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class THPLQU8(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class THPLQU9(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class THPLTR3(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )


#------------------------------------------------------------
class THPLTR6(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class THAXQU4(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXQU8(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXQU9(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXTR3(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXTR6(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXQL4(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXQL9(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CHAR_THER_TNL(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.ETHE_ELEM(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.RIGI_THER_CONV_T(te=-1),

        OP.RIGI_THER_TRANS(te=-1),

        OP.SOUR_ELGA(te=-1),

        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXTL3(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THAXTL6(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','NOEU=NOEU',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )

    calculs = (
        OP.CHAR_THER_TNL(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.ETHE_ELEM(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.RIGI_THER_CONV_T(te=-1),

        OP.RIGI_THER_TRANS(te=-1),

        OP.SOUR_ELGA(te=-1),

        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),

        OP.CHAR_THER_SOURNL(te=-1),

        OP.RESI_THER_SOURNL(te=-1),

        OP.MTAN_THER_SOURNL(te=-1),
    )


#------------------------------------------------------------
class THPLQL4(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THPLQL9(THPLQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU=NOEU',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CHAR_THER_TNL(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.ETHE_ELEM(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.RIGI_THER_CONV_T(te=-1),

        OP.RIGI_THER_TRANS(te=-1),

        OP.SOUR_ELGA(te=-1),

        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),

        OP.CHAR_THER_SOURNL(te=-1),

        OP.RESI_THER_SOURNL(te=-1),

        OP.MTAN_THER_SOURNL(te=-1),
    )


#------------------------------------------------------------
class THPLTL3(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )

    calculs = (
        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),
    )


#------------------------------------------------------------
class THPLTL6(THPLQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1','NOEU=NOEU',), mater=('FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG3','NOEU=NOEU',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )

    calculs = (
        OP.CHAR_THER_TNL(te=-1),

        OP.DURT_ELNO(te=-1),

        OP.ETHE_ELEM(te=-1),

        OP.META_ELNO(te=-1),

        OP.META_INIT_ELNO(te=-1),

        OP.RIGI_THER_CONV_T(te=-1),

        OP.RIGI_THER_TRANS(te=-1),

        OP.SOUR_ELGA(te=-1),

        OP.CARA_CISA(te=-1),

        OP.CARA_GAUCHI(te=-1),

        OP.CARA_TORSION(te=-1),

        OP.CHAR_THER_SOURNL(te=-1),

        OP.RESI_THER_SOURNL(te=-1),

        OP.MTAN_THER_SOURNL(te=-1),
    )
