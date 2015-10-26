
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
    components=('C','ALPHA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]','NVI_C','NVI_T',
          'NVI_H','NVI_M',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('PRE[2]',)),
    ('EN2',()),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


E1NEUTK  = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
    components=('Z1',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM','FHYDR[2]','FTHERM',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('M11','FH11X','FH11Y','M12','FH12X',
          'FH12Y','M21','FH21X','FH21Y','M22',
          'FH22X','FH22Y',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('M11','FH11X','FH11Y','M12','FH12X',
          'FH12Y','M21','FH21X','FH21Y','M22',
          'FH22X','FH22Y',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('M11','FH11X','FH11Y','M12','FH12X',
          'FH12Y','M21','FH21X','FH21Y','M22',
          'FH22X','FH22Y',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class HH2_DPQ8D(Element):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=NOEU_S',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
    calculs = (

        OP.ADD_SIGM(te=581,
            para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
                     ),
            para_out=((SP.PEPCON3, ECONTPG), ),
        ),

        OP.COOR_ELGA(te=479,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.FORC_NODA(te=600,
            para_in=((OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=600,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.RAPH_MECA(te=600,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=600,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PREFCO, EREFCO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.RIGI_MECA_TANG(te=600,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.SIEF_ELNO(te=600,
            para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
                     ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.VAEX_ELGA(te=549,
            para_in=((OP.VAEX_ELGA.PCOMPOR, CCOMPOR), (SP.PNOVARI, E1NEUTK),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((SP.PVARIGS, LC.E1GNEUT), ),
        ),

        OP.VAEX_ELNO(te=549,
            para_in=((OP.VAEX_ELNO.PCOMPOR, CCOMPOR), (SP.PNOVARI, E1NEUTK),
                     (OP.VAEX_ELNO.PVARINR, LC.ZVARINO), ),
            para_out=((SP.PVARINS, LC.E1NNEUT), ),
        ),

        OP.VARI_ELNO(te=600,
            para_in=((OP.VARI_ELNO.PCOMPOR, CCOMPOR), (SP.PVARIGR, ZVARIPG),
                     ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class HH2_DPTR6D(HH2_DPQ8D):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=NOEU_S',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class HH2_DPQ8S(HH2_DPQ8D):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG4NOS','MASS=FPG4','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4NOS','MASS=FPG4','NOEU_S=NOEU_S',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class HH2_DPTR6S(HH2_DPQ8D):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3NOS','MASS=FPG3','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3NOS','MASS=FPG3','NOEU_S=NOEU_S',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
