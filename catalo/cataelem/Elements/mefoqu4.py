
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
    components=('C','ALPHA',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EENERNO  = LocatedComponents(phys=PHY.ENER_R, type='ELNO',
    components=('TOTALE',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


CEPSINF  = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


EFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELGA', location='RIGI',
    components=('FX','FY','FZ',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


ENGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MEFOQU4(Element):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
    calculs = (

        OP.CHAR_MECA_EPSI_F(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINF, CEPSINF),
                     (SP.PGEOMER, NGEOMER), (SP.PHARMON, LC.CHARMON),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_EPSI_R(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINR, CEPSINR),
                     (SP.PGEOMER, NGEOMER), (SP.PHARMON, LC.CHARMON),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FF2D2D(te=200,
            para_in=((SP.PFF2D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D2D(te=199,
            para_in=((SP.PFR2D2D, EFORCER), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=196,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=197,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PROTATR, LC.CROTATR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=198,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PHARMON, LC.CHARMON), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=479,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPOT_ELEM(te=286,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PHARMON, LC.CHARMON),
                     (SP.PMATERC, LC.CMATERC), (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.EPSI_ELGA(te=114,
            para_in=((SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PHARMON, LC.CHARMON), (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.FORC_NODA(te=117,
            para_in=((OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     (SP.PHARMON, LC.CHARMON), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.REPERE_LOCAL(te=133,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     ),
        ),

        OP.RIGI_MECA(te=190,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PHARMON, LC.CHARMON), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_GE(te=112,
            para_in=((OP.RIGI_MECA_GE.PCONTRR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     (SP.PHARMON, LC.CHARMON), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=115,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PHARMON, LC.CHARMON),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRC, ECONTPC), (OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
        ),

        OP.SIEQ_ELGA(te=335,
            para_in=((OP.SIEQ_ELGA.PCONTRR, ECONTPG), ),
            para_out=((OP.SIEQ_ELGA.PCONTEQ, ECOEQPG), ),
        ),

        OP.SIEQ_ELNO(te=335,
            para_in=((OP.SIEQ_ELNO.PCONTRR, ECONTNO), ),
            para_out=((OP.SIEQ_ELNO.PCONTEQ, LC.ECOEQNO), ),
        ),

        OP.SIGM_ELGA(te=546,
            para_in=((SP.PSIEFR, ECONTPG), ),
            para_out=((SP.PSIGMC, ECONTPC), (SP.PSIGMR, ECONTPG),
                     ),
        ),

        OP.SIGM_ELNO(te=4,
            para_in=((OP.SIGM_ELNO.PCONTRR, ECONTPG), ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIGM_ELNO.PSIEFNOR, ECONTNO),
                     ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class MEFOQU8(MEFOQU4):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MEFOQU9(MEFOQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MEFOTR3(MEFOQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG1','MASS=FPG3','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )


#------------------------------------------------------------
class MEFOTR6(MEFOQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
