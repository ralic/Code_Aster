# coding=utf-8

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

from cataelem.Tools.base_objects import LocatedComponents, ArrayOfComponents, SetOfNodes, ElrefeLoc
from cataelem.Tools.base_objects import Calcul, NewElement
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP


#-------------------------------------
# Modes locaux communs aux 3 elements:
#--------------------------------------


CCAGEPO  = LocatedComponents(phys=PHY.CAGEPO, type='ELEM',
    components=('R1','EP1',))


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))

EDFVCPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPTHER_L',))


EDFVCNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPTHER_L',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


EDEFGNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','GAXY','GAXZ','GAT','KY',
          'KZ',))


EDEFGPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','GAXY','GAXZ','GAT','KY',
          'KZ',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


CFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


ENBSP_I  = LocatedComponents(phys=PHY.NBSP_I, type='ELEM',
    components=('TUY_NCOU','TUY_NSEC',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


EEFGENOC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('N','VY','VZ','MT','MFY',
          'MFZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


EEFGENOR = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('N','VY','VZ','MT','MFY',
          'MFZ',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


EGAMIMA  = LocatedComponents(phys=PHY.SPMX_R, type='ELGA', location='RIGI',
    components=('VAL','NUCOU','NUSECT','NUFIBR','POSIC',
          'POSIS',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))



#------------------------------------------------------------
# Ce qui distingue les 3 elements de tuyau :
#------------------------------------------------------------

CABSCUR = LocatedComponents(phys=PHY.ABSC_R, type='ELEM', components=('ABSC[3]',))

CCAORIE = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
components=('ALPHA','BETA','GAMMA','ALPHA2','BETA2',
      'GAMMA2','ALPHA3','BETA3','GAMMA3','ICOUDE',
      'DN1N2','RCOURB','ANGCOU','ANGZZK',))

NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
components=('DX','DY','DZ','DRX','DRY',
      'DRZ','UI2','VI2','WI2','UI3',
      'VI3','WI3','UO2','VO2','WO2',
      'UO3','VO3','WO3','WO','WI1',
      'WO1',))

DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
components=('DX','DY','DZ','DRX','DRY',
      'DRZ','UI2','VI2','WI2','UI3',
      'VI3','WI3','UO2','VO2','WO2',
      'UO3','VO3','WO3','WO','WI1',
      'WO1',))

MVECTUC  = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=(NDEPLAC,))
MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))
MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))
MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

CABSCUR.setName('CABSCUR')
CCAORIE.setName('CCAORIE')
NDEPLAC.setName('NDEPLAC')
DDL_MECA.setName('DDL_MECA')
MVECTUC.setName('MVECTUC')
MVECTUR.setName('MVECTUR')
MMATUUR.setName('MMATUUR')
MMATUUC.setName('MMATUUC')


class MET3SEG3(NewElement):
    """Please document this element"""
    meshType = MT.SEG3
    elrefe =(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1',), mater=('RIGI','FPG1',),),
    )

    calculs = (
        OP.AMOR_MECA(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                     (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
                     (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CHAR_MECA_FC1D1D(te=583,
            para_in=((OP.CHAR_MECA_FC1D1D.PCAORIE, CCAORIE), (SP.PFC1D1D, LC.CFORCEC),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PVECTUC, MVECTUC), ),
        ),

        OP.CHAR_MECA_FF1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE),
                     (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FF1D1D.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE),
                     (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FR1D1D.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_PESA_R.PNBSP_I, ENBSP_I), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=583,
            para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                     (OP.CHAR_MECA_PRES_F.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PNBSP_I, ENBSP_I), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PRES_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_R.PNBSP_I, ENBSP_I),
                     (SP.PPRESSR, EPRESNO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=589,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_TEMP_R.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.COOR_ELGA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.COOR_ELGA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEGE_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELGA.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.DEGE_ELGA.PDEFOPG, EDEFGPG), ),
        ),

        OP.DEGE_ELNO(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELNO.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELNO.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOGR, EDEFGNO), ),
        ),

        OP.EFGE_ELGA(te=587,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELGA.PCAORIE, CCAORIE),
                     (OP.EFGE_ELGA.PNBSP_I, ENBSP_I), (SP.PSIEFR, ECONTPG),
                     ),
            para_out=((SP.PEFGEC, LC.EEFGEGAC), (SP.PEFGER, LC.EEFGEGAR),
                     ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
                     (OP.EFGE_ELNO.PCOMPOR, CCOMPOR), (OP.EFGE_ELNO.PCONTRR, ECONTPG),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EFGE_ELNO.PNBSP_I, ENBSP_I),
                     (SP.PNONLIN, LC.ENONLIN), (SP.PTEMPSR, CTEMPSR),
                     (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PEFFORC, EEFGENOC), (OP.EFGE_ELNO.PEFFORR, EEFGENOR),
                     ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPME_ELGA(te=531,
            para_in=((OP.EPME_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
                     (OP.EPME_ELGA.PNBSP_I, ENBSP_I), (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPSI_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EPSI_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (OP.EPSI_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.EPSP_ELGA(te=531,
            para_in=((OP.EPSP_ELGA.PCONTRR, ECONTPG), (OP.EPSP_ELGA.PDEFORR, EDEFOPG),
                     (SP.PMATERC, LC.CMATERC), (OP.EPSP_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPVC_ELGA(te=531,
            para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPVC_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                 ),
            para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
        ),

        OP.EPVC_ELNO(te=4,
            para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
            para_out=((SP.PDEFONO, EDFVCNO), ),
        ),

        OP.FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.FORC_NODA.PCAORIE, CCAORIE),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PNBSP_I, ENBSP_I),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.FULL_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.FULL_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_INER(te=38,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_INER.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PNBSP_I, ENBSP_I), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MINMAX_SP(te=99,
            para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
                     ),
        ),

        OP.M_GAMMA(te=582,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGEPO, CCAGEPO),
                     (OP.M_GAMMA.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.M_GAMMA.PNBSP_I, ENBSP_I),
                     (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), (OP.NSPG_NBVA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PAS_COURANT(te=404,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.RAPH_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RAPH_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.REFE_FORC_NODA.PCAORIE, CCAORIE),
                     (OP.REFE_FORC_NODA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.REFE_FORC_NODA.PNBSP_I, ENBSP_I), (SP.PREFCO, EREFCO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=135,
            para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RIGI_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_TANG(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PNBSP_I, ENBSP_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRC, ECONTPC), (OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
        ),

        OP.SIEF_ELNO(te=4,
            para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
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
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), (OP.TOU_INI_ELEM.PNBSP_I, ENBSP_I),
                     ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                     (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
                     (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),
    )


# redéfinition des 4 LocatedComponents pour MET3SEG4
CABSCUR = LocatedComponents(phys=PHY.ABSC_R, type='ELEM', components=('ABSC[4]',))

CCAORIE = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA','ALPHA2','BETA2',
          'GAMMA2','ALPHA3','BETA3','GAMMA3','ALPHA4',
          'BETA4','GAMMA4','ICOUDE','DN1N2','RCOURB',
          'ANGCOU','ANGZZK',))

NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ','UI2','VI2','WI2','UI3',
          'VI3','WI3','UO2','VO2','WO2',
          'UO3','VO3','WO3','WO','WI1',
          'WO1',))

DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ','UI2','VI2','WI2','UI3',
          'VI3','WI3','UO2','VO2','WO2',
          'UO3','VO3','WO3','WO','WI1',
          'WO1',))

MVECTUC  = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=(NDEPLAC,))
MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))
MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))
MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

CABSCUR.setName('CABSCUR')
CCAORIE.setName('CCAORIE')
NDEPLAC.setName('NDEPLAC')
DDL_MECA.setName('DDL_MECA')
MVECTUC.setName('MVECTUC')
MVECTUR.setName('MVECTUR')
MMATUUR.setName('MMATUUR')
MMATUUC.setName('MMATUUC')


class MET3SEG4(NewElement):
    """Please document this element"""
    meshType = MT.SEG4
    elrefe =(
        ElrefeLoc(MT.SE4, gauss = ('RIGI=FPG3','MASS=FPG4','FPG1=FPG1',), mater=('RIGI','FPG1',),),
    )

    calculs = (
        OP.AMOR_MECA(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                     (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
                     (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CHAR_MECA_FC1D1D(te=583,
            para_in=((OP.CHAR_MECA_FC1D1D.PCAORIE, CCAORIE), (SP.PFC1D1D, LC.CFORCEC),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PVECTUC, MVECTUC), ),
        ),

        OP.CHAR_MECA_FF1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE),
                     (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FF1D1D.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE),
                     (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FR1D1D.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_PESA_R.PNBSP_I, ENBSP_I), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=583,
            para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                     (OP.CHAR_MECA_PRES_F.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PNBSP_I, ENBSP_I), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PRES_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_R.PNBSP_I, ENBSP_I),
                     (SP.PPRESSR, EPRESNO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=589,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_TEMP_R.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.COOR_ELGA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.COOR_ELGA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEGE_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELGA.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.DEGE_ELGA.PDEFOPG, EDEFGPG), ),
        ),

        OP.DEGE_ELNO(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELNO.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELNO.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOGR, EDEFGNO), ),
        ),

        OP.EFGE_ELGA(te=587,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELGA.PCAORIE, CCAORIE),
                     (OP.EFGE_ELGA.PNBSP_I, ENBSP_I), (SP.PSIEFR, ECONTPG),
                     ),
            para_out=((SP.PEFGEC, LC.EEFGEGAC), (SP.PEFGER, LC.EEFGEGAR),
                     ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
                     (OP.EFGE_ELNO.PCOMPOR, CCOMPOR), (OP.EFGE_ELNO.PCONTRR, ECONTPG),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EFGE_ELNO.PNBSP_I, ENBSP_I),
                     (SP.PNONLIN, LC.ENONLIN), (SP.PTEMPSR, CTEMPSR),
                     (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PEFFORC, EEFGENOC), (OP.EFGE_ELNO.PEFFORR, EEFGENOR),
                     ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPME_ELGA(te=531,
            para_in=((OP.EPME_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
                     (OP.EPME_ELGA.PNBSP_I, ENBSP_I), (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPSI_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EPSI_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (OP.EPSI_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.EPSP_ELGA(te=531,
            para_in=((OP.EPSP_ELGA.PCONTRR, ECONTPG), (OP.EPSP_ELGA.PDEFORR, EDEFOPG),
                     (SP.PMATERC, LC.CMATERC), (OP.EPSP_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPVC_ELGA(te=531,
            para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPVC_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                 ),
            para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
        ),

        OP.EPVC_ELNO(te=4,
            para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
            para_out=((SP.PDEFONO, EDFVCNO), ),
        ),

        OP.FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.FORC_NODA.PCAORIE, CCAORIE),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PNBSP_I, ENBSP_I),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.FULL_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.FULL_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_INER(te=38,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_INER.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PNBSP_I, ENBSP_I), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MINMAX_SP(te=99,
            para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
                     ),
        ),

        OP.M_GAMMA(te=582,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGEPO, CCAGEPO),
                     (OP.M_GAMMA.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.M_GAMMA.PNBSP_I, ENBSP_I),
                     (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), (OP.NSPG_NBVA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PAS_COURANT(te=404,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.RAPH_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RAPH_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.REFE_FORC_NODA.PCAORIE, CCAORIE),
                     (OP.REFE_FORC_NODA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.REFE_FORC_NODA.PNBSP_I, ENBSP_I), (SP.PREFCO, EREFCO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=135,
            para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RIGI_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_TANG(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PNBSP_I, ENBSP_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRC, ECONTPC), (OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
        ),

        OP.SIEF_ELNO(te=4,
            para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
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
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), (OP.TOU_INI_ELEM.PNBSP_I, ENBSP_I),
                     ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                     (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
                     (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),
    )


# redéfinition des 4 LocatedComponents pour MET6SEG3
CABSCUR = LocatedComponents(phys=PHY.ABSC_R, type='ELEM', components=('ABSC[3]',))

CCAORIE = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA','ALPHA2','BETA2',
          'GAMMA2','ALPHA3','BETA3','GAMMA3','ICOUDE',
          'DN1N2','RCOURB','ANGCOU','ANGZZK',))

NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ','UI2','VI2','WI2','UI3',
          'VI3','WI3','UI4','VI4','WI4',
          'UI5','VI5','WI5','UI6','VI6',
          'WI6','UO2','VO2','WO2','UO3',
          'VO3','WO3','UO4','VO4','WO4',
          'UO5','VO5','WO5','UO6','VO6',
          'WO6','WO','WI1','WO1',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ','UI2','VI2','WI2','UI3',
          'VI3','WI3','UI4','VI4','WI4',
          'UI5','VI5','WI5','UI6','VI6',
          'WI6','UO2','VO2','WO2','UO3',
          'VO3','WO3','UO4','VO4','WO4',
          'UO5','VO5','WO5','UO6','VO6',
          'WO6','WO','WI1','WO1',))

MVECTUC  = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=(NDEPLAC,))
MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))
MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))
MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

CABSCUR.setName('CABSCUR')
CCAORIE.setName('CCAORIE')
NDEPLAC.setName('NDEPLAC')
DDL_MECA.setName('DDL_MECA')
MVECTUC.setName('MVECTUC')
MVECTUR.setName('MVECTUR')
MMATUUR.setName('MMATUUR')
MMATUUC.setName('MMATUUC')


class MET6SEG3(NewElement):
    """Please document this element"""
    meshType = MT.SEG3
    elrefe =(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG3','MASS=FPG3','FPG1=FPG1',), mater=('RIGI','FPG1',),),
    )

    calculs = (
        OP.AMOR_MECA(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                     (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
                     (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CHAR_MECA_FC1D1D(te=583,
            para_in=((OP.CHAR_MECA_FC1D1D.PCAORIE, CCAORIE), (SP.PFC1D1D, LC.CFORCEC),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PVECTUC, MVECTUC), ),
        ),

        OP.CHAR_MECA_FF1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE),
                     (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FF1D1D.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D1D(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE),
                     (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FR1D1D.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_PESA_R.PNBSP_I, ENBSP_I), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=583,
            para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                     (OP.CHAR_MECA_PRES_F.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PNBSP_I, ENBSP_I), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=583,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_PRES_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_R.PNBSP_I, ENBSP_I),
                     (SP.PPRESSR, EPRESNO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=589,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_TEMP_R.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.COOR_ELGA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (OP.COOR_ELGA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEGE_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELGA.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.DEGE_ELGA.PDEFOPG, EDEFGPG), ),
        ),

        OP.DEGE_ELNO(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.DEGE_ELNO.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.DEGE_ELNO.PNBSP_I, ENBSP_I),
                     (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOGR, EDEFGNO), ),
        ),

        OP.EFGE_ELGA(te=587,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELGA.PCAORIE, CCAORIE),
                     (OP.EFGE_ELGA.PNBSP_I, ENBSP_I), (SP.PSIEFR, ECONTPG),
                     ),
            para_out=((SP.PEFGEC, LC.EEFGEGAC), (SP.PEFGER, LC.EEFGEGAR),
                     ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
                     (OP.EFGE_ELNO.PCOMPOR, CCOMPOR), (OP.EFGE_ELNO.PCONTRR, ECONTPG),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EFGE_ELNO.PNBSP_I, ENBSP_I),
                     (SP.PNONLIN, LC.ENONLIN), (SP.PTEMPSR, CTEMPSR),
                     (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PEFFORC, EEFGENOC), (OP.EFGE_ELNO.PEFFORR, EEFGENOR),
                     ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPME_ELGA(te=531,
            para_in=((OP.EPME_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
                     (OP.EPME_ELGA.PNBSP_I, ENBSP_I), (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPSI_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.EPSI_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (OP.EPSI_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.EPSP_ELGA(te=531,
            para_in=((OP.EPSP_ELGA.PCONTRR, ECONTPG), (OP.EPSP_ELGA.PDEFORR, EDEFOPG),
                     (SP.PMATERC, LC.CMATERC), (OP.EPSP_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPVC_ELGA(te=531,
            para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPVC_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                 ),
            para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
        ),

        OP.EPVC_ELNO(te=4,
            para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
            para_out=((SP.PDEFONO, EDFVCNO), ),
        ),

        OP.FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.FORC_NODA.PCAORIE, CCAORIE),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PNBSP_I, ENBSP_I),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.FULL_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.FULL_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_INER(te=38,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_INER.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.MASS_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PNBSP_I, ENBSP_I), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MINMAX_SP(te=99,
            para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
                     ),
        ),

        OP.M_GAMMA(te=582,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGEPO, CCAGEPO),
                     (OP.M_GAMMA.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.M_GAMMA.PNBSP_I, ENBSP_I),
                     (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), (OP.NSPG_NBVA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PAS_COURANT(te=404,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.RAPH_MECA(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RAPH_MECA.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=585,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.REFE_FORC_NODA.PCAORIE, CCAORIE),
                     (OP.REFE_FORC_NODA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (OP.REFE_FORC_NODA.PNBSP_I, ENBSP_I), (SP.PREFCO, EREFCO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=135,
            para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RIGI_MECA(te=582,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_TANG(te=586,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAMASS, CCAMASS),
                     (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=584,
            para_in=((SP.PCAGEPO, CCAGEPO), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PNBSP_I, ENBSP_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRC, ECONTPC), (OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
        ),

        OP.SIEF_ELNO(te=4,
            para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
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
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), (OP.TOU_INI_ELEM.PNBSP_I, ENBSP_I),
                     ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                     (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
                     (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),
    )
