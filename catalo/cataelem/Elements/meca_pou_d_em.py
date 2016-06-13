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


CABSCUR  = LocatedComponents(phys=PHY.ABSC_R, type='ELEM',
    components=('ABSC[2]',))


CCAGEPO  = LocatedComponents(phys=PHY.CAGEPO, type='ELEM',
    components=('R1','EP1',))


CCAGNPO  = LocatedComponents(phys=PHY.CAGNPO, type='ELEM',
    components=('A1','IY1','IZ1','AY1','AZ1',
          'EY1','EZ1','JX1','RY1','RZ1',
          'RT1','JG1','A2','IY2','IZ2',
          'AY2','AZ2','EY2','EZ2','JX2',
          'RY2','RZ2','RT2','JG2','TVAR',))


CCAORIE  = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ',))


NVITER   = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX',))


EDEFGNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','GAXY','GAXZ','GAT','KY',
          'KZ',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPX','KY','KZ',))
    
    
CEPSINF  = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
                             components=('EPX', 'KY', 'KZ',))


EDFVCPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPTHER_L',))


EDFVCNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPTHER_L',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


CFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))




EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


ENBSP_I  = LocatedComponents(phys=PHY.NBSP_I, type='ELEM',
    components=('NBFIBR','NBGRFI','TYGRFI','NBCARMAX','NUG[10]',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='MATER',
    components=('X1',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('EFFORT','MOMENT',))


EEFGENC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('N','VY','VZ','MT','MFY',
          'MFZ',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX',))


EEFGENO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('N','VY','VZ','MT','MFY',
          'MFZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


EGAMIMA  = LocatedComponents(phys=PHY.SPMX_R, type='ELGA', location='RIGI',
    components=('VAL','NUCOU','NUSECT','NUFIBR','POSIC',
          'POSIS',))


ESTRAUX  = LocatedComponents(phys=PHY.STRX_R, type='ELGA', location='RIGI',
    components=('N','VY','VZ','MT','MFY',
          'MFZ','BX','EPXX','GAXY','GAXZ',
          'GAT','KY','KZ','GAX','DXINT',
          'ALPHA','BETA','GAMMA',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUC  = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=NDEPLAC)

MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=NDEPLAC)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MECA_POU_D_EM(Element):
    """Please document this element"""
    meshType = MT.SEG2
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','FPG1=FPG1','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
        )
    calculs = (

        OP.ADD_SIGM(te=581,
            para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
                     ),
            para_out=((SP.PEPCON3, ECONTPG), ),
        ),

        OP.AMOR_MECA(te=50,
            para_in=((OP.AMOR_MECA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMASSEL, MMATUUR), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CHAR_MECA_EPSI_R(te=20,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_EPSI_R.PCAORIE, CCAORIE),
                     (OP.CHAR_MECA_EPSI_R.PCOMPOR, CCOMPOR), (SP.PEPSINR, CEPSINR),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_EPSI_R.PNBSP_I, ENBSP_I),
                     (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),
        
        OP.CHAR_MECA_EPSI_F(te=20,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_EPSI_F.PCAORIE, CCAORIE),
                     (OP.CHAR_MECA_EPSI_F.PCOMPOR, CCOMPOR), (SP.PEPSINF, CEPSINF),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_EPSI_F.PNBSP_I, ENBSP_I),
                     (OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),
        
        OP.CHAR_MECA_FC1D1D(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_FC1D1D.PCAORIE, CCAORIE),
                     (SP.PFC1D1D, LC.CFORCEC), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUC, MVECTUC), ),
        ),

        OP.CHAR_MECA_FF1D1D(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE),
                     (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D1D(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE),
                     (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FRELEC(te=145,
            para_in=((SP.PFRELEC, LC.CFRELEC), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FRLAPL(te=148,
            para_in=((SP.PFLAPLA, LC.CFLAPLA), (SP.PGEOMER, NGEOMER),
                     (SP.PLISTMA, LC.CLISTMA), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_HYDR_R(te=312,
            para_in=((SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                     (OP.CHAR_MECA_PESA_R.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_PESA_R.PNBSP_I, ENBSP_I), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_ROTA_R.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PROTATR, LC.CROTATR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SECH_R(te=312,
            para_in=((SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SF1D1D(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_SF1D1D.PCAORIE, CCAORIE),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SR1D1D(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_SR1D1D.PCAORIE, CCAORIE),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PVENTCX, LC.CVENTCX),
                     (SP.PVITER, NVITER), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=150,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
                     (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_TEMP_R.PNBSP_I, ENBSP_I), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((OP.COOR_ELGA.PCAORIE, CCAORIE), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (OP.COOR_ELGA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.COOR_ELGA_MATER(te=463,
            para_in=((OP.COOR_ELGA_MATER.PCAORIE, CCAORIE), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (OP.COOR_ELGA_MATER.PNBSP_I, ENBSP_I),
                     ),
            para_out=((SP.PCOOPGM, LC.EGGEMA_R), ),
        ),

        OP.DEGE_ELNO(te=158,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.DEGE_ELNO.PCAORIE, CCAORIE),
                     (OP.DEGE_ELNO.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.DEGE_ELNO.PNBSP_I, ENBSP_I), (OP.DEGE_ELNO.PSTRXRR, ESTRAUX),
                     (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOGR, EDEFGNO), ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
                     (SP.PCHDYNR, DDL_MECA), (SP.PCOEFFC, LC.CCOEFC),
                     (SP.PCOEFFR, LC.CCOEFR), (OP.EFGE_ELNO.PCOMPOR, CCOMPOR),
                     (OP.EFGE_ELNO.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFF1D1D, CFORCEF), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EFGE_ELNO.PNBSP_I, ENBSP_I),
                     (SP.PNONLIN, LC.ENONLIN), (SP.PPESANR, LC.CPESANR),
                     (OP.EFGE_ELNO.PSTRXRR, ESTRAUX), (SP.PSUROPT, LC.CSUROPT),
                     (SP.PTEMPSR, CTEMPSR), (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PEFFORC, EEFGENC), (OP.EFGE_ELNO.PEFFORR, EEFGENO),
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
            para_in=((OP.EPME_ELGA.PCOMPOR, CCOMPOR), (OP.EPME_ELGA.PDEFORR, EDEFOPG),
                     (SP.PMATERC, LC.CMATERC), (OP.EPME_ELGA.PNBSP_I, ENBSP_I),
                     (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPOT_ELEM(te=151,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.EPOT_ELEM.PCAORIE, CCAORIE),
                     (OP.EPOT_ELEM.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPOT_ELEM.PNBSP_I, ENBSP_I),
                     (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPOT_ELEM.PENERDR, LC.EENEDNO), ),
        ),

        OP.EPSI_ELGA(te=537,
            para_in=((OP.EPSI_ELGA.PCAORIE, CCAORIE), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (OP.EPSI_ELGA.PNBSP_I, ENBSP_I), (OP.EPSI_ELGA.PSTRXRR, ESTRAUX),
                     (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.EPSP_ELGA(te=531,
            para_in=((OP.EPSP_ELGA.PCOMPOR, CCOMPOR), (OP.EPSP_ELGA.PCONTRR, ECONTPG),
                     (OP.EPSP_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
                     (OP.EPSP_ELGA.PNBSP_I, ENBSP_I), (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPVC_ELGA(te=531,
            para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PMATERC, LC.CMATERC),
                     (OP.EPVC_ELGA.PNBSP_I, ENBSP_I), (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
        ),

        OP.EPVC_ELNO(te=4,
            para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
            para_out=((SP.PDEFONO, EDFVCNO), ),
        ),

        OP.FORC_NODA(te=517,
            para_in=((OP.FORC_NODA.PCAORIE, CCAORIE), (OP.FORC_NODA.PCOMPOR, CCOMPOR),
                     (OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PNBSP_I, ENBSP_I),
                     (SP.PSTRXMR, ESTRAUX), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=535,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.FULL_MECA.PCAORIE, CCAORIE),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.FULL_MECA.PNBSP_I, ENBSP_I), (SP.PSTRXMR, ESTRAUX),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (SP.PSTRXPR, ESTRAUX),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.INI_SP_MATER(te=99,
            para_out=((SP.PHYDMAT, LC.EHYDRMA), (SP.PNEUMAT, EMNEUT_R),
                     (SP.PTEMMAT, LC.ETEMPMA), ),
        ),

        OP.INI_STRX(te=23,
            para_in=((OP.INI_STRX.PCAORIE, CCAORIE), ),
            para_out=((SP.PSTRX_R, ESTRAUX), ),
        ),

        OP.MASS_FLUI_STRU(te=141,
            para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                     (SP.PCAGNPO, CCAGNPO), (OP.MASS_FLUI_STRU.PCAORIE, CCAORIE),
                     (OP.MASS_FLUI_STRU.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_FLUI_STRU.PNBSP_I, ENBSP_I), (OP.MASS_FLUI_STRU.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MASS_INER(te=38,
            para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                     (SP.PCAGNPO, CCAGNPO), (OP.MASS_INER.PCAORIE, CCAORIE),
                     (OP.MASS_INER.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PNBSP_I, ENBSP_I), (OP.MASS_INER.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=141,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.MASS_MECA.PCAORIE, CCAORIE),
                     (OP.MASS_MECA.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PNBSP_I, ENBSP_I), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MASS_MECA_DIAG(te=141,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.MASS_MECA_DIAG.PCAORIE, CCAORIE),
                     (OP.MASS_MECA_DIAG.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA_DIAG.PNBSP_I, ENBSP_I), (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MECA_GYRO(te=259,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.MECA_GYRO.PCAORIE, CCAORIE),
                     (OP.MECA_GYRO.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MECA_GYRO.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.MINMAX_SP(te=99,
            para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
                     ),
        ),

        OP.M_GAMMA(te=141,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGNPO, CCAGNPO),
                     (OP.M_GAMMA.PCAORIE, CCAORIE), (OP.M_GAMMA.PCOMPOR, CCOMPOR),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
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

        OP.RAPH_MECA(te=535,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RAPH_MECA.PCAORIE, CCAORIE),
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
                     (OP.RAPH_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PNBSP_I, ENBSP_I), (SP.PSTRXMR, ESTRAUX),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (SP.PSTRXPR, ESTRAUX), (OP.RAPH_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.REFE_FORC_NODA(te=517,
            para_in=((SP.PREFCO, EREFCO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=135,
            para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
            para_out=((SP.PREPLO1, LC.CGEOM3D), (SP.PREPLO2, LC.CGEOM3D),
                     (SP.PREPLO3, LC.CGEOM3D), ),
        ),

        OP.RIGI_FLUI_STRU(te=140,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_FLUI_STRU.PCAORIE, CCAORIE),
                     (OP.RIGI_FLUI_STRU.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_FLUI_STRU.PNBSP_I, ENBSP_I), (OP.RIGI_FLUI_STRU.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_GYRO(te=262,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_GYRO.PCAORIE, CCAORIE),
                     (OP.RIGI_GYRO.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_GYRO.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA(te=140,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (OP.RIGI_MECA.PCOMPOR, CCOMPOR), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PNBSP_I, ENBSP_I), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_TANG(te=535,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE),
                     (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
                     (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PFIBRES, LC.ECAFIEL),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PNBSP_I, ENBSP_I), (SP.PSTRXMR, ESTRAUX),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=537,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (OP.SIEF_ELGA.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PNBSP_I, ENBSP_I),
                     (OP.SIEF_ELGA.PSTRXRR, ESTRAUX), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
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

        OP.SIPM_ELNO(te=149,
            para_in=((OP.SIPM_ELNO.PNBSP_I, ENBSP_I), (OP.SIPM_ELNO.PSIEFNOR, ECONTNO),
                     (OP.SIPM_ELNO.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PSIMXRC, LC.ESIMXNC), (SP.PSIMXRR, LC.ESIMXNO),
                     ),
        ),

        OP.STRX_ELGA(te=537,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.STRX_ELGA.PCAORIE, CCAORIE),
                     (OP.STRX_ELGA.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFIBRES, LC.ECAFIEL), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.STRX_ELGA.PNBSP_I, ENBSP_I),
                     (OP.STRX_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.STRX_ELGA.PSTRXRR, ESTRAUX), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((SP.PCAFI_R, LC.ECAFIEL), (OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D),
                     (OP.TOU_INI_ELEM.PNBSP_I, ENBSP_I), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PINST_R, LC.EEINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.EENEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.EENEUT_R),
                     (OP.TOU_INI_ELNO.PSIEF_R, EEFGENO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
                     ),
        ),

    )
