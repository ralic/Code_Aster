
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


CCACO3D  = LocatedComponents(phys=PHY.CACO3D, type='ELEM',
    components=('CRF',))


CCACOQU  = LocatedComponents(phys=PHY.CACOQU, type='ELEM',
    components=('EP','ALPHA','BETA','KAPPA','CTOR',
          'EXCENT','INERTIE',))


CCAORIE  = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','REP',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ','DRX','DRY',
          'DRZ',)),
    ('EN2',('DRX','DRY','DRZ',)),))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ','DRX','DRY',
          'DRZ',)),
    ('EN2',('DRX','DRY','DRZ',)),))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE','MEMBRANE','FLEXION',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='MASS',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFGPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='MASS',
    components=('EXX','EYY','EXY','KXX','KYY',
          'KXY','GAX','GAY',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='MASS',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='MASS',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


EDEFGPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='MASS',
    components=('EXX','EYY','EXY','KXX','KYY',
          'KXY','GAX','GAY',))


EDEFGNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EXX','EYY','EXY','KXX','KYY',
          'KXY','GAX','GAY',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


EFORCNO  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='MASS',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


ENBSP_I  = LocatedComponents(phys=PHY.NBSP_I, type='ELEM',
    components=('COQ_NCOU',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='MASS',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='MASS',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='MASS',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


EEFGENOC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('NXX','NYY','NXY','MXX','MYY',
          'MXY','QX','QY',))


EEFGEPGC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='MASS',
    components=('NXX','NYY','NXY','MXX','MYY',
          'MXY','QX','QY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='MASS',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ESIGMNOR = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='MASS',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


EEFGENOR = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('NXX','NYY','NXY','MXX','MYY',
          'MXY','QX','QY',))


EEFGEPGR = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='MASS',
    components=('NXX','NYY','NXY','MXX','MYY',
          'MXY','QX','QY',))


EGAMIMA  = LocatedComponents(phys=PHY.SPMX_R, type='ELGA', location='MASS',
    components=('VAL','NUCOU','NUSECT','NUFIBR','POSIC',
          'POSIS',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='MASS',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class TemplateElement(Element):
    """Only a template to shared definitions of options"""
    calculs = (

        OP.ADD_SIGM(te=581,
            para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
                     ),
            para_out=((SP.PEPCON3, ECONTPG), ),
        ),

        OP.AMOR_MECA(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                     (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
                     (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CHAR_MECA_FFCO3D(te=403,
            para_in=((SP.PFFCO3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FRCO3D(te=403,
            para_in=((SP.PFRCO3D, EFORCNO), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_HYDR_R(te=312,
            para_in=((SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=403,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=403,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=403,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PPRESSR, EPRESNO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRSU_F(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRSU_R(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=403,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PROTATR, LC.CROTATR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SECH_R(te=312,
            para_in=((SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SFCO3D(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PFFCO3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SRCO3D(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PFRCO3D, EFORCNO), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=419,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_TEMP_R.PNBSP_I, ENBSP_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=488,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (OP.COOR_ELGA.PNBSP_I, ENBSP_I), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEGE_ELGA(te=410,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.DEGE_ELGA.PNBSP_I, ENBSP_I), (OP.DEGE_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.DEGE_ELGA.PDEFOPG, EDEFGPG), ),
        ),

        OP.DEGE_ELNO(te=410,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.DEGE_ELNO.PNBSP_I, ENBSP_I), (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOGR, EDEFGNO), ),
        ),

        OP.ECIN_ELEM(te=406,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.POMEGA2, LC.COMEG2R), (OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVITESR, DDL_MECA), ),
            para_out=((SP.PENERCR, EENERR), ),
        ),

        OP.EFGE_ELGA(te=451,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PMATERC, LC.CMATERC),
                     (OP.EFGE_ELGA.PNBSP_I, ENBSP_I), (SP.PSIEFR, ECONTPG),
                     ),
            para_out=((SP.PEFGEC, EEFGEPGC), (SP.PEFGER, EEFGEPGR),
                     ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCACOQU, CCACOQU), (OP.EFGE_ELNO.PCOMPOR, CCOMPOR),
                     (OP.EFGE_ELNO.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.EFGE_ELNO.PNBSP_I, ENBSP_I), (SP.PNONLIN, LC.ENONLIN),
                     (SP.PTEMPSR, CTEMPSR), (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
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

        OP.EPOT_ELEM(te=401,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.EPOT_ELEM.PNBSP_I, ENBSP_I), (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.EPSI_ELGA(te=410,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.EPSI_ELGA.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=40,
            para_in=((SP.PCACOQU, CCACOQU), (OP.EPSI_ELNO.PDEFOPG, EDEFOPG),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.EPSI_ELNO.PNBSP_I, ENBSP_I), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.FERRAILLAGE(te=146,
            para_in=((SP.PCACOQU, CCACOQU), (OP.FERRAILLAGE.PEFFORR, EEFGENOR),
                     (SP.PFERRA1, LC.CFER1_R), ),
            para_out=((SP.PFERRA2, LC.CFER2_R), ),
        ),

        OP.FORC_NODA(te=416,
            para_in=((SP.PCACOQU, CCACOQU), (OP.FORC_NODA.PCOMPOR, CCOMPOR),
                     (OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.FORC_NODA.PNBSP_I, ENBSP_I), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=414,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDDEPLA, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (OP.FULL_MECA.PNBSP_I, ENBSP_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((OP.FULL_MECA.PCACO3D, CCACO3D), (SP.PCODRET, LC.ECODRET),
                     (OP.FULL_MECA.PCONTPR, ECONTPG), (SP.PMATUNS, MMATUNS),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA_ELAS(te=414,
            para_in=((SP.PCARCRI, CCARCRI), (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA_ELAS.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.FULL_MECA_ELAS.PCACO3D, CCACO3D), (SP.PCODRET, LC.ECODRET),
                     (OP.FULL_MECA_ELAS.PCONTPR, ECONTPG), (SP.PMATUNS, MMATUNS),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_INER(te=417,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.MASS_INER.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=406,
            para_in=((SP.PCACOQU, CCACOQU), (OP.MASS_MECA.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MINMAX_SP(te=99,
            para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
                     ),
        ),

        OP.M_GAMMA(te=406,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCACOQU, CCACOQU),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), (OP.NSPG_NBVA.PNBSP_I, ENBSP_I),
                     ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PAS_COURANT(te=405,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.PREP_VRC(te=408,
            para_in=((SP.PCACOQU, CCACOQU), (OP.PREP_VRC.PINST_R, CTEMPSR),
                     (OP.PREP_VRC.PNBSP_I, ENBSP_I), (SP.PTEMPEF, LC.CTEMPEF),
                     (SP.PTEMPER, LC.NTEMPER), ),
            para_out=((SP.PTEMPCR, LC.CTEREFE), ),
        ),

        OP.RAPH_MECA(te=414,
            para_in=((OP.RAPH_MECA.PCACO3D, CCACO3D), (SP.PCACOQU, CCACOQU),
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
                     (OP.RAPH_MECA.PCONTMR, ECONTPG), (SP.PDDEPLA, DDL_MECA),
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

        OP.REFE_FORC_NODA(te=416,
            para_in=((SP.PCACOQU, CCACOQU), (OP.REFE_FORC_NODA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.REFE_FORC_NODA.PNBSP_I, ENBSP_I),
                     (SP.PREFCO, EREFCO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=134,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.REPE_GENE(te=443,
            para_in=((SP.PANGREP, CCAORIE), (SP.PCACOQU, CCACOQU),
                     (SP.PDGGAIN, EDEFGPG), (SP.PDGGAINC, EDEFGPC),
                     (SP.PDGNOIN, EDEFGNO), (SP.PDGNOINC, LC.EDEFGNC),
                     (SP.PEFGAIN, EEFGEPGR), (SP.PEFGAINC, EEFGEPGC),
                     (SP.PEFNOIN, EEFGENOR), (SP.PEFNOINC, EEFGENOC),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PDGGAOUC, EDEFGPC), (SP.PDGGAOUT, EDEFGPG),
                     (SP.PDGNOOUC, LC.EDEFGNC), (SP.PDGNOOUT, EDEFGNO),
                     (SP.PEFGAOUC, EEFGEPGC), (SP.PEFGAOUT, EEFGEPGR),
                     (SP.PEFNOOUC, EEFGENOC), (SP.PEFNOOUT, EEFGENOR),
                     ),
        ),

        OP.REPE_TENS(te=443,
            para_in=((SP.PANGREP, CCAORIE), (SP.PCACOQU, CCACOQU),
                     (SP.PCOGAIN, ECONTPG), (SP.PCONOIN, ESIGMNOR),
                     (SP.PDEGAIN, EDEFOPG), (SP.PDENOIN, EDEFONO),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCOGAOUT, ECONTPG), (SP.PCONOOUT, ESIGMNOR),
                     (SP.PDEGAOUT, EDEFOPG), (SP.PDENOOUT, EDEFONO),
                     ),
        ),

        OP.RIGI_MECA(te=401,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PNBSP_I, ENBSP_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((OP.RIGI_MECA.PCACO3D, CCACO3D), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_MECA_ELAS(te=414,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_ELAS.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_ELAS.PNBSP_I, ENBSP_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.RIGI_MECA_ELAS.PCACO3D, CCACO3D), (SP.PMATUNS, MMATUNS),
                     (SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_GE(te=402,
            para_in=((SP.PCACOQU, CCACOQU), (OP.RIGI_MECA_GE.PCONTRR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_PRSU_F(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA_PRSU_R(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA_SFCO3D(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PFFCO3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA_SRCO3D(te=486,
            para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PFRCO3D, EFORCNO), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA_TANG(te=414,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDDEPLA, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA_TANG.PNBSP_I, ENBSP_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
            para_out=((OP.RIGI_MECA_TANG.PCACO3D, CCACO3D), (SP.PMATUNS, MMATUNS),
                     (SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=410,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.SIEF_ELGA.PNBSP_I, ENBSP_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PCONTRC, ECONTPC), (OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
        ),

        OP.SIEF_ELNO(te=40,
            para_in=((SP.PCACOQU, CCACOQU), (OP.SIEF_ELNO.PCOMPOR, CCOMPOR),
                     (OP.SIEF_ELNO.PCONTRR, ECONTPG), (SP.PDEPPLU, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.SIEF_ELNO.PNBSP_I, ENBSP_I),
                     (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG), ),
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

        OP.SIGM_ELNO(te=40,
            para_in=((SP.PCACOQU, CCACOQU), (OP.SIGM_ELNO.PCOMPOR, CCOMPOR),
                     (OP.SIGM_ELNO.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.SIGM_ELNO.PNBSP_I, ENBSP_I),
                     ),
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

        OP.VARI_ELNO(te=415,
            para_in=((OP.VARI_ELNO.PCOMPOR, CCOMPOR), (OP.VARI_ELNO.PNBSP_I, ENBSP_I),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),

        OP.VERI_CARA_ELEM(te=119,
            para_in=((SP.PCACOQU, CCACOQU), ),
            para_out=((SP.PBIDON, LC.ECOURAN), ),
        ),

    )


class MEC3QU9H(TemplateElement):
    """Please document this element"""
    meshType = MT.QUAD9
    nodes = (
            SetOfNodes('EN2', (9,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG4','MASS=FPG9COQ','FPG1=FPG1',), mater=('RIGI','MASS','FPG1',),),
            ElrefeLoc(MT.MEC3QU9H,),
        )

    calculs = (
        OP.CHAR_MECA_SFCO3D(te=486),
    )


#------------------------------------------------------------
class MEC3TR7H(TemplateElement):
    """Please document this element"""
    meshType = MT.TRIA7
    nodes = (
            SetOfNodes('EN2', (7,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR7, gauss = ('RIGI=FPG3','MASS=FPG7','FPG1=FPG1',), mater=('RIGI','MASS','FPG1',),),
            ElrefeLoc(MT.MEC3TR7H,),
        )

del TemplateElement
