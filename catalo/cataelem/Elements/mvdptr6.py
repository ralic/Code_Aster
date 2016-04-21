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
    components=('C','ALPHA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


NDEPLAR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','VARI','LAG_GV',)),
    ('EN2',('DX','DY',)),))


CEPSINF  = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
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


ECOPILO  = LocatedComponents(phys=PHY.PILO_R, type='ELGA', location='RIGI',
    components=('A0','A[3]','ETA',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM','VARI','LAG_GV',))


ESIGMPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ESIGMNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIGV_A',
          'SIGV_L','SIGV_GX','SIGV_GY',))


ESIGMPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ESIGMNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIGV_A',
          'SIGV_L','SIGV_GX','SIGV_GY',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIGV_A',
          'SIGV_L','SIGV_GX','SIGV_GY',))


EGVALO_R = LocatedComponents(phys=PHY.VALO_R, type='ELGA', location='RIGI',
    components=('VALEUR','GRAD_X','GRAD_Y',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MVECTDR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=NDEPLAR)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MVDPTR6(Element):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG6',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
    calculs = (

        OP.AMOR_MECA(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                     (SP.PMATERC, LC.CMATERC), (SP.PRIGINS, MMATUNS),
                     (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.CHAR_MECA_EPSA_R(te=421,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSA_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_EPSI_F(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINF, CEPSINF),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_EPSI_R(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINR, CEPSINR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_FF2D2D(te=94,
            para_in=((SP.PFF2D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_FR2D2D(te=93,
            para_in=((SP.PFR2D2D, NFORCER), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_HYDR_R(te=492,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=85,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=84,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PROTATR, LC.CROTATR), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_SECH_R(te=492,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=83,
            para_in=((SP.PCAMASS, CCAMASS), (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.COOR_ELGA(te=479,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.FORC_NODA(te=508,
            para_in=((OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPSG_ELGA(te=87,
            para_in=((SP.PDEPLAR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPSG_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPSG_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSG_ELNO(te=4,
            para_in=((OP.EPSG_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPSI_ELGA(te=87,
            para_in=((OP.EPSI_ELGA.PCOMPOR, CCOMPOR), (SP.PDEPLAR, NDEPLAR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.FULL_MECA(te=545,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.FULL_MECA_ELAS(te=545,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR), (OP.FULL_MECA_ELAS.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_MECA(te=562,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MASS_MECA_DIAG(te=562,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MASS_MECA_EXPLI(te=562,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PILO_PRED_DEFO(te=518,
            para_in=((SP.PCDTAU, LC.CCDTAU), (SP.PDDEPLR, DDL_MECA),
                     (SP.PDEPL0R, DDL_MECA), (SP.PDEPL1R, DDL_MECA),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PTYPEPI, LC.CTYPEPI), ),
            para_out=((OP.PILO_PRED_DEFO.PCOPILO, ECOPILO), ),
        ),

        OP.PILO_PRED_ELAS(te=518,
            para_in=((SP.PBORNPI, LC.CBORNPI), (SP.PCDTAU, LC.CCDTAU),
                     (OP.PILO_PRED_ELAS.PCOMPOR, CCOMPOR), (OP.PILO_PRED_ELAS.PCONTMR, ECONTPG),
                     (SP.PDDEPLR, DDL_MECA), (SP.PDEPL0R, DDL_MECA),
                     (SP.PDEPL1R, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTYPEPI, LC.CTYPEPI), (OP.PILO_PRED_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.PILO_PRED_ELAS.PCOPILO, ECOPILO), ),
        ),

        OP.RAPH_MECA(te=545,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=508,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PREFCO, EREFCO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=133,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, LC.CGEOM2D), (SP.PREPLO2, LC.CGEOM2D),
                     ),
        ),

        OP.RIGI_MECA_ELAS(te=545,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_ELAS.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_MECA_TANG(te=545,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.SIEF_ELNO(te=4,
            para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
                     ),
        ),

        OP.SIEQ_ELGA(te=335,
            para_in=((OP.SIEQ_ELGA.PCONTRR, ESIGMPG), ),
            para_out=((OP.SIEQ_ELGA.PCONTEQ, ECOEQPG), ),
        ),

        OP.SIEQ_ELNO(te=335,
            para_in=((OP.SIEQ_ELNO.PCONTRR, ESIGMNO), ),
            para_out=((OP.SIEQ_ELNO.PCONTEQ, LC.ECOEQNO), ),
        ),

        OP.SIGM_ELGA(te=546,
            para_in=((SP.PSIEFR, ESIGMPG), ),
            para_out=((SP.PSIGMC, ESIGMPC), (SP.PSIGMR, ESIGMPG),
                     ),
        ),

        OP.SIGM_ELNO(te=4,
            para_in=((OP.SIGM_ELNO.PCONTRR, ESIGMPG), ),
            para_out=((SP.PSIEFNOC, ESIGMNC), (OP.SIGM_ELNO.PSIEFNOR, ESIGMNO),
                     ),
        ),

        OP.SING_ELEM(te=99,
            para_out=((SP.PSING_R, LC.ESINGUL), ),
        ),

        OP.SING_ELNO(te=99,
            para_out=((SP.PSINGNO, LC.ESINGNO), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM2D), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
                     (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R), (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG),
                     (SP.PVALO_R, EGVALO_R), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PDOMMAG, LC.EDOMGNO), (OP.TOU_INI_ELNO.PEPSI_R, EDEFONO),
                     (OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
                     ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

    )


#------------------------------------------------------------
class MVDPQS8(MVDPTR6):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG4','MASS=FPG9','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG9',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MVAXTR6(MVDPTR6):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG6',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MVAXQS8(MVDPTR6):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG4','MASS=FPG9','FPG1=FPG1',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG9',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
