
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
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','EPXX','EPYY',
          'EPZZ','EPXY','EPXZ','EPYZ',))


NDEPLAR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EENERPG  = LocatedComponents(phys=PHY.ENER_R, type='ELGA', location='RIGI',
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


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


ECOPILO  = LocatedComponents(phys=PHY.PILO_R, type='ELGA', location='RIGI',
    components=('A0','A[3]','ETA',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MVECTDR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(NDEPLAR,))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MGCA_TETRA4(Element):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG1','FPG1=FPG1','MASS=FPG4',), mater=('RIGI','FPG1',),),
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG1','MASS=FPG4',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','MASS=COT3',),),
        )
    calculs = (

        OP.ADD_SIGM(te=581,
            para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
                     ),
            para_out=((SP.PEPCON3, ECONTPG), ),
        ),

        OP.CHAR_MECA_EPSA_R(te=426,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSA_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_EPSI_F(te=49,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINF, CEPSINF),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_EPSI_R(te=49,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINR, CEPSINR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_FF3D3D(te=17,
            para_in=((SP.PFF3D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_FR3D3D(te=16,
            para_in=((SP.PFR3D3D, NFORCER), (SP.PGEOMER, NGEOMER),
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

        OP.CHAR_MECA_PESA_R(te=15,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_SECH_R(te=492,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=13,
            para_in=((SP.PCAMASS, CCAMASS), (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTDR), ),
        ),

        OP.COOR_ELGA(te=488,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.ECIN_ELEM(te=12,
            para_in=((SP.PDEPLAR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.POMEGA2, LC.COMEG2R),
                     (OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVITESR, NDEPLAR),
                     ),
            para_out=((SP.PENERCR, EENERR), ),
        ),

        OP.ENEL_ELEM(te=491,
            para_in=((OP.ENEL_ELEM.PCOMPOR, CCOMPOR), (OP.ENEL_ELEM.PCONTPR, ECONTPG),
                     (SP.PDEPLR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.ENEL_ELGA(te=576,
            para_in=((SP.PCAMASS, CCAMASS), (OP.ENEL_ELGA.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELGA.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.ENEL_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIGR, ZVARIPG),
                     ),
            para_out=((OP.ENEL_ELGA.PENERDR, EENERPG), ),
        ),

        OP.ENEL_ELNO(te=4,
            para_in=((OP.ENEL_ELNO.PENERPG, EENERPG), ),
            para_out=((SP.PENERNO, EENERNO), ),
        ),

        OP.ENER_TOTALE(te=491,
            para_in=((OP.ENER_TOTALE.PCOMPOR, CCOMPOR), (OP.ENER_TOTALE.PCONTMR, ECONTPG),
                     (OP.ENER_TOTALE.PCONTPR, ECONTPG), (SP.PDEPLM, NDEPLAR),
                     (SP.PDEPLR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.ENER_TOTALE.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENER_TOTALE.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.EPEQ_ELGA(te=335,
            para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPEQ_ELNO(te=335,
            para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPME_ELGA(te=25,
            para_in=((OP.EPME_ELGA.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPMG_ELGA(te=25,
            para_in=((OP.EPMG_ELGA.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.EPMG_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPMG_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPMG_ELNO(te=4,
            para_in=((OP.EPMG_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPMQ_ELGA(te=335,
            para_in=((OP.EPMQ_ELGA.PDEFORR, EDEFOPG), ),
            para_out=((OP.EPMQ_ELGA.PDEFOEQ, EDFEQPG), ),
        ),

        OP.EPMQ_ELNO(te=335,
            para_in=((OP.EPMQ_ELNO.PDEFORR, EDEFONO), ),
            para_out=((OP.EPMQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
        ),

        OP.EPOT_ELEM(te=218,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, NDEPLAR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.EPSG_ELGA(te=25,
            para_in=((SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPSG_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPSG_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSG_ELNO(te=4,
            para_in=((OP.EPSG_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPSI_ELGA(te=25,
            para_in=((SP.PDEPLAR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
                     ),
        ),

        OP.EPSI_ELNO(te=4,
            para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
                     ),
        ),

        OP.EPSP_ELGA(te=333,
            para_in=((OP.EPSP_ELGA.PCOMPOR, CCOMPOR), (OP.EPSP_ELGA.PCONTRR, ECONTPG),
                     (SP.PDEPLAR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.FORC_NODA(te=437,
            para_in=((OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=113,
            para_in=((SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PITERAT, LC.CITERAT), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA_ELAS(te=113,
            para_in=((SP.PCARCRI, CCARCRI), (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA_ELAS.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INDIC_ENER(te=491,
            para_in=((OP.INDIC_ENER.PCOMPOR, CCOMPOR), (OP.INDIC_ENER.PCONTPR, ECONTPG),
                     (SP.PDEPLR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.INDIC_ENER.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.INDIC_ENER.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), (SP.PENERD2, EENERR),
                     ),
        ),

        OP.INDIC_SEUIL(te=491,
            para_in=((OP.INDIC_SEUIL.PCOMPOR, CCOMPOR), (OP.INDIC_SEUIL.PCONTPR, ECONTPG),
                     (SP.PDEPLR, NDEPLAR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.INDIC_SEUIL.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.INDIC_SEUIL.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), (SP.PENERD2, EENERR),
                     ),
        ),

        OP.INIT_MAIL_VOIS(te=99,
            para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PILO_PRED_DEFO(te=543,
            para_in=((OP.PILO_PRED_DEFO.PCOMPOR, CCOMPOR), (OP.PILO_PRED_DEFO.PCONTMR, ECONTPG),
                     (SP.PDDEPLR, NDEPLAR), (SP.PDEPL0R, NDEPLAR),
                     (SP.PDEPL1R, NDEPLAR), (SP.PDEPLMR, NDEPLAR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTYPEPI, LC.CTYPEPI), (OP.PILO_PRED_DEFO.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.PILO_PRED_DEFO.PCOPILO, ECOPILO), ),
        ),

        OP.PILO_PRED_ELAS(te=544,
            para_in=((SP.PBORNPI, LC.CBORNPI), (SP.PCDTAU, LC.CCDTAU),
                     (OP.PILO_PRED_ELAS.PCOMPOR, CCOMPOR), (OP.PILO_PRED_ELAS.PCONTMR, ECONTPG),
                     (SP.PDDEPLR, DDL_MECA), (SP.PDEPL0R, DDL_MECA),
                     (SP.PDEPL1R, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTYPEPI, LC.CTYPEPI), (OP.PILO_PRED_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.PILO_PRED_ELAS.PCOPILO, ECOPILO), ),
        ),

        OP.RAPH_MECA(te=113,
            para_in=((SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
                     (OP.RAPH_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PITERAT, LC.CITERAT), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REPERE_LOCAL(te=133,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RIGI_MECA_ELAS(te=113,
            para_in=((SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR),
                     (OP.RIGI_MECA_ELAS.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG), ),
            para_out=((SP.PMATUNS, MMATUNS), ),
        ),

        OP.RIGI_MECA_TANG(te=113,
            para_in=((SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
                     (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PITERAT, LC.CITERAT), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), ),
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
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
                     (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R), (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG),
                     (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PDOMMAG, LC.EDOMGNO), (OP.TOU_INI_ELNO.PEPSI_R, EDEFONO),
                     (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
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
