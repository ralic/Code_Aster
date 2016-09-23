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
          'ALPHA','LC_EXT2[2]','POSTINCR',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='RIGI',
    components=('DX','DY',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EENERPG  = LocatedComponents(phys=PHY.ENER_R, type='ELGA', location='RIGI',
    components=('TOTALE',))


EENERNO  = LocatedComponents(phys=PHY.ENER_R, type='ELNO',
    components=('TOTALE',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY',))


CEPSINF  = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


EDFVCPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPTHER_L','EPTHER_T','EPTHER_N','EPSECH','EPHYDR',
          'EPPTOT',))


EDFVCNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPTHER_L','EPTHER_T','EPTHER_N','EPSECH','EPHYDR',
          'EPPTOT',))


EERREUR  = LocatedComponents(phys=PHY.ERRE_R, type='ELEM',
    components=('ERREST','NUEST','SIGCAL','TERMRE','TERMR2',
          'TERMNO','TERMN2','TERMSA','TERMS2','TAILLE',))


EERRENO  = LocatedComponents(phys=PHY.ERRE_R, type='ELNO',
    components=('ERREST','NUEST','SIGCAL','TERMRE','TERMR2',
          'TERMNO','TERMN2','TERMSA','TERMS2','TAILLE',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY',))


EFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELGA', location='RIGI',
    components=('FX','FY',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[2]','K[2]',))


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


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


ECOPILO  = LocatedComponents(phys=PHY.PILO_R, type='ELGA', location='RIGI',
    components=('A0','A[3]','ETA',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ESOURCR  = LocatedComponents(phys=PHY.SOUR_R, type='ELGA', location='RIGI',
    components=('SOUR',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MVECZZR  = ArrayOfComponents(phys=PHY.VSIZ_R, locatedComponents=LC.DDL_NOZ1)

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=NDEPLAC)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=DDL_MECA)

MMATZZR  = ArrayOfComponents(phys=PHY.MSIZ_R, locatedComponents=LC.DDL_NOZ1)


#------------------------------------------------------------
class MEAXQU4(Element):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','FPG1=FPG1','MASS=FPG4','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
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

        OP.CALC_ESTI_ERRE(te=291,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CALC_ESTI_ERRE.PSIEF_R, ECONTPG), (SP.PSIGMA, LC.NCONTNO),
                     (OP.CALC_ESTI_ERRE.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.CALC_ESTI_ERRE.PERREUR, EERREUR), ),
        ),

        OP.CALC_G(te=96,
            para_in=((SP.PACCELE, DDL_MECA), (OP.CALC_G.PCOMPOR, CCOMPOR),
                     (SP.PCONTGR, ECONTPG), (OP.CALC_G.PCONTRR, ECONTPG),
                     (SP.PDEFOPL, EDEFONO), (SP.PDEPLAR, DDL_MECA),
                     (SP.PEPSINR, CEPSINR), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGINR, ECONTNO), (SP.PTHETAR, DDL_MECA),
                     (OP.CALC_G.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (OP.CALC_G.PVARIPR, LC.ZVARINO), (SP.PVITESS, DDL_MECA),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP(te=96,
            para_in=((SP.PACCELE, DDL_MECA), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
                     (SP.PCONTGR, ECONTPG), (OP.CALC_GTP.PCONTRR, ECONTPG),
                     (SP.PDEFOPL, EDEFONO), (SP.PDEPLAR, DDL_MECA),
                     (SP.PEPSINR, CEPSINR), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGINR, ECONTNO), (SP.PTHETAR, DDL_MECA),
                     (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (OP.CALC_GTP.PVARIPR, LC.ZVARINO), (SP.PVITESS, DDL_MECA),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP_F(te=96,
            para_in=((SP.PACCELE, DDL_MECA), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
                     (SP.PCONTGR, ECONTPG), (OP.CALC_GTP_F.PCONTRR, ECONTPG),
                     (SP.PDEFOPL, EDEFONO), (SP.PDEPLAR, DDL_MECA),
                     (SP.PEPSINF, CEPSINF), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGINR, ECONTNO), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECA), (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.CALC_GTP_F.PVARIPR, LC.ZVARINO),
                     (SP.PVITESS, DDL_MECA), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=96,
            para_in=((SP.PACCELE, DDL_MECA), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
                     (SP.PCONTGR, ECONTPG), (OP.CALC_G_F.PCONTRR, ECONTPG),
                     (SP.PDEFOPL, EDEFONO), (SP.PDEPLAR, DDL_MECA),
                     (SP.PEPSINF, CEPSINF), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGINR, ECONTNO), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECA), (OP.CALC_G_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.CALC_G_F.PVARIPR, LC.ZVARINO),
                     (SP.PVITESS, DDL_MECA), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_K_G(te=299,
            para_in=((OP.CALC_K_G.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFISSR, LC.CFISSR), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGINR, ECONTNO),
                     (SP.PTHETAR, DDL_MECA), (OP.CALC_K_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=299,
            para_in=((OP.CALC_K_G_F.PCOMPOR, CCOMPOR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFFVOLU, CFORCEF), (SP.PFISSR, LC.CFISSR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGINR, ECONTNO),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECA),
                     (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_NOEU_BORD(te=290,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_EPSA_R(te=421,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSA_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_EPSI_F(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINF, CEPSINF),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_EPSI_R(te=284,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PEPSINR, CEPSINR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FF2D2D(te=94,
            para_in=((SP.PFF2D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D2D(te=93,
            para_in=((SP.PFR2D2D, NFORCER), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_HYDR_R(te=492,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_META_Z(te=353,
            para_in=((OP.CHAR_MECA_META_Z.PCOMPOR, CCOMPOR), (OP.CHAR_MECA_META_Z.PCONTMR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.CHAR_MECA_META_Z.PVARCPR, LC.ZVARCPG),
                     (OP.CHAR_MECA_META_Z.PVARIPR, ZVARIPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=85,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=84,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PROTATR, LC.CROTATR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_SECH_R(te=492,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=83,
            para_in=((SP.PCAMASS, CCAMASS), (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=479,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DERA_ELGA(te=489,
            para_in=((OP.DERA_ELGA.PCOMPOR, CCOMPOR), (OP.DERA_ELGA.PCONTMR, ECONTPG),
                     (OP.DERA_ELGA.PCONTPR, ECONTPG), (SP.PDERAMG, LC.EDERAPG),
                     (SP.PMATERC, LC.CMATERC), (OP.DERA_ELGA.PVARCPR, LC.ZVARCPG),
                     (OP.DERA_ELGA.PVARIMR, ZVARIPG), (OP.DERA_ELGA.PVARIPR, ZVARIPG),
                     ),
            para_out=((OP.DERA_ELGA.PDERAPG, LC.EDERAPG), ),
        ),

        OP.DERA_ELNO(te=4,
            para_in=((OP.DERA_ELNO.PDERAPG, LC.EDERAPG), ),
            para_out=((SP.PDERANO, LC.EDERANO), ),
        ),

        OP.ECIN_ELEM(te=82,
            para_in=((SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.POMEGA2, LC.COMEG2R),
                     (OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVITESR, DDL_MECA),
                     ),
            para_out=((SP.PENERCR, EENERR), ),
        ),

        OP.ENDO_ELGA(te=512,
            para_in=((OP.ENDO_ELGA.PCOMPOR, CCOMPOR), (SP.PCONTGP, ECONTPG),
                     (SP.PMATERC, LC.CMATERC), (SP.PTRIAGM, LC.ETRIAPG),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.ENDO_ELGA.PVARCPR, LC.ZVARCPG),
                     (OP.ENDO_ELGA.PVARIMR, ZVARIPG), (OP.ENDO_ELGA.PVARIPR, ZVARIPG),
                     ),
            para_out=((OP.ENDO_ELGA.PTRIAPG, LC.ETRIAPG), ),
        ),

        OP.ENDO_ELNO(te=4,
            para_in=((OP.ENDO_ELNO.PTRIAPG, LC.ETRIAPG), ),
            para_out=((SP.PTRIANO, LC.ETRIANO), ),
        ),

        OP.ENEL_ELEM(te=490,
            para_in=((OP.ENEL_ELEM.PCOMPOR, CCOMPOR), (OP.ENEL_ELEM.PCONTPR, ECONTPG),
                     (SP.PDEPLR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.ENTR_ELEM(te=490,
            para_in=((OP.ENTR_ELEM.PCOMPOR, CCOMPOR), (OP.ENTR_ELEM.PCONTPR, ECONTPG),
                     (SP.PDEPLR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.ENTR_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENTR_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENTRD1, EENERR), ),
        ),

        OP.ENEL_ELGA(te=575,
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

        OP.ENER_TOTALE(te=490,
            para_in=((OP.ENER_TOTALE.PCOMPOR, CCOMPOR), (OP.ENER_TOTALE.PCONTMR, ECONTPG),
                     (OP.ENER_TOTALE.PCONTPR, ECONTPG), (SP.PDEPLM, DDL_MECA),
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

        OP.EPFD_ELGA(te=528,
            para_in=((OP.EPFD_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.EPFD_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPFD_ELNO(te=4,
            para_in=((OP.EPFD_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPFP_ELGA(te=528,
            para_in=((OP.EPFP_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPFP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.EPFP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPFP_ELNO(te=4,
            para_in=((OP.EPFP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPME_ELGA(te=87,
            para_in=((SP.PCAMASS, CCAMASS), (OP.EPME_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPME_ELNO(te=4,
            para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPMG_ELGA(te=87,
            para_in=((SP.PCAMASS, CCAMASS), (OP.EPMG_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPMG_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
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

        OP.EPOT_ELEM(te=286,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.EPSG_ELGA(te=87,
            para_in=((SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
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
            para_in=((SP.PCAMASS, CCAMASS), (OP.EPSI_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
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

        OP.EPSP_ELGA(te=334,
            para_in=((OP.EPSP_ELGA.PCOMPOR, CCOMPOR), (OP.EPSP_ELGA.PCONTRR, ECONTPG),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
        ),

        OP.EPSP_ELNO(te=4,
            para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
            para_out=((SP.PDEFONO, EDEFONO), ),
        ),

        OP.EPVC_ELGA(te=529,
            para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
        ),

        OP.EPVC_ELNO(te=4,
            para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
            para_out=((SP.PDEFONO, EDFVCNO), ),
        ),

        OP.ERME_ELEM(te=377,
            para_in=((SP.PCONTNO, ECONTNO), (SP.PFFVOLU, CFORCEF),
                     (SP.PFORCE, LC.CREFERI), (SP.PFRVOLU, EFORCER),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (SP.PPRESS, LC.CREFERI),
                     (SP.PROTATR, LC.CROTATR), (SP.PTEMPSR, CTEMPSR),
                     (OP.ERME_ELEM.PVOISIN, LC.EVOISIN), ),
            para_out=((OP.ERME_ELEM.PERREUR, EERREUR), ),
        ),

        OP.ERME_ELNO(te=379,
            para_in=((OP.ERME_ELNO.PERREUR, EERREUR), ),
            para_out=((SP.PERRENO, EERRENO), ),
        ),

        OP.ERRE_QIZZ(te=292,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PSIEFD_R, ECONTPG), (SP.PSIEFP_R, ECONTPG),
                     (SP.PSIGMAD, LC.NCONTNO), (SP.PSIGMAP, LC.NCONTNO),
                     (OP.ERRE_QIZZ.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.ERRE_QIZZ.PERREUR, EERREUR), ),
        ),

        OP.ETOT_ELEM(te=575,
            para_in=((OP.ETOT_ELEM.PCONTMR, ECONTPG), (OP.ETOT_ELEM.PCONTPR, ECONTPG),
                     (SP.PDEPLM, DDL_MECA), (SP.PDEPLR, DDL_MECA),
                     (OP.ETOT_ELEM.PENERDM, EENERR), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((OP.ETOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.ETOT_ELGA(te=575,
            para_in=((OP.ETOT_ELGA.PCONTMR, ECONTPG), (OP.ETOT_ELGA.PCONTPR, ECONTPG),
                     (SP.PDEPLM, DDL_MECA), (SP.PDEPLR, DDL_MECA),
                     (OP.ETOT_ELGA.PENERDM, EENERPG), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((OP.ETOT_ELGA.PENERDR, EENERPG), ),
        ),

        OP.ETOT_ELNO(te=4,
            para_in=((OP.ETOT_ELNO.PENERPG, EENERPG), ),
            para_out=((SP.PENERNO, EENERNO), ),
        ),

        OP.FORC_NODA(te=8,
            para_in=((OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.FULL_MECA_ELAS(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
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

        OP.INDIC_ENER(te=490,
            para_in=((OP.INDIC_ENER.PCOMPOR, CCOMPOR), (OP.INDIC_ENER.PCONTPR, ECONTPG),
                     (SP.PDEPLR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.INDIC_ENER.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.INDIC_ENER.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), (SP.PENERD2, EENERR),
                     ),
        ),

        OP.INDIC_SEUIL(te=490,
            para_in=((OP.INDIC_SEUIL.PCOMPOR, CCOMPOR), (OP.INDIC_SEUIL.PCONTPR, ECONTPG),
                     (SP.PDEPLR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.INDIC_SEUIL.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.INDIC_SEUIL.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), (SP.PENERD2, EENERR),
                     ),
        ),

        OP.INDL_ELGA(te=30,
            para_in=((OP.INDL_ELGA.PCOMPOR, CCOMPOR), (OP.INDL_ELGA.PCONTPR, ECONTPG),
                     (SP.PMATERC, LC.CMATERC), (OP.INDL_ELGA.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PINDLOC, LC.EGINDLO), ),
        ),

        OP.INIT_MAIL_VOIS(te=99,
            para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), (OP.INIT_VARC.PVARCNO, LC.ZVARCNO),),
        ),

        OP.MASS_INER(te=285,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=82,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.MASS_ZZ1(te=293,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PMATZZR, MMATZZR), ),
        ),

        OP.M_GAMMA(te=82,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.M_GAMMA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NORME_FROB(te=563,
            para_in=((SP.PCALCI, LC.EMNEUT_I), (SP.PCHAMPG, EGNEUT_R),
                     (SP.PCOEFR, EMNEUT_R), (OP.NORME_FROB.PCOORPG, EGGEOP_R),
                     ),
            para_out=((SP.PNORME, LC.ENORME), ),
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

        OP.PAS_COURANT(te=404,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.PAS_COURANT.PVARCPR, LC.ZVARCPG),),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.PILO_PRED_DEFO(te=543,
            para_in=((OP.PILO_PRED_DEFO.PCOMPOR, CCOMPOR), (OP.PILO_PRED_DEFO.PCONTMR, ECONTPG),
                     (SP.PDDEPLR, DDL_MECA), (SP.PDEPL0R, DDL_MECA),
                     (SP.PDEPL1R, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTYPEPI, LC.CTYPEPI), (OP.PILO_PRED_DEFO.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.PILO_PRED_DEFO.PCOPILO, ECOPILO), ),
        ),

        OP.PILO_PRED_ELAS(te=543,
            para_in=((SP.PBORNPI, LC.CBORNPI), (SP.PCDTAU, LC.CCDTAU),
                     (OP.PILO_PRED_ELAS.PCOMPOR, CCOMPOR), (OP.PILO_PRED_ELAS.PCONTMR, ECONTPG),
                     (SP.PDDEPLR, DDL_MECA), (SP.PDEPL0R, DDL_MECA),
                     (SP.PDEPL1R, DDL_MECA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTYPEPI, LC.CTYPEPI), (OP.PILO_PRED_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((OP.PILO_PRED_ELAS.PCOPILO, ECOPILO), ),
        ),

        OP.QIRE_ELEM(te=378,
            para_in=((SP.PCONSTR, LC.CCONSTR), (SP.PCONTNOD, ECONTNO),
                     (SP.PCONTNOP, ECONTNO), (SP.PFFVOLUD, CFORCEF),
                     (SP.PFFVOLUP, CFORCEF), (SP.PFORCED, LC.CREFERI),
                     (SP.PFORCEP, LC.CREFERI), (SP.PFRVOLUD, EFORCER),
                     (SP.PFRVOLUP, EFORCER), (SP.PGEOMER, NGEOMER),
                     (SP.PPESANRD, LC.CPESANR), (SP.PPESANRP, LC.CPESANR),
                     (SP.PPRESSD, LC.CREFERI), (SP.PPRESSP, LC.CREFERI),
                     (SP.PROTATRD, LC.CROTATR), (SP.PROTATRP, LC.CROTATR),
                     (SP.PTEMPSR, CTEMPSR), (OP.QIRE_ELEM.PVOISIN, LC.EVOISIN),
                     ),
            para_out=((OP.QIRE_ELEM.PERREUR, EERREUR), ),
        ),

        OP.QIRE_ELNO(te=379,
            para_in=((OP.QIRE_ELNO.PERREUR, EERREUR), ),
            para_out=((SP.PERRENO, EERRENO), ),
        ),

        OP.RAPH_MECA(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=8,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PREFCO, EREFCO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.REPERE_LOCAL(te=133,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PREPLO1, LC.CGEOM2D), (SP.PREPLO2, LC.CGEOM2D),
                     ),
        ),

        OP.REST_ECRO(te=116,
            para_in=((SP.PCARCRI, CCARCRI), (OP.REST_ECRO.PCOMPOR, CCOMPOR),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.REST_ECRO.PVARCPR, LC.ZVARCPG),
                     (OP.REST_ECRO.PVARIMR, ZVARIPG), ),
            para_out=((OP.REST_ECRO.PVARIPR, ZVARIPG), ),
        ),

        OP.RICE_TRACEY(te=332,
            para_in=((OP.RICE_TRACEY.PCOMPOR, CCOMPOR), (OP.RICE_TRACEY.PCONTPR, ECONTPG),
                     (SP.PGEOMER, NGEOMER), (SP.PSDRMR, LC.EGNEUT1R),
                     (SP.PSOUSOP, LC.CSOUSOP), (OP.RICE_TRACEY.PVARIMR, ZVARIPG),
                     (OP.RICE_TRACEY.PVARIPR, ZVARIPG), ),
            para_out=((SP.PRICTRA, LC.ERICTRA), (SP.PSDRPR, LC.EGNEUT1R),
                     ),
        ),

        OP.RIGI_MECA(te=81,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_ELAS(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
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

        OP.RIGI_MECA_GE(te=92,
            para_in=((OP.RIGI_MECA_GE.PCONTRR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=50,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_IMPLEX(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
                     (OP.RIGI_MECA_IMPLEX.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_IMPLEX.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_IMPLEX.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_IMPLEX.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCONTXR, ECONTPG), (SP.PMATUNS, MMATUNS),
                     (SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_TANG(te=100,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI), (SP.PMULCOM, LC.CMLCOMP),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.SECM_ZZ1(te=294,
            para_in=((SP.PGEOMER, NGEOMER), (OP.SECM_ZZ1.PSIEF_R, ECONTPG),
                     ),
            para_out=((SP.PVECTR1, MVECZZR), (SP.PVECTR2, MVECZZR),
                     (SP.PVECTR3, MVECZZR), (SP.PVECTR4, MVECZZR),
                     ),
        ),

        OP.SIEF_ELGA(te=22,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
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
            para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA),
                     (OP.TOU_INI_ELGA.PEPSI_R, EDEFOPG), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
                     (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R), (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG),
                     (OP.TOU_INI_ELGA.PSOUR_R, ESOURCR), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PDOMMAG, LC.EDOMGNO), (OP.TOU_INI_ELNO.PEPSI_R, EDEFONO),
                     (OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
                     ),
        ),

        OP.VARC_ELGA(te=530,
            para_in=((OP.VARC_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVARC_R, LC.EVARC_R), ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),

        OP.VERI_JACOBIEN(te=328,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((SP.PCODRET, LC.ECODRET), ),
        ),

        OP.WEIBULL(te=331,
            para_in=((OP.WEIBULL.PCOMPOR, CCOMPOR), (SP.PCONTRG, ECONTPG),
                     (OP.WEIBULL.PDEFORR, EDEFOPG), (OP.WEIBULL.PDOMMAG, LC.EDOMGGA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PSOUSOP, LC.CSOUSOP), (OP.WEIBULL.PVARCPR, LC.ZVARCPG),
                     (SP.PVARIPG, ZVARIPG), ),
            para_out=((SP.PSIGISG, LC.EDOMGGA), (SP.PWEIBUL, LC.EWEIBUL),
                     ),
        ),

    )


#------------------------------------------------------------
class MDAXQU4(MEAXQU4):
    """QUAD4 AXIS_ELDI element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','FPG1=FPG1','MASS=FPG4','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
        )

    calculs = (
        OP.ENTR_ELEM(te=-1),
    )


#------------------------------------------------------------
class MEAXQU8(MEAXQU4):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','FPG1=FPG1','MASS=FPG9','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MEAXQU9(MEAXQU4):
    """Please document this element"""
    meshType = MT.QUAD9
    elrefe =(
            ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9','FPG1=FPG1','MASS=FPG9','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )


#------------------------------------------------------------
class MEAXTR3(MEAXQU4):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG1','FPG1=FPG1','MASS=FPG3','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )


#------------------------------------------------------------
class MEAXTR6(MEAXQU4):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','FPG1=FPG1','MASS=FPG6','NOEU_S=NOEU_S','NOEU=NOEU',), mater=('RIGI','NOEU','FPG1',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
        )
