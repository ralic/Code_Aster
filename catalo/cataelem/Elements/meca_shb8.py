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


CCAMASS = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
                            components=('C', 'ALPHA', 'BETA', 'KAPPA', 'X',
                                        'Y', 'Z',))


CCARCRI = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
                            components=(
                                'ITECREL', 'MACOMP', 'RESCREL', 'THETA', 'ITEDEC',
                            'INTLOC', 'PERTURB', 'TOLDEBO', 'ITEDEBO', 'TSSEUIL',
                            'TSAMPL', 'TSRETOUR', 'POSTITER', 'LC_EXT[3]', 'MODECALC',
                            'ALPHA', 'LC_EXT2[2]',))


CCOMPOR = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
                            components=(
                                'RELCOM', 'NBVARI', 'DEFORM', 'INCELA', 'C_PLAN',
                            'NUME_LC', 'SD_COMP', 'KIT[9]',))


NDEPLAC = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
                            components=('DX', 'DY', 'DZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
                             components=('DX', 'DY', 'DZ',))


CEPSINF = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
                            components=('EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ',
                                        'EPYZ',))


EDEFONO = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
                            components=('EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ',
                                        'EPYZ',))


CEPSINR = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
                            components=('EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ',
                                        'EPYZ',))


EFACY_R = LocatedComponents(phys=PHY.FACY_R, type='ELGA', location='RIGI',
                            components=(
                                'DTAUM1', 'VNM1X', 'VNM1Y', 'VNM1Z', 'SINMAX1',
                            'SINMOY1', 'EPNMAX1', 'EPNMOY1', 'SIGEQ1', 'NBRUP1',
                            'ENDO1', 'DTAUM2', 'VNM2X', 'VNM2Y', 'VNM2Z',
                            'SINMAX2', 'SINMOY2', 'EPNMAX2', 'EPNMOY2', 'SIGEQ2',
                            'NBRUP2', 'ENDO2',))


CFORCEF = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
                            components=('FX', 'FY', 'FZ',))


NFORCER = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
                            components=('FX', 'FY', 'FZ',))


NGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
                            components=('X', 'Y', 'Z',))




EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                             components=('X', 'Y', 'Z', 'W',))


CTEMPSR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
                             components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                             components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                             components=('X[30]',))


ECONTPC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ', 'QXX', 'QXY', 'QYX', 'QYY',
                                        'QZX', 'QZY', 'Q2XX', 'Q2XY', 'Q2YX',
                                        'Q2YY', 'Q2ZX', 'Q2ZY',))


ECONTNC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ', 'QXX', 'QXY', 'QYX', 'QYY',
                                        'QZX', 'QZY', 'Q2XX', 'Q2XY', 'Q2YX',
                                        'Q2YY', 'Q2ZX', 'Q2ZY',))


ESIGMPC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ',))


ESIGMNC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ',))


ECONTPG = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ', 'QXX', 'QXY', 'QYX', 'QYY',
                                        'QZX', 'QZY', 'Q2XX', 'Q2XY', 'Q2YX',
                                        'Q2YY', 'Q2ZX', 'Q2ZY',))


ECONTNO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ', 'QXX', 'QXY', 'QYX', 'QYY',
                                        'QZX', 'QZY', 'Q2XX', 'Q2XY', 'Q2YX',
                                        'Q2YY', 'Q2ZX', 'Q2ZY',))


ECOEQPG = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                            components=(
                                'VMIS', 'TRESCA', 'PRIN_[3]', 'VMIS_SG', 'VECT_1_X',
                            'VECT_1_Y', 'VECT_1_Z', 'VECT_2_X', 'VECT_2_Y', 'VECT_2_Z',
                            'VECT_3_X', 'VECT_3_Y', 'VECT_3_Z', 'TRSIG', 'TRIAX',))


ESIGMPG = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ',))


ESIGMNO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ',
                                        'SIYZ',))


ZVARIPG = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
                            components=('VARI',))


MVECTUR = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC = ArrayOfComponents(
    phys=PHY.MDEP_C, locatedComponents=(NDEPLAC, NDEPLAC))

MMATUUR = ArrayOfComponents(
    phys=PHY.MDEP_R, locatedComponents=(DDL_MECA, DDL_MECA))


#------------------------------------------------------------
class MECA_SHB8(Element):

    """Please document this element"""
    meshType = MT.HEXA8
    elrefe = (
        ElrefeLoc(
            MT.HE8, gauss=(
                'RIGI=SHB5', 'MASS=SHB5', 'FPG1=FPG1',), mater=('RIGI', 'FPG1',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4', 'MASS=FPG4',),),
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

        OP.CHAR_MECA_EPSI_F(te=49,
                            para_in=(
                                (SP.PCAMASS, CCAMASS), (SP.PEPSINF, CEPSINF),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                                (SP.PTEMPSR, CTEMPSR), (
                            OP.CHAR_MECA_EPSI_F.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_EPSI_R(te=49,
                            para_in=(
                                (SP.PCAMASS, CCAMASS), (SP.PEPSINR, CEPSINR),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                                (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FF3D3D(te=17,
                            para_in=(
                                (SP.PFF3D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                            (SP.PTEMPSR, CTEMPSR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FR3D3D(te=16,
                            para_in=(
                                (SP.PFR3D3D, NFORCER), (SP.PGEOMER, NGEOMER),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PESA_R(te=15,
                            para_in=(
                                (SP.PGEOMER, NGEOMER), (
                                    SP.PMATERC, LC.CMATERC),
                            (SP.PPESANR, LC.CPESANR), (
                            OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.COOR_ELGA(te=488,
                     para_in=((SP.PGEOMER, NGEOMER), ),
                     para_out=((OP.COOR_ELGA.PCOORPG, EGGEOM_R), ),
                     ),

        OP.FORC_NODA(te=484,
                     para_in=(
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (
                     OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (
                     OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=((SP.PVECTUR, MVECTUR), ),
                     ),

        OP.FULL_MECA(te=477,
                     para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                              (OP.FULL_MECA.PCOMPOR, CCOMPOR), (
                              OP.FULL_MECA.PCONTMR, ECONTPG),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                              (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                              (SP.PVARCMR, LC.ZVARCPG), (
                              OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                              (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                              (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
                     ),

        OP.INIT_MAIL_VOIS(te=99,
                          para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
                          ),

        OP.INIT_VARC(te=99,
                     para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
                     ),

        OP.MASS_INER(te=65,
                     para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMASSINE, LC.EMASSINE), ),
                     ),

        OP.MASS_MECA(te=12,
                     para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.MASS_MECA_DIAG(te=12,
                          para_in=(
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                          (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG), ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.MASS_MECA_EXPLI(te=12,
                           para_in=(
                               (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                           (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG), ),
                           para_out=((SP.PMATUUR, MMATUUR), ),
                           ),

        OP.M_GAMMA(te=12,
                   para_in=((SP.PACCELR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (
                            OP.M_GAMMA.PVARCPR, LC.ZVARCPG),
                            ),
                   para_out=((SP.PVECTUR, MVECTUR), ),
                   ),

        OP.NORME_L2(te=563,
                    para_in=((SP.PCALCI, LC.EMNEUT_I), (SP.PCHAMPG, EGNEUT_R),
                             (SP.PCOEFR, EMNEUT_R), (
                                 OP.NORME_L2.PCOORPG, EGGEOM_R),
                             ),
                    para_out=((SP.PNORME, LC.ENORME), ),
                    ),

        OP.NSPG_NBVA(te=496,
                     para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
                     para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
                     ),

        OP.PAS_COURANT(te=404,
                       para_in=(
                           (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                       ),
                       para_out=((SP.PCOURAN, LC.ECOURAN), ),
                       ),

        OP.RAPH_MECA(te=477,
                     para_in=((SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                              (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (
                              OP.RAPH_MECA.PCONTMR, ECONTPG),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                              (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                              (SP.PVARCMR, LC.ZVARCPG), (
                              OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                              (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                              (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
                     ),

        OP.REPERE_LOCAL(te=133,
                        para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                                 ),
                        para_out=((SP.PREPLO1, LC.CGEOM3D), (SP.PREPLO2, LC.CGEOM3D),
                                  (SP.PREPLO3, LC.CGEOM3D), ),
                        ),

        OP.RIGI_MECA(te=473,
                     para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (
                              OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                              ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.RIGI_MECA_GE(te=474,
                        para_in=(
                        (OP.RIGI_MECA_GE.PCONTRR, ECONTPG), (
                            SP.PGEOMER, NGEOMER),
                        ),
                        para_out=((SP.PMATUUR, MMATUUR), ),
                        ),

        OP.RIGI_MECA_HYST(te=50,
                          para_in=(
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                          (SP.PRIGIEL, MMATUUR), (
                          OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
                          ),
                          para_out=((SP.PMATUUC, MMATUUC), ),
                          ),

        OP.RIGI_MECA_TANG(te=477,
                          para_in=(
                              (SP.PCAMASS, CCAMASS), (SP.PCARCRI, CCARCRI),
                          (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (
                          OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                          (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                          (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                          (SP.PVARCMR, LC.ZVARCPG), (
                          OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                          (SP.PVARCRR, LC.ZVARCPG), (
                          OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                          ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.SIEF_ELGA(te=485,
                     para_in=((SP.PCAMASS, CCAMASS), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              ),
                     para_out=(
                         (SP.PCONTRC, ECONTPC), (
                             OP.SIEF_ELGA.PCONTRR, ECONTPG),
                     ),
                     ),

        OP.SIEF_ELNO(te=4,
                     para_in=(
                     (OP.SIEF_ELNO.PCONTRR, ECONTPG), (
                     OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=(
                     (SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
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
                     para_out=(
                     (SP.PSIEFNOC, ESIGMNC), (OP.SIGM_ELNO.PSIEFNOR, ESIGMNO),
                     ),
                     ),

        OP.TOU_INI_ELEM(te=99,
                        para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
                        ),

        OP.TOU_INI_ELGA(te=99,
                        para_out=(
                        (OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA), (
                            SP.PFACY_R, EFACY_R),
                        (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (
                        OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                        (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (
                        OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                        (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (
                        OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                        ),
                        ),

        OP.TOU_INI_ELNO(te=99,
                        para_out=(
                        (OP.TOU_INI_ELNO.PDOMMAG, LC.EDOMGNO), (
                        OP.TOU_INI_ELNO.PEPSI_R, EDEFONO),
                        (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (
                        OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
                        (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (
                        OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                        (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), (
                        OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
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
class MECA_SHB6(MECA_SHB8):

    """Please document this element"""
    meshType = MT.PENTA6
    elrefe = (
        ElrefeLoc(
            MT.PE6, gauss=(
                'RIGI=SHB6', 'MASS=SHB6', 'FPG1=FPG1',), mater=('RIGI', 'FPG1',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3', 'MASS=FPG3',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4', 'MASS=FPG4',),),
    )


#------------------------------------------------------------
class MECA_SHB15(MECA_SHB8):

    """Please document this element"""
    meshType = MT.PENTA15
    elrefe = (
        ElrefeLoc(
            MT.P15, gauss=(
                'RIGI=SHB15', 'MASS=SHB15', 'FPG1=FPG1',), mater=('RIGI', 'FPG1',),),
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9', 'MASS=FPG9',),),
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6', 'MASS=FPG6',),),
    )


#------------------------------------------------------------
class MECA_SHB20(MECA_SHB8):

    """Please document this element"""
    meshType = MT.HEXA20
    elrefe = (
        ElrefeLoc(
            MT.H20, gauss=(
                'RIGI=SHB20', 'MASS=SHB20', 'FPG1=FPG1',), mater=('RIGI', 'FPG1',),),
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9', 'MASS=FPG9',),),
    )
