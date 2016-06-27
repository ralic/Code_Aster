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


CABSCUR = LocatedComponents(phys=PHY.ABSC_R, type='ELEM',
                            components=('ABSC[2]',))


CCAGEPO = LocatedComponents(phys=PHY.CAGEPO, type='ELEM',
                            components=('R1', 'EP1',))


CCAGNPO = LocatedComponents(phys=PHY.CAGNPO, type='ELEM',
                            components=('A1', 'IY1', 'IZ1', 'AY1', 'AZ1',
                                        'EY1', 'EZ1', 'JX1', 'RY1', 'RZ1',
                                        'RT1', 'JG1', 'A2', 'IY2', 'IZ2',
                                        'AY2', 'AZ2', 'EY2', 'EZ2', 'JX2',
                                        'RY2', 'RZ2', 'RT2', 'JG2', 'TVAR',))


CCAORIE = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
                            components=('ALPHA', 'BETA', 'GAMMA',))


CCARCRI = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
                            components=(
                                'ITECREL', 'MACOMP', 'RESCREL', 'THETA', 'ITEDEC',
                            'INTLOC', 'PERTURB', 'TOLDEBO', 'ITEDEBO', 'TSSEUIL',
                            'TSAMPL', 'TSRETOUR', 'POSTITER', 'LC_EXT[3]', 'MODECALC',
                            'ALPHA', 'LC_EXT2[2]',))


CCOMPOR = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
                            components=('RELCOM', 'NBVARI', 'DEFORM', 'INCELA', 'C_PLAN',))


NDEPLAC = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
                            components=('DX', 'DY', 'DZ', 'DRX', 'DRY',
                                        'DRZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
                             components=('DX', 'DY', 'DZ', 'DRX', 'DRY',
                                         'DRZ',))


NVITER = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
                           components=('DX', 'DY', 'DZ',))


EENERR = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
                           components=('TOTALE', 'TRAC_COM', 'TORSION', 'FLEX_Y', 'FLEX_Z',))


CEPSINR = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
                            components=('EPX', 'KY', 'KZ',))


EDEFGNO = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
                            components=('EPXX', 'GAXY', 'GAXZ', 'GAT', 'KY',
                                        'KZ',))


CFORCEF = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
                            components=('FX', 'FY', 'FZ', 'MX', 'MY',
                                        'MZ', 'REP',))


CFORCER = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
                            components=('FX', 'FY', 'FZ', 'MX', 'MY',
                                        'MZ', 'REP',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                             components=('X', 'Y', 'Z',))


NGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
                            components=('X', 'Y', 'Z',))




EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                             components=('X', 'Y', 'Z', 'W',))


CTEMPSR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
                             components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                             components=('X[30]',))


EREFCO = LocatedComponents(phys=PHY.PREC, type='ELEM',
                           components=('EFFORT', 'MOMENT',))


EEFGEGC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                            components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                        'MFZ',))


EEFGENC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                            components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                        'MFZ',))


ECONTPC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                            components=('SN', 'SVY', 'SVZ', 'SMT', 'SMFY',
                                        'SMFZ',))


EEFGEGA = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                            components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                        'MFZ',))


EEFGENO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('N', 'VY', 'VZ', 'MT', 'MFY',
                                        'MFZ',))


ESTRAUX = LocatedComponents(phys=PHY.STRX_R, type='ELGA', location='RIGI',
                            components=('ALPHA', 'BETA', 'GAMMA',))


ZVARIPG = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
                            components=('VARI',))


MVECTUC = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=NDEPLAC)

MVECTUR = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUC = ArrayOfComponents(
    phys=PHY.MDEP_C, locatedComponents=NDEPLAC)

MMATUUR = ArrayOfComponents(
    phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS = ArrayOfComponents(
    phys=PHY.MDNS_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MECA_POU_D_E(Element):

    """Please document this element"""
    meshType = MT.SEG2
    elrefe = (
        ElrefeLoc(MT.SE2, gauss=('RIGI=FPG3', 'NOEU=NOEU', 'FPG1=FPG1',),
                  mater=('RIGI', 'NOEU', 'FPG1',),),
    )
    calculs = (

        OP.ADD_SIGM(te=581,
                    para_in=((SP.PEPCON1, EEFGEGA), (SP.PEPCON2, EEFGEGA),
                             ),
                    para_out=((SP.PEPCON3, EEFGEGA), ),
                    ),

        OP.AMOR_MECA(te=50,
                     para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
                              (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
                              (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.CHAR_MECA_EPSI_R(te=20,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_EPSI_R.PCAORIE, CCAORIE),
                            (SP.PEPSINR, CEPSINR), (SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (
                            OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FC1D1D(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_FC1D1D.PCAORIE, CCAORIE),
                            (SP.PFC1D1D, LC.CFORCEC), (SP.PGEOMER, NGEOMER),
                            ),
                            para_out=((SP.PVECTUC, MVECTUC), ),
                            ),

        OP.CHAR_MECA_FF1D1D(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE),
                            (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                            (SP.PTEMPSR, CTEMPSR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FR1D1D(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE),
                            (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FRELEC(te=145,
                            para_in=(
                                (SP.PFRELEC, LC.CFRELEC), (
                                    SP.PGEOMER, NGEOMER),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FRLAPL(te=148,
                            para_in=(
                                (SP.PFLAPLA, LC.CFLAPLA), (
                                    SP.PGEOMER, NGEOMER),
                            (SP.PLISTMA, LC.CLISTMA), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_HYDR_R(te=312,
                            para_in=(
                            (SP.PMATERC, LC.CMATERC), (
                            OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PESA_R(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                            (SP.PPESANR, LC.CPESANR), (
                            OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_ROTA_R(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_ROTA_R.PCAORIE, CCAORIE),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                            (SP.PROTATR, LC.CROTATR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_SECH_R(te=312,
                            para_in=(
                            (SP.PMATERC, LC.CMATERC), (
                            OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_SF1D1D(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_SF1D1D.PCAORIE, CCAORIE),
                            (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                            (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_SR1D1D(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_SR1D1D.PCAORIE, CCAORIE),
                            (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                            (SP.PGEOMER, NGEOMER), (SP.PSTRXMR, ESTRAUX),
                            (SP.PVENTCX, LC.CVENTCX), (SP.PVITER, NVITER),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_TEMP_R(te=150,
                            para_in=(
                            (SP.PCAGNPO, CCAGNPO), (
                                OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                            (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (
                            SP.PVARCRR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.COOR_ELGA(te=478,
                     para_in=((SP.PGEOMER, NGEOMER), ),
                     para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
                     ),

        OP.DEGE_ELNO(te=158,
                     para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAGNPO, CCAGNPO),
                              (OP.DEGE_ELNO.PCAORIE, CCAORIE), (
                                  SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              ),
                     para_out=((SP.PDEFOGR, EDEFGNO), ),
                     ),

        OP.ECIN_ELEM(te=151,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.ECIN_ELEM.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMASDIA, LC.CMASDIA), (SP.PMATERC, LC.CMATERC),
                     (SP.POMEGA2, LC.COMEG2R), (
                         OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVITESR, DDL_MECA), ),
                     para_out=((SP.PENERCR, LC.EENECNO), ),
                     ),

        OP.EFGE_ELGA(te=546,
                     para_in=((SP.PSIEFR, EEFGEGA), ),
                     para_out=((SP.PEFGEC, EEFGEGC), (SP.PEFGER, EEFGEGA),
                               ),
                     ),

        OP.EFGE_ELNO(te=185,
                     para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAGNPO, CCAGNPO),
                              (OP.EFGE_ELNO.PCAORIE, CCAORIE), (
                                  SP.PCHDYNR, DDL_MECA),
                              (SP.PCOEFFC, LC.CCOEFC), (SP.PCOEFFR, LC.CCOEFR),
                              (OP.EFGE_ELNO.PCOMPOR, CCOMPOR), (
                              OP.EFGE_ELNO.PCONTRR, EEFGEGA),
                              (SP.PDEPLAR, DDL_MECA), (SP.PFF1D1D, CFORCEF),
                              (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (
                                  SP.PNONLIN, LC.ENONLIN),
                              (SP.PPESANR, LC.CPESANR), (
                                  SP.PSUROPT, LC.CSUROPT),
                              (SP.PTEMPSR, CTEMPSR), (
                              OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG),
                              (SP.PVARCRR, LC.ZVARCPG), ),
                     para_out=(
                         (SP.PEFFORC, EEFGENC), (
                             OP.EFGE_ELNO.PEFFORR, EEFGENO),
                     ),
                     ),

        OP.EPOT_ELEM(te=151,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.EPOT_ELEM.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (
                         OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
                     para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
                     ),

        OP.FORC_NODA(te=347,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.FORC_NODA.PCAORIE, CCAORIE),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (
                     OP.FORC_NODA.PCONTMR, EEFGEGA),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PSTRXMR, ESTRAUX), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=((SP.PVECTUR, MVECTUR), ),
                     ),

        OP.FULL_MECA(te=247,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.FULL_MECA.PCAORIE, CCAORIE),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (SP.PSTRXMR, ESTRAUX),
                     (SP.PVARCMR, LC.ZVARCPG), (
                         OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
                     ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, EEFGEGA),
                     (SP.PMATUUR, MMATUUR), (SP.PSTRXPR, ESTRAUX),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
                     ),

        OP.INIT_VARC(te=99,
                     para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
                     ),

        OP.INI_STRX(te=23,
                    para_in=((OP.INI_STRX.PCAORIE, CCAORIE), ),
                    para_out=((SP.PSTRX_R, ESTRAUX), ),
                    ),

        OP.MASS_FLUI_STRU(te=141,
                          para_in=(
                              (SP.PABSCUR, CABSCUR), (SP.PCAGEPO, CCAGEPO),
                          (SP.PCAGNPO, CCAGNPO), (
                          OP.MASS_FLUI_STRU.PCAORIE, CCAORIE),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                          (OP.MASS_FLUI_STRU.PVARCPR, LC.ZVARCPG), ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.MASS_INER(te=38,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.MASS_INER.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMASSINE, LC.EMASSINE), ),
                     ),

        OP.MASS_MECA(te=141,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.MASS_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.MASS_MECA_DIAG(te=141,
                          para_in=(
                          (SP.PCAGNPO, CCAGNPO), (
                              OP.MASS_MECA_DIAG.PCAORIE, CCAORIE),
                          (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                          (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG), ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.MASS_MECA_EXPLI(te=141,
                           para_in=(
                           (SP.PCAGNPO, CCAGNPO), (
                               OP.MASS_MECA_EXPLI.PCAORIE, CCAORIE),
                           (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                           (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG), ),
                           para_out=((SP.PMATUUR, MMATUUR), ),
                           ),

        OP.MECA_GYRO(te=259,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.MECA_GYRO.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
                     para_out=((SP.PMATUNS, MMATUNS), ),
                     ),

        OP.M_GAMMA(te=141,
                   para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGNPO, CCAGNPO),
                            (OP.M_GAMMA.PCAORIE, CCAORIE), (
                                SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (
                            OP.M_GAMMA.PVARCPR, LC.ZVARCPG),
                            ),
                   para_out=((SP.PVECTUR, MVECTUR), ),
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

        OP.RAPH_MECA(te=247,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.RAPH_MECA.PCAORIE, CCAORIE),
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
                     (OP.RAPH_MECA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (SP.PMATERC, LC.CMATERC), (SP.PSTRXMR, ESTRAUX),
                     (SP.PVARCMR, LC.ZVARCPG), (
                         OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, EEFGEGA),
                     (SP.PSTRXPR, ESTRAUX), (OP.RAPH_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
                     ),

        OP.REFE_FORC_NODA(te=347,
                          para_in=((SP.PREFCO, EREFCO), ),
                          para_out=((SP.PVECTUR, MVECTUR), ),
                          ),

        OP.REPERE_LOCAL(te=135,
                        para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
                        para_out=((SP.PREPLO1, LC.CGEOM3D), (SP.PREPLO2, LC.CGEOM3D),
                                  (SP.PREPLO3, LC.CGEOM3D), ),
                        ),

        OP.RIGI_FLUI_STRU(te=140,
                          para_in=(
                          (SP.PCAGNPO, CCAGNPO), (
                              OP.RIGI_FLUI_STRU.PCAORIE, CCAORIE),
                          (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                          (OP.RIGI_FLUI_STRU.PVARCPR, LC.ZVARCPG), ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.RIGI_GYRO(te=262,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.RIGI_GYRO.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
                     para_out=((SP.PMATUNS, MMATUNS), ),
                     ),

        OP.RIGI_MECA(te=140,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.RIGI_MECA_GE(te=143,
                        para_in=(
                        (SP.PCAGNPO, CCAGNPO), (
                            OP.RIGI_MECA_GE.PCAORIE, CCAORIE),
                        (OP.RIGI_MECA_GE.PEFFORR, EEFGEGA), (
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

        OP.RIGI_MECA_TANG(te=247,
                          para_in=(
                          (SP.PCAGNPO, CCAGNPO), (
                              OP.RIGI_MECA_TANG.PCAORIE, CCAORIE),
                          (SP.PCARCRI, CCARCRI), (
                          OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
                          (OP.RIGI_MECA_TANG.PCONTMR, EEFGEGA), (
                          SP.PDEPLMR, DDL_MECA),
                          (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                          (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                          (SP.PMATERC, LC.CMATERC), (SP.PSTRXMR, ESTRAUX),
                          (SP.PVARCMR, LC.ZVARCPG), (
                          OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                          (SP.PVARCRR, LC.ZVARCPG), (
                          OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                          ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.SIEF_ELGA(te=144,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (
                         OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
                     para_out=(
                         (SP.PCONTRC, EEFGEGC), (
                             OP.SIEF_ELGA.PCONTRR, EEFGEGA),
                     ),
                     ),

        OP.SIEF_ELNO(te=347,
                     para_in=(
                         (SP.PCAGNPO, CCAGNPO), (
                             OP.SIEF_ELNO.PCAORIE, CCAORIE),
                     (OP.SIEF_ELNO.PCOMPOR, CCOMPOR), (
                     OP.SIEF_ELNO.PCONTRR, EEFGEGA),
                     (SP.PDEPPLU, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (
                         OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=(
                     (SP.PSIEFNOC, EEFGENC), (OP.SIEF_ELNO.PSIEFNOR, EEFGENO),
                     ),
                     ),

        OP.SIPM_ELNO(te=149,
                     para_in=((SP.PCAGEPO, LC.CCAGRPO), (SP.PCAGNPO, CCAGNPO),
                              (OP.SIPM_ELNO.PCAORIE, CCAORIE), (
                                  SP.PCHDYNR, DDL_MECA),
                              (SP.PCOEFFC, LC.CCOEFC), (SP.PCOEFFR, LC.CCOEFR),
                              (SP.PDEPLAR, DDL_MECA), (SP.PFF1D1D, CFORCEF),
                              (SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (
                                  SP.PPESANR, LC.CPESANR),
                              (SP.PSUROPT, LC.CSUROPT), (SP.PTEMPSR, CTEMPSR),
                              (OP.SIPM_ELNO.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              ),
                     para_out=(
                         (SP.PSIMXRC, LC.ESIMXNC), (SP.PSIMXRR, LC.ESIMXNO),
                     ),
                     ),

        OP.SIPO_ELNO(te=149,
                     para_in=((SP.PABSCUR, CABSCUR), (SP.PCAGEPO, LC.CCAGRPO),
                              (SP.PCAGNPO, CCAGNPO), (
                                  OP.SIPO_ELNO.PCAORIE, CCAORIE),
                              (SP.PCHDYNR, DDL_MECA), (SP.PCOEFFC, LC.CCOEFC),
                              (SP.PCOEFFR, LC.CCOEFR), (SP.PDEPLAR, DDL_MECA),
                              (SP.PFF1D1D, CFORCEF), (SP.PFR1D1D, CFORCER),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (SP.PPESANR, LC.CPESANR), (
                                  SP.PSUROPT, LC.CSUROPT),
                              (SP.PTEMPSR, CTEMPSR), (
                              OP.SIPO_ELNO.PVARCPR, LC.ZVARCPG),
                              (SP.PVARCRR, LC.ZVARCPG), ),
                     para_out=((SP.PCONTPC, ECONTPC), (SP.PCONTPO, LC.ECONTPO),
                               ),
                     ),

        OP.TOU_INI_ELEM(te=99,
                        para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
                        ),

        OP.TOU_INI_ELGA(te=99,
                        para_out=(
                        (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (
                        OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                        (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (
                        OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                        (OP.TOU_INI_ELGA.PSIEF_R, EEFGEGA), (
                        OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                        ),
                        ),

        OP.TOU_INI_ELNO(te=99,
                        para_out=(
                        (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (
                        OP.TOU_INI_ELNO.PINST_R, LC.EEINST_R),
                        (OP.TOU_INI_ELNO.PNEUT_F, LC.EENEUT_F), (
                        OP.TOU_INI_ELNO.PNEUT_R, LC.EENEUT_R),
                        (OP.TOU_INI_ELNO.PSIEF_R, EEFGENO), (
                        OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
                        ),
                        ),

        OP.VARI_ELNO(te=347,
                     para_in=(
                         (OP.VARI_ELNO.PCOMPOR, CCOMPOR), (
                             SP.PVARIGR, ZVARIPG),
                     ),
                     para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
                     ),

    )
