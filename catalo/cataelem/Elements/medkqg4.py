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


CCACOQU = LocatedComponents(phys=PHY.CACOQU, type='ELEM',
                            components=(
                                'EP', 'ALPHA', 'BETA', 'CTOR', 'EXCENT',
                            'INERTIE',))


CCAORIE = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
                            components=(
                                'ALPHA', 'BETA', 'REP', 'AXE_X', 'AXE_Y',
                            'AXE_Z', 'O_X', 'O_Y', 'O_Z',))


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
                            components=('DX', 'DY', 'DZ', 'DRX', 'DRY',
                                        'DRZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
                             components=('DX', 'DY', 'DZ', 'DRX', 'DRY',
                                         'DRZ',))


EDISSR = LocatedComponents(phys=PHY.DISS_R, type='ELEM',
                           components=('ENDO',))


EDISSPG = LocatedComponents(phys=PHY.DISS_R, type='ELGA', location='RIGI',
                            components=('ENDO',))


EDISSNO = LocatedComponents(phys=PHY.DISS_R, type='ELNO',
                            components=('ENDO',))


EENERR = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
                           components=('TOTALE', 'MEMBRANE', 'FLEXION', 'CISAILLE', 'COUPL_MF',))


EENERPG = LocatedComponents(phys=PHY.ENER_R, type='ELGA', location='RIGI',
                            components=('TOTALE', 'MEMBRANE', 'FLEXION', 'CISAILLE', 'COUPL_MF',))


EENERNO = LocatedComponents(phys=PHY.ENER_R, type='ELNO',
                            components=('TOTALE', 'MEMBRANE', 'FLEXION', 'CISAILLE', 'COUPL_MF',))


EDEFGPC = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
                            components=('EXX', 'EYY', 'EXY', 'KXX', 'KYY',
                                        'KXY', 'GAX', 'GAY',))


CEPSINR = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
                            components=('EXX', 'EYY', 'EXY', 'KXX', 'KYY',
                                        'KXY',))


EDEFGNO = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
                            components=('EXX', 'EYY', 'EXY', 'KXX', 'KYY',
                                        'KXY', 'GAX', 'GAY',))


EDEFGPG = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
                            components=('EXX', 'EYY', 'EXY', 'KXX', 'KYY',
                                        'KXY', 'GAX', 'GAY',))


CFORCEF = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
                            components=('FX', 'FY', 'FZ', 'MX', 'MY',
                                        'MZ', 'REP', 'PLAN',))


EFORCNO = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
                            components=('FX', 'FY', 'FZ', 'MX', 'MY',
                                        'MZ', 'REP', 'PLAN',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                             components=('X', 'Y', 'Z', 'W',))




NGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
                            components=('X', 'Y', 'Z',))


CTEMPSR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
                             components=('X[30]',))


ECASECT = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
                            components=('X[16]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
                             components=('X[30]',))


EREFCO = LocatedComponents(phys=PHY.PREC, type='ELEM',
                           components=('EFFORT', 'MOMENT',))


EPRESNF = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
                            components=('PRES',))


EPRESNO = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
                            components=('PRES',))


EEFGEGC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                            components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                        'MXY', 'QX', 'QY',))


EEFGENC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                            components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                        'MXY', 'QX', 'QY',))


EEFGEPGC = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
                             components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                         'MXY', 'QX', 'QY',))


EEFGENOC = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
                             components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                         'MXY', 'QX', 'QY',))


EEFGEGA = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                            components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                        'MXY', 'QX', 'QY',))


EEFGENO = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                            components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                        'MXY', 'QX', 'QY',))


EEFGEPGR = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
                             components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                         'MXY', 'QX', 'QY',))


EEFGENOR = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
                             components=('NXX', 'NYY', 'NXY', 'MXX', 'MYY',
                                         'MXY', 'QX', 'QY',))


ZVARIPG = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
                            components=('VARI',))


MVECTUR = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUC = ArrayOfComponents(
    phys=PHY.MDEP_C, locatedComponents=NDEPLAC)

MMATUUR = ArrayOfComponents(
    phys=PHY.MDEP_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MEDKQG4(Element):

    """Please document this element"""
    meshType = MT.QUAD4
    elrefe = (
        ElrefeLoc(
            MT.QU4, gauss=(
                'RIGI=FPG4', 'MASS=FPG9', 'NOEU=NOEU', 'FPG1=FPG1',),
            mater=('RIGI', 'NOEU', 'FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
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

        OP.CARA_SECT_POUT3(te=513,
                           para_in=((SP.PGEOMER, NGEOMER), ),
                           para_out=((SP.PCASECT, ECASECT), ),
                           ),

        OP.CARA_SECT_POUT4(te=513,
                           para_in=(
                               (SP.PGEOMER, NGEOMER), (SP.PORIGIN, LC.CGEOM3D),
                           ),
                           para_out=(
                               (SP.PVECTU1, MVECTUR), (SP.PVECTU2, MVECTUR),
                           ),
                           ),

        OP.CHAR_MECA_EPSI_R(te=35,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PEPSINR, CEPSINR),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                                (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FFCO3D(te=32,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PFFCO3D, CFORCEF),
                            (SP.PGEOMER, NGEOMER), (SP.PTEMPSR, CTEMPSR),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FRCO3D(te=32,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PFRCO3D, EFORCNO),
                            (SP.PGEOMER, NGEOMER), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_HYDR_R(te=312,
                            para_in=(
                            (SP.PMATERC, LC.CMATERC), (
                            OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PESA_R(te=32,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                                (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PRES_F(te=32,
                            para_in=(
                                (SP.PGEOMER, NGEOMER), (SP.PPRESSF, EPRESNF),
                            (SP.PTEMPSR, CTEMPSR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PRES_R(te=32,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                            (SP.PPRESSR, EPRESNO), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_SECH_R(te=312,
                            para_in=(
                            (SP.PMATERC, LC.CMATERC), (
                            OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_TEMP_R(te=423,
                            para_in=(
                                (SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                            (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                                (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (
                            SP.PVARCRR, LC.ZVARCPG),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.COOR_ELGA(te=488,
                     para_in=((SP.PGEOMER, NGEOMER), ),
                     para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
                     ),

        OP.DEGE_ELGA(te=33,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.DEGE_ELGA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((OP.DEGE_ELGA.PDEFOPG, EDEFGPG), ),
                     ),

        OP.DEGE_ELNO(te=33,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.DEGE_ELNO.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PDEFOGR, EDEFGNO), ),
                     ),

        OP.DISS_ELEM(te=413,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.DISS_ELEM.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.DISS_ELEM.PVARIPR, ZVARIPG), ),
                     para_out=((SP.PDISSD1, EDISSR), ),
                     ),

        OP.DISS_ELGA(te=413,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.DISS_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARIGR, ZVARIPG), ),
                     para_out=((OP.DISS_ELGA.PDISSPG, EDISSPG), ),
                     ),

        OP.DISS_ELNO(te=4,
                     para_in=((OP.DISS_ELNO.PDISSPG, EDISSPG), ),
                     para_out=((SP.PDISSNO, EDISSNO), ),
                     ),

        OP.ECIN_ELEM(te=444,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (SP.POMEGA2, LC.COMEG2R), (
                              OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG),
                              (SP.PVITESR, DDL_MECA), ),
                     para_out=((SP.PENERCR, EENERR), ),
                     ),

        OP.EFGE_ELGA(te=546,
                     para_in=((SP.PSIEFR, EEFGEGA), ),
                     para_out=((SP.PEFGEC, EEFGEGC), (SP.PEFGER, EEFGEGA),
                               ),
                     ),

        OP.EFGE_ELNO(te=185,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.EFGE_ELNO.PCOMPOR, CCOMPOR),
                     (OP.EFGE_ELNO.PCONTRR, EEFGEGA), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PNONLIN, LC.ENONLIN), (SP.PTEMPSR, CTEMPSR),
                     (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG), (
                         SP.PVARCRR, LC.ZVARCPG),
                     ),
                     para_out=(
                         (SP.PEFFORC, EEFGENC), (
                             OP.EFGE_ELNO.PEFFORR, EEFGENO),
                     ),
                     ),

        OP.EFGE_EXCENT(te=452,
                       para_in=((SP.PCACOQU, CCACOQU), (SP.PEFFOGC, EEFGEPGC),
                                (SP.PEFFOGR, EEFGEPGR), (SP.PEFFONC, EEFGENOC),
                                (SP.PEFFONR, EEFGENOR), ),
                       para_out=(
                           (SP.PEFFOEGC, EEFGEPGC), (SP.PEFFOEGR, EEFGEPGR),
                       (SP.PEFFOENC, EEFGENOC), (SP.PEFFOENR, EEFGENOR),
                       ),
                       ),

        OP.ENEL_ELEM(te=412,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.ENEL_ELEM.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELEM.PCONTPR, EEFGEGA), (SP.PDEPLR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG), (
                         SP.PVARCRR, LC.ZVARCPG),
                     (OP.ENEL_ELEM.PVARIPR, ZVARIPG), ),
                     para_out=((SP.PENERD1, EENERR), ),
                     ),

        OP.ENEL_ELGA(te=412,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.ENEL_ELGA.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELGA.PCONTRR, EEFGEGA), (SP.PDEPLAR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (SP.PTEMPSR, CTEMPSR), (OP.ENEL_ELGA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARIGR, ZVARIPG), ),
                     para_out=((OP.ENEL_ELGA.PENERDR, EENERPG), ),
                     ),

        OP.ENEL_ELNO(te=4,
                     para_in=((OP.ENEL_ELNO.PENERPG, EENERPG), ),
                     para_out=((SP.PENERNO, EENERNO), ),
                     ),

        OP.EPOT_ELEM(te=444,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              ),
                     para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
                     ),

        OP.FERRAILLAGE(te=146,
                       para_in=(
                           (SP.PCACOQU, CCACOQU), (
                               OP.FERRAILLAGE.PEFFORR, EEFGENO),
                       (SP.PFERRA1, LC.CFER1_R), ),
                       para_out=((SP.PFERRA2, LC.CFER2_R), ),
                       ),

        OP.FORC_NODA(te=446,
                     para_in=(
                         (SP.PCACOQU, CCACOQU), (
                             OP.FORC_NODA.PCOMPOR, CCOMPOR),
                     (OP.FORC_NODA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (
                         OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=((SP.PVECTUR, MVECTUR), ),
                     ),

        OP.FULL_MECA(te=409,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                              (OP.FULL_MECA.PCOMPOR, CCOMPOR), (
                              OP.FULL_MECA.PCONTMR, EEFGEGA),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                              (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                              (SP.PMATERC, LC.CMATERC), (
                                  SP.PVARCMR, LC.ZVARCPG),
                              (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              (SP.PVARIMP, ZVARIPG), (
                              OP.FULL_MECA.PVARIMR, ZVARIPG),
                              ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, EEFGEGA),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
                     ),

        OP.FULL_MECA_ELAS(te=409,
                          para_in=(
                          (SP.PCARCRI, CCARCRI), (
                              OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
                          (OP.FULL_MECA_ELAS.PCONTMR, EEFGEGA), (
                          SP.PDEPLMR, DDL_MECA),
                          (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                          (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                          (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                          (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (
                          SP.PVARCRR, LC.ZVARCPG),
                          (SP.PVARIMP, ZVARIPG), (
                          OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
                          ),
                          para_out=(
                          (SP.PCODRET, LC.ECODRET), (
                              OP.FULL_MECA_ELAS.PCONTPR, EEFGEGA),
                          (SP.PMATUUR, MMATUUR), (
                          OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
                          (SP.PVECTUR, MVECTUR), ),
                          ),

        OP.INIT_VARC(te=99,
                     para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
                     ),

        OP.MASS_INER(te=444,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (
                              OP.MASS_INER.PVARCPR, LC.ZVARCPG),
                              ),
                     para_out=((SP.PMASSINE, LC.EMASSINE), ),
                     ),

        OP.MASS_MECA(te=444,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (
                              OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                              ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.MASS_MECA_DIAG(te=444,
                          para_in=(
                              (SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                          (SP.PMATERC, LC.CMATERC), (
                          OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG),
                          ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.MASS_MECA_EXPLI(te=444,
                           para_in=(
                               (SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                           (SP.PMATERC, LC.CMATERC), (
                           OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG),
                           ),
                           para_out=((SP.PMATUUR, MMATUUR), ),
                           ),

        OP.M_GAMMA(te=444,
                   para_in=((SP.PACCELR, DDL_MECA), (SP.PCACOQU, CCACOQU),
                            (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                            (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
                   para_out=((SP.PVECTUR, MVECTUR), ),
                   ),

        OP.NSPG_NBVA(te=496,
                     para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
                     para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
                     ),

        OP.PAS_COURANT(te=404,
                       para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                                (SP.PMATERC, LC.CMATERC), ),
                       para_out=((SP.PCOURAN, LC.ECOURAN), ),
                       ),

        OP.RAPH_MECA(te=409,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                              (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (
                              OP.RAPH_MECA.PCONTMR, EEFGEGA),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                              (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                              (SP.PMATERC, LC.CMATERC), (
                                  SP.PVARCMR, LC.ZVARCPG),
                              (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              (SP.PVARIMP, ZVARIPG), (
                              OP.RAPH_MECA.PVARIMR, ZVARIPG),
                              ),
                     para_out=(
                     (SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, EEFGEGA),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
                     ),

        OP.REFE_FORC_NODA(te=446,
                          para_in=((SP.PGEOMER, NGEOMER), (SP.PREFCO, EREFCO),
                                   ),
                          para_out=((SP.PVECTUR, MVECTUR), ),
                          ),

        OP.REPERE_LOCAL(te=134,
                        para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                                 ),
                        para_out=((SP.PREPLO1, LC.CGEOM3D), (SP.PREPLO2, LC.CGEOM3D),
                                  (SP.PREPLO3, LC.CGEOM3D), ),
                        ),

        OP.REPE_GENE(te=442,
                     para_in=((SP.PANGREP, CCAORIE), (SP.PCACOQU, CCACOQU),
                              (SP.PDGGAIN, EDEFGPG), (SP.PDGGAINC, EDEFGPC),
                              (SP.PDGNOIN, EDEFGNO), (SP.PDGNOINC, LC.EDEFGNC),
                              (SP.PEFGAIN, EEFGEPGR), (SP.PEFGAINC, EEFGEPGC),
                              (SP.PEFNOIN, EEFGENOR), (SP.PEFNOINC, EEFGENOC),
                              (SP.PGEOMER, NGEOMER), ),
                     para_out=((SP.PDGGAOUC, EDEFGPC), (SP.PDGGAOUT, EDEFGPG),
                               (SP.PDGNOOUC, LC.EDEFGNC), (
                                   SP.PDGNOOUT, EDEFGNO),
                               (SP.PEFGAOUC, EEFGEPGC), (
                                   SP.PEFGAOUT, EEFGEPGR),
                               (SP.PEFNOOUC, EEFGENOC), (
                                   SP.PEFNOOUT, EEFGENOR),
                               ),
                     ),

        OP.RIGI_MECA(te=409,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                              (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                              (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
                     para_out=((SP.PMATUUR, MMATUUR), ),
                     ),

        OP.RIGI_MECA_ELAS(te=409,
                          para_in=(
                              (SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                          (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR), (
                          OP.RIGI_MECA_ELAS.PCONTMR, EEFGEGA),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                          (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                          (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
                          (SP.PVARCMR, LC.ZVARCPG), (
                          OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG),
                          (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                          (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG), ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.RIGI_MECA_GE(te=428,
                        para_in=(
                            (SP.PCACOQU, CCACOQU), (
                                OP.RIGI_MECA_GE.PCONTRR, EEFGEGA),
                        (SP.PGEOMER, NGEOMER), ),
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

        OP.RIGI_MECA_TANG(te=409,
                          para_in=(
                              (SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
                          (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (
                          OP.RIGI_MECA_TANG.PCONTMR, EEFGEGA),
                              (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                          (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
                          (SP.PINSTPR, CTEMPSR), (SP.PITERAT, LC.CITERAT),
                          (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                          (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (
                          SP.PVARCRR, LC.ZVARCPG),
                          (SP.PVARIMP, ZVARIPG), (
                          OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                          ),
                          para_out=((SP.PMATUUR, MMATUUR), ),
                          ),

        OP.SIEF_ELGA(te=422,
                     para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                              (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (
                              SP.PVARCRR, LC.ZVARCPG),
                              ),
                     para_out=((OP.SIEF_ELGA.PCONTRR, EEFGEGA), ),
                     ),

        OP.SIEF_ELNO(te=4,
                     para_in=(
                     (OP.SIEF_ELNO.PCONTRR, EEFGEGA), (
                     OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
                     para_out=(
                     (SP.PSIEFNOC, EEFGENC), (OP.SIEF_ELNO.PSIEFNOR, EEFGENO),
                     ),
                     ),

        OP.TOU_INI_ELEM(te=99,
                        para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
                        ),

        OP.TOU_INI_ELGA(te=99,
                        para_out=(
                        (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (
                        OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                        (OP.TOU_INI_ELGA.PSIEF_R, EEFGEGA), (
                        OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                        ),
                        ),

        OP.TOU_INI_ELNO(te=99,
                        para_out=(
                        (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (
                        OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                        (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (
                        OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
                        ),
                        ),

        OP.VARI_ELNO(te=4,
                     para_in=((SP.PVARIGR, ZVARIPG), ),
                     para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
                     ),

        OP.VERI_CARA_ELEM(te=119,
                          para_in=((SP.PCACOQU, CCACOQU), ),
                          para_out=((SP.PBIDON, LC.ECOURAN), ),
                          ),

    )


#------------------------------------------------------------
class MEDKTG3(MEDKQG4):

    """Please document this element"""
    meshType = MT.TRIA3
    elrefe = (
        ElrefeLoc(
            MT.TR3, gauss=(
                'RIGI=FPG3', 'MASS=FPG4', 'NOEU=NOEU', 'FPG1=FPG1',),
            mater=('RIGI', 'NOEU', 'FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )


#------------------------------------------------------------
class MEQ4GG4(MEDKQG4):

    """Please document this element"""
    meshType = MT.QUAD4
    elrefe = (
        ElrefeLoc(
            MT.QU4, gauss=(
                'RIGI=FPG4', 'MASS=FPG9', 'NOEU=NOEU', 'FPG1=FPG1',),
            mater=('RIGI', 'NOEU', 'FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )

    calculs = (
        OP.DISS_ELGA(te=-1),

        OP.DISS_ELNO(te=-1),

        OP.DISS_ELEM(te=-1),

        OP.RIGI_MECA_GE(te=-1),

        OP.CHAR_MECA_HYDR_R(te=-1),

        OP.CHAR_MECA_SECH_R(te=-1),

        OP.CHAR_MECA_TEMP_R(te=-1),
    )


#------------------------------------------------------------
class MET3GG3(MEDKQG4):

    """Please document this element"""
    meshType = MT.TRIA3
    elrefe = (
        ElrefeLoc(
            MT.TR3, gauss=(
                'RIGI=FPG1', 'MASS=FPG4', 'NOEU=NOEU', 'FPG1=FPG1',),
            mater=('RIGI', 'NOEU', 'FPG1',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
    )

    calculs = (
        OP.DISS_ELGA(te=-1),

        OP.DISS_ELNO(te=-1),

        OP.DISS_ELEM(te=-1),

        OP.RIGI_MECA_GE(te=-1),

        OP.CHAR_MECA_HYDR_R(te=-1),

        OP.CHAR_MECA_SECH_R(te=-1),

        OP.CHAR_MECA_TEMP_R(te=-1),
    )
