
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
from cataelem.Tools.base_objects import Calcul, Element, AbstractElement
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP

#----------------
# Modes locaux :
#----------------


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
    components=('DX','DY','DZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


NVITER   = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EDEFOPC  = LocatedComponents(phys=PHY.EPSI_C, type='ELGA', location='RIGI',
    components=('EPXX',))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPXX',))


EDFVCPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPTHER_L',))


EDFVCNO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPTHER_L',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ','REP',))


CFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ','REP',))


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


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('EFFORT',))


EEFGEGC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('N',))


EEFGENC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('N',))


EEFGEGA  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('N',))


EEFGENO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('N',))


ZVARENO  = LocatedComponents(phys=PHY.VARI_R, type='ELNO',
    components=('VARI',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.ADD_SIGM, te=581,
    para_in=((SP.PEPCON1, EEFGEGA), (SP.PEPCON2, EEFGEGA),
             ),
    para_out=((SP.PEPCON3, EEFGEGA), ),
)

ele.addCalcul(OP.AMOR_MECA, te=50,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
             (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
             (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.CHAR_MECA_EPSI_R, te=580,
    para_in=((SP.PEPSINR, CEPSINR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FF1D1D, te=155,
    para_in=((OP.CHAR_MECA_FF1D1D.PCAORIE, CCAORIE), (SP.PFF1D1D, CFORCEF),
             (SP.PGEOMER, NGEOMER), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR1D1D, te=155,
    para_in=((OP.CHAR_MECA_FR1D1D.PCAORIE, CCAORIE), (SP.PFR1D1D, CFORCER),
             (SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_HYDR_R, te=155,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.CHAR_MECA_HYDR_R.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_HYDR_R.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PESA_R, te=155,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_SECH_R, te=155,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.CHAR_MECA_SECH_R.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_SECH_R.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_SF1D1D, te=155,
    para_in=((OP.CHAR_MECA_SF1D1D.PCAORIE, CCAORIE), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PFF1D1D, CFORCEF),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_SR1D1D, te=155,
    para_in=((OP.CHAR_MECA_SR1D1D.PCAORIE, CCAORIE), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PVENTCX, LC.CVENTCX), (SP.PVITER, NVITER),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_TEMP_R, te=155,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.CHAR_MECA_TEMP_R.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=478,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.ECIN_ELEM, te=154,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.ECIN_ELEM.PCAORIE, CCAORIE),
             (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.POMEGA2, LC.COMEG2R),
             (OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVITESR, DDL_MECA),
             ),
    para_out=((SP.PENERCR, EENERR), ),
)

ele.addCalcul(OP.EFGE_ELGA, te=546,
    para_in=((SP.PSIEFR, EEFGEGA), ),
    para_out=((SP.PEFGEC, EEFGEGC), (SP.PEFGER, EEFGEGA),
             ),
)

ele.addCalcul(OP.EFGE_ELNO, te=185,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
             (OP.EFGE_ELNO.PCONTRR, EEFGEGA), (SP.PDEPLAR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PNONLIN, LC.ENONLIN), (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PEFFORC, EEFGENC), (OP.EFGE_ELNO.PEFFORR, EEFGENO),
             ),
)

ele.addCalcul(OP.EPEQ_ELGA, te=335,
    para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
    para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
)

ele.addCalcul(OP.EPEQ_ELNO, te=335,
    para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
    para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
)

ele.addCalcul(OP.EPME_ELGA, te=531,
    para_in=((OP.EPME_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
             (OP.EPME_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((OP.EPME_ELGA.PDEFOPG, EDEFOPG), ),
)

ele.addCalcul(OP.EPME_ELNO, te=4,
    para_in=((OP.EPME_ELNO.PDEFOPG, EDEFOPG), ),
    para_out=((SP.PDEFONO, EDEFONO), ),
)

ele.addCalcul(OP.EPOT_ELEM, te=154,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.EPOT_ELEM.PCAORIE, CCAORIE),
             (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
)

ele.addCalcul(OP.EPSI_ELGA, te=154,
    para_in=((OP.EPSI_ELGA.PCAORIE, CCAORIE), (SP.PDEPLAR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PDEFOPC, EDEFOPC), (OP.EPSI_ELGA.PDEFOPG, EDEFOPG),
             ),
)

ele.addCalcul(OP.EPSI_ELNO, te=4,
    para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
    para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
             ),
)

ele.addCalcul(OP.EPSP_ELGA, te=531,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.EPSP_ELGA.PCONTRR, EEFGEGA),
             (OP.EPSP_ELGA.PDEFORR, EDEFOPG), (SP.PMATERC, LC.CMATERC),
             (OP.EPSP_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((OP.EPSP_ELGA.PDEFOPG, EDEFOPG), ),
)

ele.addCalcul(OP.EPSP_ELNO, te=4,
    para_in=((OP.EPSP_ELNO.PDEFOPG, EDEFOPG), ),
    para_out=((SP.PDEFONO, EDEFONO), ),
)

ele.addCalcul(OP.EPVC_ELGA, te=531,
    para_in=((OP.EPVC_ELGA.PCOMPOR, CCOMPOR), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.EPVC_ELGA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((OP.EPVC_ELGA.PDEFOPG, EDFVCPG), ),
)

ele.addCalcul(OP.EPVC_ELNO, te=4,
    para_in=((OP.EPVC_ELNO.PDEFOPG, EDFVCPG), ),
    para_out=((SP.PDEFONO, EDFVCNO), ),
)

ele.addCalcul(OP.FORC_NODA, te=156,
    para_in=((OP.FORC_NODA.PCAORIE, CCAORIE), (OP.FORC_NODA.PCOMPOR, CCOMPOR),
             (OP.FORC_NODA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.FULL_MECA.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
             (OP.FULL_MECA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, EEFGEGA),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA_ELAS, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.FULL_MECA_ELAS.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
             (OP.FULL_MECA_ELAS.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.PVARIMP, ZVARIPG), (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, EEFGEGA),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.MASS_INER, te=245,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.MASS_INER.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=153,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.MASS_MECA.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.MASS_MECA_DIAG, te=153,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.MASS_MECA_DIAG.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.MASS_MECA_EXPLI, te=153,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.MASS_MECA_EXPLI.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.M_GAMMA, te=153,
    para_in=((SP.PACCELR, DDL_MECA), (SP.PCAGNBA, LC.CCAGNBA),
             (OP.M_GAMMA.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.M_GAMMA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.PAS_COURANT, te=404,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PCOURAN, LC.ECOURAN), ),
)

ele.addCalcul(OP.RAPH_MECA, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.RAPH_MECA.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
             (OP.RAPH_MECA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, EEFGEGA),
             (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.REFE_FORC_NODA, te=156,
    para_in=((SP.PREFCO, EREFCO), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.REPERE_LOCAL, te=135,
    para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
    para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
             (SP.PREPLO3, CGEOMER), ),
)

ele.addCalcul(OP.RIGI_MECA, te=153,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.RIGI_MECA.PCAORIE, CCAORIE),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_ELAS, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.RIGI_MECA_ELAS.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR),
             (OP.RIGI_MECA_ELAS.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_GE, te=98,
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_HYST, te=50,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUC, MMATUUC), ),
)

ele.addCalcul(OP.RIGI_MECA_IMPLEX, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.RIGI_MECA_IMPLEX.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_IMPLEX.PCOMPOR, CCOMPOR),
             (OP.RIGI_MECA_IMPLEX.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RIGI_MECA_IMPLEX.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (OP.RIGI_MECA_IMPLEX.PVARIMR, ZVARIPG), ),
    para_out=((SP.PCONTXR, EEFGEGA), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=248,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE),
             (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
             (OP.RIGI_MECA_TANG.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.SIEF_ELGA, te=154,
    para_in=((SP.PCAGNBA, LC.CCAGNBA), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
             (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PCONTRC, EEFGEGC), (OP.SIEF_ELGA.PCONTRR, EEFGEGA),
             ),
)

ele.addCalcul(OP.SIEF_ELNO, te=4,
    para_in=((OP.SIEF_ELNO.PCONTRR, EEFGEGA), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PSIEFNOC, EEFGENC), (OP.SIEF_ELNO.PSIEFNOR, EEFGENO),
             ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PSIEF_R, EEFGEGA), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PINST_R, LC.EEINST_R),
             (OP.TOU_INI_ELNO.PNEUT_F, LC.EENEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.EENEUT_R),
             (OP.TOU_INI_ELNO.PSIEF_R, EEFGENO), (OP.TOU_INI_ELNO.PVARI_R, ZVARENO),
             ),
)


#------------------------------------------------------------
MECA_BARRE = Element(modele=abstractElement)
ele = MECA_BARRE
ele.meshType = MT.SEG2
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG1','FPG1=FPG1',), mater=('RIGI','FPG1',),),
    )
