
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


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','DEFORM','INCELA',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


NVITER   = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


CFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ','MX','MY',
          'MZ','REP',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
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


EEFGENC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('N',))


EEFGEGA  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('N',))


EEFGENO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('N',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

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

ele.addCalcul(OP.CHAR_MECA_FF1D1D, te=161,
    para_in=((SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR1D1D, te=161,
    para_in=((SP.PFR1D1D, CFORCER), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FRLAPL, te=163,
    para_in=((SP.PFLAPLA, LC.CFLAPLA), (SP.PGEOMER, NGEOMER),
             (SP.PLISTMA, LC.CLISTMA), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PESA_R, te=161,
    para_in=((SP.PCACABL, LC.CCACABL), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_SF1D1D, te=161,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PFF1D1D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_SR1D1D, te=161,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PVENTCX, LC.CVENTCX),
             (SP.PVITER, NVITER), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_TEMP_R, te=162,
    para_in=((SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=478,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.EFGE_ELGA, te=546,
    para_in=((SP.PSIEFR, EEFGEGA), ),
    para_out=((SP.PEFGER, EEFGEGA), ),
)

ele.addCalcul(OP.FORC_NODA, te=164,
    para_in=((OP.FORC_NODA.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=160,
    para_in=((SP.PCACABL, LC.CCACABL), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, EEFGEGA),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.MASS_INER, te=111,
    para_in=((SP.PCACABL, LC.CCACABL), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=168,
    para_in=((SP.PCACABL, LC.CCACABL), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.M_GAMMA, te=168,
    para_in=((SP.PACCELR, DDL_MECA), (SP.PCACABL, LC.CCACABL),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
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

ele.addCalcul(OP.RAPH_MECA, te=160,
    para_in=((SP.PCACABL, LC.CCACABL), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, EEFGEGA),
             (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.REFE_FORC_NODA, te=164,
    para_in=((SP.PREFCO, EREFCO), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.RIGI_MECA, te=98,
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_GE, te=86,
    para_in=((SP.PCACABL, LC.CCACABL), (OP.RIGI_MECA_GE.PCONTRR, EEFGEGA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=160,
    para_in=((SP.PCACABL, LC.CCACABL), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.SIEF_ELNO, te=4,
    para_in=((OP.SIEF_ELNO.PCONTRR, EEFGEGA), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PSIEFNOC, EEFGENC), (OP.SIEF_ELNO.PSIEFNOR, EEFGENO),
             ),
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
             (OP.TOU_INI_ELNO.PSIEF_R, EEFGENO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
             ),
)

ele.addCalcul(OP.VARI_ELNO, te=4,
    para_in=((SP.PVARIGR, ZVARIPG), ),
    para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
)


#------------------------------------------------------------
MECABL2 = Element(modele=abstractElement)
ele = MECABL2
ele.meshType = MT.SEG2
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG1','FPG1=FPG1',), mater=('RIGI','FPG1',),),
        ElrefeLoc(MT.CABPOU,),
    )
