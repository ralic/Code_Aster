
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


CCADISA  = LocatedComponents(phys=PHY.CADISA, type='ELEM',
    components=('A[4]',))


CCADISK  = LocatedComponents(phys=PHY.CADISK, type='ELEM',
    components=('K[4]',))


CCADISM  = LocatedComponents(phys=PHY.CADISM, type='ELEM',
    components=('M[4]',))


CCAORIE  = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE','DX','DY',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('EFFORT',))


EEFGEGC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('N','VY',))


EEFGENC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('N','VY',))


EEFGEGA  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('N','VY',))


EEFGENO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('N','VY',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MECA_2D_DIS_T_N(Element):
    """Please document this element"""
    meshType = MT.POI1
    elrefe =(
            ElrefeLoc(MT.PO1, gauss = ('RIGI=NOEU','FPG1=FPG1',), mater=('RIGI','FPG1',),),
        )
    calculs = (

        OP.AMOR_MECA(te=41,
            para_in=((SP.PCADISA, CCADISA), (OP.AMOR_MECA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.CHAR_MECA_PESA_R(te=43,
            para_in=((SP.PCADISM, CCADISM), (OP.CHAR_MECA_PESA_R.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=43,
            para_in=((SP.PCINFDI, LC.CCINFDI), (SP.PROTATR, LC.CROTATR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=99,
            para_in=((SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.ECIN_ELEM(te=44,
            para_in=((SP.PCADISM, CCADISM), (OP.ECIN_ELEM.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (SP.PDEPLAR, DDL_MECA),
                     (SP.POMEGA2, LC.COMEG2R), (OP.ECIN_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVITESR, DDL_MECA), ),
            para_out=((SP.PENERCR, EENERR), ),
        ),

        OP.EFGE_ELGA(te=546,
            para_in=((SP.PSIEFR, EEFGEGA), ),
            para_out=((SP.PEFGEC, EEFGEGC), (SP.PEFGER, EEFGEGA),
                     ),
        ),

        OP.EFGE_ELNO(te=185,
            para_in=((SP.PCADISK, CCADISK), (OP.EFGE_ELNO.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.EFGE_ELNO.PCONTRR, EEFGEGA),
                     (SP.PDEPLAR, DDL_MECA), (SP.PNONLIN, LC.ENONLIN),
                     (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PEFFORC, EEFGENC), (OP.EFGE_ELNO.PEFFORR, EEFGENO),
                     ),
        ),

        OP.EPOT_ELEM(te=44,
            para_in=((SP.PCADISK, CCADISK), (OP.EPOT_ELEM.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (SP.PDEPLAR, DDL_MECA),
                     (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
        ),

        OP.FORC_NODA(te=39,
            para_in=((OP.FORC_NODA.PCAORIE, CCAORIE), (SP.PCINFDI, LC.CCINFDI),
                     (OP.FORC_NODA.PCONTMR, EEFGEGA), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=47,
            para_in=((SP.PCADISK, CCADISK), (OP.FULL_MECA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA.PCONTMR, EEFGEGA), (SP.PDEPENT, DDL_MECA),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, LC.CINSTPR),
                     (SP.PINSTPR, LC.CINSTPR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), (SP.PVITENT, DDL_MECA),
                     (SP.PVITPLU, DDL_MECA), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, EEFGEGA),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA_ELAS(te=47,
            para_in=((SP.PCADISK, CCADISK), (OP.FULL_MECA_ELAS.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
                     (OP.FULL_MECA_ELAS.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, LC.CINSTPR), (SP.PINSTPR, LC.CINSTPR),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, EEFGEGA),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.MASS_INER(te=45,
            para_in=((SP.PCADISM, CCADISM), (OP.MASS_INER.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (SP.PGEOMER, NGEOMER),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=41,
            para_in=((SP.PCADISM, CCADISM), (OP.MASS_MECA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.MASS_MECA_DIAG(te=41,
            para_in=((SP.PCADISM, CCADISM), (OP.MASS_MECA_DIAG.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.MASS_MECA_EXPLI(te=41,
            para_in=((SP.PCADISM, CCADISM), (OP.MASS_MECA_EXPLI.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.M_GAMMA(te=41,
            para_in=((SP.PACCELR, DDL_MECA), (SP.PCADISM, CCADISM),
                     (OP.M_GAMMA.PCAORIE, CCAORIE), (SP.PCINFDI, LC.CCINFDI),
                     (OP.M_GAMMA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.PAS_COURANT(te=405,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PCOURAN, LC.ECOURAN), ),
        ),

        OP.RAPH_MECA(te=47,
            para_in=((SP.PCADISK, CCADISK), (OP.RAPH_MECA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
                     (OP.RAPH_MECA.PCONTMR, EEFGEGA), (SP.PDEPENT, DDL_MECA),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, LC.CINSTPR),
                     (SP.PINSTPR, LC.CINSTPR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), (SP.PVITENT, DDL_MECA),
                     (SP.PVITPLU, DDL_MECA), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, EEFGEGA),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.REFE_FORC_NODA(te=39,
            para_in=((SP.PCINFDI, LC.CCINFDI), (SP.PREFCO, EREFCO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.RIGI_MECA(te=41,
            para_in=((SP.PCADISK, CCADISK), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_MECA_ELAS(te=47,
            para_in=((SP.PCADISK, CCADISK), (OP.RIGI_MECA_ELAS.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR),
                     (OP.RIGI_MECA_ELAS.PCONTMR, EEFGEGA), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (SP.PINSTMR, LC.CINSTPR), (SP.PINSTPR, LC.CINSTPR),
                     (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG), (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_HYST(te=41,
            para_in=((SP.PCINFDI, LC.CCINFDI), (SP.PRIGIEL, MMATUUR),
                     (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUC, MMATUUC), ),
        ),

        OP.RIGI_MECA_TANG(te=47,
            para_in=((SP.PCADISK, CCADISK), (OP.RIGI_MECA_TANG.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
                     (OP.RIGI_MECA_TANG.PCONTMR, EEFGEGA), (SP.PDEPENT, DDL_MECA),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (SP.PINSTMR, LC.CINSTPR),
                     (SP.PINSTPR, LC.CINSTPR), (SP.PMATERC, LC.CMATERC),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), (SP.PVITENT, DDL_MECA),
                     (SP.PVITPLU, DDL_MECA), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=42,
            para_in=((SP.PCADISK, CCADISK), (OP.SIEF_ELGA.PCAORIE, CCAORIE),
                     (SP.PCINFDI, LC.CCINFDI), (SP.PDEPLAR, DDL_MECA),
                     (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRC, EEFGEGC), (OP.SIEF_ELGA.PCONTRR, EEFGEGA),
                     ),
        ),

        OP.SIEF_ELNO(te=4,
            para_in=((OP.SIEF_ELNO.PCONTRR, EEFGEGA), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, EEFGENC), (OP.SIEF_ELNO.PSIEFNOR, EEFGENO),
                     ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, EEFGEGA), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PINST_R, LC.EEINST_R),
                     (OP.TOU_INI_ELNO.PNEUT_F, LC.EENEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.EENEUT_R),
                     (OP.TOU_INI_ELNO.PSIEF_R, EEFGENO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
                     ),
        ),

        OP.VARI_ELNO(te=4,
            para_in=((SP.PVARIGR, ZVARIPG), ),
            para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
        ),

    )
