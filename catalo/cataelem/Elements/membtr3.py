
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


CCACOQU  = LocatedComponents(phys=PHY.CACOQU, type='ELEM',
    components=('ALPHA','BETA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC',))


NDEPLAC  = LocatedComponents(phys=PHY.DEPL_C, type='ELNO',
    components=('DX','DY','DZ',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EXX','EYY','EXY',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EXX','EYY','EXY',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EXX','EYY','EXY',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('EPSI',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('NXX','NYY','NXY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('NXX','NYY','NXY',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('NXX','NYY','NXY',))


EGAMIMA  = LocatedComponents(phys=PHY.SPMX_R, type='ELGA', location='RIGI',
    components=('VAL','NUCOU','NUSECT','NUFIBR','POSIC',
          'POSIS',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUC  = ArrayOfComponents(phys=PHY.MDEP_C, locatedComponents=(NDEPLAC,NDEPLAC))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.ADD_SIGM, te=581,
    para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
             ),
    para_out=((SP.PEPCON3, ECONTPG), ),
)

ele.addCalcul(OP.AMOR_MECA, te=50,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMASSEL, MMATUUR),
             (SP.PMATERC, LC.CMATERC), (SP.PRIGIEL, MMATUUR),
             (OP.AMOR_MECA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.CHAR_MECA_EPSI_R, te=434,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PEPSINR, CEPSINR),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.CHAR_MECA_EPSI_R.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PESA_R, te=434,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_TEMP_R, te=434,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
             (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=488,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.EFGE_ELGA, te=436,
    para_in=((SP.PSIEFR, ECONTPG), ),
    para_out=((SP.PEFGER, ECONTPG), ),
)

ele.addCalcul(OP.EFGE_ELNO, te=185,
    para_in=((SP.PCACOQU, CCACOQU), (OP.EFGE_ELNO.PCONTRR, ECONTPG),
             (SP.PDEPLAR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PNONLIN, LC.ENONLIN),
             (SP.PTEMPSR, CTEMPSR), (OP.EFGE_ELNO.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PEFFORC, ECONTNC), (OP.EFGE_ELNO.PEFFORR, ECONTNO),
             ),
)

ele.addCalcul(OP.EPOT_ELEM, te=436,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (OP.EPOT_ELEM.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((OP.EPOT_ELEM.PENERDR, EENERR), ),
)

ele.addCalcul(OP.EPSI_ELGA, te=436,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (OP.EPSI_ELGA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((OP.EPSI_ELGA.PDEFOPG, EDEFOPG), ),
)

ele.addCalcul(OP.EPSI_ELNO, te=4,
    para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
    para_out=((SP.PDEFONO, EDEFONO), ),
)

ele.addCalcul(OP.FORC_NODA, te=434,
    para_in=((SP.PCACOQU, CCACOQU), (OP.FORC_NODA.PCONTMR, ECONTPG),
             (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
             (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
             (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA_ELAS, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
             (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR), (OP.FULL_MECA_ELAS.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
             (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG), ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, ECONTPG),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.MASS_INER, te=436,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.MASS_INER.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=439,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.MASS_MECA_DIAG, te=439,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA_DIAG.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.MASS_MECA_EXPLI, te=439,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA_EXPLI.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.MINMAX_SP, te=99,
    para_out=((SP.PGAMIMA, EGAMIMA), (SP.PNOMIMA, LC.ENOMIMA),
             ),
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

ele.addCalcul(OP.RAPH_MECA, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
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
)

ele.addCalcul(OP.REFE_FORC_NODA, te=434,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PREFCO, EREFCO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.RIGI_MECA, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_ELAS, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
             (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_ELAS.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
             (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_HYST, te=50,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PRIGIEL, MMATUUR), (OP.RIGI_MECA_HYST.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUC, MMATUUC), ),
)

ele.addCalcul(OP.RIGI_MECA_IMPLEX, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
             (OP.RIGI_MECA_IMPLEX.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_IMPLEX.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
             (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_IMPLEX.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.RIGI_MECA_IMPLEX.PVARIMR, ZVARIPG), ),
    para_out=((SP.PCONTXR, ECONTPG), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=435,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PCARCRI, CCARCRI),
             (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PINSTMR, CTEMPSR),
             (SP.PINSTPR, CTEMPSR), (SP.PMATERC, LC.CMATERC),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.SIEF_ELGA, te=436,
    para_in=((SP.PCACOQU, CCACOQU), (SP.PDEPLAR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTEMPSR, CTEMPSR), (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((OP.SIEF_ELGA.PCONTRR, ECONTPG), ),
)

ele.addCalcul(OP.SIEF_ELNO, te=4,
    para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
             ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)

ele.addCalcul(OP.VARI_ELNO, te=4,
    para_in=((SP.PVARIGR, ZVARIPG), ),
    para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
)


#------------------------------------------------------------
MEMBTR3 = Element(modele=abstractElement)
ele = MEMBTR3
ele.meshType = MT.TRIA3
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG1','MASS=FPG3',), mater=('RIGI','MASS',),),
    )


#------------------------------------------------------------
MEMBTR6 = Element(modele=abstractElement)
ele = MEMBTR6
ele.meshType = MT.TRIA6
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6',), mater=('RIGI','MASS',),),
    )


#------------------------------------------------------------
MEMBQU4 = Element(modele=abstractElement)
ele = MEMBQU4
ele.meshType = MT.QUAD4
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG4',), mater=('RIGI','MASS',),),
    )


#------------------------------------------------------------
MEMBQU8 = Element(modele=abstractElement)
ele = MEMBQU8
ele.meshType = MT.QUAD8
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9',), mater=('RIGI','MASS',),),
    )
