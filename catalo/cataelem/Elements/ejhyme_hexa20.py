
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


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ',)),
    ('EN2',('DX','DY','DZ',)),
    ('EN3',('PRE1',)),
    ('EN4',('DX','DY','DZ','PRE1',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO', diff=True,
    components=(
    ('EN1',('X','Y','Z',)),
    ('EN2',()),
    ('EN3',()),
    ('EN4',()),))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


ENGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


EREFCO   = LocatedComponents(phys=PHY.PREC, type='ELEM',
    components=('SIGM','FHYDR1',))


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIGN','SITX','SITY','FH11X','FH11Y',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIGN','SITX','SITY','FH11X','FH11Y',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIGN','SITX','SITY','FH11X','FH11Y',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='RIGI',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.AMOR_MECA, te=324,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PRIGINS, MMATUNS), (SP.PVARIPG, ZVARIPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=362,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.FORC_NODA, te=323,
    para_in=((OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PGEOMER, NGEOMER),
             (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=322,
    para_in=((SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCOMPOR, CCOMPOR),
             (OP.FULL_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.FULL_MECA.PVARCPR, LC.ZVARCPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
             (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.FULL_MECA_ELAS, te=322,
    para_in=((SP.PCARCRI, CCARCRI), (OP.FULL_MECA_ELAS.PCOMPOR, CCOMPOR),
             (OP.FULL_MECA_ELAS.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.FULL_MECA_ELAS.PVARCPR, LC.ZVARCPG), (OP.FULL_MECA_ELAS.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA_ELAS.PCONTPR, ECONTPG),
             (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             (OP.FULL_MECA_ELAS.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.PAS_COURANT, te=405,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PCOURAN, LC.ECOURAN), ),
)

ele.addCalcul(OP.RAPH_MECA, te=322,
    para_in=((SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCOMPOR, CCOMPOR),
             (OP.RAPH_MECA.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
             (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.REFE_FORC_NODA, te=323,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PREFCO, EREFCO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.RIGI_MECA_ELAS, te=322,
    para_in=((SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_ELAS.PCOMPOR, CCOMPOR),
             (OP.RIGI_MECA_ELAS.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RIGI_MECA_ELAS.PVARCPR, LC.ZVARCPG), (OP.RIGI_MECA_ELAS.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=322,
    para_in=((SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR),
             (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (SP.PMATERC, LC.CMATERC), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.SIEF_ELNO, te=122,
    para_in=((OP.SIEF_ELNO.PCONTRR, ECONTPG), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
             ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
             (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
             (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), (OP.TOU_INI_ELNO.PVARI_R, LC.ZVARINO),
             ),
)

ele.addCalcul(OP.VARI_ELNO, te=122,
    para_in=((SP.PVARIGR, ZVARIPG), ),
    para_out=((OP.VARI_ELNO.PVARINR, LC.ZVARINO), ),
)


#------------------------------------------------------------
EJHYME_HEXA20 = Element(modele=abstractElement)
ele = EJHYME_HEXA20
ele.meshType = MT.HEXA20
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,9,10,11,12,)),
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN3', (16,13,14,15,)),
        SetOfNodes('EN4', (17,18,19,20,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',), mater=('RIGI',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
    )


#------------------------------------------------------------
EJHYME_PENTA15 = Element(modele=abstractElement)
ele = EJHYME_PENTA15
ele.meshType = MT.PENTA15
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,7,8,9,)),
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN3', (10,11,12,)),
        SetOfNodes('EN4', (13,14,15,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',), mater=('RIGI',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
    )


#------------------------------------------------------------
MEFI_HEXA20 = Element(modele=abstractElement)
ele = MEFI_HEXA20
ele.meshType = MT.HEXA20
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,9,10,11,12,)),
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN3', (16,13,14,15,)),
        SetOfNodes('EN4', (17,18,19,20,)),
    )
ele.attrs= (('TYPE_VOISIN','F3'),)
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',), mater=('RIGI',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
    )


#------------------------------------------------------------
MEFI_PENTA15 = Element(modele=abstractElement)
ele = MEFI_PENTA15
ele.meshType = MT.PENTA15
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,7,8,9,)),
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN3', (10,11,12,)),
        SetOfNodes('EN4', (13,14,15,)),
    )
ele.attrs= (('TYPE_VOISIN','F3'),)
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',), mater=('RIGI',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
    )
