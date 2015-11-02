
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


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','H1X','H1Y','LAGS_C',
          'LAGS_F1',)),
    ('EN2',('DX','DY','H1X','H1Y','H2X',
          'H2Y','LAGS_C','LAGS_F1','LAG2_C','LAG2_F1',)),
    ('EN3',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','LAGS_C','LAGS_F1',
          'LAG2_C','LAG2_F1','LAG3_C','LAG3_F1',)),
    ('EN4',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','H4X','H4Y',
          'LAGS_C','LAGS_F1','LAG2_C','LAG2_F1','LAG3_C',
          'LAG3_F1','LAG4_C','LAG4_F1',)),
    ('EN5',('DX','DY','H1X','H1Y',)),
    ('EN6',('DX','DY','H1X','H1Y','H2X',
          'H2Y',)),
    ('EN7',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y',)),
    ('EN8',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','H4X','H4Y',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CCONCF   = LocatedComponents(phys=PHY.N120_I, type='ELEM',
    components=('X[9]',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELEM',
    components=('X[32]',))


CCONPI   = LocatedComponents(phys=PHY.N120_R, type='ELEM',
    components=('X[14]',))


CCONHE   = LocatedComponents(phys=PHY.N240_I, type='ELEM',
    components=('X[8]',))


CCONAI   = LocatedComponents(phys=PHY.N480_R, type='ELEM',
    components=('X[35]',))


PLALA_I  = LocatedComponents(phys=PHY.NEUT_I, type='ELEM',
    components=('X[4]',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_MECA_CONT, te=367,
    para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
             (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, PLALA_I),
             (OP.CHAR_MECA_CONT.PHEA_FA, CCONHE), (OP.CHAR_MECA_CONT.PHEA_NO, LC.N40NEUI),
             (OP.CHAR_MECA_CONT.PSTANO, STANO_I), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FROT, te=367,
    para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
             (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, PLALA_I),
             (OP.CHAR_MECA_FROT.PHEA_FA, CCONHE), (OP.CHAR_MECA_FROT.PHEA_NO, LC.N40NEUI),
             (OP.CHAR_MECA_FROT.PSTANO, STANO_I), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.RIGI_CONT, te=366,
    para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
             (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, PLALA_I),
             (OP.RIGI_CONT.PHEA_FA, CCONHE), (OP.RIGI_CONT.PHEA_NO, LC.N40NEUI),
             (OP.RIGI_CONT.PSTANO, STANO_I), ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_FROT, te=366,
    para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
             (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
             (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, PLALA_I),
             (OP.RIGI_FROT.PHEA_FA, CCONHE), (OP.RIGI_FROT.PHEA_NO, LC.N40NEUI),
             (OP.RIGI_FROT.PSTANO, STANO_I), ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)

ele.addCalcul(OP.XCVBCA, te=363,
    para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_PT, LC.CCONPT),
             (SP.PDEPL_P, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PHEAVNO, PLALA_I), (OP.XCVBCA.PHEA_FA, CCONHE),
             (OP.XCVBCA.PHEA_NO, LC.N40NEUI), ),
    para_out=((SP.PINDCOO, LC.I3NEUT_I), ),
)


#------------------------------------------------------------
MECPT3HT32_XH = Element(modele=abstractElement)
ele = MECPT3HT32_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
        SetOfNodes('EN6', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT3HT33_XH = Element(modele=abstractElement)
ele = MECPT3HT33_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
        SetOfNodes('EN7', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT3HT34_XH = Element(modele=abstractElement)
ele = MECPT3HT34_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
        SetOfNodes('EN8', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT32T3H_XH = Element(modele=abstractElement)
ele = MECPT32T3H_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN5', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT32T32_XH = Element(modele=abstractElement)
ele = MECPT32T32_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN6', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT32T33_XH = Element(modele=abstractElement)
ele = MECPT32T33_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN7', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT32T34_XH = Element(modele=abstractElement)
ele = MECPT32T34_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,)),
        SetOfNodes('EN8', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT33T3H_XH = Element(modele=abstractElement)
ele = MECPT33T3H_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,)),
        SetOfNodes('EN5', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT33T32_XH = Element(modele=abstractElement)
ele = MECPT33T32_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,)),
        SetOfNodes('EN6', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT33T33_XH = Element(modele=abstractElement)
ele = MECPT33T33_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,)),
        SetOfNodes('EN7', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT33T34_XH = Element(modele=abstractElement)
ele = MECPT33T34_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,)),
        SetOfNodes('EN8', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT34T3H_XH = Element(modele=abstractElement)
ele = MECPT34T3H_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,)),
        SetOfNodes('EN5', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT34T32_XH = Element(modele=abstractElement)
ele = MECPT34T32_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,)),
        SetOfNodes('EN6', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT34T33_XH = Element(modele=abstractElement)
ele = MECPT34T33_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,)),
        SetOfNodes('EN7', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPT34T34_XH = Element(modele=abstractElement)
ele = MECPT34T34_XH
ele.meshType = MT.TR3TR3
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,)),
        SetOfNodes('EN8', (4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ4HQ42_XH = Element(modele=abstractElement)
ele = MECPQ4HQ42_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
        SetOfNodes('EN6', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ4HQ43_XH = Element(modele=abstractElement)
ele = MECPQ4HQ43_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
        SetOfNodes('EN7', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ4HQ44_XH = Element(modele=abstractElement)
ele = MECPQ4HQ44_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
        SetOfNodes('EN8', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ42Q4H_XH = Element(modele=abstractElement)
ele = MECPQ42Q4H_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN5', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ42Q42_XH = Element(modele=abstractElement)
ele = MECPQ42Q42_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN6', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ42Q43_XH = Element(modele=abstractElement)
ele = MECPQ42Q43_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN7', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ42Q44_XH = Element(modele=abstractElement)
ele = MECPQ42Q44_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN2', (1,2,3,4,)),
        SetOfNodes('EN8', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ43Q4H_XH = Element(modele=abstractElement)
ele = MECPQ43Q4H_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,4,)),
        SetOfNodes('EN5', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ43Q42_XH = Element(modele=abstractElement)
ele = MECPQ43Q42_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,4,)),
        SetOfNodes('EN6', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ43Q43_XH = Element(modele=abstractElement)
ele = MECPQ43Q43_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,4,)),
        SetOfNodes('EN7', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ43Q44_XH = Element(modele=abstractElement)
ele = MECPQ43Q44_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN3', (1,2,3,4,)),
        SetOfNodes('EN8', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ44Q4H_XH = Element(modele=abstractElement)
ele = MECPQ44Q4H_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,4,)),
        SetOfNodes('EN5', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ44Q42_XH = Element(modele=abstractElement)
ele = MECPQ44Q42_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,4,)),
        SetOfNodes('EN6', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ44Q43_XH = Element(modele=abstractElement)
ele = MECPQ44Q43_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,4,)),
        SetOfNodes('EN7', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
MECPQ44Q44_XH = Element(modele=abstractElement)
ele = MECPQ44Q44_XH
ele.meshType = MT.QU4QU4
ele.nodes = (
        SetOfNodes('EN4', (1,2,3,4,)),
        SetOfNodes('EN8', (5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )
