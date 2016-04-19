# coding=utf-8
# CATALOGUES DES ELEMENTS TARDIF DP X-FEM GRAND GLISSEMENT (HEAVISIDE ET CRACK TIP)

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


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
                             components=(
                             ('EN1', ('DX', 'DY', 'H1X', 'H1Y', 'LAGS_C',
                                      'LAGS_F1',)),
                             ('EN2', ('DX', 'DY', 'H1X', 'H1Y',)),
                                 ('EN3', ('DX', 'DY', 'H1X', 'H1Y', 'E1X',
                                          'E1Y', 'LAGS_C', 'LAGS_F1',)),
                                 ('EN4', ('DX', 'DY', 'H1X', 'H1Y', 'E1X',
                                          'E1Y',)),
                                 ('EN5', ('E1X', 'E1Y', 'LAGS_C', 'LAGS_F1',)),))


NGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
                            components=('X', 'Y',))


CCONCF = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                           components=('X[9]',))


STANO_I = LocatedComponents(phys=PHY.N120_I, type='ELEM',
                            components=('X[16]',))


CCONPI = LocatedComponents(phys=PHY.N120_R, type='ELEM',
                           components=('X[14]',))


CCONAI = LocatedComponents(phys=PHY.N480_R, type='ELEM',
                           components=('X[35]',))


MVECTUR = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR = ArrayOfComponents(
    phys=PHY.MDEP_R, locatedComponents=(DDL_MECA, DDL_MECA))

MMATUNS = ArrayOfComponents(
    phys=PHY.MDNS_R, locatedComponents=(DDL_MECA, DDL_MECA))


#------------------------------------------------------------
class MEDPQ4HQ4H_XH(Element):

    """Please document this element"""
    meshType = MT.QU4QU4
    nodes = (
        SetOfNodes('EN2', (5, 6, 7, 8,)),
        SetOfNodes('EN1', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )
    calculs = (

        OP.CHAR_MECA_CONT(te=367,
                          para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                                   (SP.PCAR_PI, CCONPI), (
                                       SP.PCAR_PT, LC.CCONPT),
                                   (SP.PDEPL_M, DDL_MECA), (
                                       SP.PDEPL_P, DDL_MECA),
                                   (SP.PGEOMER, NGEOMER), (
                                   OP.CHAR_MECA_CONT.PHEA_NO, LC.N40NEUI),
                                   (OP.CHAR_MECA_CONT.PSTANO, STANO_I), ),
                          para_out=((SP.PVECTUR, MVECTUR), ),
                          ),

        OP.CHAR_MECA_FROT(te=367,
                          para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                                   (SP.PCAR_PI, CCONPI), (
                                       SP.PCAR_PT, LC.CCONPT),
                                   (SP.PDEPL_M, DDL_MECA), (
                                       SP.PDEPL_P, DDL_MECA),
                                   (SP.PGEOMER, NGEOMER), (
                                   OP.CHAR_MECA_FROT.PHEA_NO, LC.N40NEUI),
                                   (OP.CHAR_MECA_FROT.PSTANO, STANO_I), ),
                          para_out=((SP.PVECTUR, MVECTUR), ),
                          ),

        OP.RIGI_CONT(te=366,
                     para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                              (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                         (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                         (SP.PGEOMER, NGEOMER), (
                             OP.RIGI_CONT.PHEA_NO, LC.N40NEUI),
                         (OP.RIGI_CONT.PSTANO, STANO_I), ),
                     para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                               ),
                     ),

        OP.RIGI_FROT(te=366,
                     para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                              (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                              (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                              (SP.PGEOMER, NGEOMER), (
                              OP.RIGI_FROT.PHEA_NO, LC.N40NEUI),
                              (OP.RIGI_FROT.PSTANO, STANO_I), ),
                     para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                               ),
                     ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM2D), ),
        ),


        OP.TOU_INI_ELNO(te=99,
                        para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
                        ),

        OP.XCVBCA(te=363,
                  para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_PT, LC.CCONPT),
                           (SP.PDEPL_P, DDL_MECA), (SP.PGEOMER, NGEOMER),
                           (OP.XCVBCA.PHEA_NO, LC.N40NEUI), ),
                  para_out=((SP.PINDCOO, LC.I3NEUT_I), ),
                  ),

    )


#------------------------------------------------------------
class MEDPQ4HQ4C_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QU4QU4
    nodes = (
        SetOfNodes('EN4', (5, 6, 7, 8,)),
        SetOfNodes('EN1', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPQ4CQ4H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QU4QU4
    nodes = (
        SetOfNodes('EN2', (5, 6, 7, 8,)),
        SetOfNodes('EN3', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPQ4CQ4C_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QU4QU4
    nodes = (
        SetOfNodes('EN4', (5, 6, 7, 8,)),
        SetOfNodes('EN3', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPQ4T_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
        SetOfNodes('EN5', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3HT3H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR3TR3
    nodes = (
        SetOfNodes('EN2', (4, 5, 6,)),
        SetOfNodes('EN1', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3HT3C_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR3TR3
    nodes = (
        SetOfNodes('EN4', (4, 5, 6,)),
        SetOfNodes('EN1', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3CT3H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR3TR3
    nodes = (
        SetOfNodes('EN2', (4, 5, 6,)),
        SetOfNodes('EN3', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3CT3C_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR3TR3
    nodes = (
        SetOfNodes('EN4', (4, 5, 6,)),
        SetOfNodes('EN3', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3T_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
        SetOfNodes('EN5', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
# appariement croises pour P1P1
class MEDPQ4HT3H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QU4TR3
    nodes = (
        SetOfNodes('EN2', (5, 6, 7,)),
        SetOfNodes('EN1', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT3HQ4H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR3QU4
    nodes = (
        SetOfNodes('EN2', (4, 5, 6, 7,)),
        SetOfNodes('EN1', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR3, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.QU4, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE2, gauss = ('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPQ8HQ8H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.QU8QU8
    nodes = (
        SetOfNodes('EN2', (5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,)),
        SetOfNodes('EN1', (1, 2, 3, 4,)),
    )
    elrefe = (
        ElrefeLoc(MT.QU8, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE3, gauss=('NOEU=NOEU',),),
    )


#------------------------------------------------------------
class MEDPT6HT6H_XH(MEDPQ4HQ4H_XH):

    """Please document this element"""
    meshType = MT.TR6TR6
    nodes = (
        SetOfNodes('EN2', (4, 5, 6, 7, 8, 9, 10, 11, 12,)),
        SetOfNodes('EN1', (1, 2, 3,)),
    )
    elrefe = (
        ElrefeLoc(MT.TR6, gauss=('NOEU=NOEU',),),
        ElrefeLoc(MT.SE3, gauss=('NOEU=NOEU',),),
    )
