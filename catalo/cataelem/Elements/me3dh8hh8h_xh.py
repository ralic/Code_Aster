# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    ('EN1',('DX','DY','DZ','H1X','H1Y',
          'H1Z','LAGS_C','LAGS_F[2]',)),
    ('EN2',('DX','DY','DZ','H1X','H1Y',
          'H1Z',)),
    ('EN3',('DX','DY','DZ','H1X','H1Y',
          'H1Z','E1X','E1Y','E1Z','LAGS_C',
          'LAGS_F[2]',)),
    ('EN4',('DX','DY','DZ','H1X','H1Y',
          'H1Z','E1X','E1Y','E1Z',)),
    ('EN5',('E1X','E1Y','E1Z','LAGS_C','LAGS_F[2]',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CCONCF   = LocatedComponents(phys=PHY.N120_I, type='ELEM',
    components=('X[90]',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELEM',
    components=('X[40]',))


CCONPI   = LocatedComponents(phys=PHY.N120_R, type='ELEM',
    components=('X[102]',))


CCONAI   = LocatedComponents(phys=PHY.N480_R, type='ELEM',
    components=('X[170]',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class ME3DH8HH8H_XH(Element):
    """Please document this element"""
    meshType = MT.HE8HE8
    nodes = (
            SetOfNodes('EN2', (9,10,11,12,13,14,15,16,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )
    calculs = (

        OP.CHAR_MECA_CONT(te=367,
            para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                     (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_CONT.PHEA_NO, LC.N80NEUI),
                     (OP.CHAR_MECA_CONT.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FROT(te=367,
            para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                     (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FROT.PHEA_NO, LC.N80NEUI),
                     (OP.CHAR_MECA_FROT.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.RIGI_CONT(te=366,
            para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                     (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_CONT.PHEA_NO, LC.N80NEUI),
                     (OP.RIGI_CONT.PSTANO, STANO_I), ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_FROT(te=366,
            para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_CF, CCONCF),
                     (SP.PCAR_PI, CCONPI), (SP.PCAR_PT, LC.CCONPT),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_FROT.PHEA_NO, LC.N80NEUI),
                     (OP.RIGI_FROT.PSTANO, STANO_I), ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.XCVBCA(te=363,
            para_in=((SP.PCAR_AI, CCONAI), (SP.PCAR_PT, LC.CCONPT),
                     (SP.PDEPL_P, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.XCVBCA.PHEA_NO, LC.N80NEUI), ),
            para_out=((SP.PINDCOO, LC.I3NEUT_I), ),
        ),

    )


#------------------------------------------------------------
class ME3DH8HH8C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.HE8HE8
    nodes = (
            SetOfNodes('EN4', (9,10,11,12,13,14,15,16,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DH8CH8H_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.HE8HE8
    nodes = (
            SetOfNodes('EN2', (9,10,11,12,13,14,15,16,)),
            SetOfNodes('EN3', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DH8CH8C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.HE8HE8
    nodes = (
            SetOfNodes('EN4', (9,10,11,12,13,14,15,16,)),
            SetOfNodes('EN3', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DH8T_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.HEXA8
    nodes = (
            SetOfNodes('EN5', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DP6HP6H_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.PE6PE6
    nodes = (
            SetOfNodes('EN2', (7,8,9,10,11,12,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DP6HP6C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.PE6PE6
    nodes = (
            SetOfNodes('EN4', (7,8,9,10,11,12,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DP6CP6H_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.PE6PE6
    nodes = (
            SetOfNodes('EN2', (7,8,9,10,11,12,)),
            SetOfNodes('EN3', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DP6CP6C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.PE6PE6
    nodes = (
            SetOfNodes('EN4', (7,8,9,10,11,12,)),
            SetOfNodes('EN3', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DP6T_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.PENTA6
    nodes = (
            SetOfNodes('EN5', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DT4HT4H_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.TE4TE4
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DT4HT4C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.TE4TE4
    nodes = (
            SetOfNodes('EN4', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DT4CT4H_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.TE4TE4
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN3', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DT4CT4C_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.TE4TE4
    nodes = (
            SetOfNodes('EN4', (5,6,7,8,)),
            SetOfNodes('EN3', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DT4T_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.TETRA4
    nodes = (
            SetOfNodes('EN5', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DHVHHVH_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.H20H20
    nodes = (
            SetOfNodes('EN2', (9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.H20, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DPQHPQH_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.P15P15
    nodes = (
            SetOfNodes('EN2', (7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,)),
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.P15, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )


#------------------------------------------------------------
class ME3DTDHTDH_XH(ME3DH8HH8H_XH):
    """Please document this element"""
    meshType = MT.T10T10
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.T10, gauss = ('NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('NOEU=NOEU',),),
        )
