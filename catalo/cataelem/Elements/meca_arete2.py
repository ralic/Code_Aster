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


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='RIGI',
    components=('DX','DY','DZ',))


MFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


MFORCE   = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ',))




NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MECA_ARETE2(Element):
    """Please document this element"""
    meshType = MT.SEG2
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
    calculs = (

        OP.CHAR_MECA_FF1D3D(te=469,
            para_in=((SP.PFF1D3D, MFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, LC.MTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D3D(te=469,
            para_in=((SP.PFR1D3D, MFORCE), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                     (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), ),
        ),

    )


#------------------------------------------------------------
class MECA_ARETE3(MECA_ARETE2):
    """Please document this element"""
    meshType = MT.SEG3
    elrefe =(
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG3',),),
        )
