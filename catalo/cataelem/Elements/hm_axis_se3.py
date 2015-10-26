
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
    ('EN1',('DX','DY','PRE1',)),
    ('EN2',('DX','DY',)),))


EFLHN    = LocatedComponents(phys=PHY.FLHN_R, type='ELGA', location='RIGI',
    components=('FH11',))


CFLUXF   = LocatedComponents(phys=PHY.FTHM_F, type='ELEM',
    components=('PFLU1',))


EFLUXE   = LocatedComponents(phys=PHY.FTHM_R, type='ELGA', location='RIGI',
    components=('PFLU1',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


NSIEF_R  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO', diff=True,
    components=(
    ('EN1',('FH11X','FH11Y',)),
    ('EN2',()),))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
class HM_AXIS_SE3(Element):
    """Please document this element"""
    meshType = MT.SEG3
    nodes = (
            SetOfNodes('EN2', (3,)),
            SetOfNodes('EN1', (1,2,)),
        )
    elrefe =(
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG4',),),
        )
    calculs = (

        OP.CHAR_MECA_FLUX_F(te=472,
            para_in=((SP.PFLUXF, CFLUXF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FLUX_R(te=472,
            para_in=((SP.PFLUXR, EFLUXE), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=472,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=472,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=467,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.FLHN_ELGA(te=468,
            para_in=((SP.PCONTR, NSIEF_R), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PFLHN, EFLHN), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )
