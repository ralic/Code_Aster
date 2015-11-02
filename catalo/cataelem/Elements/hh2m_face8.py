
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
    ('EN1',('DX','DY','DZ','PRE[2]',)),
    ('EN2',('DX','DY','DZ',)),))


EFLHN    = LocatedComponents(phys=PHY.FLHN_R, type='ELGA', location='RIGI',
    components=('FH1[2]','FH2[2]',))


EFORCNO  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


CFLUXF   = LocatedComponents(phys=PHY.FTHM_F, type='ELEM',
    components=('PFLU[2]',))


EFLUXE   = LocatedComponents(phys=PHY.FTHM_R, type='ELGA', location='RIGI',
    components=('PFLU[2]',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


NSIEF_R  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO', diff=True,
    components=(
    ('EN1',('FH11X','FH11Y','FH11Z','FH12X','FH12Y',
          'FH12Z','FH21X','FH21Y','FH21Z','FH22X',
          'FH22Y','FH22Z',)),
    ('EN2',()),))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CHAR_MECA_FLUX_F, te=466,
    para_in=((SP.PFLUXF, CFLUXF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FLUX_R, te=466,
    para_in=((SP.PFLUXR, EFLUXE), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR2D3D, te=466,
    para_in=((SP.PFR2D3D, EFORCNO), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_F, te=466,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_R, te=466,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=488,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.FLHN_ELGA, te=493,
    para_in=((SP.PCONTR, NSIEF_R), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PFLHN, EFLHN), ),
)

ele.addCalcul(OP.SIRO_ELEM, te=411,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PSIG3D, ECONTNO),
             ),
    para_out=((SP.PPJSIGM, LC.EPJSIGM), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)


#------------------------------------------------------------
HH2M_FACE8 = Element(modele=abstractElement)
ele = HH2M_FACE8
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
    )


#------------------------------------------------------------
HH2M_FACE6 = Element(modele=abstractElement)
ele = HH2M_FACE6
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
    )
