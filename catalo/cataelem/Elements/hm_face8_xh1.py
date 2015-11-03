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
    ('EN1',('DX','DY','DZ','PRE1','H1X',
          'H1Y','H1Z','H1PRE1',)),
    ('EN2',('DX','DY','DZ','H1X','H1Y',
          'H1Z',)),
    ('EN3',('DX','DY','DZ','PRE1','H1X',
          'H1Y','H1Z','H1PRE1','H2X','H2Y',
          'H2Z','H2PRE1',)),
    ('EN4',('DX','DY','DZ','H1X','H1Y',
          'H1Z','H2X','H2Y','H2Z',)),
    ('EN5',('DX','DY','DZ','PRE1','H1X',
          'H1Y','H1Z','H1PRE1','H2X','H2Y',
          'H2Z','H2PRE1','H3X','H3Y','H3Z',
          'H3PRE1',)),
    ('EN6',('DX','DY','DZ','H1X','H1Y',
          'H1Z','H2X','H2Y','H2Z','H3X',
          'H3Y','H3Z',)),))


EFLHN    = LocatedComponents(phys=PHY.FLHN_R, type='ELGA', location='RIGI',
    components=('FH11',))


EFORCNO  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


CFLUXF   = LocatedComponents(phys=PHY.FTHM_F, type='ELEM',
    components=('PFLU1',))


EFLUXE   = LocatedComponents(phys=PHY.FTHM_R, type='ELNO',
    components=('PFLU1',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E24NEUTI = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[24]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


CPRES_R  = LocatedComponents(phys=PHY.PRES_R, type='ELEM',
    components=('PRES',))


NSIEF_R  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO', diff=True,
    components=(
    ('EN1',('FH11X','FH11Y','FH11Z',)),
    ('EN3',('FH11X','FH11Y','FH11Z',)),
    ('EN5',('FH11X','FH11Y','FH11Z',)),
    ('EN2',()),
    ('EN4',()),
    ('EN6',()),))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
class HM_FACE8_XH1(Element):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN2', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )
    calculs = (

        OP.CHAR_MECA_FLUX_F(te=579,
            para_in=((OP.CHAR_MECA_FLUX_F.PCNSETO, LC.E144NEUI), (OP.CHAR_MECA_FLUX_F.PFISNO, LC.FISNO_I),
                     (SP.PFLUXF, CFLUXF), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FLUX_F.PHEAVTO, E24NEUTI), (OP.CHAR_MECA_FLUX_F.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_FLUX_F.PHEA_SE, E24NEUTI), (OP.CHAR_MECA_FLUX_F.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_FLUX_F.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_FLUX_F.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_FLUX_F.PPMILTO, LC.E88NEUTR), (OP.CHAR_MECA_FLUX_F.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FLUX_R(te=579,
            para_in=((OP.CHAR_MECA_FLUX_R.PCNSETO, LC.E144NEUI), (OP.CHAR_MECA_FLUX_R.PFISNO, LC.FISNO_I),
                     (SP.PFLUXR, EFLUXE), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_FLUX_R.PHEAVTO, E24NEUTI), (OP.CHAR_MECA_FLUX_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_FLUX_R.PHEA_SE, E24NEUTI), (OP.CHAR_MECA_FLUX_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_FLUX_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_FLUX_R.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_FLUX_R.PPMILTO, LC.E88NEUTR), (OP.CHAR_MECA_FLUX_R.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D3D(te=466,
            para_in=((SP.PFR2D3D, EFORCNO), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=36,
            para_in=((OP.CHAR_MECA_PRES_F.PCNSETO, LC.E144NEUI), (OP.CHAR_MECA_PRES_F.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_F.PHEAVTO, E24NEUTI),
                     (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PHEA_SE, E24NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PRES_F.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_PRES_F.PPMILTO, LC.E88NEUTR), (SP.PPRESSF, CPRESSF),
                     (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=36,
            para_in=((OP.CHAR_MECA_PRES_R.PCNSETO, LC.E144NEUI), (OP.CHAR_MECA_PRES_R.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_R.PHEAVTO, E24NEUTI),
                     (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PHEA_SE, E24NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PRES_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_PRES_R.PPMILTO, LC.E88NEUTR), (SP.PPRESSR, EPRESNO),
                     (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FLHN_ELGA(te=493,
            para_in=((SP.PCONTR, NSIEF_R), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PFLHN, EFLHN), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PFISNO, LC.FISNO_I), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
                     ),
        ),

        OP.SIRO_ELEM(te=411,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PSIG3D, ECONTNO),
                     ),
            para_out=((SP.PPJSIGM, LC.EPJSIGM), ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, LC.E144NEUI), (SP.PFISCO, LC.FISCO_I),
                     (OP.TOPONO.PFISNO, LC.FISNO_I), (OP.TOPONO.PHEAVTO, E24NEUTI),
                     (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, E24NEUTI),
                     ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PFISCO, LC.FISCO_I), (SP.PGEOMER, NGEOMER),
                     (SP.PLEVSET, LC.N1NEUT_R), ),
            para_out=((OP.TOPOSE.PCNSETO, LC.E144NEUI), (OP.TOPOSE.PHEAVTO, E24NEUTI),
                     (OP.TOPOSE.PLONCHA, LC.E10NEUTI), (OP.TOPOSE.PPINTTO, LC.E24NEUTR),
                     (OP.TOPOSE.PPMILTO, LC.E88NEUTR), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PPRES_R, CPRES_R), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PPRES_R, EPRESNO), ),
        ),

    )


#------------------------------------------------------------
class HM_FACE6_XH1(HM_FACE8_XH1):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN2', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )


#------------------------------------------------------------
class HM_FACE8_XH2(HM_FACE8_XH1):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN4', (5,6,7,8,)),
            SetOfNodes('EN3', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )


#------------------------------------------------------------
class HM_FACE6_XH2(HM_FACE8_XH1):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN4', (4,5,6,)),
            SetOfNodes('EN3', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )


#------------------------------------------------------------
class HM_FACE8_XH3(HM_FACE8_XH1):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN6', (5,6,7,8,)),
            SetOfNodes('EN5', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )


#------------------------------------------------------------
class HM_FACE6_XH3(HM_FACE8_XH1):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN6', (4,5,6,)),
            SetOfNodes('EN5', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6',),),
        )
