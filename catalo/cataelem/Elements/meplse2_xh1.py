
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
    ('EN1',('DX','DY','H1X','H1Y',)),
    ('EN2',('DX','DY','H1X','H1Y','H2X',
          'H2Y',)),
    ('EN3',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y',)),
    ('EN4',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','H4X','H4Y',)),))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E24NEUI  = LocatedComponents(phys=PHY.N1280I, type='ELEM',
    components=('X[24]',))


E8NEUTI  = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[8]',))


E16NEUTR = LocatedComponents(phys=PHY.N792_R, type='ELEM',
    components=('X[16]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES','CISA',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES','CISA',))


CPRES_R  = LocatedComponents(phys=PHY.PRES_R, type='ELEM',
    components=('PRES','CISA',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
class MEPLSE2_XH1(Element):
    """Please document this element"""
    meshType = MT.SEG2
    nodes = (
            SetOfNodes('EN1', (1,2,)),
        )
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
    calculs = (

        OP.CHAR_MECA_FF1D2D(te=36,
            para_in=((OP.CHAR_MECA_FF1D2D.PCNSETO, E24NEUI), (SP.PFF1D2D, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FF1D2D.PHEAVTO, E8NEUTI),
                     (OP.CHAR_MECA_FF1D2D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FF1D2D.PHEA_SE, E8NEUTI),
                     (OP.CHAR_MECA_FF1D2D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FF1D2D.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FF1D2D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FF1D2D.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_FF1D2D.PPMILTO, E16NEUTR), (OP.CHAR_MECA_FF1D2D.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR1D2D(te=36,
            para_in=((OP.CHAR_MECA_FR1D2D.PCNSETO, E24NEUI), (SP.PFR1D2D, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR1D2D.PHEAVTO, E8NEUTI),
                     (OP.CHAR_MECA_FR1D2D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FR1D2D.PHEA_SE, E8NEUTI),
                     (OP.CHAR_MECA_FR1D2D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FR1D2D.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FR1D2D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FR1D2D.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_FR1D2D.PPMILTO, E16NEUTR), (OP.CHAR_MECA_FR1D2D.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=36,
            para_in=((OP.CHAR_MECA_PRES_F.PCNSETO, E24NEUI), (OP.CHAR_MECA_PRES_F.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_F.PHEAVTO, E8NEUTI),
                     (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PHEA_SE, E8NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PRES_F.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_PRES_F.PPMILTO, E16NEUTR), (SP.PPRESSF, CPRESSF),
                     (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=36,
            para_in=((OP.CHAR_MECA_PRES_R.PCNSETO, E24NEUI), (OP.CHAR_MECA_PRES_R.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_PRES_R.PHEAVTO, E8NEUTI),
                     (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PHEA_SE, E8NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PRES_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTTO, LC.E24NEUTR),
                     (OP.CHAR_MECA_PRES_R.PPMILTO, E16NEUTR), (SP.PPRESSR, EPRESNO),
                     (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PFISNO, LC.FISNO_I), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
                     ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, E24NEUI), (SP.PFISCO, LC.FISCO_I),
                     (OP.TOPONO.PFISNO, LC.FISNO_I), (OP.TOPONO.PHEAVTO, E8NEUTI),
                     (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, E8NEUTI),
                     ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PFISCO, LC.FISCO_I), (SP.PGEOMER, NGEOMER),
                     (SP.PLEVSET, LC.N1NEUT_R), ),
            para_out=((OP.TOPOSE.PCNSETO, E24NEUI), (OP.TOPOSE.PHEAVTO, E8NEUTI),
                     (OP.TOPOSE.PLONCHA, LC.E10NEUTI), (OP.TOPOSE.PPINTTO, LC.E24NEUTR),
                     (OP.TOPOSE.PPMILTO, E16NEUTR), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PPRES_R, CPRES_R), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGEOMER), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
                     (OP.TOU_INI_ELNO.PPRES_R, EPRESNO), ),
        ),

    )


#------------------------------------------------------------
class MEPLSE2_XH2(MEPLSE2_XH1):
    """Please document this element"""
    meshType = MT.SEG2
    nodes = (
            SetOfNodes('EN2', (1,2,)),
        )
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )


#------------------------------------------------------------
class MEPLSE2_XH3(MEPLSE2_XH1):
    """Please document this element"""
    meshType = MT.SEG2
    nodes = (
            SetOfNodes('EN3', (1,2,)),
        )
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )


#------------------------------------------------------------
class MEPLSE2_XH4(MEPLSE2_XH1):
    """Please document this element"""
    meshType = MT.SEG2
    nodes = (
            SetOfNodes('EN4', (1,2,)),
        )
    elrefe =(
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',),),
        )
