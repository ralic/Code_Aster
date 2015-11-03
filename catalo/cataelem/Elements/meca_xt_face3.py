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


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','E1X','E1Y',
          'E1Z','E2X','E2Y','E2Z','E3X',
          'E3Y','E3Z','E4X','E4Y','E4Z',))


NTHETAR  = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


EFORCNO  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[3]','K[3]','BETA',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E6NEUTI  = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[6]',))


E33NEUTR = LocatedComponents(phys=PHY.N792_R, type='ELEM',
    components=('X[33]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


CPRES_R  = LocatedComponents(phys=PHY.PRES_R, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
class MECA_XT_FACE3(Element):
    """Please document this element"""
    meshType = MT.TRIA3
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','FPG3=FPG3','XCON=FPG12',),),
        )
    calculs = (

        OP.CALC_G(te=118,
            para_in=((OP.CALC_G.PCNSETO, LC.E36NEUI), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFR2D3D, EFORCNO), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_G.PHEAVTO, E6NEUTI), (OP.CALC_G.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_G.PLSN, LC.N1NEUT_R), (OP.CALC_G.PLST, LC.N1NEUT_R),
                     (OP.CALC_G.PPINTTO, LC.E12NEUTR), (SP.PPRESSR, EPRESNO),
                     (SP.PTHETAR, NTHETAR), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=580,
            para_in=((SP.PFF2D3D, CFORCEF), (SP.PPRESSF, CPRESSF),
                     (SP.PTHETAR, NTHETAR), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_K_G(te=580,
            para_in=((SP.PFR2D3D, EFORCNO), (SP.PPRESSR, EPRESNO),
                     (SP.PTHETAR, NTHETAR), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=580,
            para_in=((SP.PFF2D3D, CFORCEF), (SP.PPRESSF, CPRESSF),
                     (SP.PTHETAR, NTHETAR), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CHAR_MECA_FF2D3D(te=36,
            para_in=((OP.CHAR_MECA_FF2D3D.PCNSETO, LC.E36NEUI), (SP.PFF2D3D, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FF2D3D.PHEAVTO, E6NEUTI),
                     (OP.CHAR_MECA_FF2D3D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FF2D3D.PHEA_SE, E6NEUTI),
                     (OP.CHAR_MECA_FF2D3D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FF2D3D.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FF2D3D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FF2D3D.PPINTTO, LC.E12NEUTR),
                     (OP.CHAR_MECA_FF2D3D.PPMILTO, E33NEUTR), (OP.CHAR_MECA_FF2D3D.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D3D(te=36,
            para_in=((OP.CHAR_MECA_FR2D3D.PCNSETO, LC.E36NEUI), (SP.PFR2D3D, EFORCNO),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR2D3D.PHEAVTO, E6NEUTI),
                     (OP.CHAR_MECA_FR2D3D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FR2D3D.PHEA_SE, E6NEUTI),
                     (OP.CHAR_MECA_FR2D3D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FR2D3D.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FR2D3D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FR2D3D.PPINTTO, LC.E12NEUTR),
                     (OP.CHAR_MECA_FR2D3D.PPMILTO, E33NEUTR), (OP.CHAR_MECA_FR2D3D.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=36,
            para_in=((OP.CHAR_MECA_PRES_F.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_PRES_F.PHEA_SE, E6NEUTI), (OP.CHAR_MECA_PRES_F.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_F.PPINTTO, LC.E12NEUTR), (OP.CHAR_MECA_PRES_F.PPMILTO, E33NEUTR),
                     (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=36,
            para_in=((OP.CHAR_MECA_PRES_R.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_R.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_PRES_R.PHEA_SE, E6NEUTI), (OP.CHAR_MECA_PRES_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_R.PPINTTO, LC.E12NEUTR), (OP.CHAR_MECA_PRES_R.PPMILTO, E33NEUTR),
                     (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PSTANO, STANO_I), ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, LC.E36NEUI), (OP.TOPONO.PHEAVTO, E6NEUTI),
                     (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, E6NEUTI),
                     ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
                     ),
            para_out=((OP.TOPOSE.PCNSETO, LC.E36NEUI), (OP.TOPOSE.PHEAVTO, E6NEUTI),
                     (OP.TOPOSE.PLONCHA, LC.E10NEUTI), (OP.TOPOSE.PPINTTO, LC.E12NEUTR),
                     (OP.TOPOSE.PPMILTO, E33NEUTR), ),
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
class MECA_XT_FACE4(MECA_XT_FACE3):
    """Please document this element"""
    meshType = MT.QUAD4
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=COT3','FPG3=FPG3','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XT_FACE6(MECA_XT_FACE3):
    """Please document this element"""
    meshType = MT.TRIA6
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','XCON=FPG12',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XT_FACE8(MECA_XT_FACE3):
    """Please document this element"""
    meshType = MT.QUAD8
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','XCON=FPG12',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6','XCON=FPG12',),),
        )
