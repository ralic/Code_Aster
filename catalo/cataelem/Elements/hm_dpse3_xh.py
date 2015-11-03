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
                             ('EN1', ('DX', 'DY', 'PRE1', 'H1X', 'H1Y',
                                      'H1PRE1',)),
                             ('EN2', ('DX', 'DY', 'H1X', 'H1Y',)),))


EFLHN = LocatedComponents(phys=PHY.FLHN_R, type='ELGA', location='RIGI',
                          components=('FH11',))


CFLUXF = LocatedComponents(phys=PHY.FTHM_F, type='ELEM',
                           components=('PFLU1',))


EFLUXE = LocatedComponents(phys=PHY.FTHM_R, type='ELNO',
                           components=('PFLU1',))


NGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
                            components=('X', 'Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
                             components=('X', 'Y', 'W',))


CTEMPSR = LocatedComponents(phys=PHY.INST_R, type='ELEM',
                            components=('INST', 'DELTAT', 'THETA',))


STANO_I = LocatedComponents(phys=PHY.N120_I, type='ELNO',
                            components=('X1',))


E6NEUTI = LocatedComponents(phys=PHY.N1280I, type='ELEM',
                            components=('X[6]',))


CPRESSF = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
                            components=('PRES', 'CISA',))


EPRESNO = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
                            components=('PRES', 'CISA',))


CPRES_R = LocatedComponents(phys=PHY.PRES_R, type='ELEM',
                            components=('PRES', 'CISA',))


NSIEF_R = LocatedComponents(phys=PHY.SIEF_R, type='ELNO', diff=True,
                            components=(
                            ('EN1', ('FH11X', 'FH11Y',)),
                            ('EN2', ()),))


MVECTUR = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))


#------------------------------------------------------------
class HM_DPSE3_XH(Element):

    """Please document this element"""
    meshType = MT.SEG3
    nodes = (
        SetOfNodes('EN2', (3,)),
        SetOfNodes('EN1', (1, 2,)),
    )
    elrefe = (
        ElrefeLoc(MT.SE3, gauss=('RIGI=FPG4',),),
        ElrefeLoc(MT.SE2, gauss=('RIGI=FPG4',),),
    )
    calculs = (

        OP.CHAR_MECA_FLUX_F(te=579,
                            para_in=(
                            (OP.CHAR_MECA_FLUX_F.PCNSETO, E6NEUTI), (
                                SP.PFLUXF, CFLUXF),
                            (SP.PGEOMER, NGEOMER), (
                            OP.CHAR_MECA_FLUX_F.PHEAVTO, LC.E2NEUTI),
                            (OP.CHAR_MECA_FLUX_F.PHEA_NO, LC.N5NEUTI), (
                            OP.CHAR_MECA_FLUX_F.PHEA_SE, LC.E2NEUTI),
                            (OP.CHAR_MECA_FLUX_F.PLONCHA, LC.E10NEUTI), (
                            OP.CHAR_MECA_FLUX_F.PLSN, LC.N1NEUT_R),
                            (OP.CHAR_MECA_FLUX_F.PPINTTO, LC.E6NEUTR), (
                            OP.CHAR_MECA_FLUX_F.PPMILTO, LC.E4NEUTR),
                            (OP.CHAR_MECA_FLUX_F.PSTANO, STANO_I), (
                            SP.PTEMPSR, CTEMPSR),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_FLUX_R(te=579,
                            para_in=(
                            (OP.CHAR_MECA_FLUX_R.PCNSETO, E6NEUTI), (
                                SP.PFLUXR, EFLUXE),
                            (SP.PGEOMER, NGEOMER), (
                            OP.CHAR_MECA_FLUX_R.PHEAVTO, LC.E2NEUTI),
                            (OP.CHAR_MECA_FLUX_R.PHEA_NO, LC.N5NEUTI), (
                            OP.CHAR_MECA_FLUX_R.PHEA_SE, LC.E2NEUTI),
                            (OP.CHAR_MECA_FLUX_R.PLONCHA, LC.E10NEUTI), (
                            OP.CHAR_MECA_FLUX_R.PLSN, LC.N1NEUT_R),
                            (OP.CHAR_MECA_FLUX_R.PPINTTO, LC.E6NEUTR), (
                            OP.CHAR_MECA_FLUX_R.PPMILTO, LC.E4NEUTR),
                            (OP.CHAR_MECA_FLUX_R.PSTANO, STANO_I), (
                            SP.PTEMPSR, CTEMPSR),
                            ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PRES_F(te=36,
                            para_in=(
                            (OP.CHAR_MECA_PRES_F.PCNSETO, E6NEUTI), (
                                SP.PGEOMER, NGEOMER),
                            (OP.CHAR_MECA_PRES_F.PHEAVTO, LC.E2NEUTI), (
                            OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI),
                            (OP.CHAR_MECA_PRES_F.PHEA_SE, LC.E2NEUTI), (
                            OP.CHAR_MECA_PRES_F.PLONCHA, LC.E10NEUTI),
                            (OP.CHAR_MECA_PRES_F.PLSN, LC.N1NEUT_R), (
                            OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R),
                            (OP.CHAR_MECA_PRES_F.PPINTTO, LC.E6NEUTR), (
                            OP.CHAR_MECA_PRES_F.PPMILTO, LC.E4NEUTR),
                            (SP.PPRESSF, CPRESSF), (
                            OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
                            (SP.PTEMPSR, CTEMPSR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.CHAR_MECA_PRES_R(te=36,
                            para_in=(
                            (OP.CHAR_MECA_PRES_R.PCNSETO, E6NEUTI), (
                                SP.PGEOMER, NGEOMER),
                            (OP.CHAR_MECA_PRES_R.PHEAVTO, LC.E2NEUTI), (
                            OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI),
                            (OP.CHAR_MECA_PRES_R.PHEA_SE, LC.E2NEUTI), (
                            OP.CHAR_MECA_PRES_R.PLONCHA, LC.E10NEUTI),
                            (OP.CHAR_MECA_PRES_R.PLSN, LC.N1NEUT_R), (
                            OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R),
                            (OP.CHAR_MECA_PRES_R.PPINTTO, LC.E6NEUTR), (
                            OP.CHAR_MECA_PRES_R.PPMILTO, LC.E4NEUTR),
                            (SP.PPRESSR, EPRESNO), (
                            OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
                            (SP.PTEMPSR, CTEMPSR), ),
                            para_out=((SP.PVECTUR, MVECTUR), ),
                            ),

        OP.FLHN_ELGA(te=468,
                     para_in=((SP.PCONTR, NSIEF_R), (SP.PGEOMER, NGEOMER),
                              ),
                     para_out=((SP.PFLHN, EFLHN), ),
                     ),

        OP.INI_XFEM_ELNO(te=99,
                         para_out=(
                         (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R), (
                         OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R),
                         (OP.INI_XFEM_ELNO.PSTANO, STANO_I), ),
                         ),

        OP.TOPONO(te=120,
                  para_in=(
                  (OP.TOPONO.PCNSETO, E6NEUTI), (
                      OP.TOPONO.PHEAVTO, LC.E2NEUTI),
                  (SP.PLEVSET, LC.N1NEUT_R), (
                  OP.TOPONO.PLONCHA, LC.E10NEUTI),
                  ),
                  para_out=(
                  (OP.TOPONO.PHEA_NO, LC.N5NEUTI), (
                  OP.TOPONO.PHEA_SE, LC.E2NEUTI),
                  ),
                  ),

        OP.TOPOSE(te=514,
                  para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
                           ),
                  para_out=(
                  (OP.TOPOSE.PCNSETO, E6NEUTI), (
                      OP.TOPOSE.PHEAVTO, LC.E2NEUTI),
                  (OP.TOPOSE.PLONCHA, LC.E10NEUTI), (
                  OP.TOPOSE.PPINTTO, LC.E6NEUTR),
                  (OP.TOPOSE.PPMILTO, LC.E4NEUTR), ),
                  ),

        OP.TOU_INI_ELEM(te=99,
                        para_out=((OP.TOU_INI_ELEM.PPRES_R, CPRES_R), ),
                        ),

        OP.TOU_INI_ELGA(te=99,
                        para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
                        ),

        OP.TOU_INI_ELNO(te=99,
                        para_out=(
                        (OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (
                        OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
                        (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (
                        OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
                        ),
                        ),

    )
