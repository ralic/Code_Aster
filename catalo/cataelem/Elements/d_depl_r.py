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
import cataelem.Commons.attributes as AT


MGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO', diff=True,
components=(
    ('EN1', ()),
    ('EN2', ('X', 'Y', 'Z',)),))


for cmp in (
    'DH', 'DRX', 'DRY', 'DRZ', 'DX', 'DY', 'DZ', 'K1', 'K2', 'K3',
    'GLIS', 'GONF', 'GRX', 'H1PRE1', 'H1X', 'H1Y',
    'H1Z', 'H2PRE1', 'H2X', 'H2Y', 'H2Z', 'H3PRE1', 'H3X', 'H3Y', 'H3Z', 'H4X', 'H4Y', 'H4Z',
    'LAG2_C', 'LAG2_F1', 'LAG2_F2', 'LAG3_C', 'LAG3_F1', 'LAG3_F2', 'LAG4_C', 'LAG4_F1',
    'LAG4_F2', 'LAGS_C', 'LAGS_F1', 'LAGS_F2', 'LH1', 'PHI', 'PRE1', 'PRE2', 'PRES', 'PRES11',
    'PRES12', 'PRES13', 'PRES21', 'PRES22', 'PRES23', 'PRES31', 'PRES32', 'PRES33', 'TEMP',
    'UI2', 'UI3', 'UI4', 'UI5', 'UI6', 'UO2', 'UO3', 'UO4', 'UO5', 'UO6', 'V11', 'V12', 'V13',
    'V21', 'V22', 'V23', 'V31', 'V32', 'V33', 'VI2', 'VI3', 'VI4', 'VI5', 'VI6', 'VO2', 'VO3',
    'VO4', 'VO5', 'VO6', 'WI1', 'WI2', 'WI3', 'WI4', 'WI5', 'WI6',
        'WO', 'WO1', 'WO2', 'WO3', 'WO4', 'WO5', 'WO6',):

    #----------------
    # Modes locaux :
    #----------------
    DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
        ('EN1', ('LAGR',)),
        ('EN2', (cmp,)),))

    NDEPLAC = LocatedComponents(phys=PHY.DEPL_C, type='ELNO', diff=True,
    components=(
        ('EN1', ('LAGR',)),
        ('EN2', (cmp,)),))

    MVECTUR = ArrayOfComponents(
        phys=PHY.VDEP_R, locatedComponents=DDL_MECA)
    MVECTUC = ArrayOfComponents(phys=PHY.VDEP_C, locatedComponents=NDEPLAC)

    MMATUUR = ArrayOfComponents(
        phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

#     Attention : il faut nommer explicitement TOUS les modes locaux crees dans la boucle
#     --------------------------------------------------------------------
    DDL_MECA.setName('DDL_MECA')
    NDEPLAC.setName('NDEPLAC')
    MVECTUR.setName('MVECTUR')
    MVECTUC.setName('MVECTUC')
    MMATUUR.setName('MMATUUR')

    name = 'D_DEPL_R_' + cmp

    class TempClass(Element):

        """Please document this element"""
        _name = name

        meshType = MT.SEG3
        nodes = (
            SetOfNodes('EN1', (2, 3,)),
            SetOfNodes('EN2', (1,)),
        )
        attrs = ((AT.CL_DUAL, 'OUI'),)

        calculs = (
            OP.MECA_BTLA_R(te=2,
                para_in=((SP.PDDLMUR, LC.MDDLMUR), (
                    OP.MECA_BTLA_R.PLAGRAR, DDL_MECA), ),
                para_out=((SP.PVECTUR, MVECTUR), ),
            ),
            OP.MECA_BU_R(te=2,
                para_in=(
                    (SP.PALPHAR, LC.MALPHAR), (
                        OP.MECA_BU_R.PDDLIMR, DDL_MECA), (
                            SP.PDDLMUR, LC.MDDLMUR),
                ),
                para_out=((SP.PVECTUR, MVECTUR), ),
            ),
            OP.MECA_DDLI_F(te=2,
                para_in=(
                    (SP.PDDLIMF, LC.MDDLIMF), (
                        SP.PGEOMER, MGEOMER), (SP.PTEMPSR, LC.MTEMPSR),
                ),
                para_out=((SP.PVECTUR, MVECTUR), ),
            ),
            OP.MECA_DDLI_R(te=2,
                para_in=((OP.MECA_DDLI_R.PDDLIMR, LC.MDDLIMR), ),
                para_out=((SP.PVECTUR, MVECTUR), ),
            ),
            OP.MECA_DDLI_C(te=2,
                para_in=((SP.PDDLIMC, LC.MDDLIMC), ),
                para_out=((SP.PVECTUC, MVECTUC), ),
            ),
            OP.MECA_DDLM_R(te=2,
                para_in=((SP.PDDLMUR, LC.MDDLMUR), ),
                para_out=((SP.PMATUUR, MMATUUR), ),
            )
        )

    exec name + " = TempClass"
    del TempClass
