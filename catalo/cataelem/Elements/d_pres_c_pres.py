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
import cataelem.Commons.attributes as AT

#----------------
# Modes locaux :
#----------------


MDDLMUC = LocatedComponents(phys=PHY.DDLM_C, type='ELEM',
                            components=('A1',))


MGEOMER = LocatedComponents(phys=PHY.GEOM_R, type='ELNO', diff=True,
                            components=(
                            ('EN1', ()),
                            ('EN2', ('X', 'Y', 'Z',)),))


DDL_ACOU = LocatedComponents(phys=PHY.PRES_C, type='ELNO', diff=True,
                             components=(
                             ('EN1', ('LAGR',)),
                             ('EN2', ('PRES',)),))


MVECTTC = ArrayOfComponents(phys=PHY.VPRE_C, locatedComponents=(DDL_ACOU,))

MMATTTC = ArrayOfComponents(
    phys=PHY.MPRE_C, locatedComponents=(DDL_ACOU, DDL_ACOU))

#------------------------------------------------------------


class D_PRES_C_PRES(Element):

    """Please document this element"""
    meshType = MT.SEG3
    nodes = (
        SetOfNodes('EN1', (2, 3,)),
        SetOfNodes('EN2', (1,)),
    )
    attrs = ((AT.CL_DUAL, 'OUI'),)

    calculs = (
        OP.ACOU_DDLI_C(te=2,
                       para_in=((SP.PDDLIMC, LC.MDDLIMC), ),
                       para_out=((SP.PVECTTC, MVECTTC), ),
                       ),

        OP.ACOU_DDLI_F(te=2,
                       para_in=(
                           (SP.PDDLIMF, LC.MDDLIMF), (SP.PGEOMER, MGEOMER),
                       (SP.PTEMPSR, LC.MTEMPSR), ),
                       para_out=((SP.PVECTTC, MVECTTC), ),
                       ),

        OP.ACOU_DDLM_C(te=2,
                       para_in=((SP.PDDLMUC, MDDLMUC), ),
                       para_out=((SP.PMATTTC, MMATTTC), ),
                       ),
    )
