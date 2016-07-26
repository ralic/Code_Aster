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
# person_in_charge: mathieu.courtois@edf.fr

from cataelem.Tools.base_objects import Phenomenon, Modelisation, objects_from_context
import cataelem.Commons.mesh_types as MT
from cataelem.Elements.elements import EL
import cataelem.Commons.attributes as AT

PhenMod={}


############################################################
# Pour le phenomene : MECANIQUE :
############################################################
MECANIQUE = Phenomenon(code='ME')
phen = MECANIQUE

phen.add('2D_BARRE', Modelisation(dim=(1,2), code='2DB',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_2D_BARRE),
    )))

phen.add('2D_DIS_T', Modelisation(dim=(-1,2), code='2DT',
    attrs=(
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_2D_DIS_T_L),
        (MT.POI1      , EL.MECA_2D_DIS_T_N),
    )))

phen.add('2D_DIS_TR', Modelisation(dim=(-1,2), code='2TR',
    attrs=(
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_2D_DIS_TR_L),
        (MT.POI1      , EL.MECA_2D_DIS_TR_N),
    )))

phen.add('2D_FLUIDE', Modelisation(dim=(2,2), code='2FL',
    attrs=(
        (AT.FLUIDE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEFLTR3),
        (MT.QUAD4     , EL.MEFLQU4),
        (MT.TRIA6     , EL.MEFLTR6),
        (MT.QUAD8     , EL.MEFLQU8),
        (MT.QUAD9     , EL.MEFLQU9),
        (MT.SEG2      , EL.MEFLSE2),
        (MT.SEG3      , EL.MEFLSE3),
    )))

phen.add('2D_FLUI_ABSO', Modelisation(dim=(1,2), code='2FA',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.ABSO,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MEFASE2),
        (MT.SEG3      , EL.MEFASE3),
    )))

phen.add('2D_FLUI_PESA', Modelisation(dim=(2,2), code='2FP',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.PESA,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEFP_FACE3),
        (MT.QUAD4     , EL.MEFP_FACE4),
        (MT.TRIA6     , EL.MEFP_FACE6),
        (MT.QUAD8     , EL.MEFP_FACE8),
        (MT.QUAD9     , EL.MEFP_FACE9),
    )))

phen.add('2D_FLUI_STRU', Modelisation(dim=(1,2), code='FS2',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.FSI,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MEFSSE2),
        (MT.SEG3      , EL.MEFSSE3),
    )))

phen.add('3D', Modelisation(dim=(3,3), code='3D_',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_HEXA8),
        (MT.PENTA6    , EL.MECA_PENTA6),
        (MT.TETRA4    , EL.MECA_TETRA4),
        (MT.QUAD4     , EL.MECA_FACE4),
        (MT.TRIA3     , EL.MECA_FACE3),
        (MT.SEG2      , EL.MECA_ARETE2),
        (MT.HEXA27    , EL.MECA_HEXA27),
        (MT.HEXA20    , EL.MECA_HEXA20),
        (MT.PENTA15   , EL.MECA_PENTA15),
        (MT.PENTA18   , EL.MECA_PENTA18),
        (MT.TETRA10   , EL.MECA_TETRA10),
        (MT.QUAD9     , EL.MECA_FACE9),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
        (MT.PYRAM5    , EL.MECA_PYRAM5),
        (MT.PYRAM13   , EL.MECA_PYRAM13),
    )))

phen.add('3D1XH', Modelisation(dim=(3,3), code='3X1',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.HEXA20    , EL.MECA_XH_HEXA20),
        (MT.HEXA8     , EL.MECA_XH_HEXA8),
        (MT.PENTA15   , EL.MECA_XH_PENTA15),
        (MT.PENTA6    , EL.MECA_XH_PENTA6),
        (MT.PYRAM13   , EL.MECA_XH_PYRAM13),
        (MT.PYRAM5    , EL.MECA_XH_PYRAM5),
        (MT.TETRA10   , EL.MECA_XH_TETRA10),
        (MT.TETRA4    , EL.MECA_XH_TETRA4),
        (MT.TRIA3     , EL.MECA_XH_FACE3),
        (MT.TRIA6     , EL.MECA_XH_FACE6),
        (MT.QUAD4     , EL.MECA_XH_FACE4),
        (MT.QUAD8     , EL.MECA_XH_FACE8),
    )))

phen.add('3D1XHT', Modelisation(dim=(3,3), code='3X3',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XHT_HEXA8),
        (MT.PENTA6    , EL.MECA_XHT_PENTA6),
        (MT.PYRAM5    , EL.MECA_XHT_PYRAM5),
        (MT.TETRA4    , EL.MECA_XHT_TETRA4),
        (MT.QUAD4     , EL.MECA_XHT_FACE4),
        (MT.TRIA3     , EL.MECA_XHT_FACE3),
    )))

phen.add('3D1XT', Modelisation(dim=(3,3), code='3X2',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XT_HEXA8),
        (MT.PENTA6    , EL.MECA_XT_PENTA6),
        (MT.PYRAM5    , EL.MECA_XT_PYRAM5),
        (MT.TETRA4    , EL.MECA_XT_TETRA4),
        (MT.QUAD4     , EL.MECA_XT_FACE4),
        (MT.TRIA3     , EL.MECA_XT_FACE3),
    )))

phen.add('3D2XHT', Modelisation(dim=(3,3), code='3X6',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.HEXA20    , EL.MECA_XHT_HEXA20),
        (MT.PENTA15   , EL.MECA_XHT_PENTA15),
        (MT.PYRAM13   , EL.MECA_XHT_PYRAM13),
        (MT.TETRA10   , EL.MECA_XHT_TETRA10),
        (MT.QUAD8     , EL.MECA_XHT_FACE8),
        (MT.TRIA6     , EL.MECA_XHT_FACE6),
    )))

phen.add('3D2XT', Modelisation(dim=(3,3), code='3X5',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.HEXA20    , EL.MECA_XT_HEXA20),
        (MT.PENTA15   , EL.MECA_XT_PENTA15),
        (MT.PYRAM13   , EL.MECA_XT_PYRAM13),
        (MT.TETRA10   , EL.MECA_XT_TETRA10),
        (MT.QUAD8     , EL.MECA_XT_FACE8),
        (MT.TRIA6     , EL.MECA_XT_FACE6),
    )))

phen.add('3DXH1', Modelisation(dim=(3,3), code='3XA',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH1'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH1_HEXA8),
        (MT.PENTA6    , EL.MECA_XH1_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH1_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH1_TETRA4),
        (MT.QUAD4     , EL.MECA_XH1_FACE4),
        (MT.TRIA3     , EL.MECA_XH1_FACE3),
    )))

phen.add('3DXH2', Modelisation(dim=(3,3), code='3XB',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH2_HEXA8),
        (MT.PENTA6    , EL.MECA_XH2_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH2_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH2_TETRA4),
        (MT.QUAD4     , EL.MECA_XH2_FACE4),
        (MT.TRIA3     , EL.MECA_XH2_FACE3),
    )))

phen.add('3DXH2C', Modelisation(dim=(3,3), code='3XE',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH2C_HEXA8),
        (MT.PENTA6    , EL.MECA_XH2C_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH2C_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH2C_TETRA4),
    )))

phen.add('3DXH3', Modelisation(dim=(3,3), code='3XC',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH3_HEXA8),
        (MT.PENTA6    , EL.MECA_XH3_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH3_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH3_TETRA4),
        (MT.QUAD4     , EL.MECA_XH3_FACE4),
        (MT.TRIA3     , EL.MECA_XH3_FACE3),
    )))

phen.add('3DXH3C', Modelisation(dim=(3,3), code='3XF',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH3C_HEXA8),
        (MT.PENTA6    , EL.MECA_XH3C_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH3C_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH3C_TETRA4),
    )))

phen.add('3DXH4', Modelisation(dim=(3,3), code='3XD',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH4_HEXA8),
        (MT.PENTA6    , EL.MECA_XH4_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH4_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH4_TETRA4),
        (MT.QUAD4     , EL.MECA_XH4_FACE4),
        (MT.TRIA3     , EL.MECA_XH4_FACE3),
    )))

phen.add('3DXH4C', Modelisation(dim=(3,3), code='3XG',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XH4C_HEXA8),
        (MT.PENTA6    , EL.MECA_XH4C_PENTA6),
        (MT.PYRAM5    , EL.MECA_XH4C_PYRAM5),
        (MT.TETRA4    , EL.MECA_XH4C_TETRA4),
    )))

phen.add('3DXHC', Modelisation(dim=(3,3), code='3X7',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XHC_HEXA8),
        (MT.PENTA6    , EL.MECA_XHC_PENTA6),
        (MT.PYRAM5    , EL.MECA_XHC_PYRAM5),
        (MT.TETRA4    , EL.MECA_XHC_TETRA4),
        (MT.HEXA20    , EL.MECA_XHC_HEXA20),
        (MT.PENTA15   , EL.MECA_XHC_PENTA15),
        (MT.PYRAM13   , EL.MECA_XHC_PYRAM13),
        (MT.TETRA10   , EL.MECA_XHC_TETRA10),
    )))

phen.add('3DXHC3', Modelisation(dim=(3,3), code='3XH',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC3'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XHC3_HEXA8),
        (MT.PENTA6    , EL.MECA_XHC3_PENTA6),
        (MT.PYRAM5    , EL.MECA_XHC3_PYRAM5),
        (MT.TETRA4    , EL.MECA_XHC3_TETRA4),
    )))

phen.add('3DXHTC', Modelisation(dim=(3,3), code='3X9',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XHTC_HEXA8),
        (MT.PENTA6    , EL.MECA_XHTC_PENTA6),
        (MT.PYRAM5    , EL.MECA_XHTC_PYRAM5),
        (MT.TETRA4    , EL.MECA_XHTC_TETRA4),
        (MT.HEXA20    , EL.MECA_XHTC_HE20),
        (MT.PENTA15   , EL.MECA_XHTC_PE15),
        (MT.PYRAM13   , EL.MECA_XHTC_PY13),
        (MT.TETRA10   , EL.MECA_XHTC_TE10),
    )))

phen.add('3DXTC', Modelisation(dim=(3,3), code='3X8',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_XTC_HEXA8),
        (MT.PENTA6    , EL.MECA_XTC_PENTA6),
        (MT.PYRAM5    , EL.MECA_XTC_PYRAM5),
        (MT.TETRA4    , EL.MECA_XTC_TETRA4),
        (MT.HEXA20    , EL.MECA_XTC_HEXA20),
        (MT.PENTA15   , EL.MECA_XTC_PENTA15),
        (MT.PYRAM13   , EL.MECA_XTC_PYRAM13),
        (MT.TETRA10   , EL.MECA_XTC_TETRA10),
    )))

phen.add('3D_ABSO', Modelisation(dim=(2,3), code='3DA',
    attrs=(
        (AT.ABSO,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAB_FACE3),
        (MT.QUAD4     , EL.MEAB_FACE4),
        (MT.TRIA6     , EL.MEAB_FACE6),
        (MT.QUAD8     , EL.MEAB_FACE8),
        (MT.QUAD9     , EL.MEAB_FACE9),
    )))

phen.add('3D_DIL', Modelisation(dim=(3,3), code='D3D',
    elements=(
        (MT.TETRA10   , EL.T10_3D),
        (MT.PENTA15   , EL.P15_3D),
        (MT.HEXA20    , EL.H20_3D),
    )))

phen.add('3D_FAISCEAU', Modelisation(dim=(3,3), code='3DF',
    elements=(
        (MT.HEXA8     , EL.MECA_POHO_HEXA8),
        (MT.HEXA20    , EL.MECA_POHO_HEXA20),
    )))

phen.add('3D_FLUIDE', Modelisation(dim=(3,3), code='3FL',
    attrs=(
        (AT.FLUIDE,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MEFL_HEXA8),
        (MT.HEXA20    , EL.MEFL_HEXA20),
        (MT.HEXA27    , EL.MEFL_HEXA27),
        (MT.PENTA6    , EL.MEFL_PENTA6),
        (MT.PENTA15   , EL.MEFL_PENTA15),
        (MT.TETRA4    , EL.MEFL_TETRA4),
        (MT.TETRA10   , EL.MEFL_TETRA10),
        (MT.TRIA3     , EL.MEFL_FACE3),
        (MT.QUAD4     , EL.MEFL_FACE4),
        (MT.TRIA6     , EL.MEFL_FACE6),
        (MT.QUAD8     , EL.MEFL_FACE8),
        (MT.QUAD9     , EL.MEFL_FACE9),
    )))

phen.add('3D_FLUI_ABSO', Modelisation(dim=(2,3), code='3FA',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.ABSO,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEFA_FACE3),
        (MT.QUAD4     , EL.MEFA_FACE4),
        (MT.TRIA6     , EL.MEFA_FACE6),
        (MT.QUAD8     , EL.MEFA_FACE8),
        (MT.QUAD9     , EL.MEFA_FACE9),
    )))

phen.add('3D_GRAD_EPSI', Modelisation(dim=(3,3), code='3DG',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
        (AT.TYPMOD2,'GRADEPSI'),
    ),
    elements=(
        (MT.HEXA20    , EL.MGCA_HEXA20),
        (MT.TETRA4    , EL.MGCA_TETRA4),
        (MT.TETRA10   , EL.MGCA_TETRA10),
        (MT.PENTA15   , EL.MGCA_PENTA15),
        (MT.PYRAM13   , EL.MGCA_PYRAM13),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.TRIA3     , EL.MECA_FACE3),
        (MT.SEG3      , EL.MECA_ARETE3),
        (MT.SEG2      , EL.MECA_ARETE2),
    )))

phen.add('3D_GRAD_VARI', Modelisation(dim=(3,3), code='3DV',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MVCA_HEXA20),
        (MT.PENTA15   , EL.MVCA_PENTA15),
        (MT.PYRAM13   , EL.MVCA_PYRAM13),
        (MT.TETRA10   , EL.MVCA_TETRA10),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
    )))

phen.add('3D_GVNO', Modelisation(dim=(3,3), code='3GN',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MNVG_HEXA20),
        (MT.PENTA15   , EL.MNVG_PENTA15),
        (MT.PYRAM13   , EL.MNVG_PYRAM13),
        (MT.TETRA10   , EL.MNVG_TETRA10),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
    )))

phen.add('3D_HH2D', Modelisation(dim=(3,3), code='3Z4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH2_TETRA10D),
        (MT.PENTA15   , EL.HH2_PENTA15D),
        (MT.HEXA20    , EL.HH2_HEXA20D),
        (MT.QUAD8     , EL.HH2_FACE8),
        (MT.TRIA6     , EL.HH2_FACE6),
    )))

phen.add('3D_HH2MD', Modelisation(dim=(3,3), code='3A3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH2M_TETRA10D),
        (MT.PENTA15   , EL.HH2M_PENTA15D),
        (MT.HEXA20    , EL.HH2M_HEXA20D),
        (MT.QUAD8     , EL.HH2M_FACE8),
        (MT.TRIA6     , EL.HH2M_FACE6),
    )))

phen.add('3D_HH2MS', Modelisation(dim=(3,3), code='3R9',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH2M_TETRA10S),
        (MT.PENTA15   , EL.HH2M_PENTA15S),
        (MT.HEXA20    , EL.HH2M_HEXA20S),
        (MT.QUAD8     , EL.HH2M_FACE8),
        (MT.TRIA6     , EL.HH2M_FACE6),
    )))

phen.add('3D_HH2M_SI', Modelisation(dim=(3,3), code='3M1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH2M_TETRA10M),
        (MT.PENTA15   , EL.HH2M_PENTA15M),
        (MT.HEXA20    , EL.HH2M_HEXA20M),
        (MT.QUAD8     , EL.HH2M_FACE8),
        (MT.TRIA6     , EL.HH2M_FACE6),
    )))

phen.add('3D_HH2S', Modelisation(dim=(3,3), code='3Z3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH2_TETRA10S),
        (MT.PENTA15   , EL.HH2_PENTA15S),
        (MT.HEXA20    , EL.HH2_HEXA20S),
        (MT.QUAD8     , EL.HH2_FACE8),
        (MT.TRIA6     , EL.HH2_FACE6),
    )))

phen.add('3D_HH2SUDA', Modelisation(dim=(3,3), code='3AD',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'SUSHI'),
    ),
    elements=(
        (MT.HEXA27    , EL.ZHH2_HEXA27_SUDA),
        (MT.QUAD9     , EL.ZHH2_FACE9_SUDA),
    )))

phen.add('3D_HHD', Modelisation(dim=(3,3), code='3Z2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH_TETRA10D),
        (MT.PENTA15   , EL.HH_PENTA15D),
        (MT.HEXA20    , EL.HH_HEXA20D),
        (MT.QUAD8     , EL.HH_FACE8),
        (MT.TRIA6     , EL.HH_FACE6),
    )))

phen.add('3D_HHM', Modelisation(dim=(3,3), code='3H1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HHM_TETRA10),
        (MT.PENTA15   , EL.HHM_PENTA15),
        (MT.HEXA20    , EL.HHM_HEXA20),
        (MT.QUAD8     , EL.HHM_FACE8),
        (MT.TRIA6     , EL.HHM_FACE6),
    )))

phen.add('3D_HHMD', Modelisation(dim=(3,3), code='3H6',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HHM_TETRA10D),
        (MT.PENTA15   , EL.HHM_PENTA15D),
        (MT.HEXA20    , EL.HHM_HEXA20D),
        (MT.QUAD8     , EL.HHM_FACE8),
        (MT.TRIA6     , EL.HHM_FACE6),
    )))

phen.add('3D_HHMS', Modelisation(dim=(3,3), code='3R1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.HHM_TETRA10S),
        (MT.PENTA15   , EL.HHM_PENTA15S),
        (MT.HEXA20    , EL.HHM_HEXA20S),
        (MT.QUAD8     , EL.HHM_FACE8),
        (MT.TRIA6     , EL.HHM_FACE6),
    )))

phen.add('3D_HHS', Modelisation(dim=(3,3), code='3Z1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.HH_TETRA10S),
        (MT.PENTA15   , EL.HH_PENTA15S),
        (MT.HEXA20    , EL.HH_HEXA20S),
        (MT.QUAD8     , EL.HH_FACE8),
        (MT.TRIA6     , EL.HH_FACE6),
    )))

phen.add('3D_HM', Modelisation(dim=(3,3), code='3H2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10),
        (MT.PYRAM13   , EL.HM_PYRAM13),
        (MT.PENTA15   , EL.HM_PENTA15),
        (MT.HEXA20    , EL.HM_HEXA20),
        (MT.QUAD8     , EL.HM_FACE8),
        (MT.TRIA6     , EL.HM_FACE6),
    )))

phen.add('3D_HMD', Modelisation(dim=(3,3), code='3H7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10D),
        (MT.PYRAM13   , EL.HM_PYRAM13D),
        (MT.PENTA15   , EL.HM_PENTA15D),
        (MT.HEXA20    , EL.HM_HEXA20D),
        (MT.QUAD8     , EL.HM_FACE8),
        (MT.TRIA6     , EL.HM_FACE6),
    )))

phen.add('3D_HMS', Modelisation(dim=(3,3), code='3R2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10S),
        (MT.PYRAM13   , EL.HM_PYRAM13S),
        (MT.PENTA15   , EL.HM_PENTA15S),
        (MT.HEXA20    , EL.HM_HEXA20S),
        (MT.QUAD8     , EL.HM_FACE8),
        (MT.TRIA6     , EL.HM_FACE6),
    )))

phen.add('3D_HM_SI', Modelisation(dim=(3,3), code='3M2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10M),
        (MT.PYRAM13   , EL.HM_PYRAM13M),
        (MT.PENTA15   , EL.HM_PENTA15M),
        (MT.HEXA20    , EL.HM_HEXA20M),
        (MT.QUAD8     , EL.HM_FACE8),
        (MT.TRIA6     , EL.HM_FACE6),
    )))

phen.add('3D_HM_XH', Modelisation(dim=(3,3), code='3XL',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10_XH),
        (MT.PYRAM13   , EL.HM_PYRAM13_XH),
        (MT.PENTA15   , EL.HM_PENTA15_XH),
        (MT.HEXA20    , EL.HM_HEXA20_XH),
        (MT.QUAD8     , EL.HM_FACE8_XH),
        (MT.TRIA6     , EL.HM_FACE6_XH),
    )))

phen.add('3D_HM_XH1', Modelisation(dim=(3,3), code='3XM',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH1'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10_XH1),
        (MT.PYRAM13   , EL.HM_PYRAM13_XH1),
        (MT.PENTA15   , EL.HM_PENTA15_XH1),
        (MT.HEXA20    , EL.HM_HEXA20_XH1),
        (MT.QUAD8     , EL.HM_FACE8_XH1),
        (MT.TRIA6     , EL.HM_FACE6_XH1),
    )))

phen.add('3D_HM_XH2', Modelisation(dim=(3,3), code='3XN',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10_XH2),
        (MT.PYRAM13   , EL.HM_PYRAM13_XH2),
        (MT.PENTA15   , EL.HM_PENTA15_XH2),
        (MT.HEXA20    , EL.HM_HEXA20_XH2),
        (MT.QUAD8     , EL.HM_FACE8_XH2),
        (MT.TRIA6     , EL.HM_FACE6_XH2),
    )))

phen.add('3D_HM_XH3', Modelisation(dim=(3,3), code='3XO',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10_XH3),
        (MT.PYRAM13   , EL.HM_PYRAM13_XH3),
        (MT.PENTA15   , EL.HM_PENTA15_XH3),
        (MT.HEXA20    , EL.HM_HEXA20_XH3),
        (MT.QUAD8     , EL.HM_FACE8_XH3),
        (MT.TRIA6     , EL.HM_FACE6_XH3),
    )))

phen.add('3D_HM_XH_D', Modelisation(dim=(3,3), code='3XJ',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10D_XH),
        (MT.PENTA15   , EL.HM_PENTA15D_XH),
        (MT.HEXA20    , EL.HM_HEXA20D_XH),
        (MT.QUAD8     , EL.HM_FACE8_XH),
        (MT.TRIA6     , EL.HM_FACE6_XH),
    )))

phen.add('3D_HM_XH_S', Modelisation(dim=(3,3), code='3XK',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10S_XH),
        (MT.PENTA15   , EL.HM_PENTA15S_XH),
        (MT.HEXA20    , EL.HM_HEXA20S_XH),
        (MT.QUAD8     , EL.HM_FACE8_XH),
        (MT.TRIA6     , EL.HM_FACE6_XH),
    )))

phen.add('3D_HM_XH_SI', Modelisation(dim=(3,3), code='3XI',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TETRA10   , EL.HM_TETRA10M_XH),
        (MT.PENTA15   , EL.HM_PENTA15M_XH),
        (MT.HEXA20    , EL.HM_HEXA20M_XH),
        (MT.QUAD8     , EL.HM_FACE8_XH),
        (MT.TRIA6     , EL.HM_FACE6_XH),
    )))

phen.add('3D_HS', Modelisation(dim=(3,3), code='EH3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'H'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.H_TETRA10S),
        (MT.PENTA15   , EL.H_PENTA15S),
        (MT.HEXA20    , EL.H_HEXA20S),
        (MT.QUAD8     , EL.H_FACE8),
        (MT.TRIA6     , EL.H_FACE6),
    )))

phen.add('3D_INCO_UP', Modelisation(dim=(3,3), code='3UP',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.INCO,'C2'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MIUP_HEXA20),
        (MT.PENTA15   , EL.MIUP_PENTA15),
        (MT.TETRA10   , EL.MIUP_TETRA10),
        (MT.TETRA4    , EL.MIUP_TETRA4),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.TRIA3     , EL.MECA_FACE3),
        (MT.SEG3      , EL.MECA_ARETE3),
        (MT.SEG2      , EL.MECA_ARETE2),
    )))

phen.add('3D_INCO_UPG', Modelisation(dim=(3,3), code='3DI',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.INCO,'C3'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MINC_HEXA20),
        (MT.PENTA15   , EL.MINC_PENTA15),
        (MT.TETRA10   , EL.MINC_TETRA10),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
    )))

phen.add('3D_INCO_UPGB', Modelisation(dim=(3,3), code='3DB',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.INCO,'C3B'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MBNC_HEXA20),
        (MT.PENTA15   , EL.MBNC_PENTA15),
        (MT.TETRA10   , EL.MBNC_TETRA10),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
    )))

phen.add('3D_INCO_UPO', Modelisation(dim=(3,3), code='3OS',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.INCO,'C2O'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA8     , EL.MINCOS_HEXA8),
        (MT.PENTA6    , EL.MINCOS_PENTA6),
        (MT.PYRAM5    , EL.MINCOS_PYRAM5),
        (MT.TETRA4    , EL.MINCOS_TETRA4),
        (MT.QUAD4     , EL.MECA_FACE4),
        (MT.TRIA3     , EL.MECA_FACE3),
        (MT.SEG3      , EL.MECA_ARETE3),
        (MT.SEG2      , EL.MECA_ARETE2),
    )))

phen.add('3D_INTERFACE', Modelisation(dim=(3,3), code='3EI',
    attrs=(
        (AT.TYPMOD,'COMP3D'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.HEXA20    , EL.MEEI_HEXA20),
        (MT.PENTA15   , EL.MEEI_PENTA15),
    )))

phen.add('3D_INTERFACE_S', Modelisation(dim=(3,3), code='3IS',
    attrs=(
        (AT.TYPMOD,'COMP3D'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.HEXA20    , EL.MEEI_HEXS20),
        (MT.PENTA15   , EL.MEEI_PENTS15),
    )))

phen.add('3D_JOINT', Modelisation(dim=(3,3), code='3FI',
    attrs=(
        (AT.TYPMOD,'COMP3D'),
        (AT.TYPMOD2,'ELEMJOIN'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.MEFI_HEXA8),
        (MT.PENTA6    , EL.MEFI_PENTA6),
        (MT.HEXA20    , EL.MEFI_HEXA20),
        (MT.PENTA15   , EL.MEFI_PENTA15),
    )))

phen.add('3D_JOINT_HYME', Modelisation(dim=(3,3), code='3FH',
    attrs=(
        (AT.TYPMOD,'COMP3D'),
        (AT.TYPMOD2,'EJ_HYME'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.HEXA20    , EL.EJHYME_HEXA20),
        (MT.PENTA15   , EL.EJHYME_PENTA15),
    )))

phen.add('3D_SI', Modelisation(dim=(3,3), code='3DS',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'COMP3D'),
    ),
    elements=(
        (MT.HEXA20    , EL.MECA_HEXS20),
        (MT.HEXA8     , EL.MECA_HEXS8),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.QUAD4     , EL.MECA_FACE4),
        (MT.TETRA10   , EL.MECA_TETRS10),
        (MT.TRIA6     , EL.MECA_FACE6),
    )))

phen.add('3D_THH2D', Modelisation(dim=(3,3), code='3A1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH2_TETRA10D),
        (MT.PENTA15   , EL.THH2_PENTA15D),
        (MT.HEXA20    , EL.THH2_HEXA20D),
        (MT.QUAD8     , EL.THH2_FACE8),
        (MT.TRIA6     , EL.THH2_FACE6),
    )))

phen.add('3D_THH2MD', Modelisation(dim=(3,3), code='3A2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH2M_TETRA10D),
        (MT.PENTA15   , EL.THH2M_PENTA15D),
        (MT.HEXA20    , EL.THH2M_HEXA20D),
        (MT.QUAD8     , EL.THH2M_FACE8),
        (MT.TRIA6     , EL.THH2M_FACE6),
    )))

phen.add('3D_THH2MS', Modelisation(dim=(3,3), code='3R8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH2M_TETRA10S),
        (MT.PENTA15   , EL.THH2M_PENTA15S),
        (MT.HEXA20    , EL.THH2M_HEXA20S),
        (MT.QUAD8     , EL.THH2M_FACE8),
        (MT.TRIA6     , EL.THH2M_FACE6),
    )))

phen.add('3D_THH2S', Modelisation(dim=(3,3), code='3R7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH2_TETRA10S),
        (MT.PENTA15   , EL.THH2_PENTA15S),
        (MT.HEXA20    , EL.THH2_HEXA20S),
        (MT.QUAD8     , EL.THH2_FACE8),
        (MT.TRIA6     , EL.THH2_FACE6),
    )))

phen.add('3D_THHD', Modelisation(dim=(3,3), code='3H8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH_TETRA10D),
        (MT.PENTA15   , EL.THH_PENTA15D),
        (MT.HEXA20    , EL.THH_HEXA20D),
        (MT.QUAD8     , EL.THH_FACE8),
        (MT.TRIA6     , EL.THH_FACE6),
    )))

phen.add('3D_THHM', Modelisation(dim=(3,3), code='3H4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
    ),
    elements=(
        (MT.HEXA20    , EL.THHM_HEXA20),
        (MT.TETRA10   , EL.THHM_TETRA10),
        (MT.PENTA15   , EL.THHM_PENTA15),
        (MT.TRIA6     , EL.THHM_FACE6),
        (MT.QUAD8     , EL.THHM_FACE8),
    )))

phen.add('3D_THHMD', Modelisation(dim=(3,3), code='3H9',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.HEXA20    , EL.THHM_HEXA20D),
        (MT.TETRA10   , EL.THHM_TETRA10D),
        (MT.PENTA15   , EL.THHM_PENTA15D),
        (MT.TRIA6     , EL.THHM_FACE6),
        (MT.QUAD8     , EL.THHM_FACE8),
    )))

phen.add('3D_THHMS', Modelisation(dim=(3,3), code='3R5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.HEXA20    , EL.THHM_HEXA20S),
        (MT.TETRA10   , EL.THHM_TETRA10S),
        (MT.PENTA15   , EL.THHM_PENTA15S),
        (MT.TRIA6     , EL.THHM_FACE6),
        (MT.QUAD8     , EL.THHM_FACE8),
    )))

phen.add('3D_THHS', Modelisation(dim=(3,3), code='3R4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.THH_TETRA10S),
        (MT.PENTA15   , EL.THH_PENTA15S),
        (MT.HEXA20    , EL.THH_HEXA20S),
        (MT.QUAD8     , EL.THH_FACE8),
        (MT.TRIA6     , EL.THH_FACE6),
    )))

phen.add('3D_THM', Modelisation(dim=(3,3), code='3H5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
    ),
    elements=(
        (MT.TETRA10   , EL.THM_TETRA10),
        (MT.PENTA15   , EL.THM_PENTA15),
        (MT.HEXA20    , EL.THM_HEXA20),
        (MT.QUAD8     , EL.THM_FACE8),
        (MT.TRIA6     , EL.THM_FACE6),
    )))

phen.add('3D_THMD', Modelisation(dim=(3,3), code='3H0',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.TETRA10   , EL.THM_TETRA10D),
        (MT.PENTA15   , EL.THM_PENTA15D),
        (MT.HEXA20    , EL.THM_HEXA20D),
        (MT.QUAD8     , EL.THM_FACE8),
        (MT.TRIA6     , EL.THM_FACE6),
    )))

phen.add('3D_THMS', Modelisation(dim=(3,3), code='3R6',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.THM_TETRA10S),
        (MT.PENTA15   , EL.THM_PENTA15S),
        (MT.HEXA20    , EL.THM_HEXA20S),
        (MT.QUAD8     , EL.THM_FACE8),
        (MT.TRIA6     , EL.THM_FACE6),
    )))

phen.add('3D_THVD', Modelisation(dim=(3,3), code='3I3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
    ),
    elements=(
        (MT.TETRA10   , EL.THV_TETRA10D),
        (MT.PENTA15   , EL.THV_PENTA15D),
        (MT.HEXA20    , EL.THV_HEXA20D),
        (MT.QUAD8     , EL.THV_FACE8),
        (MT.TRIA6     , EL.THV_FACE6),
    )))

phen.add('3D_THVS', Modelisation(dim=(3,3), code='3R3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.TETRA10   , EL.THV_TETRA10S),
        (MT.PENTA15   , EL.THV_PENTA15S),
        (MT.HEXA20    , EL.THV_HEXA20S),
        (MT.QUAD8     , EL.THV_FACE8),
        (MT.TRIA6     , EL.THV_FACE6),
    )))

phen.add('AXIS', Modelisation(dim=(2,2), code='AX_',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAXTR3),
        (MT.QUAD4     , EL.MEAXQU4),
        (MT.SEG2      , EL.MEAXSE2),
        (MT.TRIA6     , EL.MEAXTR6),
        (MT.QUAD8     , EL.MEAXQU8),
        (MT.QUAD9     , EL.MEAXQU9),
        (MT.SEG3      , EL.MEAXSE3),
    )))

phen.add('AXIS_ELDI', Modelisation(dim=(2,2), code='ADI',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
        (AT.TYPMOD2,'ELEMDISC'),
    ),
    elements=(
        (MT.QUAD4     , EL.MDAXQU4),
        (MT.SEG2      , EL.MEAXSE2),
    )))

phen.add('AXIS_FLUIDE', Modelisation(dim=(2,2), code='AXF',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAXFLT3),
        (MT.QUAD4     , EL.MEAXFLQ4),
        (MT.TRIA6     , EL.MEAXFLT6),
        (MT.QUAD8     , EL.MEAXFLQ8),
        (MT.QUAD9     , EL.MEAXFLQ9),
        (MT.SEG2      , EL.MEAXFLS2),
        (MT.SEG3      , EL.MEAXFLS3),
    )))

phen.add('AXIS_FLUI_STRU', Modelisation(dim=(1,2), code='FSA',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.AXIS,'OUI'),
        (AT.FSI,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MEAXFSS2),
        (MT.SEG3      , EL.MEAXFSS3),
    )))

phen.add('AXIS_FOURIER', Modelisation(dim=(2,2), code='AFO',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'6'),
        (AT.FOURIER,'OUI'),
        (AT.TYPMOD,'AXIS'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEFOTR3),
        (MT.QUAD4     , EL.MEFOQU4),
        (MT.SEG2      , EL.MEFOSE2),
        (MT.TRIA6     , EL.MEFOTR6),
        (MT.QUAD8     , EL.MEFOQU8),
        (MT.QUAD9     , EL.MEFOQU9),
        (MT.SEG3      , EL.MEFOSE3),
    )))

phen.add('AXIS_GRAD_VARI', Modelisation(dim=(2,2), code='AXV',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
    ),
    elements=(
        (MT.SEG3      , EL.MEAXSE3),
        (MT.TRIA6     , EL.MVAXTR6),
        (MT.QUAD8     , EL.MVAXQS8),
    )))

phen.add('AXIS_GVNO', Modelisation(dim=(2,2), code='AGN',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
    ),
    elements=(
        (MT.SEG3      , EL.MEAXSE3),
        (MT.TRIA6     , EL.MNAXTR6),
        (MT.QUAD8     , EL.MNAXQS8),
    )))

phen.add('AXIS_HH2D', Modelisation(dim=(2,2), code='AZ4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2_AXIS_QU8D),
        (MT.TRIA6     , EL.HH2_AXIS_TR6D),
        (MT.SEG3      , EL.HH2_AXIS_SE3),
    )))

phen.add('AXIS_HH2MD', Modelisation(dim=(2,2), code='AA3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2M_AXIS_QU8D),
        (MT.TRIA6     , EL.HH2M_AXIS_TR6D),
        (MT.SEG3      , EL.HH2M_AXIS_SE3),
    )))

phen.add('AXIS_HH2MS', Modelisation(dim=(2,2), code='AR2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2M_AXIS_QU8S),
        (MT.TRIA6     , EL.HH2M_AXIS_TR6S),
        (MT.SEG3      , EL.HH2M_AXIS_SE3),
    )))

phen.add('AXIS_HH2S', Modelisation(dim=(2,2), code='AZ3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2_AXIS_QU8S),
        (MT.TRIA6     , EL.HH2_AXIS_TR6S),
        (MT.SEG3      , EL.HH2_AXIS_SE3),
    )))

phen.add('AXIS_HHD', Modelisation(dim=(2,2), code='AZ2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH_AXIS_QU8D),
        (MT.TRIA6     , EL.HH_AXIS_TR6D),
        (MT.SEG3      , EL.HH_AXIS_SE3),
    )))

phen.add('AXIS_HHM', Modelisation(dim=(2,2), code='AH1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_AXIS_QU8),
        (MT.TRIA6     , EL.HHM_AXIS_TR6),
        (MT.SEG3      , EL.HHM_AXIS_SE3),
    )))

phen.add('AXIS_HHMD', Modelisation(dim=(2,2), code='AH6',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_AXIS_QU8D),
        (MT.TRIA6     , EL.HHM_AXIS_TR6D),
        (MT.SEG3      , EL.HHM_AXIS_SE3),
    )))

phen.add('AXIS_HHMS', Modelisation(dim=(2,2), code='AR1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_AXIS_QU8S),
        (MT.TRIA6     , EL.HHM_AXIS_TR6S),
        (MT.SEG3      , EL.HHM_AXIS_SE3),
    )))

phen.add('AXIS_HHS', Modelisation(dim=(2,2), code='AZ1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH_AXIS_QU8S),
        (MT.TRIA6     , EL.HH_AXIS_TR6S),
        (MT.SEG3      , EL.HH_AXIS_SE3),
    )))

phen.add('AXIS_HM', Modelisation(dim=(2,2), code='AH2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_AXIS_QU8),
        (MT.TRIA6     , EL.HM_AXIS_TR6),
        (MT.SEG3      , EL.HM_AXIS_SE3),
    )))

phen.add('AXIS_HMD', Modelisation(dim=(2,2), code='AH7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_AXIS_QU8D),
        (MT.TRIA6     , EL.HM_AXIS_TR6D),
        (MT.SEG3      , EL.HM_AXIS_SE3),
    )))

phen.add('AXIS_HMS', Modelisation(dim=(2,2), code='AR3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_AXIS_QU8S),
        (MT.TRIA6     , EL.HM_AXIS_TR6S),
        (MT.SEG3      , EL.HM_AXIS_SE3),
    )))

phen.add('AXIS_INCO_UP', Modelisation(dim=(2,2), code='AUP',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
        (AT.INCO,'C2'),
    ),
    elements=(
        (MT.TRIA6     , EL.MUAXTR6),
        (MT.TRIA3     , EL.MUAXTR3),
        (MT.QUAD8     , EL.MUAXQU8),
        (MT.SEG3      , EL.MEAXSE3),
        (MT.SEG2      , EL.MEAXSE2),
    )))

phen.add('AXIS_INCO_UPG', Modelisation(dim=(2,2), code='AXC',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
        (AT.INCO,'C3'),
    ),
    elements=(
        (MT.TRIA6     , EL.MIAXTR6),
        (MT.QUAD8     , EL.MIAXQU8),
        (MT.SEG3      , EL.MEAXSE3),
    )))

phen.add('AXIS_INCO_UPGB', Modelisation(dim=(2,2), code='AXB',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
        (AT.INCO,'C3B'),
    ),
    elements=(
        (MT.TRIA6     , EL.MBAXTR6),
        (MT.QUAD8     , EL.MBAXQU8),
        (MT.SEG3      , EL.MEAXSE3),
    )))

phen.add('AXIS_INCO_UPO', Modelisation(dim=(2,2), code='AOS',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
        (AT.INCO,'C2O'),
    ),
    elements=(
        (MT.TRIA3     , EL.MIAXOSTR3),
        (MT.QUAD4     , EL.MIAXOSQU4),
        (MT.SEG3      , EL.MEAXSE3),
        (MT.SEG2      , EL.MEAXSE2),
    )))

phen.add('AXIS_INTERFACE', Modelisation(dim=(2,2), code='AEI',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.EIAXQU8),
    )))

phen.add('AXIS_INTERFACE_S', Modelisation(dim=(2,2), code='AIS',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.EIAXQS8),
    )))

phen.add('AXIS_JHMS', Modelisation(dim=(2,2), code='JH2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_J_AXQ8S),
        (MT.SEG3      , EL.HM_J_AXSE3),
    )))

phen.add('AXIS_JOINT', Modelisation(dim=(2,2), code='AFI',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.TYPMOD2,'ELEMJOIN'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD4     , EL.MFAXQU4),
    )))

phen.add('AXIS_SI', Modelisation(dim=(2,2), code='AXS',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.NBSIGM,'4'),
        (AT.TYPMOD,'AXIS'),
    ),
    elements=(
        (MT.QUAD8     , EL.MEAXQS8),
        (MT.SEG3      , EL.MEAXSE3),
    )))

phen.add('AXIS_THH2D', Modelisation(dim=(2,2), code='AA1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2_AXIS_QU8D),
        (MT.TRIA6     , EL.THH2_AXIS_TR6D),
        (MT.SEG3      , EL.THH2_AXIS_SE3),
    )))

phen.add('AXIS_THH2MD', Modelisation(dim=(2,2), code='AA2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2M_AXIS_QU8D),
        (MT.TRIA6     , EL.THH2M_AXIS_TR6D),
        (MT.SEG3      , EL.THH2M_AXIS_SE3),
    )))

phen.add('AXIS_THH2MS', Modelisation(dim=(2,2), code='AR7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2M_AXIS_QU8S),
        (MT.TRIA6     , EL.THH2M_AXIS_TR6S),
        (MT.SEG3      , EL.THH2M_AXIS_SE3),
    )))

phen.add('AXIS_THH2S', Modelisation(dim=(2,2), code='AR5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2_AXIS_QU8S),
        (MT.TRIA6     , EL.THH2_AXIS_TR6S),
        (MT.SEG3      , EL.THH2_AXIS_SE3),
    )))

phen.add('AXIS_THHD', Modelisation(dim=(2,2), code='AH8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH_AXIS_QU8D),
        (MT.TRIA6     , EL.THH_AXIS_TR6D),
        (MT.SEG3      , EL.THH_AXIS_SE3),
    )))

phen.add('AXIS_THHMD', Modelisation(dim=(2,2), code='AH9',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THHM_AXIS_QU8D),
        (MT.TRIA6     , EL.THHM_AXIS_TR6D),
        (MT.SEG3      , EL.THHM_AXIS_SE3),
    )))

phen.add('AXIS_THHMS', Modelisation(dim=(2,2), code='AR6',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THHM_AXIS_QU8S),
        (MT.TRIA6     , EL.THHM_AXIS_TR6S),
        (MT.SEG3      , EL.THHM_AXIS_SE3),
    )))

phen.add('AXIS_THHS', Modelisation(dim=(2,2), code='AR4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH_AXIS_QU8S),
        (MT.TRIA6     , EL.THH_AXIS_TR6S),
        (MT.SEG3      , EL.THH_AXIS_SE3),
    )))

phen.add('AXIS_THM', Modelisation(dim=(2,2), code='AH5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_AXIS_QU8),
        (MT.TRIA6     , EL.THM_AXIS_TR6),
        (MT.SEG3      , EL.THM_AXIS_SE3),
    )))

phen.add('AXIS_THMD', Modelisation(dim=(2,2), code='AH0',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_AXIS_QU8D),
        (MT.TRIA6     , EL.THM_AXIS_TR6D),
        (MT.SEG3      , EL.THM_AXIS_SE3),
    )))

phen.add('AXIS_THMS', Modelisation(dim=(2,2), code='AR8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_AXIS_QU8S),
        (MT.TRIA6     , EL.THM_AXIS_TR6S),
        (MT.SEG3      , EL.THM_AXIS_SE3),
    )))

phen.add('AXIS_THVD', Modelisation(dim=(2,2), code='AG3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THV_AXIS_QU8D),
        (MT.TRIA6     , EL.THV_AXIS_TR6D),
        (MT.SEG3      , EL.THV_AXIS_SE3),
    )))

phen.add('AXIS_THVS', Modelisation(dim=(2,2), code='AR9',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
        (AT.AXIS,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THV_AXIS_QU8S),
        (MT.TRIA6     , EL.THV_AXIS_TR6S),
        (MT.SEG3      , EL.THV_AXIS_SE3),
    )))

phen.add('AXIS_XH', Modelisation(dim=(2,2), code='AX1',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEAXTR6_XH),
        (MT.TRIA3     , EL.MEAXTR3_XH),
        (MT.QUAD8     , EL.MEAXQU8_XH),
        (MT.QUAD4     , EL.MEAXQU4_XH),
        (MT.SEG3      , EL.MEAXSE3_XH),
        (MT.SEG2      , EL.MEAXSE2_XH),
    )))

phen.add('AXIS_XHC', Modelisation(dim=(2,2), code='AX4',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAXTR3_XHC),
        (MT.TRIA6     , EL.MEAXTR6_XHC),
        (MT.QUAD4     , EL.MEAXQU4_XHC),
        (MT.QUAD8     , EL.MEAXQU8_XHC),
    )))

phen.add('AXIS_XHT', Modelisation(dim=(2,2), code='AX3',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEAXTR6_XHT),
        (MT.TRIA3     , EL.MEAXTR3_XHT),
        (MT.QUAD8     , EL.MEAXQU8_XHT),
        (MT.QUAD4     , EL.MEAXQU4_XHT),
        (MT.SEG3      , EL.MEAXSE3_XHT),
        (MT.SEG2      , EL.MEAXSE2_XHT),
    )))

phen.add('AXIS_XHTC', Modelisation(dim=(2,2), code='AX6',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAXTR3_XHTC),
        (MT.QUAD4     , EL.MEAXQU4_XHTC),
    )))

phen.add('AXIS_XT', Modelisation(dim=(2,2), code='AX2',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEAXTR6_XT),
        (MT.TRIA3     , EL.MEAXTR3_XT),
        (MT.QUAD8     , EL.MEAXQU8_XT),
        (MT.QUAD4     , EL.MEAXQU4_XT),
        (MT.SEG3      , EL.MEAXSE3_XT),
        (MT.SEG2      , EL.MEAXSE2_XT),
    )))

phen.add('AXIS_XTC', Modelisation(dim=(2,2), code='AX5',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'AXIS'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEAXTR3_XTC),
        (MT.QUAD4     , EL.MEAXQU4_XTC),
    )))

phen.add('BARRE', Modelisation(dim=(1,3), code='BAR',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_BARRE),
    )))

phen.add('CABLE', Modelisation(dim=(1,3), code='CAB',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECABL2),
    )))

phen.add('CABLE_GAINE', Modelisation(dim=(1,3), code='1GC',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG3      , EL.MECGSEG3),
    )))

phen.add('CABLE_POULIE', Modelisation(dim=(1,3), code='CAP',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG3      , EL.MEPOULI),
    )))

#------------------------------------------------------------------------------------
# Modelisations sous-terraines pour le contact/frottement en methode "continue" :
#------------------------------------------------------------------------------------

phen.add('CF_CONT1', Modelisation(dim=(2,3), code='CC1',
    attrs=(
        (AT.CONTACT,'OUI'),
    ),
    elements=(
        (MT.QU4QU8    , EL.COQ4Q8),
        (MT.QU4QU9    , EL.COQ4Q9),
        (MT.QU4TR3    , EL.COQ4T3),
        (MT.QU4TR6    , EL.COQ4T6),
        (MT.QU8QU4    , EL.COQ8Q4),
        (MT.QU8QU9    , EL.COQ8Q9),
        (MT.QU8TR3    , EL.COQ8T3),
        (MT.QU8TR6    , EL.COQ8T6),
        (MT.QU9QU4    , EL.COQ9Q4),
        (MT.QU9QU8    , EL.COQ9Q8),
        (MT.QU9TR3    , EL.COQ9T3),
        (MT.QU9TR6    , EL.COQ9T6),
        (MT.QUAD44    , EL.COQ4Q4),
        (MT.QUAD88    , EL.COQ8Q8),
        (MT.QUAD99    , EL.COQ9Q9),
        (MT.SE2QU4    , EL.COS2Q4),
        (MT.SE2QU8    , EL.COS2Q8),
        (MT.SE2QU9    , EL.COS2Q9),
        (MT.SE2TR3    , EL.COS2T3),
        (MT.SE2TR6    , EL.COS2T6),
        (MT.SE3QU4    , EL.COS3Q4),
        (MT.SE3QU8    , EL.COS3Q8),
        (MT.SE3QU9    , EL.COS3Q9),
        (MT.SE3TR3    , EL.COS3T3),
        (MT.SE3TR6    , EL.COS3T6),
        (MT.TR3QU4    , EL.COT3Q4),
        (MT.TR3QU8    , EL.COT3Q8),
        (MT.TR3QU9    , EL.COT3Q9),
        (MT.TR3TR6    , EL.COT3T6),
        (MT.TR6QU4    , EL.COT6Q4),
        (MT.TR6QU8    , EL.COT6Q8),
        (MT.TR6QU9    , EL.COT6Q9),
        (MT.TR6TR3    , EL.COT6T3),
        (MT.TRIA33    , EL.COT3T3),
        (MT.TRIA66    , EL.COT6T6),
    )))

phen.add('CF_CONT2', Modelisation(dim=(1,2), code='CC2',
    attrs=(
        (AT.CONTACT,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.COS2S2),
        (MT.SEG23     , EL.COS2S3),
        (MT.SEG32     , EL.COS3S2),
        (MT.SEG33     , EL.COS3S3),
    )))

phen.add('CF_CONT3', Modelisation(dim=(1,2), code='CC3',
    attrs=(
        (AT.CONTACT,'OUI'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.COS2S2A),
        (MT.SEG23     , EL.COS2S3A),
        (MT.SEG32     , EL.COS3S2A),
        (MT.SEG33     , EL.COS3S3A),
    )))

phen.add('CF_CONT4', Modelisation(dim=(1,2), code='CC4',
    attrs=(
        (AT.CONTACT,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.COP2P2),
    )))

phen.add('CF_CONT5', Modelisation(dim=(2,3), code='CC5',
    attrs=(
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.QU4QU8    , EL.CFQ4Q8),
        (MT.QU4QU9    , EL.CFQ4Q9),
        (MT.QU4TR3    , EL.CFQ4T3),
        (MT.QU4TR6    , EL.CFQ4T6),
        (MT.QU8QU4    , EL.CFQ8Q4),
        (MT.QU8QU9    , EL.CFQ8Q9),
        (MT.QU8TR3    , EL.CFQ8T3),
        (MT.QU8TR6    , EL.CFQ8T6),
        (MT.QU9QU4    , EL.CFQ9Q4),
        (MT.QU9QU8    , EL.CFQ9Q8),
        (MT.QU9TR3    , EL.CFQ9T3),
        (MT.QU9TR6    , EL.CFQ9T6),
        (MT.QUAD44    , EL.CFQ4Q4),
        (MT.QUAD88    , EL.CFQ8Q8),
        (MT.QUAD99    , EL.CFQ9Q9),
        (MT.SE2QU4    , EL.CFS2Q4),
        (MT.SE2QU8    , EL.CFS2Q8),
        (MT.SE2QU9    , EL.CFS2Q9),
        (MT.SE2TR3    , EL.CFS2T3),
        (MT.SE2TR6    , EL.CFS2T6),
        (MT.SE3QU4    , EL.CFS3Q4),
        (MT.SE3QU8    , EL.CFS3Q8),
        (MT.SE3QU9    , EL.CFS3Q9),
        (MT.SE3TR3    , EL.CFS3T3),
        (MT.SE3TR6    , EL.CFS3T6),
        (MT.TR3QU4    , EL.CFT3Q4),
        (MT.TR3QU8    , EL.CFT3Q8),
        (MT.TR3QU9    , EL.CFT3Q9),
        (MT.TR3TR6    , EL.CFT3T6),
        (MT.TR6QU4    , EL.CFT6Q4),
        (MT.TR6QU8    , EL.CFT6Q8),
        (MT.TR6QU9    , EL.CFT6Q9),
        (MT.TR6TR3    , EL.CFT6T3),
        (MT.TRIA33    , EL.CFT3T3),
        (MT.TRIA66    , EL.CFT6T6),
    )))

phen.add('CF_CONT6', Modelisation(dim=(1,2), code='CC6',
    attrs=(
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.CFS2S2),
        (MT.SEG23     , EL.CFS2S3),
        (MT.SEG32     , EL.CFS3S2),
        (MT.SEG33     , EL.CFS3S3),
    )))

phen.add('CF_CONT7', Modelisation(dim=(1,2), code='CC7',
    attrs=(
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.CFS2S2A),
        (MT.SEG23     , EL.CFS2S3A),
        (MT.SEG32     , EL.CFS3S2A),
        (MT.SEG33     , EL.CFS3S3A),
    )))

phen.add('CF_CONT8', Modelisation(dim=(1,2), code='CC8',
    attrs=(
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.SEG22     , EL.CFP2P2),
    )))

#------------------------------------------------------------------------------------
# Modelisations sous-terraines pour :
#  * Forces nodales
#------------------------------------------------------------------------------------

phen.add('CL_FNOD2', Modelisation(dim=(0,2), code='CL3',
    elements=(
        (MT.POI1      , EL.FORCE_NOD_2DDL),
    )))

phen.add('CL_FNOD3', Modelisation(dim=(0,3), code='CL4',
    elements=(
        (MT.POI1      , EL.FORCE_NOD_3DDL),
    )))

phen.add('CL_FNOD6', Modelisation(dim=(0,3), code='CL5',
    elements=(
        (MT.POI1      , EL.FORCE_NOD_6DDL),
    )))

phen.add('CL_FNODCQ2', Modelisation(dim=(0,2), code='CL6',
    elements=(
        (MT.POI1      , EL.FORCE_NOD_COQ2D),
    )))
#------------------------------------------------------------------------------------

phen.add('CONT_2D', Modelisation(dim=(1,2), code='CT2',
    elements=(
        (MT.SEG2      , EL.COS2E2D),
        (MT.SEG3      , EL.COS3E2D),
    )))

phen.add('CONT_3D', Modelisation(dim=(2,3), code='CT3',
    elements=(
        (MT.TRIA3     , EL.COT3E3D),
        (MT.QUAD4     , EL.COQ4E3D),
        (MT.QUAD8     , EL.COQ8E3D),
        (MT.TRIA6     , EL.COT6E3D),
        (MT.QUAD9     , EL.COQ9E3D),
        (MT.SEG2      , EL.COP2E3D),
    )))
#------------------------------------------------------------------------------------

phen.add('COQUE_3D', Modelisation(dim=(2,3), code='CQ3',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.QUAD9     , EL.MEC3QU9H),
        (MT.TRIA7     , EL.MEC3TR7H),
        (MT.SEG3      , EL.MEBOCQ3),
    )))

phen.add('COQUE_AXIS', Modelisation(dim=(1,2), code='CQA',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
    ),
    elements=(
        (MT.SEG3      , EL.MECXSE3),
    )))

phen.add('C_PLAN', Modelisation(dim=(2,2), code='CPL',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3),
        (MT.QUAD4     , EL.MECPQU4),
        (MT.TRIA6     , EL.MECPTR6),
        (MT.QUAD8     , EL.MECPQU8),
        (MT.QUAD9     , EL.MECPQU9),
        (MT.SEG2      , EL.MEPLSE2),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('C_PLAN2XH', Modelisation(dim=(2,2), code='CX4',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA6     , EL.MECPTR6_XH),
        (MT.QUAD8     , EL.MECPQU8_XH),
        (MT.SEG3      , EL.MEPLSE3_XH),
    )))

phen.add('C_PLAN2XHC', Modelisation(dim=(2,2), code='CX7',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XHC),
        (MT.TRIA6     , EL.MECPTR6_XHC),
        (MT.QUAD4     , EL.MECPQU4_XHC),
        (MT.QUAD8     , EL.MECPQU8_XHC),
    )))

phen.add('C_PLAN2XHT', Modelisation(dim=(2,2), code='CX6',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MECPTR6_XHT),
        (MT.QUAD8     , EL.MECPQU8_XHT),
        (MT.SEG3      , EL.MEPLSE3_XHT),
    )))

phen.add('C_PLAN2XHTC', Modelisation(dim=(2,2), code='CX9',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XHTC),
        (MT.QUAD4     , EL.MECPQU4_XHTC),
        (MT.TRIA6     , EL.MECPTR6_XHTC),
        (MT.QUAD8     , EL.MECPQU8_XHTC),
    )))

phen.add('C_PLAN2XT', Modelisation(dim=(2,2), code='CX5',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MECPTR6_XT),
        (MT.QUAD8     , EL.MECPQU8_XT),
        (MT.SEG3      , EL.MEPLSE3_XT),
    )))

phen.add('C_PLAN2XTC', Modelisation(dim=(2,2), code='CX8',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XTC),
        (MT.TRIA6     , EL.MECPTR6_XTC),
        (MT.QUAD4     , EL.MECPQU4_XTC),
        (MT.QUAD8     , EL.MECPQU8_XTC),
    )))

phen.add('C_PLAN_GRAD_EPSI', Modelisation(dim=(2,2), code='CPG',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
    ),
    elements=(
        (MT.SEG2      , EL.MEPLSE2),
        (MT.SEG3      , EL.MEPLSE3),
        (MT.TRIA3     , EL.MGCPTR3),
        (MT.TRIA6     , EL.MGCPTR6),
        (MT.QUAD8     , EL.MGCPQU8),
    )))

phen.add('C_PLAN_SI', Modelisation(dim=(2,2), code='CPS',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
    ),
    elements=(
        (MT.QUAD8     , EL.MECPQS8),
        (MT.QUAD4     , EL.MECPQS4),
        (MT.SEG3      , EL.MEPLSE3),
        (MT.SEG2      , EL.MEPLSE2),
    )))

phen.add('C_PLAN_XH', Modelisation(dim=(2,2), code='CX1',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA6     , EL.MECPTR6_XH),
        (MT.TRIA3     , EL.MECPTR3_XH),
        (MT.QUAD8     , EL.MECPQU8_XH),
        (MT.QUAD4     , EL.MECPQU4_XH),
        (MT.SEG3      , EL.MEPLSE3_XH),
        (MT.SEG2      , EL.MEPLSE2_XH),
    )))

phen.add('C_PLAN_XH1', Modelisation(dim=(2,2), code='CXA',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH1'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH1),
        (MT.QUAD4     , EL.MECPQU4_XH1),
    )))

phen.add('C_PLAN_XH2', Modelisation(dim=(2,2), code='CXB',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH2),
        (MT.QUAD4     , EL.MECPQU4_XH2),
    )))

phen.add('C_PLAN_XH2C', Modelisation(dim=(2,2), code='CXE',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH2C),
        (MT.QUAD4     , EL.MECPQU4_XH2C),
    )))

phen.add('C_PLAN_XH3', Modelisation(dim=(2,2), code='CXC',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH3),
        (MT.QUAD4     , EL.MECPQU4_XH3),
    )))

phen.add('C_PLAN_XH3C', Modelisation(dim=(2,2), code='CXF',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH3C),
        (MT.QUAD4     , EL.MECPQU4_XH3C),
    )))

phen.add('C_PLAN_XH4', Modelisation(dim=(2,2), code='CXD',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH4),
        (MT.QUAD4     , EL.MECPQU4_XH4),
    )))

phen.add('C_PLAN_XH4C', Modelisation(dim=(2,2), code='CXG',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XH4C),
        (MT.QUAD4     , EL.MECPQU4_XH4C),
    )))

phen.add('C_PLAN_XHC3', Modelisation(dim=(2,2), code='CXH',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC3'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XHC3),
        (MT.QUAD4     , EL.MECPQU4_XHC3),
    )))

phen.add('C_PLAN_XHT', Modelisation(dim=(2,2), code='CX3',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XHT),
        (MT.QUAD4     , EL.MECPQU4_XHT),
        (MT.SEG3      , EL.MEPLSE2_XHT),
    )))

phen.add('C_PLAN_XT', Modelisation(dim=(2,2), code='CX2',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.C_PLAN,'OUI'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA3     , EL.MECPTR3_XT),
        (MT.QUAD4     , EL.MECPQU4_XT),
        (MT.SEG3      , EL.MEPLSE2_XT),
    )))

phen.add('DIS_T', Modelisation(dim=(-1,3), code='DIT',
    attrs=(
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_DIS_T_L),
        (MT.POI1      , EL.MECA_DIS_T_N),
    )))

phen.add('DIS_TR', Modelisation(dim=(-1,3), code='DTR',
    attrs=(
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_DIS_TR_L),
        (MT.POI1      , EL.MECA_DIS_TR_N),
    )))

phen.add('DKT', Modelisation(dim=(2,3), code='DKT',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDKTR3),
        (MT.QUAD4     , EL.MEDKQU4),
        (MT.SEG2      , EL.MEBODKT),
    )))

phen.add('DKTG', Modelisation(dim=(2,3), code='DTG',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDKTG3),
        (MT.QUAD4     , EL.MEDKQG4),
        (MT.SEG2      , EL.MEBODKT),
    )))

phen.add('DST', Modelisation(dim=(2,3), code='DST',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDSTR3),
        (MT.QUAD4     , EL.MEDSQU4),
        (MT.SEG2      , EL.MEBODST),
    )))

phen.add('D_PLAN', Modelisation(dim=(2,2), code='DPL',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3),
        (MT.QUAD4     , EL.MEDPQU4),
        (MT.TRIA6     , EL.MEDPTR6),
        (MT.QUAD8     , EL.MEDPQU8),
        (MT.QUAD9     , EL.MEDPQU9),
        (MT.SEG2      , EL.MEPLSE2),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('D_PLAN2XH', Modelisation(dim=(2,2), code='DX4',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEDPTR6_XH),
        (MT.QUAD8     , EL.MEDPQU8_XH),
        (MT.SEG3      , EL.MEPLSE3_XH),
    )))

phen.add('D_PLAN2XHC', Modelisation(dim=(2,2), code='DX7',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XHC),
        (MT.TRIA6     , EL.MEDPTR6_XHC),
        (MT.QUAD4     , EL.MEDPQU4_XHC),
        (MT.QUAD8     , EL.MEDPQU8_XHC),
    )))

phen.add('D_PLAN2XHT', Modelisation(dim=(2,2), code='DX6',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEDPTR6_XHT),
        (MT.QUAD8     , EL.MEDPQU8_XHT),
        (MT.SEG3      , EL.MEPLSE3_XHT),
    )))

phen.add('D_PLAN2XHTC', Modelisation(dim=(2,2), code='DX9',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XHTC),
        (MT.QUAD4     , EL.MEDPQU4_XHTC),
        (MT.TRIA6     , EL.MEDPTR6_XHTC),
        (MT.QUAD8     , EL.MEDPQU8_XHTC),
    )))

phen.add('D_PLAN2XT', Modelisation(dim=(2,2), code='DX5',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEDPTR6_XT),
        (MT.QUAD8     , EL.MEDPQU8_XT),
        (MT.SEG3      , EL.MEPLSE3_XT),
    )))

phen.add('D_PLAN2XTC', Modelisation(dim=(2,2), code='DX8',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XTC'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XTC),
        (MT.QUAD4     , EL.MEDPQU4_XTC),
        (MT.TRIA6     , EL.MEDPTR6_XTC),
        (MT.QUAD8     , EL.MEDPQU8_XTC),
    )))

phen.add('D_PLAN_2DG', Modelisation(dim=(2,2), code='D2G',
    attrs=(
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.TRIA7     , EL.TR7_DP_2G),
        (MT.QUAD9     , EL.QU9_DP_2G),
    )))

phen.add('D_PLAN_ABSO', Modelisation(dim=(1,2), code='DPA',
    attrs=(
        (AT.ABSO,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MEPASE2),
        (MT.SEG3      , EL.MEPASE3),
    )))

phen.add('D_PLAN_DIL', Modelisation(dim=(2,2), code='D2D',
    attrs=(
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.TRIA7     , EL.TR7_DP_2D),
        (MT.TRIA6     , EL.TR6_DP_2D),
        (MT.QUAD9     , EL.QU9_DP_2D),
        (MT.QUAD8     , EL.QU8_DP_2D),
    )))

phen.add('D_PLAN_GRAD_EPSI', Modelisation(dim=(2,2), code='DPG',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.TRIA3     , EL.MGDPTR3),
        (MT.TRIA6     , EL.MGDPTR6),
        (MT.QUAD8     , EL.MGDPQU8),
        (MT.SEG2      , EL.MEPLSE2),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('D_PLAN_GRAD_SIGM', Modelisation(dim=(2,2), code='DSG',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.TRIA6     , EL.MGSPTR6),
        (MT.QUAD8     , EL.MGSPQU8),
        (MT.SEG2      , EL.MEPLSE2),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('D_PLAN_GRAD_VARI', Modelisation(dim=(2,2), code='DPV',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.SEG3      , EL.MEPLSE3),
        (MT.TRIA6     , EL.MVDPTR6),
        (MT.QUAD8     , EL.MVDPQS8),
    )))

phen.add('D_PLAN_GVNO', Modelisation(dim=(2,2), code='DGN',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.SEG3      , EL.MEPLSE3),
        (MT.TRIA6     , EL.MNDPTR6),
        (MT.QUAD8     , EL.MNDPQS8),
    )))

phen.add('D_PLAN_HH2D', Modelisation(dim=(2,2), code='DZ4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2_DPQ8D),
        (MT.TRIA6     , EL.HH2_DPTR6D),
        (MT.SEG3      , EL.HH2_DPSE3),
    )))

phen.add('D_PLAN_HH2MD', Modelisation(dim=(2,2), code='DA3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2M_DPQ8D),
        (MT.TRIA6     , EL.HH2M_DPTR6D),
        (MT.SEG3      , EL.HH2M_DPSE3),
    )))

phen.add('D_PLAN_HH2MS', Modelisation(dim=(2,2), code='DR3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2M_DPQ8S),
        (MT.TRIA6     , EL.HH2M_DPTR6S),
        (MT.SEG3      , EL.HH2M_DPSE3),
    )))

phen.add('D_PLAN_HH2M_SI', Modelisation(dim=(2,2), code='DM2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2M'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2M_DPQ8M),
        (MT.TRIA6     , EL.HH2M_DPTR6M),
        (MT.SEG3      , EL.HH2M_DPSE3),
    )))

phen.add('D_PLAN_HH2S', Modelisation(dim=(2,2), code='DZ3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH2'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH2_DPQ8S),
        (MT.TRIA6     , EL.HH2_DPTR6S),
        (MT.SEG3      , EL.HH2_DPSE3),
    )))

phen.add('D_PLAN_HH2SUDA', Modelisation(dim=(2,2), code='2DA',
    attrs=(
        (AT.D_PLAN,'OUI'),
        (AT.THM,'OUI'),
        (AT.MODTHM,'SUSHI'),
    ),
    elements=(
        (MT.TRIA7     , EL.DHH2T7_SUDA),
        (MT.QUAD9     , EL.DHH2Q9_SUDA),
        (MT.SEG3      , EL.DHH2S3_SUDA),
    )))

phen.add('D_PLAN_HHD', Modelisation(dim=(2,2), code='DZ2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH_DPQ8D),
        (MT.TRIA6     , EL.HH_DPTR6D),
        (MT.SEG3      , EL.HH_DPSE3),
    )))

phen.add('D_PLAN_HHM', Modelisation(dim=(2,2), code='DH1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_DPQ8),
        (MT.TRIA6     , EL.HHM_DPTR6),
        (MT.SEG3      , EL.HHM_DPSE3),
    )))

phen.add('D_PLAN_HHMD', Modelisation(dim=(2,2), code='DH6',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_DPQ8D),
        (MT.TRIA6     , EL.HHM_DPTR6D),
        (MT.SEG3      , EL.HHM_DPSE3),
    )))

phen.add('D_PLAN_HHMS', Modelisation(dim=(2,2), code='DR2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HHM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HHM_DPQ8S),
        (MT.TRIA6     , EL.HHM_DPTR6S),
        (MT.SEG3      , EL.HHM_DPSE3),
    )))

phen.add('D_PLAN_HHS', Modelisation(dim=(2,2), code='DZ1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HH'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HH_DPQ8S),
        (MT.TRIA6     , EL.HH_DPTR6S),
        (MT.SEG3      , EL.HH_DPSE3),
    )))

phen.add('D_PLAN_HM', Modelisation(dim=(2,2), code='DH2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8),
        (MT.TRIA6     , EL.HM_DPTR6),
        (MT.SEG3      , EL.HM_DPSE3),
    )))

phen.add('D_PLAN_HMD', Modelisation(dim=(2,2), code='DH7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8D),
        (MT.TRIA6     , EL.HM_DPTR6D),
        (MT.SEG3      , EL.HM_DPSE3),
    )))

phen.add('D_PLAN_HMS', Modelisation(dim=(2,2), code='DR1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8S),
        (MT.TRIA6     , EL.HM_DPTR6S),
        (MT.SEG3      , EL.HM_DPSE3),
    )))

phen.add('D_PLAN_HM_P', Modelisation(dim=(2,2), code='DHB',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8_P),
        (MT.TRIA6     , EL.HM_DPTR6_P),
        (MT.SEG3      , EL.HM_DPSE3_P),
    )))

phen.add('D_PLAN_HM_SI', Modelisation(dim=(2,2), code='DM1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8M),
        (MT.TRIA6     , EL.HM_DPTR6M),
        (MT.SEG3      , EL.HM_DPSE3),
    )))

phen.add('D_PLAN_HM_XH', Modelisation(dim=(2,2), code='DXI',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8_XH),
        (MT.TRIA6     , EL.HM_DPTR6_XH),
        (MT.SEG3      , EL.HM_DPSE3_XH),
    )))

phen.add('D_PLAN_HM_XH1', Modelisation(dim=(2,2), code='DXM',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH1'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8_XH1),
        (MT.TRIA6     , EL.HM_DPTR6_XH1),
        (MT.SEG3      , EL.HM_DPSE3_XH1),
    )))

phen.add('D_PLAN_HM_XH2', Modelisation(dim=(2,2), code='DXN',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8_XH2),
        (MT.TRIA6     , EL.HM_DPTR6_XH2),
        (MT.SEG3      , EL.HM_DPSE3_XH2),
    )))

phen.add('D_PLAN_HM_XH3', Modelisation(dim=(2,2), code='DXO',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8_XH3),
        (MT.TRIA6     , EL.HM_DPTR6_XH3),
        (MT.SEG3      , EL.HM_DPSE3_XH3),
    )))

phen.add('D_PLAN_HM_XH_D', Modelisation(dim=(2,2), code='DXK',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8D_XH),
        (MT.TRIA6     , EL.HM_DPTR6D_XH),
        (MT.SEG3      , EL.HM_DPSE3_XH),
    )))

phen.add('D_PLAN_HM_XH_S', Modelisation(dim=(2,2), code='DXL',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8S_XH),
        (MT.TRIA6     , EL.HM_DPTR6S_XH),
        (MT.SEG3      , EL.HM_DPSE3_XH),
    )))

phen.add('D_PLAN_HM_XH_SI', Modelisation(dim=(2,2), code='DXJ',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_DPQ8M_XH),
        (MT.TRIA6     , EL.HM_DPTR6M_XH),
        (MT.SEG3      , EL.HM_DPSE3_XH),
    )))

phen.add('D_PLAN_HS', Modelisation(dim=(2,2), code='DHA',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'H'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.H_DPQ8S),
        (MT.TRIA6     , EL.H_DPTR6S),
        (MT.SEG3      , EL.H_DPSE3),
    )))

phen.add('D_PLAN_INCO_UP', Modelisation(dim=(2,2), code='PUP',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.INCO,'C2'),
    ),
    elements=(
        (MT.TRIA6     , EL.MUPLTR6),
        (MT.TRIA3     , EL.MUPLTR3),
        (MT.QUAD8     , EL.MUPLQU8),
        (MT.SEG3      , EL.MEPLSE3),
        (MT.SEG2      , EL.MEPLSE2),
    )))

phen.add('D_PLAN_INCO_UPG', Modelisation(dim=(2,2), code='PLI',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.INCO,'C3'),
    ),
    elements=(
        (MT.TRIA6     , EL.MIPLTR6),
        (MT.QUAD8     , EL.MIPLQU8),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('D_PLAN_INCO_UPGB', Modelisation(dim=(2,2), code='PLB',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.INCO,'C3B'),
    ),
    elements=(
        (MT.TRIA6     , EL.MBPLTR6),
        (MT.QUAD8     , EL.MBPLQU8),
        (MT.SEG3      , EL.MEPLSE3),
    )))

phen.add('D_PLAN_INCO_UPO', Modelisation(dim=(2,2), code='POS',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.INCO,'C2O'),
    ),
    elements=(
        (MT.TRIA3     , EL.MIPLOSTR3),
        (MT.QUAD4     , EL.MIPLOSQU4),
        (MT.SEG3      , EL.MEPLSE3),
        (MT.SEG2      , EL.MEPLSE2),
    )))

phen.add('D_PLAN_SI', Modelisation(dim=(2,2), code='DPS',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
    ),
    elements=(
        (MT.QUAD8     , EL.MEDPQS8),
        (MT.QUAD4     , EL.MEDPQS4),
        (MT.SEG3      , EL.MEPLSE3),
        (MT.SEG2      , EL.MEPLSE2),
    )))

phen.add('D_PLAN_THH2D', Modelisation(dim=(2,2), code='DA1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2_DPQ8D),
        (MT.TRIA6     , EL.THH2_DPTR6D),
        (MT.SEG3      , EL.THH2_DPSE3),
    )))

phen.add('D_PLAN_THH2MD', Modelisation(dim=(2,2), code='DA2',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2M_DPQ8D),
        (MT.TRIA6     , EL.THH2M_DPTR6D),
        (MT.SEG3      , EL.THH2M_DPSE3),
    )))

phen.add('D_PLAN_THH2MS', Modelisation(dim=(2,2), code='DR7',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2M'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2M_DPQ8S),
        (MT.TRIA6     , EL.THH2M_DPTR6S),
        (MT.SEG3      , EL.THH2M_DPSE3),
    )))

phen.add('D_PLAN_THH2S', Modelisation(dim=(2,2), code='DR5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH2'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH2_DPQ8S),
        (MT.TRIA6     , EL.THH2_DPTR6S),
        (MT.SEG3      , EL.THH2_DPSE3),
    )))

phen.add('D_PLAN_THHD', Modelisation(dim=(2,2), code='DH8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH_DPQ8D),
        (MT.TRIA6     , EL.THH_DPTR6D),
        (MT.SEG3      , EL.THH_DPSE3),
    )))

phen.add('D_PLAN_THHMD', Modelisation(dim=(2,2), code='DH9',
    attrs=(
        (AT.D_PLAN,'OUI'),
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THHM_DPQ8D),
        (MT.TRIA6     , EL.THHM_DPTR6D),
        (MT.SEG3      , EL.THHM_DPSE3),
    )))

phen.add('D_PLAN_THHMS', Modelisation(dim=(2,2), code='DR6',
    attrs=(
        (AT.D_PLAN,'OUI'),
        (AT.THM,'OUI'),
        (AT.MODTHM,'THHM'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THHM_DPQ8S),
        (MT.TRIA6     , EL.THHM_DPTR6S),
        (MT.SEG3      , EL.THHM_DPSE3),
    )))

phen.add('D_PLAN_THHS', Modelisation(dim=(2,2), code='DR4',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THH'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THH_DPQ8S),
        (MT.TRIA6     , EL.THH_DPTR6S),
        (MT.SEG3      , EL.THH_DPSE3),
    )))

phen.add('D_PLAN_THM', Modelisation(dim=(2,2), code='DH5',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.D_PLAN,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_DPQ8),
        (MT.TRIA6     , EL.THM_DPTR6),
        (MT.SEG3      , EL.THM_DPSE3),
    )))

phen.add('D_PLAN_THMD', Modelisation(dim=(2,2), code='DH0',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_DPQ8D),
        (MT.TRIA6     , EL.THM_DPTR6D),
        (MT.SEG3      , EL.THM_DPSE3),
    )))

phen.add('D_PLAN_THMS', Modelisation(dim=(2,2), code='DR8',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THM_DPQ8S),
        (MT.TRIA6     , EL.THM_DPTR6S),
        (MT.SEG3      , EL.THM_DPSE3),
    )))

phen.add('D_PLAN_THVD', Modelisation(dim=(2,2), code='DG3',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'LUM'),
    ),
    elements=(
        (MT.QUAD8     , EL.THV_DPQ8D),
        (MT.TRIA6     , EL.THV_DPTR6D),
        (MT.SEG3      , EL.THV_DPSE3),
    )))

phen.add('D_PLAN_THVS', Modelisation(dim=(2,2), code='DR9',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'THV'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.THV_DPQ8S),
        (MT.TRIA6     , EL.THV_DPTR6S),
        (MT.SEG3      , EL.THV_DPSE3),
    )))

phen.add('D_PLAN_XH', Modelisation(dim=(2,2), code='DX1',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA6     , EL.MEDPTR6_XH),
        (MT.TRIA3     , EL.MEDPTR3_XH),
        (MT.QUAD8     , EL.MEDPQU8_XH),
        (MT.QUAD4     , EL.MEDPQU4_XH),
        (MT.SEG3      , EL.MEPLSE3_XH),
        (MT.SEG2      , EL.MEPLSE2_XH),
    )))

phen.add('D_PLAN_XH1', Modelisation(dim=(2,2), code='DXA',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH1'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH1),
        (MT.QUAD4     , EL.MEDPQU4_XH1),
        (MT.SEG2      , EL.MEPLSE2_XH1),
    )))

phen.add('D_PLAN_XH2', Modelisation(dim=(2,2), code='DXB',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH2),
        (MT.QUAD4     , EL.MEDPQU4_XH2),
        (MT.SEG2      , EL.MEPLSE2_XH2),
    )))

phen.add('D_PLAN_XH2C', Modelisation(dim=(2,2), code='DXE',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH2C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH2C),
        (MT.QUAD4     , EL.MEDPQU4_XH2C),
    )))

phen.add('D_PLAN_XH3', Modelisation(dim=(2,2), code='DXC',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH3),
        (MT.QUAD4     , EL.MEDPQU4_XH3),
        (MT.SEG2      , EL.MEPLSE2_XH3),
    )))

phen.add('D_PLAN_XH3C', Modelisation(dim=(2,2), code='DXF',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH3C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH3C),
        (MT.QUAD4     , EL.MEDPQU4_XH3C),
    )))

phen.add('D_PLAN_XH4', Modelisation(dim=(2,2), code='DXD',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH4),
        (MT.QUAD4     , EL.MEDPQU4_XH4),
        (MT.SEG2      , EL.MEPLSE2_XH4),
    )))

phen.add('D_PLAN_XH4C', Modelisation(dim=(2,2), code='DXG',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH4C'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XH4C),
        (MT.QUAD4     , EL.MEDPQU4_XH4C),
    )))

phen.add('D_PLAN_XHC3', Modelisation(dim=(2,2), code='DXH',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHC3'),
        (AT.XLAG,'NOEUD'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XHC3),
        (MT.QUAD4     , EL.MEDPQU4_XHC3),
    )))

phen.add('D_PLAN_XHT', Modelisation(dim=(2,2), code='DX3',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XHT),
        (MT.QUAD4     , EL.MEDPQU4_XHT),
        (MT.SEG3      , EL.MEPLSE2_XHT),
    )))

phen.add('D_PLAN_XT', Modelisation(dim=(2,2), code='DX2',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'D_PLAN'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEDPTR3_XT),
        (MT.QUAD4     , EL.MEDPQU4_XT),
        (MT.SEG3      , EL.MEPLSE2_XT),
    )))

phen.add('FLUI_STRU', Modelisation(dim=(3,3), code='FLS',
    attrs=(
        (AT.FLUIDE,'OUI'),
        (AT.FSI,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEFS_FACE3),
        (MT.QUAD4     , EL.MEFS_FACE4),
        (MT.TRIA6     , EL.MEFS_FACE6),
        (MT.QUAD8     , EL.MEFS_FACE8),
        (MT.QUAD9     , EL.MEFS_FACE9),
    )))

phen.add('FROT_2D', Modelisation(dim=(1,2), code='CF2',
    elements=(
        (MT.SEG2      , EL.CFS2E2D),
        (MT.SEG3      , EL.CFS3E2D),
    )))

phen.add('FROT_3D', Modelisation(dim=(2,3), code='CF3',
    elements=(
        (MT.TRIA3     , EL.CFT3E3D),
        (MT.QUAD4     , EL.CFQ4E3D),
        (MT.QUAD8     , EL.CFQ8E3D),
        (MT.TRIA6     , EL.CFT6E3D),
        (MT.QUAD9     , EL.CFQ9E3D),
        (MT.SEG2      , EL.CFP2E3D),
    )))

phen.add('GRILLE_EXCENTRE', Modelisation(dim=(2,3), code='GRC',
    attrs=(
        (AT.GRILLE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.COQUE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEGCTR3),
        (MT.QUAD4     , EL.MEGCQU4),
    )))

phen.add('GRILLE_MEMBRANE', Modelisation(dim=(2,3), code='GRM',
    attrs=(
        (AT.GRILLE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.COQUE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEGMTR3),
        (MT.TRIA6     , EL.MEGMTR6),
        (MT.QUAD4     , EL.MEGMQU4),
        (MT.QUAD8     , EL.MEGMQU8),
    )))

phen.add('MEMBRANE', Modelisation(dim=(2,3), code='MMB',
    attrs=(
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.TRIA3     , EL.MEMBTR3),
        (MT.TRIA6     , EL.MEMBTR6),
        (MT.QUAD4     , EL.MEMBQU4),
        (MT.QUAD8     , EL.MEMBQU8),
    )))

phen.add('PLAN_ELDI', Modelisation(dim=(2,2), code='PDI',
    attrs=(
        (AT.NBSIGM,'4'),
        (AT.D_PLAN,'OUI'),
        (AT.TYPMOD,'PLAN'),
        (AT.TYPMOD2,'ELEMDISC'),
    ),
    elements=(
        (MT.QUAD4     , EL.MDDPQU4),
        (MT.SEG2      , EL.MEPLSE2),
    )))

phen.add('PLAN_INTERFACE', Modelisation(dim=(2,2), code='PEI',
    attrs=(
        (AT.TYPMOD,'PLAN'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.EIPLQU8),
    )))

phen.add('PLAN_INTERFACE_S', Modelisation(dim=(2,2), code='PIS',
    attrs=(
        (AT.TYPMOD,'PLAN'),
        (AT.TYPMOD2,'INTERFAC'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.EIPLQS8),
    )))

phen.add('PLAN_JHMS', Modelisation(dim=(2,2), code='JH1',
    attrs=(
        (AT.THM,'OUI'),
        (AT.MODTHM,'HM'),
        (AT.D_PLAN,'OUI'),
        (AT.INTTHM,'RED'),
    ),
    elements=(
        (MT.QUAD8     , EL.HM_J_DPQ8S),
        (MT.SEG3      , EL.HM_J_DPSE3),
    )))

phen.add('PLAN_JOINT', Modelisation(dim=(2,2), code='PFI',
    attrs=(
        (AT.TYPMOD,'PLAN'),
        (AT.TYPMOD2,'ELEMJOIN'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD4     , EL.MFPLQU4),
        (MT.QUAD8     , EL.MFPLQU8),
    )))

phen.add('PLAN_JOINT_HYME', Modelisation(dim=(2,2), code='PFH',
    attrs=(
        (AT.TYPMOD,'PLAN'),
        (AT.TYPMOD2,'EJ_HYME'),
        (AT.INTERFACE,'OUI'),
    ),
    elements=(
        (MT.QUAD8     , EL.EJHYME_PLQU8),
    )))

phen.add('POU_D_E', Modelisation(dim=(1,3), code='PDE',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EULER,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_E),
    )))

phen.add('POU_D_EM', Modelisation(dim=(1,3), code='PFM',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.TYPMOD2,'PMF'),
        (AT.EULER,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_EM),
    )))

phen.add('POU_D_T', Modelisation(dim=(1,3), code='PDT',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_T),
    )))

phen.add('POU_D_TG', Modelisation(dim=(1,3), code='PDG',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_TG),
    )))

phen.add('POU_D_TGM', Modelisation(dim=(1,3), code='PGM',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TYPMOD,'COMP1D'),
        (AT.TYPMOD2,'PMF'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_TGM),
    )))

phen.add('POU_D_T_GD', Modelisation(dim=(1,3), code='PGD',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.SEG2      , EL.MECA_POU_D_T_GD),
    )))

phen.add('POU_FLUI_STRU', Modelisation(dim=(1,3), code='FS1',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.FLUIDE,'OUI'),
        (AT.FSI,'OUI'),
    ),
    elements=(
        (MT.SEG2      , EL.MEFS_POU_D_T),
    )))

phen.add('Q4G', Modelisation(dim=(2,3), code='Q4G',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
    ),
    elements=(
        (MT.QUAD4     , EL.MEQ4QU4),
        (MT.TRIA3     , EL.MET3TR3),
        (MT.SEG2      , EL.MEBOQ4G),
    )))

phen.add('Q4GG', Modelisation(dim=(2,3), code='Q4S',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.COQUE,'OUI'),
        (AT.EFGE,'OUI'),
        (AT.SIGM,'NON'),
    ),
    elements=(
        (MT.QUAD4     , EL.MEQ4GG4),
        (MT.TRIA3     , EL.MET3GG3),
        (MT.SEG2      , EL.MEBOQ4G),
    )))

phen.add('SHB', Modelisation(dim=(3,3), code='SHB',
    attrs=(
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
    ),
    elements=(
        (MT.HEXA8     , EL.MECA_SHB8),
        (MT.QUAD4     , EL.MECA_FACE4),
        (MT.SEG2      , EL.MECA_ARETE2),
        (MT.PENTA6    , EL.MECA_SHB6),
        (MT.TRIA3     , EL.MECA_FACE3),
        (MT.PENTA15   , EL.MECA_SHB15),
        (MT.QUAD8     , EL.MECA_FACE8),
        (MT.TRIA6     , EL.MECA_FACE6),
        (MT.SEG3      , EL.MECA_ARETE3),
        (MT.HEXA20    , EL.MECA_SHB20),
    )))

phen.add('TUYAU_3M', Modelisation(dim=(1,3), code='TU3',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TUYAU,'OUI'),
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.SEG3      , EL.MET3SEG3),
        (MT.SEG4      , EL.MET3SEG4),
    )))

phen.add('TUYAU_6M', Modelisation(dim=(1,3), code='TU6',
    attrs=(
        (AT.POUTRE,'OUI'),
        (AT.TUYAU,'OUI'),
        (AT.NBSIGM,'6'),
        (AT.TYPMOD,'C_PLAN'),
        (AT.EFGE,'OUI'),
        (AT.SOUS_POINT,'OUI'),
    ),
    elements=(
        (MT.SEG3      , EL.MET6SEG3),
    )))

phen.add('XFEM_GG_C_C_A', Modelisation(dim=(2,2), code='Y34',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MECPQ4CQ4C_XH),
        (MT.TR3TR3    , EL.MECPT3CT3C_XH),
    )))

phen.add('XFEM_GG_C_C_B', Modelisation(dim=(3,3), code='Y35',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.HE8HE8    , EL.ME3DH8CH8C_XH),
        (MT.PE6PE6    , EL.ME3DP6CP6C_XH),
        (MT.TE4TE4    , EL.ME3DT4CT4C_XH),
    )))

phen.add('XFEM_GG_C_C_C', Modelisation(dim=(2,2), code='Y36',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MEDPQ4CQ4C_XH),
        (MT.TR3TR3    , EL.MEDPT3CT3C_XH),
    )))

phen.add('XFEM_GG_C_H_A', Modelisation(dim=(2,2), code='Y01',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MECPQ4CQ4H_XH),
        (MT.TR3TR3    , EL.MECPT3CT3H_XH),
    )))

phen.add('XFEM_GG_C_H_B', Modelisation(dim=(3,3), code='Y02',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.HE8HE8    , EL.ME3DH8CH8H_XH),
        (MT.PE6PE6    , EL.ME3DP6CP6H_XH),
        (MT.TE4TE4    , EL.ME3DT4CT4H_XH),
    )))

phen.add('XFEM_GG_C_H_C', Modelisation(dim=(2,2), code='Y03',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'C'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MEDPQ4CQ4H_XH),
        (MT.TR3TR3    , EL.MEDPT3CT3H_XH),
    )))

phen.add('XFEM_GG_H2_H2_A', Modelisation(dim=(2,2), code='Y04',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT32T32_XH),
        (MT.QU4QU4    , EL.MECPQ42Q42_XH),
    )))

phen.add('XFEM_GG_H2_H2_B', Modelisation(dim=(2,2), code='Y05',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT32T32_XH),
        (MT.QU4QU4    , EL.MEDPQ42Q42_XH),
    )))

phen.add('XFEM_GG_H2_H2_C', Modelisation(dim=(3,3), code='Y06',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT42T42_XH),
        (MT.PE6PE6    , EL.ME3DP62P62_XH),
        (MT.HE8HE8    , EL.ME3DH82H82_XH),
    )))

phen.add('XFEM_GG_H2_H3_A', Modelisation(dim=(2,2), code='Y10',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT32T33_XH),
        (MT.QU4QU4    , EL.MECPQ42Q43_XH),
    )))

phen.add('XFEM_GG_H2_H3_B', Modelisation(dim=(2,2), code='Y11',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT32T33_XH),
        (MT.QU4QU4    , EL.MEDPQ42Q43_XH),
    )))

phen.add('XFEM_GG_H2_H3_C', Modelisation(dim=(3,3), code='Y12',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT42T43_XH),
        (MT.PE6PE6    , EL.ME3DP62P63_XH),
        (MT.HE8HE8    , EL.ME3DH82H83_XH),
    )))

phen.add('XFEM_GG_H2_H4_A', Modelisation(dim=(2,2), code='Y55',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT32T34_XH),
        (MT.QU4QU4    , EL.MECPQ42Q44_XH),
    )))

phen.add('XFEM_GG_H2_H4_B', Modelisation(dim=(2,2), code='Y56',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT32T34_XH),
        (MT.QU4QU4    , EL.MEDPQ42Q44_XH),
    )))

phen.add('XFEM_GG_H2_H4_C', Modelisation(dim=(3,3), code='Y57',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT42T44_XH),
        (MT.PE6PE6    , EL.ME3DP62P64_XH),
        (MT.HE8HE8    , EL.ME3DH82H84_XH),
    )))

phen.add('XFEM_GG_H2_H_A', Modelisation(dim=(2,2), code='Y13',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT32T3H_XH),
        (MT.QU4QU4    , EL.MECPQ42Q4H_XH),
    )))

phen.add('XFEM_GG_H2_H_B', Modelisation(dim=(2,2), code='Y14',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT32T3H_XH),
        (MT.QU4QU4    , EL.MEDPQ42Q4H_XH),
    )))

phen.add('XFEM_GG_H2_H_C', Modelisation(dim=(3,3), code='Y15',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H2'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT42T4H_XH),
        (MT.PE6PE6    , EL.ME3DP62P6H_XH),
        (MT.HE8HE8    , EL.ME3DH82H8H_XH),
    )))

phen.add('XFEM_GG_H3_H2_A', Modelisation(dim=(2,2), code='Y40',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT33T32_XH),
        (MT.QU4QU4    , EL.MECPQ43Q42_XH),
    )))

phen.add('XFEM_GG_H3_H2_B', Modelisation(dim=(2,2), code='Y41',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT33T32_XH),
        (MT.QU4QU4    , EL.MEDPQ43Q42_XH),
    )))

phen.add('XFEM_GG_H3_H2_C', Modelisation(dim=(3,3), code='Y42',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT43T42_XH),
        (MT.PE6PE6    , EL.ME3DP63P62_XH),
        (MT.HE8HE8    , EL.ME3DH83H82_XH),
    )))

phen.add('XFEM_GG_H3_H3_A', Modelisation(dim=(2,2), code='Y52',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT33T33_XH),
        (MT.QU4QU4    , EL.MECPQ43Q43_XH),
    )))

phen.add('XFEM_GG_H3_H3_B', Modelisation(dim=(2,2), code='Y53',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT33T33_XH),
        (MT.QU4QU4    , EL.MEDPQ43Q43_XH),
    )))

phen.add('XFEM_GG_H3_H3_C', Modelisation(dim=(3,3), code='Y54',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT43T43_XH),
        (MT.PE6PE6    , EL.ME3DP63P63_XH),
        (MT.HE8HE8    , EL.ME3DH83H83_XH),
    )))

phen.add('XFEM_GG_H3_H4_A', Modelisation(dim=(2,2), code='Y07',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT33T34_XH),
        (MT.QU4QU4    , EL.MECPQ43Q44_XH),
    )))

phen.add('XFEM_GG_H3_H4_B', Modelisation(dim=(2,2), code='Y08',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT33T34_XH),
        (MT.QU4QU4    , EL.MEDPQ43Q44_XH),
    )))

phen.add('XFEM_GG_H3_H4_C', Modelisation(dim=(3,3), code='Y09',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT43T44_XH),
        (MT.PE6PE6    , EL.ME3DP63P64_XH),
        (MT.HE8HE8    , EL.ME3DH83H84_XH),
    )))

phen.add('XFEM_GG_H3_H_A', Modelisation(dim=(2,2), code='Y28',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT33T3H_XH),
        (MT.QU4QU4    , EL.MECPQ43Q4H_XH),
    )))

phen.add('XFEM_GG_H3_H_B', Modelisation(dim=(2,2), code='Y29',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT33T3H_XH),
        (MT.QU4QU4    , EL.MEDPQ43Q4H_XH),
    )))

phen.add('XFEM_GG_H3_H_C', Modelisation(dim=(3,3), code='Y30',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H3'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT43T4H_XH),
        (MT.PE6PE6    , EL.ME3DP63P6H_XH),
        (MT.HE8HE8    , EL.ME3DH83H8H_XH),
    )))

phen.add('XFEM_GG_H4_H2_A', Modelisation(dim=(2,2), code='Y58',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT34T32_XH),
        (MT.QU4QU4    , EL.MECPQ44Q42_XH),
    )))

phen.add('XFEM_GG_H4_H2_B', Modelisation(dim=(2,2), code='Y59',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT34T32_XH),
        (MT.QU4QU4    , EL.MEDPQ44Q42_XH),
    )))

phen.add('XFEM_GG_H4_H2_C', Modelisation(dim=(3,3), code='Y60',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT44T42_XH),
        (MT.PE6PE6    , EL.ME3DP64P62_XH),
        (MT.HE8HE8    , EL.ME3DH84H82_XH),
    )))

phen.add('XFEM_GG_H4_H3_A', Modelisation(dim=(2,2), code='Y25',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT34T33_XH),
        (MT.QU4QU4    , EL.MECPQ44Q43_XH),
    )))

phen.add('XFEM_GG_H4_H3_B', Modelisation(dim=(2,2), code='Y26',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT34T33_XH),
        (MT.QU4QU4    , EL.MEDPQ44Q43_XH),
    )))

phen.add('XFEM_GG_H4_H3_C', Modelisation(dim=(3,3), code='Y27',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT44T43_XH),
        (MT.PE6PE6    , EL.ME3DP64P63_XH),
        (MT.HE8HE8    , EL.ME3DH84H83_XH),
    )))

phen.add('XFEM_GG_H4_H4_A', Modelisation(dim=(2,2), code='Y43',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT34T34_XH),
        (MT.QU4QU4    , EL.MECPQ44Q44_XH),
    )))

phen.add('XFEM_GG_H4_H4_B', Modelisation(dim=(2,2), code='Y44',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT34T34_XH),
        (MT.QU4QU4    , EL.MEDPQ44Q44_XH),
    )))

phen.add('XFEM_GG_H4_H4_C', Modelisation(dim=(3,3), code='Y45',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT44T44_XH),
        (MT.PE6PE6    , EL.ME3DP64P64_XH),
        (MT.HE8HE8    , EL.ME3DH84H84_XH),
    )))

phen.add('XFEM_GG_H4_H_A', Modelisation(dim=(2,2), code='Y31',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT34T3H_XH),
        (MT.QU4QU4    , EL.MECPQ44Q4H_XH),
    )))

phen.add('XFEM_GG_H4_H_B', Modelisation(dim=(2,2), code='Y32',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT34T3H_XH),
        (MT.QU4QU4    , EL.MEDPQ44Q4H_XH),
    )))

phen.add('XFEM_GG_H4_H_C', Modelisation(dim=(3,3), code='Y33',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H4'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT44T4H_XH),
        (MT.PE6PE6    , EL.ME3DP64P6H_XH),
        (MT.HE8HE8    , EL.ME3DH84H8H_XH),
    )))

phen.add('XFEM_GG_H_C_A', Modelisation(dim=(2,2), code='Y46',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MECPQ4HQ4C_XH),
        (MT.TR3TR3    , EL.MECPT3HT3C_XH),
    )))

phen.add('XFEM_GG_H_C_B', Modelisation(dim=(3,3), code='Y47',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.HE8HE8    , EL.ME3DH8HH8C_XH),
        (MT.PE6PE6    , EL.ME3DP6HP6C_XH),
        (MT.TE4TE4    , EL.ME3DT4HT4C_XH),
    )))

phen.add('XFEM_GG_H_C_C', Modelisation(dim=(2,2), code='Y48',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'C'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MEDPQ4HQ4C_XH),
        (MT.TR3TR3    , EL.MEDPT3HT3C_XH),
    )))

phen.add('XFEM_GG_H_H2_A', Modelisation(dim=(2,2), code='Y22',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT3HT32_XH),
        (MT.QU4QU4    , EL.MECPQ4HQ42_XH),
    )))

phen.add('XFEM_GG_H_H2_B', Modelisation(dim=(2,2), code='Y23',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT3HT32_XH),
        (MT.QU4QU4    , EL.MEDPQ4HQ42_XH),
    )))

phen.add('XFEM_GG_H_H2_C', Modelisation(dim=(3,3), code='Y24',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H2'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT4HT42_XH),
        (MT.PE6PE6    , EL.ME3DP6HP62_XH),
        (MT.HE8HE8    , EL.ME3DH8HH82_XH),
    )))

phen.add('XFEM_GG_H_H3_A', Modelisation(dim=(2,2), code='Y19',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT3HT33_XH),
        (MT.QU4QU4    , EL.MECPQ4HQ43_XH),
    )))

phen.add('XFEM_GG_H_H3_B', Modelisation(dim=(2,2), code='Y20',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT3HT33_XH),
        (MT.QU4QU4    , EL.MEDPQ4HQ43_XH),
    )))

phen.add('XFEM_GG_H_H3_C', Modelisation(dim=(3,3), code='Y21',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H3'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT4HT43_XH),
        (MT.PE6PE6    , EL.ME3DP6HP63_XH),
        (MT.HE8HE8    , EL.ME3DH8HH83_XH),
    )))

phen.add('XFEM_GG_H_H4_A', Modelisation(dim=(2,2), code='Y37',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MECPT3HT34_XH),
        (MT.QU4QU4    , EL.MECPQ4HQ44_XH),
    )))

phen.add('XFEM_GG_H_H4_B', Modelisation(dim=(2,2), code='Y38',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TR3TR3    , EL.MEDPT3HT34_XH),
        (MT.QU4QU4    , EL.MEDPQ4HQ44_XH),
    )))

phen.add('XFEM_GG_H_H4_C', Modelisation(dim=(3,3), code='Y39',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H4'),
    ),
    elements=(
        (MT.TE4TE4    , EL.ME3DT4HT44_XH),
        (MT.PE6PE6    , EL.ME3DP6HP64_XH),
        (MT.HE8HE8    , EL.ME3DH8HH84_XH),
    )))

phen.add('XFEM_GG_H_H_A', Modelisation(dim=(2,2), code='Y16',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MECPQ4HQ4H_XH),
        (MT.TR3TR3    , EL.MECPT3HT3H_XH),
        (MT.QU4TR3    , EL.MECPQ4HT3H_XH),
        (MT.TR3QU4    , EL.MECPT3HQ4H_XH),
        (MT.QU8QU8    , EL.MECPQ8HQ8H_XH),
        (MT.TR6TR6    , EL.MECPT6HT6H_XH),
    )))

phen.add('XFEM_GG_H_H_B', Modelisation(dim=(3,3), code='Y17',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.HE8HE8    , EL.ME3DH8HH8H_XH),
        (MT.PE6PE6    , EL.ME3DP6HP6H_XH),
        (MT.TE4TE4    , EL.ME3DT4HT4H_XH),
        (MT.H20H20    , EL.ME3DHVHHVH_XH),
        (MT.P15P15    , EL.ME3DPQHPQH_XH),
        (MT.T10T10    , EL.ME3DTDHTDH_XH),
    )))

phen.add('XFEM_GG_H_H_C', Modelisation(dim=(2,2), code='Y18',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'H'),
        (AT.XFEM_M,'H'),
    ),
    elements=(
        (MT.QU4QU4    , EL.MEDPQ4HQ4H_XH),
        (MT.TR3TR3    , EL.MEDPT3HT3H_XH),
        (MT.QU4TR3    , EL.MEDPQ4HT3H_XH),
        (MT.TR3QU4    , EL.MEDPT3HQ4H_XH),
        (MT.QU8QU8    , EL.MEDPQ8HQ8H_XH),
        (MT.TR6TR6    , EL.MEDPT6HT6H_XH),
    )))

phen.add('XFEM_GG_T_T_A', Modelisation(dim=(2,2), code='Y49',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'T'),
        (AT.XFEM_M,'T'),
    ),
    elements=(
        (MT.QUAD4     , EL.MECPQ4T_XH),
        (MT.TRIA3     , EL.MECPT3T_XH),
    )))

phen.add('XFEM_GG_T_T_B', Modelisation(dim=(3,3), code='Y50',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'T'),
        (AT.XFEM_M,'T'),
    ),
    elements=(
        (MT.HEXA8     , EL.ME3DH8T_XH),
        (MT.PENTA6    , EL.ME3DP6T_XH),
        (MT.TETRA4    , EL.ME3DT4T_XH),
    )))

phen.add('XFEM_GG_T_T_C', Modelisation(dim=(2,2), code='Y51',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.GRAND_GLIS,'OUI'),
        (AT.CONTACT,'OUI'),
        (AT.FROTTEMENT,'OUI'),
        (AT.XFEM_E,'T'),
        (AT.XFEM_M,'T'),
    ),
    elements=(
        (MT.QUAD4     , EL.MEDPQ4T_XH),
        (MT.TRIA3     , EL.MEDPT3T_XH),
    )))


############################################################
# Pour le phenomene : THERMIQUE :
############################################################
THERMIQUE = Phenomenon(code='TH')
phen = THERMIQUE

phen.add('3D', Modelisation(dim=(3,3), code='3D_',
    elements=(
        (MT.HEXA8     , EL.THER_HEXA8),
        (MT.PENTA6    , EL.THER_PENTA6),
        (MT.TETRA4    , EL.THER_TETRA4),
        (MT.PYRAM5    , EL.THER_PYRAM5),
        (MT.QUAD4     , EL.THER_FACE4),
        (MT.TRIA3     , EL.THER_FACE3),
        (MT.HEXA27    , EL.THER_HEXA27),
        (MT.HEXA20    , EL.THER_HEXA20),
        (MT.PENTA15   , EL.THER_PENTA15),
        (MT.TETRA10   , EL.THER_TETRA10),
        (MT.PYRAM13   , EL.THER_PYRAM13),
        (MT.QUAD9     , EL.THER_FACE9),
        (MT.QUAD8     , EL.THER_FACE8),
        (MT.TRIA6     , EL.THER_FACE6),
    )))

phen.add('3D1XH', Modelisation(dim=(3,3), code='3X1',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.HEXA8     , EL.THER_XH_HEXA8),
        (MT.PENTA6    , EL.THER_XH_PENTA6),
        (MT.PYRAM5    , EL.THER_XH_PYRAM5),
        (MT.TETRA4    , EL.THER_XH_TETRA4),
        (MT.QUAD4     , EL.THER_XH_FACE4),
        (MT.TRIA3     , EL.THER_XH_FACE3),
    )))

phen.add('3D1XHT', Modelisation(dim=(3,3), code='3X3',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.HEXA8     , EL.THER_XHT_HEXA8),
        (MT.PENTA6    , EL.THER_XHT_PENTA6),
        (MT.PYRAM5    , EL.THER_XHT_PYRAM5),
        (MT.TETRA4    , EL.THER_XHT_TETRA4),
        (MT.QUAD4     , EL.THER_XHT_FACE4),
        (MT.TRIA3     , EL.THER_XHT_FACE3),
    )))

phen.add('3D1XT', Modelisation(dim=(3,3), code='3X2',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.HEXA8     , EL.THER_XT_HEXA8),
        (MT.PENTA6    , EL.THER_XT_PENTA6),
        (MT.PYRAM5    , EL.THER_XT_PYRAM5),
        (MT.TETRA4    , EL.THER_XT_TETRA4),
        (MT.QUAD4     , EL.THER_XT_FACE4),
        (MT.TRIA3     , EL.THER_XT_FACE3),
    )))

phen.add('3D_DIAG', Modelisation(dim=(3,3), code='3DD',
    attrs=(
        (AT.LUMPE,'OUI'),
    ),
    elements=(
        (MT.HEXA8     , EL.THER_HEXA8_D),
        (MT.PENTA6    , EL.THER_PENTA6_D),
        (MT.TETRA4    , EL.THER_TETRA4_D),
        (MT.QUAD4     , EL.THER_FACE4_D),
        (MT.TRIA3     , EL.THER_FACE3_D),
    )))

phen.add('AXIS', Modelisation(dim=(2,2), code='AX_',
    attrs=(
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.THAXTR3),
        (MT.QUAD4     , EL.THAXQU4),
        (MT.SEG2      , EL.THAXSE2),
        (MT.TRIA6     , EL.THAXTR6),
        (MT.QUAD8     , EL.THAXQU8),
        (MT.QUAD9     , EL.THAXQU9),
        (MT.SEG3      , EL.THAXSE3),
    )))

phen.add('AXIS_DIAG', Modelisation(dim=(2,2), code='AXD',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.LUMPE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.THAXTL3),
        (MT.QUAD4     , EL.THAXQL4),
        (MT.SEG2      , EL.THAXSL2),
        (MT.TRIA6     , EL.THAXTL6),
        (MT.QUAD9     , EL.THAXQL9),
        (MT.SEG3      , EL.THAXSL3),
    )))

phen.add('AXIS_FOURIER', Modelisation(dim=(2,2), code='AXF',
    attrs=(
        (AT.AXIS,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.THFOTR3),
        (MT.QUAD4     , EL.THFOQU4),
        (MT.SEG2      , EL.THFOSE2),
        (MT.TRIA6     , EL.THFOTR6),
        (MT.QUAD8     , EL.THFOQU8),
        (MT.QUAD9     , EL.THFOQU9),
        (MT.SEG3      , EL.THFOSE3),
    )))

phen.add('AXIS_XH', Modelisation(dim=(2,2), code='AX1',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA3     , EL.THAXTR3_XH),
        (MT.QUAD4     , EL.THAXQU4_XH),
        (MT.SEG2      , EL.THAXSE2_XH),
    )))

phen.add('AXIS_XHT', Modelisation(dim=(2,2), code='AX3',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA3     , EL.THAXTR3_XHT),
        (MT.QUAD4     , EL.THAXQU4_XHT),
        (MT.SEG2      , EL.THAXSE2_XHT),
    )))

phen.add('AXIS_XT', Modelisation(dim=(2,2), code='AX2',
    attrs=(
        (AT.AXIS,'OUI'),
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA3     , EL.THAXTR3_XT),
        (MT.QUAD4     , EL.THAXQU4_XT),
        (MT.SEG2      , EL.THAXSE2_XT),
    )))

#-------------------------------------------------------------------
# Modelisations sous-terraines pour :
#  * conditions d'echange thermique entre deux parois

phen.add('CL_ECHANGE1', Modelisation(dim=(2,3), code='CL1',
    elements=(
        (MT.TRIA33    , EL.THER_FACE33),
        (MT.QUAD44    , EL.THER_FACE44),
        (MT.TRIA66    , EL.THER_FACE66),
        (MT.QUAD88    , EL.THER_FACE88),
        (MT.QUAD99    , EL.THER_FACE99),
    )))

phen.add('CL_ECHANGE2', Modelisation(dim=(1,2), code='CL2',
    elements=(
        (MT.SEG22     , EL.THAXSE22),
        (MT.SEG33     , EL.THAXSE33),
        (MT.SEG22     , EL.THPLSE22),
        (MT.SEG33     , EL.THPLSE33),
    )))
#-------------------------------------------------------------------

phen.add('COQUE', Modelisation(dim=(2,3), code='CQ_',
    attrs=(
        (AT.COQUE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.THCOTR3),
        (MT.TRIA6     , EL.THCOTR6),
        (MT.TRIA7     , EL.THCOTR7),
        (MT.QUAD4     , EL.THCOQU4),
        (MT.QUAD8     , EL.THCOQU8),
        (MT.QUAD9     , EL.THCOQU9),
        (MT.SEG3      , EL.THCOSE3),
        (MT.SEG2      , EL.THCOSE2),
    )))

phen.add('COQUE_AXIS', Modelisation(dim=(1,2), code='CQA',
    attrs=(
        (AT.COQUE,'OUI'),
    ),
    elements=(
        (MT.SEG3      , EL.THCASE3),
    )))

phen.add('COQUE_PLAN', Modelisation(dim=(1,2), code='CQP',
    attrs=(
        (AT.COQUE,'OUI'),
    ),
    elements=(
        (MT.SEG3      , EL.THCPSE3),
    )))

phen.add('PLAN', Modelisation(dim=(2,2), code='PL_',
    elements=(
        (MT.TRIA3     , EL.THPLTR3),
        (MT.QUAD4     , EL.THPLQU4),
        (MT.SEG2      , EL.THPLSE2),
        (MT.TRIA6     , EL.THPLTR6),
        (MT.QUAD8     , EL.THPLQU8),
        (MT.QUAD9     , EL.THPLQU9),
        (MT.SEG3      , EL.THPLSE3),
    )))

phen.add('PLAN_DIAG', Modelisation(dim=(2,2), code='PLD',
    attrs=(
        (AT.LUMPE,'OUI'),
    ),
    elements=(
        (MT.TRIA3     , EL.THPLTL3),
        (MT.QUAD4     , EL.THPLQL4),
        (MT.SEG2      , EL.THPLSL2),
        (MT.TRIA6     , EL.THPLTL6),
        (MT.QUAD9     , EL.THPLQL9),
        (MT.SEG3      , EL.THPLSL3),
    )))

phen.add('PLAN_XH', Modelisation(dim=(2,2), code='PX1',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XH'),
    ),
    elements=(
        (MT.TRIA3     , EL.THPLTR3_XH),
        (MT.QUAD4     , EL.THPLQU4_XH),
        (MT.SEG2      , EL.THPLSE2_XH),
    )))

phen.add('PLAN_XHT', Modelisation(dim=(2,2), code='PX3',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XHT'),
    ),
    elements=(
        (MT.TRIA3     , EL.THPLTR3_XHT),
        (MT.QUAD4     , EL.THPLQU4_XHT),
        (MT.SEG2      , EL.THPLSE2_XHT),
    )))

phen.add('PLAN_XT', Modelisation(dim=(2,2), code='PX2',
    attrs=(
        (AT.LXFEM,'OUI'),
        (AT.XFEM,'XT'),
    ),
    elements=(
        (MT.TRIA3     , EL.THPLTR3_XT),
        (MT.QUAD4     , EL.THPLQU4_XT),
        (MT.SEG2      , EL.THPLSE2_XT),
    )))


############################################################
# Pour le phenomene : ACOUSTIQUE :
############################################################
ACOUSTIQUE = Phenomenon(code='AC')
phen = ACOUSTIQUE

phen.add('3D', Modelisation(dim=(3,3), code='3D_',
    elements=(
        (MT.HEXA8     , EL.ACOU_HEXA8),
        (MT.PENTA6    , EL.ACOU_PENTA6),
        (MT.TETRA4    , EL.ACOU_TETRA4),
        (MT.QUAD4     , EL.ACOU_FACE4),
        (MT.TRIA3     , EL.ACOU_FACE3),
        (MT.HEXA27    , EL.ACOU_HEXA27),
        (MT.HEXA20    , EL.ACOU_HEXA20),
        (MT.PENTA15   , EL.ACOU_PENTA15),
        (MT.TETRA10   , EL.ACOU_TETRA10),
        (MT.QUAD9     , EL.ACOU_FACE9),
        (MT.QUAD8     , EL.ACOU_FACE8),
        (MT.TRIA6     , EL.ACOU_FACE6),
    )))

phen.add('PLAN', Modelisation(dim=(2,2), code='PLA',
    elements=(
        (MT.TRIA3     , EL.ACPLTR3),
        (MT.TRIA6     , EL.ACPLTR6),
        (MT.QUAD4     , EL.ACPLQU4),
        (MT.QUAD8     , EL.ACPLQU8),
        (MT.QUAD9     , EL.ACPLQU9),
        (MT.SEG2      , EL.ACPLSE2),
        (MT.SEG3      , EL.ACPLSE3),
    )))


############################################################
# Pour le phenomene : PRESENTATION :
############################################################
PRESENTATION = Phenomenon(code='PR')
phen = PRESENTATION

# Les deux modelisations suivantes servent pour des calculs purement
# geometriques lies a XFEM

phen.add('2D_GEOM', Modelisation(dim=(2,2), code='G2D',
    elements=(
        (MT.QUAD8     , EL.PR_G_QUAD8),
        (MT.QUAD4     , EL.PR_G_QUAD4),
        (MT.TRIA6     , EL.PR_G_TRIA6),
        (MT.TRIA3     , EL.PR_G_TRIA3),
    )))

phen.add('3D_GEOM', Modelisation(dim=(3,3), code='G3D',
    elements=(
        (MT.HEXA20    , EL.PR_G_HEXA20),
        (MT.HEXA8     , EL.PR_G_HEXA8),
        (MT.PENTA15   , EL.PR_G_PENTA15),
        (MT.PENTA6    , EL.PR_G_PENTA6),
        (MT.TETRA10   , EL.PR_G_TETRA10),
        (MT.TETRA4    , EL.PR_G_TETRA4),
        (MT.PYRAM13   , EL.PR_G_PYRAM13),
        (MT.PYRAM5    , EL.PR_G_PYRAM5),
    )))

# La modelisation 'TOUT' sert dans IMPR_RESU
phen.add('TOUT', Modelisation(dim=(3,3), code='TOU',
    elements=(
        (MT.HEXA27    , EL.PR_HEXA27),
        (MT.HEXA20    , EL.PR_HEXA20),
        (MT.HEXA8     , EL.PR_HEXA8),
        (MT.PENTA18   , EL.PR_PENTA18),
        (MT.PENTA15   , EL.PR_PENTA15),
        (MT.PENTA6    , EL.PR_PENTA6),
        (MT.TETRA10   , EL.PR_TETRA10),
        (MT.TETRA4    , EL.PR_TETRA4),
        (MT.PYRAM13   , EL.PR_PYRAM13),
        (MT.PYRAM5    , EL.PR_PYRAM5),
        (MT.QUAD9     , EL.PR_QUAD9),
        (MT.QUAD8     , EL.PR_QUAD8),
        (MT.QUAD4     , EL.PR_QUAD4),
        (MT.TRIA7     , EL.PR_TRIA7),
        (MT.TRIA6     , EL.PR_TRIA6),
        (MT.TRIA3     , EL.PR_TRIA3),
        (MT.SEG4      , EL.PR_SEG4),
        (MT.SEG3      , EL.PR_SEG3),
        (MT.SEG2      , EL.PR_SEG2),
        (MT.POI1      , EL.PR_POI1),
    )))


# store Phenomenon objects
PHEN = objects_from_context(globals(), Phenomenon, ignore_names=['phen', ])
