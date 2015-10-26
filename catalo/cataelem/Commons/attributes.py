
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

from cataelem.Tools.base_objects import Attribute, objects_from_context

PRINCIPAL = Attribute(value=('OUI',))
DIM_TOPO_MAILLE = Attribute(value=('0', '1', '2', '3'))

ABSO    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


ALIAS8  = Attribute(auto=True, value=(
     'MEDTRSE2' ,
    ),
    comment="""
    ...
""")


AXIS    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


BORD    = Attribute( value=(
     '0' ,
     '-1' ,
     '-2' ,
    ),
    comment="""
    ...
""")

BORD_ISO    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


CL_DUAL = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


CONTACT   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


COQUE   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


C_PLAN  = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


DIM_COOR_MODELI= Attribute(auto=True, value=(
     '2' ,
     '3' ,
    ),
    comment="""
    ...
""")


DIM_TOPO_MODELI= Attribute(auto=True, value=(
     '0' ,
     '1' ,
     '2' ,
     '3' ,
    ),
    comment="""
    ...
""")


DISCRET = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


D_PLAN  = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


EFGE    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


EULER   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


FOURIER = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


FROTTEMENT= Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


GRAND_GLIS  = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


GRILLE  = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


INCO    = Attribute( value=(
     'C2' ,
     'C2O' ,
     'C3' ,
     'C3B' ,
    ),
    comment="""
    ...
""")


INTERFACE= Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


INTTHM  = Attribute( value=(
     'LUM' ,
     'RED' ,
    ),
    comment="""
    ...
""")


LUMPE   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


LXFEM   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


METH_CONTINUE= Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


MODELI  = Attribute(auto=True, value=(
     '2FA' ,
     '2FL' ,
     '2FP' ,
     '3FA' ,
     '3FI' ,
     '3FL' ,
     'AFI' ,
     'AXF' ,
     'CL1' ,
     'CL2' ,
     'D2D' ,
     'D3D' ,
     'DIT' ,
     'FLS' ,
     'FS2' ,
     'FSA' ,
     'PFI' ,
    ),
    comment="""
    ...
""")


MODTHM  = Attribute( value=(
     'H' ,
     'HH' ,
     'HH2' ,
     'HH2M' ,
     'HHM' ,
     'HM' ,
     'SUSHI' ,
     'THH' ,
     'THH2' ,
     'THH2M' ,
     'THHM' ,
     'THM' ,
     'THV' ,
    ),
    comment="""
    ...
""")


NBSIGM  = Attribute( value=(
     '4' ,
     '6' ,
    ),
    comment="""
    ...
""")


PHENO   = Attribute( value=(
     'AC' ,
     'ME' ,
     'PR' ,
     'TH' ,
    ),
    comment="""
    ...
""")


POUTRE    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


SIGM    = Attribute( value=(
     'NON' ,
    ),
    comment="""
    ...
""")


SOUS_POINT    = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


THM     = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


TUYAU   = Attribute( value=(
     'OUI' ,
    ),
    comment="""
    ...
""")


TYPE_VOISIN= Attribute( value=(
     'A2' ,
     'F3' ,
    ),
    comment="""
    ...
""")


TYPMOD  = Attribute( value=(
     'AXIS' ,
     'COMP1D' ,
     'COMP3D' ,
     'C_PLAN' ,
     'D_PLAN' ,
     'PLAN' ,
    ),
    comment="""
    ...
""")


TYPMOD2 = Attribute( value=(
     'EJ_HYME' ,
     'ELEMDISC' ,
     'ELEMJOIN' ,
     'GRADEPSI' ,
     'INTERFAC' ,
     'PMF' ,
    ),
    comment="""
    ...
""")


XFEM    = Attribute( value=(
     'XH' ,
     'XH1' ,
     'XH2' ,
     'XH2C' ,
     'XH3' ,
     'XH3C' ,
     'XH4' ,
     'XH4C' ,
     'XHC' ,
     'XHC3' ,
     'XHT' ,
     'XHTC' ,
     'XT' ,
     'XTC' ,
    ),
    comment="""
    ...
""")


XFEM_E  = Attribute( value=(
     'C' ,
     'H' ,
     'H2' ,
     'H3' ,
     'H4' ,
     'T' ,
    ),
    comment="""
    ...
""")


XFEM_M  = Attribute( value=(
     'C' ,
     'H' ,
     'H2' ,
     'H3' ,
     'H4' ,
     'T' ,
    ),
    comment="""
    ...
""")


XLAG    = Attribute( value=(
     'NOEUD' ,
    ),
    comment="""
    ...
""")


# store all Attribute objects
ATTRS = objects_from_context(globals(), Attribute)
