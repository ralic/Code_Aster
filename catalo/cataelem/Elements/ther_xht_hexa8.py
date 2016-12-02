# coding=utf-8
# person_in_charge: sam.cuvilliez at edf.fr
# CATALOGUES DES ELEMENTS THERMIQUES 3D X-FEM HEAVISIDE-CRACKTIP (LINEAIRES)

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

#----------------
# Modes locaux :
#----------------


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA','KHI','R',
          'RHO',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E33NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
    components=('X[33]',))


E1NEUTK  = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
    components=('Z1',))


ETEMXPG  = LocatedComponents(phys=PHY.TEMP_R, type='ELGA', location='XFEM',
    components=('TEMP','DTX','DTY','DTZ',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP','H1','E1',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=DDL_THER)

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=DDL_THER)


#------------------------------------------------------------
class THER_XHT_HEXA8(Element):
    """Please document this element"""
    meshType = MT.HEXA8
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','XFEM=XFEM480',), mater=('XFEM',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','XGEO=FPG5',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','XCON=FPG12',),),
        )
    calculs = (

        OP.CHAR_THER_EVOL(te=577,
            para_in=((OP.CHAR_THER_EVOL.PBASLOR, LC.N9NEUT_R), (OP.CHAR_THER_EVOL.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_THER_EVOL.PHEAVTO, LC.E32NEUTI),
                     (OP.CHAR_THER_EVOL.PHEA_NO, LC.N5NEUTI), (OP.CHAR_THER_EVOL.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_THER_EVOL.PLSN, LC.N1NEUT_R), (OP.CHAR_THER_EVOL.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_THER_EVOL.PPINTTO, E33NEUTR),
                     (OP.CHAR_THER_EVOL.PSTANO, STANO_I), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_THER_EVOL.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_PARO_F(te=599,
            para_in=((OP.CHAR_THER_PARO_F.PAINTER, LC.E200NEUT), (OP.CHAR_THER_PARO_F.PBASECO, LC.E360NEUT),
                     (OP.CHAR_THER_PARO_F.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_THER_PARO_F.PHEA_NO, LC.N5NEUTI), (SP.PHECHPF, LC.CHECHPF),
                     (OP.CHAR_THER_PARO_F.PLONGCO, LC.E3NEUTI), (OP.CHAR_THER_PARO_F.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_THER_PARO_F.PLST, LC.N1NEUT_R), (OP.CHAR_THER_PARO_F.PPINTER, LC.E120NEUT),
                     (OP.CHAR_THER_PARO_F.PSTANO, STANO_I), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_PARO_R(te=599,
            para_in=((OP.CHAR_THER_PARO_R.PAINTER, LC.E200NEUT), (OP.CHAR_THER_PARO_R.PBASECO, LC.E360NEUT),
                     (OP.CHAR_THER_PARO_R.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_THER_PARO_R.PHEA_NO, LC.N5NEUTI), (SP.PHECHPR, LC.EHECHPR),
                     (OP.CHAR_THER_PARO_R.PLONGCO, LC.E3NEUTI), (OP.CHAR_THER_PARO_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_THER_PARO_R.PLST, LC.N1NEUT_R), (OP.CHAR_THER_PARO_R.PPINTER, LC.E120NEUT),
                     (OP.CHAR_THER_PARO_R.PSTANO, STANO_I), (SP.PTEMPER, DDL_THER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N9NEUT_R), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
                     ),
        ),

        OP.MASS_THER(te=572,
            para_in=((OP.MASS_THER.PBASLOR, LC.N9NEUT_R), (OP.MASS_THER.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.MASS_THER.PHEAVTO, LC.E32NEUTI),
                     (OP.MASS_THER.PHEA_NO, LC.N5NEUTI), (OP.MASS_THER.PLONCHA, LC.E10NEUTI),
                     (OP.MASS_THER.PLSN, LC.N1NEUT_R), (OP.MASS_THER.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.MASS_THER.PPINTTO, E33NEUTR),
                     (OP.MASS_THER.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.MASS_THER.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.MASS_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER(te=571,
            para_in=((OP.RIGI_THER.PBASLOR, LC.N9NEUT_R), (OP.RIGI_THER.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_THER.PHEAVTO, LC.E32NEUTI),
                     (OP.RIGI_THER.PHEA_NO, LC.N5NEUTI), (OP.RIGI_THER.PLONCHA, LC.E10NEUTI),
                     (OP.RIGI_THER.PLSN, LC.N1NEUT_R), (OP.RIGI_THER.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_THER.PPINTTO, E33NEUTR),
                     (OP.RIGI_THER.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     (OP.RIGI_THER.PVARCPR, LC.ZVARCPG), ),
            para_out=((OP.RIGI_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_PARO_F(te=594,
            para_in=((OP.RIGI_THER_PARO_F.PAINTER, LC.E200NEUT), (OP.RIGI_THER_PARO_F.PBASECO, LC.E360NEUT),
                     (OP.RIGI_THER_PARO_F.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_THER_PARO_F.PHEA_NO, LC.N5NEUTI), (SP.PHECHPF, LC.CHECHPF),
                     (OP.RIGI_THER_PARO_F.PLONGCO, LC.E3NEUTI), (OP.RIGI_THER_PARO_F.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_THER_PARO_F.PLST, LC.N1NEUT_R), (OP.RIGI_THER_PARO_F.PPINTER, LC.E120NEUT),
                     (OP.RIGI_THER_PARO_F.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.RIGI_THER_PARO_F.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_PARO_R(te=594,
            para_in=((OP.RIGI_THER_PARO_R.PAINTER, LC.E200NEUT), (OP.RIGI_THER_PARO_R.PBASECO, LC.E360NEUT),
                     (OP.RIGI_THER_PARO_R.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_THER_PARO_R.PHEA_NO, LC.N5NEUTI), (SP.PHECHPR, LC.EHECHPR),
                     (OP.RIGI_THER_PARO_R.PLONGCO, LC.E3NEUTI), (OP.RIGI_THER_PARO_R.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_THER_PARO_R.PLST, LC.N1NEUT_R), (OP.RIGI_THER_PARO_R.PPINTER, LC.E120NEUT),
                     (OP.RIGI_THER_PARO_R.PSTANO, STANO_I), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.RIGI_THER_PARO_R.PMATTTR, MMATTTR), ),
        ),

        OP.TEMP_ELGA(te=578,
            para_in=((OP.TEMP_ELGA.PBASLOR, LC.N9NEUT_R), (OP.TEMP_ELGA.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.TEMP_ELGA.PHEAVTO, LC.E32NEUTI),
                     (OP.TEMP_ELGA.PHEA_NO, LC.N5NEUTI), (OP.TEMP_ELGA.PLONCHA, LC.E10NEUTI),
                     (OP.TEMP_ELGA.PLSN, LC.N1NEUT_R), (OP.TEMP_ELGA.PLST, LC.N1NEUT_R),
                     (OP.TEMP_ELGA.PPINTTO, E33NEUTR), (SP.PTEMPER, DDL_THER),
                     ),
            para_out=((SP.PTEMP_R, ETEMXPG), ),
        ),

        OP.TOPOFA(te=510,
            para_in=((OP.TOPOFA.PAINTTO, LC.E55NEUTR), (OP.TOPOFA.PCNSETO, LC.E320NEUI),
                     (SP.PDECOU, E1NEUTK), (SP.PGEOMER, NGEOMER),
                     (SP.PGRADLN, LC.N3NEUT_R), (SP.PGRADLT, LC.N3NEUT_R),
                     (OP.TOPOFA.PHEAVTO, LC.E32NEUTI), (OP.TOPOFA.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOFA.PLSN, LC.N1NEUT_R), (OP.TOPOFA.PLST, LC.N1NEUT_R),
                     (OP.TOPOFA.PPINTTO, E33NEUTR), (OP.TOPOFA.PPMILTO, LC.E198NEUT),
                     ),
            para_out=((OP.TOPOFA.PAINTER, LC.E200NEUT), (OP.TOPOFA.PBASECO, LC.E360NEUT),
                     (OP.TOPOFA.PCFACE, LC.E90NEUTI), (SP.PGESCLA, LC.E120NEUT),
                     (OP.TOPOFA.PLONGCO, LC.E3NEUTI), (OP.TOPOFA.PPINTER, LC.E120NEUT),
                     ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, LC.E320NEUI), (OP.TOPONO.PHEAVTO, LC.E32NEUTI),
                     (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, LC.E32NEUTI),
                     ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
                     ),
            para_out=((OP.TOPOSE.PAINTTO, LC.E55NEUTR), (OP.TOPOSE.PCNSETO, LC.E320NEUI),
                     (OP.TOPOSE.PHEAVTO, LC.E32NEUTI), (OP.TOPOSE.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOSE.PPINTTO, E33NEUTR), (OP.TOPOSE.PPMILTO, LC.E198NEUT),
                     ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM3D), ),
        ),


        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )


#------------------------------------------------------------
class THER_XHT_PENTA6(THER_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PENTA6
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','XFEM=XFEM240',), mater=('XFEM',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','XGEO=FPG5',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','XCON=FPG12',),),
        )


#------------------------------------------------------------
class THER_XHT_PYRAM5(THER_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PYRAM5
    elrefe =(
            ElrefeLoc(MT.PY5, gauss = ('RIGI=FPG5','XFEM=XFEM180',), mater=('XFEM',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','XGEO=FPG5',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','XCON=FPG12',),),
        )


#------------------------------------------------------------
class THER_XHT_TETRA4(THER_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.TETRA4
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG1','XINT=FPG15','XGEO=FPG5','XFEM=XFEM90',), mater=('XFEM',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','XCON=FPG12',),),
        )
