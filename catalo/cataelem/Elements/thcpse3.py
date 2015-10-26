
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
from cataelem.Tools.base_objects import Calcul, Element
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP

#----------------
# Modes locaux :
#----------------


CCACOQU  = LocatedComponents(phys=PHY.CACOQU, type='ELEM',
    components=('EP',))


CCOEFHF  = LocatedComponents(phys=PHY.COEH_F, type='ELEM',
    components=('H_INF','H_SUP',))


CCOEFHR  = LocatedComponents(phys=PHY.COEH_R, type='ELEM',
    components=('H_INF','H_SUP',))


CFLUXNF  = LocatedComponents(phys=PHY.FLUN_F, type='ELEM',
    components=('FLUN_INF','FLUN_SUP',))


CFLUXNR  = LocatedComponents(phys=PHY.FLUN_R, type='ELEM',
    components=('FLUN_INF','FLUN_SUP',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST','DELTAT','THETA',))


CT_EXTF  = LocatedComponents(phys=PHY.TEMP_F, type='ELEM',
    components=('TEMP','TEMP_INF','TEMP_SUP',))


DDL_THER = LocatedComponents(phys=PHY.TEMP_R, type='ELNO',
    components=('TEMP_MIL','TEMP_INF','TEMP_SUP',))


MVECTTR  = ArrayOfComponents(phys=PHY.VTEM_R, locatedComponents=(DDL_THER,))

MMATTTR  = ArrayOfComponents(phys=PHY.MTEM_R, locatedComponents=(DDL_THER,DDL_THER))


#------------------------------------------------------------
class THCPSE3(Element):
    """Please document this element"""
    meshType = MT.SEG3
    elrefe =(
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG3','FPG1=FPG1',), mater=('FPG1',),),
        )
    calculs = (

        OP.CHAR_THER_FLUN_F(te=105,
            para_in=((SP.PFLUXNF, CFLUXNF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_FLUN_R(te=106,
            para_in=((SP.PFLUXNR, CFLUXNR), (SP.PGEOMER, NGEOMER),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TEXT_F(te=107,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PT_EXTF, CT_EXTF),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.CHAR_THER_TEXT_R(te=108,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), (SP.PT_EXTR, LC.CT_EXTR),
                     ),
            para_out=((SP.PVECTTR, MVECTTR), ),
        ),

        OP.COOR_ELGA(te=478,
            para_in=((SP.PGEOMER, NGEOMER), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.MASS_THER(te=102,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.MASS_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER(te=101,
            para_in=((SP.PCACOQU, CCACOQU), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (SP.PTEMPSR, CTEMPSR),
                     ),
            para_out=((OP.RIGI_THER.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_COEH_F(te=103,
            para_in=((SP.PCOEFHF, CCOEFHF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COEH_F.PMATTTR, MMATTTR), ),
        ),

        OP.RIGI_THER_COEH_R(te=104,
            para_in=((SP.PCOEFHR, CCOEFHR), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((OP.RIGI_THER_COEH_R.PMATTTR, MMATTTR), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )
