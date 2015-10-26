
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


CCAGEPO  = LocatedComponents(phys=PHY.CAGEPO, type='ELEM',
    components=('HY1','HZ1','EPY1','EPZ1','HY2',
          'HZ2','EPY2','EPZ2','R1','EP1',
          'R2','EP2','TSEC',))


CCAGNPO  = LocatedComponents(phys=PHY.CAGNPO, type='ELEM',
    components=('A1','IY1','IZ1','AY1','AZ1',
          'EY1','EZ1','JX1','RY1','RZ1',
          'RT1','AI1','A2','IY2','IZ2',
          'AY2','AZ2','EY2','EZ2','JX2',
          'RY2','RZ2','RT2','AI2','TVAR',))


CCAORIE  = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ','DRX','DRY',
          'DRZ','PRES','PHI',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MEFS_POU_D_T(Element):
    """Please document this element"""
    meshType = MT.SEG2
    calculs = (

        OP.MASS_MECA(te=241,
            para_in=((SP.PCAGEPO, CCAGEPO), (SP.PCAGNPO, CCAGNPO),
                     (OP.MASS_MECA.PCAORIE, CCAORIE), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.REPERE_LOCAL(te=135,
            para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
            para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
                     (SP.PREPLO3, CGEOMER), ),
        ),

        OP.RIGI_MECA(te=240,
            para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

    )
