
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
from cataelem.Tools.base_objects import Calcul, Element, AbstractElement
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.located_components as LC
import cataelem.Commons.parameters as SP
import cataelem.Commons.mesh_types as MT
from cataelem.Options.options import OP

#----------------
# Modes locaux :
#----------------


CCAGNPO  = LocatedComponents(phys=PHY.CAGNPO, type='ELEM',
    components=('A1','IY1','IZ1','AY1','AZ1',
          'EY1','EZ1','JX1','RY1','RZ1',
          'RT1','A2','IY2','IZ2','AY2',
          'AZ2','EY2','EZ2','JX2','RY2',
          'RZ2','RT2','TVAR',))


CCAORIE  = LocatedComponents(phys=PHY.CAORIE, type='ELEM',
    components=('ALPHA','BETA','GAMMA',))


CCAPOUF  = LocatedComponents(phys=PHY.CAPOUF, type='ELEM',
    components=('B_T','B_N','B_TN','A_FLUI','A_CELL',
          'COEF_ECH',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ','DRX','DRY',
          'DRZ','PHI',)),
    ('EN2',('PHI',)),))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.COOR_ELGA, te=488,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.MASS_INER, te=65,
    para_in=((SP.PCAGNPO, CCAGNPO), (SP.PCAPOUF, CCAPOUF),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=470,
    para_in=((SP.PCAGNPO, CCAGNPO), (OP.MASS_MECA.PCAORIE, CCAORIE),
             (SP.PCAPOUF, CCAPOUF), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.REPERE_LOCAL, te=135,
    para_in=((OP.REPERE_LOCAL.PCAORIE, CCAORIE), ),
    para_out=((SP.PREPLO1, CGEOMER), (SP.PREPLO2, CGEOMER),
             (SP.PREPLO3, CGEOMER), ),
)

ele.addCalcul(OP.RIGI_MECA, te=471,
    para_in=((SP.PCAGNPO, CCAGNPO), (OP.RIGI_MECA.PCAORIE, CCAORIE),
             (SP.PCAPOUF, CCAPOUF), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOP_R), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)

ele.addCalcul(OP.VERI_JACOBIEN, te=328,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PCODRET, LC.ECODRET), ),
)


#------------------------------------------------------------
MECA_POHO_HEXA20 = Element(modele=abstractElement)
ele = MECA_POHO_HEXA20
ele.meshType = MT.HEXA20
ele.nodes = (
        SetOfNodes('EN2', (9,10,11,12,13,14,15,16,17,18,19,20,)),
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','FPG1=FPG1',), mater=('RIGI','FPG1',),),
        ElrefeLoc(MT.POHOH20,),
    )


#------------------------------------------------------------
MECA_POHO_HEXA8 = Element(modele=abstractElement)
ele = MECA_POHO_HEXA8
ele.meshType = MT.HEXA8
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','FPG1=FPG1',), mater=('RIGI','FPG1',),),
        ElrefeLoc(MT.POHOH8,),
    )
