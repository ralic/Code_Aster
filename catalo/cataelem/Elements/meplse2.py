
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


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='RIGI',
    components=('DX','DY','DZ',))


EERREUR  = LocatedComponents(phys=PHY.ERRE_R, type='ELEM',
    components=('ERREST','NUEST','SIGCAL','TERMRE','TERMR2',
          'TERMNO','TERMN2','TERMSA','TERMS2','TAILLE',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[2]','K[2]',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','W',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


ECASECT  = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[6]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES','CISA',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES','CISA',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CALC_G, te=282,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFR1D2D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSR, EPRESNO), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_G.PVARCPR, LC.ZVARCPG), (SP.PVITESS, DDL_MECA),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP, te=282,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFR1D2D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSR, EPRESNO), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVITESS, DDL_MECA),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP_F, te=282,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFF1D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG),
             (SP.PVITESS, DDL_MECA), ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_G_F, te=282,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFF1D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_G_F.PVARCPR, LC.ZVARCPG),
             (SP.PVITESS, DDL_MECA), ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_K_G, te=300,
    para_in=((SP.PDEPLAR, DDL_MECA), (SP.PFISSR, LC.CFISSR),
             (SP.PFR1D2D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PPRESSR, EPRESNO),
             (SP.PPULPRO, LC.CFREQR), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_K_G.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CALC_K_G_F, te=300,
    para_in=((SP.PDEPLAR, DDL_MECA), (SP.PFF1D2D, CFORCEF),
             (SP.PFISSR, LC.CFISSR), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (SP.PPRESSF, CPRESSF),
             (SP.PPULPRO, LC.CFREQR), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CARA_SECT_POUT3, te=564,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PCASECT, ECASECT), ),
)

ele.addCalcul(OP.CARA_SECT_POUT4, te=564,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PORIGIN, CGEOMER),
             ),
    para_out=((SP.PVECTU1, MVECTUR), (SP.PVECTU2, MVECTUR),
             ),
)

ele.addCalcul(OP.CHAR_MECA_FF1D2D, te=91,
    para_in=((SP.PFF1D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR1D2D, te=90,
    para_in=((SP.PFR1D2D, NFORCER), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_F, te=89,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_R, te=88,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRSU_F, te=574,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRSU_R, te=573,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=479,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.NORME_L2, te=563,
    para_in=((SP.PCALCI, LC.EMNEUT_I), (SP.PCHAMPG, EGNEUT_R),
             (SP.PCOEFR, EMNEUT_R), (OP.NORME_L2.PCOORPG, EGGEOP_R),
             ),
    para_out=((SP.PNORME, LC.ENORME), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.RIGI_MECA_PRSU_F, te=574,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.RIGI_MECA_PRSU_R, te=573,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PERREUR, EERREUR), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PPRES_R, LC.EPRESGA), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
             (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
             ),
)


#------------------------------------------------------------
MEPLSE2 = Element(modele=abstractElement)
ele = MEPLSE2
ele.meshType = MT.SEG2
ele.nodes = (
        SetOfNodes('EN1', (1,2,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MEPLSE3 = Element(modele=abstractElement)
ele = MEPLSE3
ele.meshType = MT.SEG3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MEAXSE2 = Element(modele=abstractElement)
ele = MEAXSE2
ele.meshType = MT.SEG2
ele.nodes = (
        SetOfNodes('EN1', (1,2,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MEAXSE3 = Element(modele=abstractElement)
ele = MEAXSE3
ele.meshType = MT.SEG3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',), mater=('RIGI',),),
    )
