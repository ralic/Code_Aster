
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


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='RIGI',
    components=('DX','DY','DZ',))


CEPSINF  = LocatedComponents(phys=PHY.EPSI_F, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


CEPSINR  = LocatedComponents(phys=PHY.EPSI_R, type='ELEM',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


CFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELEM',
    components=('FX','FY','FZ',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[3]','K[3]','BETA',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z','W',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='RIGI',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='RIGI',
    components=('X[30]',))


ERAYONM  = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X1',))


CEFOND   = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X1',))


ECASECT  = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[10]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='RIGI',
    components=('X[30]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


CPRES_R  = LocatedComponents(phys=PHY.PRES_R, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CALC_G, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFR2D3D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSR, EPRESNO), (SP.PSIGINR, ECONTNO),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_G.PVARCPR, LC.ZVARCPG),
             (SP.PVITESS, DDL_MECA), ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFR2D3D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSR, EPRESNO), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVITESS, DDL_MECA),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP_F, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFF2D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG),
             (SP.PVITESS, DDL_MECA), ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_G_F, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFF2D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSF, CPRESSF), (SP.PSIGINR, ECONTNO),
             (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_G_F.PVARCPR, LC.ZVARCPG), (SP.PVITESS, DDL_MECA),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_G_GLOB, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFR2D3D, NFORCER), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSR, EPRESNO), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_G_GLOB.PVARCPR, LC.ZVARCPG), (SP.PVITESS, DDL_MECA),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_G_GLOB_F, te=280,
    para_in=((SP.PACCELE, DDL_MECA), (SP.PDEPLAR, DDL_MECA),
             (SP.PFF2D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PPRESSF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_G_GLOB_F.PVARCPR, LC.ZVARCPG),
             (SP.PVITESS, DDL_MECA), ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_K_G, te=311,
    para_in=((OP.CALC_K_G.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PEPSINR, CEPSINR), (SP.PFR2D3D, NFORCER),
             (SP.PFRVOLU, NFORCER), (SP.PGEOMER, NGEOMER),
             (OP.CALC_K_G.PLSN, LC.N1NEUT_R), (OP.CALC_K_G.PLST, LC.N1NEUT_R),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (SP.PPRESSR, EPRESNO), (SP.PPULPRO, LC.CFREQR),
             (SP.PROTATR, LC.CROTATR), (SP.PSIGINR, ECONTNO),
             (SP.PTHETAR, DDL_MECA), (OP.CALC_K_G.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CALC_K_G_F, te=311,
    para_in=((OP.CALC_K_G_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PEPSINF, CEPSINF), (SP.PFF2D3D, CFORCEF),
             (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
             (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R), (OP.CALC_K_G_F.PLST, LC.N1NEUT_R),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (SP.PPRESSF, CPRESSF), (SP.PPULPRO, LC.CFREQR),
             (SP.PROTATR, LC.CROTATR), (SP.PSIGINR, ECONTNO),
             (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECA),
             (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CARA_SECT_POU3R, te=337,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PORIGIN, CGEOMER),
             ),
    para_out=((SP.PRAYONM, ERAYONM), ),
)

ele.addCalcul(OP.CARA_SECT_POUT3, te=337,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PCASECT, ECASECT), ),
)

ele.addCalcul(OP.CARA_SECT_POUT4, te=337,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PORIGIN, CGEOMER),
             ),
    para_out=((SP.PVECTU1, MVECTUR), (SP.PVECTU2, MVECTUR),
             ),
)

ele.addCalcul(OP.CARA_SECT_POUT5, te=337,
    para_in=((OP.CARA_SECT_POUT5.PCAORIE, CGEOMER), (SP.PGEOMER, NGEOMER),
             (SP.PNUMMOD, LC.CNUMMOD), (SP.PORIGFI, CGEOMER),
             (SP.PORIGIN, CGEOMER), ),
    para_out=((SP.PVECTU1, MVECTUR), (SP.PVECTU2, MVECTUR),
             (SP.PVECTU3, MVECTUR), (SP.PVECTU4, MVECTUR),
             (SP.PVECTU5, MVECTUR), (SP.PVECTU6, MVECTUR),
             ),
)

ele.addCalcul(OP.CHAR_MECA_EFON_F, te=19,
    para_in=((SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_EFON_R, te=18,
    para_in=((SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFR, EPRESNO), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_EFSU_F, te=425,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_EFSU_R, te=424,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFR, EPRESNO), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FF2D3D, te=29,
    para_in=((SP.PFF2D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR2D3D, te=28,
    para_in=((SP.PFR2D3D, NFORCER), (SP.PGEOMER, NGEOMER),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_F, te=19,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_R, te=18,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRSU_F, te=425,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRSU_R, te=424,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=488,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.G_BILI, te=298,
    para_in=((SP.PDEPLAU, DDL_MECA), (SP.PDEPLAV, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTHETAR, DDL_MECA), (SP.PVARCMR, LC.ZVARCPG),
             (OP.G_BILI.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.UEPSINR, CEPSINR), (SP.UPFR23D, NFORCER),
             (SP.UPRESSR, EPRESNO), (SP.VEPSINR, CEPSINR),
             (SP.VPFR23D, NFORCER), (SP.VPRESSR, EPRESNO),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.G_BILI_F, te=298,
    para_in=((SP.PDEPLAU, DDL_MECA), (SP.PDEPLAV, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             (SP.PTHETAR, DDL_MECA), (SP.PVARCMR, LC.ZVARCPG),
             (OP.G_BILI_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.UEPSINF, CEPSINF), (SP.UPFF23D, CFORCEF),
             (SP.UPRESSF, CPRESSF), (SP.UTEMPSR, CTEMPSR),
             (SP.VEPSINF, CEPSINF), (SP.VPFF23D, CFORCEF),
             (SP.VPRESSF, CPRESSF), (SP.VTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
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

ele.addCalcul(OP.RIGI_MECA_EFSU_F, te=425,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFF, CPRESSF), (SP.PTEMPSR, CTEMPSR),
             ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.RIGI_MECA_EFSU_R, te=424,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PEFOND, CEFOND), (SP.PGEOMER, NGEOMER),
             (SP.PPREFFR, EPRESNO), ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.RIGI_MECA_PRSU_F, te=425,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSF, CPRESSF),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.RIGI_MECA_PRSU_R, te=424,
    para_in=((SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (SP.PPRESSR, EPRESNO),
             ),
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.SIRO_ELEM, te=411,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PSIG3D, ECONTNO),
             ),
    para_out=((SP.PPJSIGM, LC.EPJSIGM), ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((SP.PFORC_R, CFORCER), (OP.TOU_INI_ELEM.PPRES_R, CPRES_R),
             ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PPRES_R, LC.EPRESGA), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F),
             (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R), (OP.TOU_INI_ELNO.PPRES_R, EPRESNO),
             (OP.TOU_INI_ELNO.PSIEF_R, ECONTNO), ),
)


#------------------------------------------------------------
MECA_FACE3 = Element(modele=abstractElement)
ele = MECA_FACE3
ele.meshType = MT.TRIA3
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MECA_FACE4 = Element(modele=abstractElement)
ele = MECA_FACE4
ele.meshType = MT.QUAD4
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MECA_FACE6 = Element(modele=abstractElement)
ele = MECA_FACE6
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MECA_FACE8 = Element(modele=abstractElement)
ele = MECA_FACE8
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9',), mater=('RIGI',),),
    )


#------------------------------------------------------------
MECA_FACE9 = Element(modele=abstractElement)
ele = MECA_FACE9
ele.meshType = MT.QUAD9
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,9,)),
    )
ele.attrs= (('BORD_ISO','OUI'),)
ele.elrefe=(
        ElrefeLoc(MT.QU9, gauss = ('RIGI=FPG9',), mater=('RIGI',),),
    )
