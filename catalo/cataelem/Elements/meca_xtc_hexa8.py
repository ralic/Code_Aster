
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


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ','E1X','E1Y',
          'E1Z','E2X','E2Y','E2Z','E3X',
          'E3Y','E3Z','E4X','E4Y','E4Z',
          'LAGS_C','LAGS_F[2]',)),
    ('EN3',('DX','DY','DZ','E1X','E1Y',
          'E1Z','E2X','E2Y','E2Z','E3X',
          'E3Y','E3Z','E4X','E4Y','E4Z',)),))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='XFEM',
    components=('DX','DY','DZ','E1X','E1Y',
          'E1Z','E2X','E2Y','E2Z','E3X',
          'E3Y','E3Z','E4X','E4Y','E4Z',
          'LAGS_C','LAGS_F[2]',))


DDL_MECC = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','DZ',)),
    ('EN3',('DX','DY','DZ',)),))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EFACY_R  = LocatedComponents(phys=PHY.FACY_R, type='ELGA', location='RIGI',
    components=('DTAUM1','VNM1X','VNM1Y','VNM1Z','SINMAX1',
          'SINMOY1','EPNMAX1','EPNMOY1','SIGEQ1','NBRUP1',
          'ENDO1','DTAUM2','VNM2X','VNM2Y','VNM2Z',
          'SINMAX2','SINMOY2','EPNMAX2','EPNMOY2','SIGEQ2',
          'NBRUP2','ENDO2',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY','FZ',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY','FZ',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[3]','K[3]','BETA',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z','W',))


XFGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E33NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
    components=('X[33]',))


ECONTSE  = LocatedComponents(phys=PHY.N1920R, type='ELEM',
    components=('X[144]',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='XFEM',
    components=('X[30]',))


E1NEUTK  = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
    components=('Z1',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='XFEM',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='XFEM',
    components=('VARI',))


CONTX_R  = LocatedComponents(phys=PHY.XCONTAC, type='ELEM',
    components=('RHON','MU','RHOTK','INTEG','COECH',
          'COSTCO','COSTFR','COPECO','COPEFR',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.CALC_G, te=288,
    para_in=((OP.CALC_G.PAINTER, LC.E170NEUT), (OP.CALC_G.PBASECO, LC.E306NEUT),
             (OP.CALC_G.PBASLOR, LC.N9NEUT_R), (OP.CALC_G.PCFACE, LC.E90NEUTI),
             (OP.CALC_G.PCNSETO, LC.E320NEUI), (OP.CALC_G.PCOMPOR, CCOMPOR),
             (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
             (SP.PGEOMER, NGEOMER), (OP.CALC_G.PHEAVTO, LC.E32NEUTI),
             (OP.CALC_G.PLONCHA, LC.E10NEUTI), (OP.CALC_G.PLONGCO, LC.E3NEUTI),
             (OP.CALC_G.PLSN, LC.N1NEUT_R), (OP.CALC_G.PLST, LC.N1NEUT_R),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (OP.CALC_G.PPINTER, LC.E102NEUT), (OP.CALC_G.PPINTTO, E33NEUTR),
             (OP.CALC_G.PPMILTO, LC.E198NEUT), (SP.PPRESSR, EPRESNO),
             (SP.PROTATR, LC.CROTATR), (SP.PTHETAR, DDL_MECC),
             (OP.CALC_G.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP, te=288,
    para_in=((OP.CALC_GTP.PAINTER, LC.E170NEUT), (OP.CALC_GTP.PBASECO, LC.E306NEUT),
             (OP.CALC_GTP.PBASLOR, LC.N9NEUT_R), (OP.CALC_GTP.PCFACE, LC.E90NEUTI),
             (OP.CALC_GTP.PCNSETO, LC.E320NEUI), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
             (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
             (SP.PGEOMER, NGEOMER), (OP.CALC_GTP.PHEAVTO, LC.E32NEUTI),
             (OP.CALC_GTP.PLONCHA, LC.E10NEUTI), (OP.CALC_GTP.PLONGCO, LC.E3NEUTI),
             (OP.CALC_GTP.PLSN, LC.N1NEUT_R), (OP.CALC_GTP.PLST, LC.N1NEUT_R),
             (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
             (OP.CALC_GTP.PPINTER, LC.E102NEUT), (OP.CALC_GTP.PPINTTO, E33NEUTR),
             (OP.CALC_GTP.PPMILTO, LC.E198NEUT), (SP.PPRESSR, EPRESNO),
             (SP.PROTATR, LC.CROTATR), (SP.PTHETAR, DDL_MECC),
             (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_GTP_F, te=288,
    para_in=((OP.CALC_GTP_F.PAINTER, LC.E170NEUT), (OP.CALC_GTP_F.PBASECO, LC.E306NEUT),
             (OP.CALC_GTP_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_GTP_F.PCFACE, LC.E90NEUTI),
             (OP.CALC_GTP_F.PCNSETO, LC.E320NEUI), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
             (OP.CALC_GTP_F.PHEAVTO, LC.E32NEUTI), (OP.CALC_GTP_F.PLONCHA, LC.E10NEUTI),
             (OP.CALC_GTP_F.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP_F.PLSN, LC.N1NEUT_R),
             (OP.CALC_GTP_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP_F.PPINTER, LC.E102NEUT),
             (OP.CALC_GTP_F.PPINTTO, E33NEUTR), (OP.CALC_GTP_F.PPMILTO, LC.E198NEUT),
             (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
             (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
             (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_G_F, te=288,
    para_in=((OP.CALC_G_F.PAINTER, LC.E170NEUT), (OP.CALC_G_F.PBASECO, LC.E306NEUT),
             (OP.CALC_G_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_G_F.PCFACE, LC.E90NEUTI),
             (OP.CALC_G_F.PCNSETO, LC.E320NEUI), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
             (OP.CALC_G_F.PHEAVTO, LC.E32NEUTI), (OP.CALC_G_F.PLONCHA, LC.E10NEUTI),
             (OP.CALC_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_G_F.PLSN, LC.N1NEUT_R),
             (OP.CALC_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CALC_G_F.PPINTER, LC.E102NEUT),
             (OP.CALC_G_F.PPINTTO, E33NEUTR), (OP.CALC_G_F.PPMILTO, LC.E198NEUT),
             (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
             (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
             (OP.CALC_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             ),
    para_out=((SP.PGTHETA, LC.EGTHETA), ),
)

ele.addCalcul(OP.CALC_K_G, te=297,
    para_in=((OP.CALC_K_G.PAINTER, LC.E170NEUT), (OP.CALC_K_G.PBASECO, LC.E306NEUT),
             (OP.CALC_K_G.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G.PCFACE, LC.E90NEUTI),
             (OP.CALC_K_G.PCNSETO, LC.E320NEUI), (OP.CALC_K_G.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PFRVOLU, NFORCER), (SP.PGEOMER, NGEOMER),
             (OP.CALC_K_G.PHEAVTO, LC.E32NEUTI), (OP.CALC_K_G.PLONCHA, LC.E10NEUTI),
             (OP.CALC_K_G.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G.PLSN, LC.N1NEUT_R),
             (OP.CALC_K_G.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G.PPINTER, LC.E102NEUT),
             (OP.CALC_K_G.PPINTTO, E33NEUTR), (OP.CALC_K_G.PPMILTO, LC.E198NEUT),
             (SP.PPRESSR, EPRESNO), (SP.PPULPRO, LC.CFREQR),
             (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
             (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CALC_K_G_F, te=297,
    para_in=((OP.CALC_K_G_F.PAINTER, LC.E170NEUT), (OP.CALC_K_G_F.PBASECO, LC.E306NEUT),
             (OP.CALC_K_G_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G_F.PCFACE, LC.E90NEUTI),
             (OP.CALC_K_G_F.PCNSETO, LC.E320NEUI), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
             (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
             (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
             (OP.CALC_K_G_F.PHEAVTO, LC.E32NEUTI), (OP.CALC_K_G_F.PLONCHA, LC.E10NEUTI),
             (OP.CALC_K_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R),
             (OP.CALC_K_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G_F.PPINTER, LC.E102NEUT),
             (OP.CALC_K_G_F.PPINTTO, E33NEUTR), (OP.CALC_K_G_F.PPMILTO, LC.E198NEUT),
             (SP.PPRESSF, CPRESSF), (SP.PPULPRO, LC.CFREQR),
             (SP.PROTATR, LC.CROTATR), (SP.PTEMPSR, CTEMPSR),
             (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), ),
    para_out=((SP.PGTHETA, EKTHETA), ),
)

ele.addCalcul(OP.CHAR_MECA_CONT, te=534,
    para_in=((OP.CHAR_MECA_CONT.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_CONT.PBASECO, LC.E306NEUT),
             (OP.CHAR_MECA_CONT.PCFACE, LC.E90NEUTI), (SP.PDEPL_M, DDL_MECA),
             (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
             (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
             (OP.CHAR_MECA_CONT.PLONGCO, LC.E3NEUTI), (OP.CHAR_MECA_CONT.PLST, LC.N1NEUT_R),
             (OP.CHAR_MECA_CONT.PPINTER, LC.E102NEUT), (OP.CHAR_MECA_CONT.PSEUIL, LC.E1NEUTR),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FF3D3D, te=440,
    para_in=((SP.PFF3D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FR3D3D, te=440,
    para_in=((OP.CHAR_MECA_FR3D3D.PCNSETO, LC.E320NEUI), (SP.PFR3D3D, NFORCER),
             (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR3D3D.PHEAVTO, LC.E32NEUTI),
             (OP.CHAR_MECA_FR3D3D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FR3D3D.PLSN, LC.N1NEUT_R),
             (OP.CHAR_MECA_FR3D3D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FR3D3D.PPINTTO, E33NEUTR),
             (OP.CHAR_MECA_FR3D3D.PSTANO, STANO_I), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_FROT, te=534,
    para_in=((OP.CHAR_MECA_FROT.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_FROT.PBASECO, LC.E306NEUT),
             (OP.CHAR_MECA_FROT.PCFACE, LC.E90NEUTI), (SP.PDEPL_M, DDL_MECA),
             (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
             (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
             (OP.CHAR_MECA_FROT.PLONGCO, LC.E3NEUTI), (OP.CHAR_MECA_FROT.PLST, LC.N1NEUT_R),
             (OP.CHAR_MECA_FROT.PPINTER, LC.E102NEUT), (OP.CHAR_MECA_FROT.PSEUIL, LC.E1NEUTR),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_F, te=37,
    para_in=((OP.CHAR_MECA_PRES_F.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_PRES_F.PBASECO, LC.E306NEUT),
             (OP.CHAR_MECA_PRES_F.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PLONGCO, LC.E3NEUTI),
             (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTER, LC.E102NEUT),
             (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_R, te=37,
    para_in=((OP.CHAR_MECA_PRES_R.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_PRES_R.PBASECO, LC.E306NEUT),
             (OP.CHAR_MECA_PRES_R.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PLONGCO, LC.E3NEUTI),
             (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTER, LC.E102NEUT),
             (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_ROTA_R, te=441,
    para_in=((OP.CHAR_MECA_ROTA_R.PCNSETO, LC.E320NEUI), (SP.PDEPLMR, DDL_MECA),
             (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_ROTA_R.PHEAVTO, LC.E32NEUTI), (OP.CHAR_MECA_ROTA_R.PLONCHA, LC.E10NEUTI),
             (OP.CHAR_MECA_ROTA_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_ROTA_R.PLST, LC.N1NEUT_R),
             (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_ROTA_R.PPINTTO, E33NEUTR),
             (SP.PROTATR, LC.CROTATR), (OP.CHAR_MECA_ROTA_R.PSTANO, STANO_I),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.COOR_ELGA, te=481,
    para_in=((OP.COOR_ELGA.PCNSETO, LC.E320NEUI), (SP.PGEOMER, NGEOMER),
             (OP.COOR_ELGA.PLONCHA, LC.E10NEUTI), (OP.COOR_ELGA.PPINTTO, E33NEUTR),
             (OP.COOR_ELGA.PPMILTO, LC.E198NEUT), ),
    para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
)

ele.addCalcul(OP.DEPL_XPG, te=566,
    para_in=((OP.DEPL_XPG.PBASLOR, LC.N9NEUT_R), (SP.PDEPLNO, DDL_MECA),
             (OP.DEPL_XPG.PHEAVTO, LC.E32NEUTI), (OP.DEPL_XPG.PLONCHA, LC.E10NEUTI),
             (OP.DEPL_XPG.PLSN, LC.N1NEUT_R), (OP.DEPL_XPG.PLST, LC.N1NEUT_R),
             (OP.DEPL_XPG.PXFGEOM, XFGEOM_R), ),
    para_out=((SP.PDEPLPG, EDEPLPG), ),
)

ele.addCalcul(OP.ENEL_ELEM, te=565,
    para_in=((OP.ENEL_ELEM.PCNSETO, LC.E320NEUI), (OP.ENEL_ELEM.PCOMPOR, CCOMPOR),
             (OP.ENEL_ELEM.PCONTPR, ECONTPG), (SP.PDEPLR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (OP.ENEL_ELEM.PLONCHA, LC.E10NEUTI),
             (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PPINTTO, E33NEUTR),
             (OP.ENEL_ELEM.PPMILTO, LC.E198NEUT), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
             ),
    para_out=((SP.PENERD1, EENERR), ),
)

ele.addCalcul(OP.FORC_NODA, te=542,
    para_in=((OP.FORC_NODA.PBASLOR, LC.N9NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E320NEUI),
             (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (OP.FORC_NODA.PHEAVTO, LC.E32NEUTI), (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI),
             (OP.FORC_NODA.PLONCHA, LC.E10NEUTI), (OP.FORC_NODA.PLSN, LC.N1NEUT_R),
             (OP.FORC_NODA.PLST, LC.N1NEUT_R), (OP.FORC_NODA.PPINTTO, E33NEUTR),
             (OP.FORC_NODA.PSTANO, STANO_I), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=539,
    para_in=((OP.FULL_MECA.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
             (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCNSETO, LC.E320NEUI),
             (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (OP.FULL_MECA.PHEAVTO, LC.E32NEUTI),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (OP.FULL_MECA.PLONCHA, LC.E10NEUTI), (OP.FULL_MECA.PLSN, LC.N1NEUT_R),
             (OP.FULL_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (OP.FULL_MECA.PPINTTO, E33NEUTR), (OP.FULL_MECA.PSTANO, STANO_I),
             (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
             (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
             (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.GEOM_FAC, te=519,
    para_in=((SP.NOMFIS, E1NEUTK), (SP.PDEPLA, DDL_MECA),
             (OP.GEOM_FAC.PGESCLO, LC.E102NEUT), (OP.GEOM_FAC.PLONGCO, LC.E3NEUTI),
             (OP.GEOM_FAC.PLST, LC.N1NEUT_R), (OP.GEOM_FAC.PPINTER, LC.E102NEUT),
             ),
    para_out=((SP.PBASESC, LC.E306NEUT), (SP.PBASMAI, LC.E306NEUT),
             (SP.PNEWGEM, LC.E102NEUT), (SP.PNEWGES, LC.E102NEUT),
             ),
)

ele.addCalcul(OP.GRAD_NEUT9_R, te=398,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PNEUTER, LC.N9NEUT_R),
             ),
    para_out=((OP.GRAD_NEUT9_R.PGNEUTR, LC.G27NEUTR), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.INI_XFEM_ELNO, te=99,
    para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N9NEUT_R), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
             (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
             ),
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

ele.addCalcul(OP.RAPH_MECA, te=539,
    para_in=((OP.RAPH_MECA.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
             (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCNSETO, LC.E320NEUI),
             (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (OP.RAPH_MECA.PHEAVTO, LC.E32NEUTI),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (OP.RAPH_MECA.PLONCHA, LC.E10NEUTI), (OP.RAPH_MECA.PLSN, LC.N1NEUT_R),
             (OP.RAPH_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (OP.RAPH_MECA.PPINTTO, E33NEUTR), (OP.RAPH_MECA.PPMILTO, LC.E198NEUT),
             (OP.RAPH_MECA.PSTANO, STANO_I), (SP.PVARCMR, LC.ZVARCPG),
             (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
             (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
             (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.RIGI_CONT, te=533,
    para_in=((OP.RIGI_CONT.PAINTER, LC.E170NEUT), (OP.RIGI_CONT.PBASECO, LC.E306NEUT),
             (OP.RIGI_CONT.PCFACE, LC.E90NEUTI), (SP.PDEPL_M, DDL_MECA),
             (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
             (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
             (OP.RIGI_CONT.PLONGCO, LC.E3NEUTI), (OP.RIGI_CONT.PLSN, LC.N1NEUT_R),
             (OP.RIGI_CONT.PLST, LC.N1NEUT_R), (OP.RIGI_CONT.PPINTER, LC.E102NEUT),
             (OP.RIGI_CONT.PSEUIL, LC.E1NEUTR), ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_FROT, te=533,
    para_in=((OP.RIGI_FROT.PAINTER, LC.E170NEUT), (OP.RIGI_FROT.PBASECO, LC.E306NEUT),
             (OP.RIGI_FROT.PCFACE, LC.E90NEUTI), (SP.PDEPL_M, DDL_MECA),
             (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
             (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
             (OP.RIGI_FROT.PLONGCO, LC.E3NEUTI), (OP.RIGI_FROT.PLSN, LC.N1NEUT_R),
             (OP.RIGI_FROT.PLST, LC.N1NEUT_R), (OP.RIGI_FROT.PPINTER, LC.E102NEUT),
             (OP.RIGI_FROT.PSEUIL, LC.E1NEUTR), ),
    para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA, te=11,
    para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=539,
    para_in=((OP.RIGI_MECA_TANG.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
             (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCNSETO, LC.E320NEUI),
             (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
             (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
             (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA_TANG.PHEAVTO, LC.E32NEUTI),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (OP.RIGI_MECA_TANG.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA_TANG.PLSN, LC.N1NEUT_R),
             (OP.RIGI_MECA_TANG.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (OP.RIGI_MECA_TANG.PPINTTO, E33NEUTR), (OP.RIGI_MECA_TANG.PSTANO, STANO_I),
             (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
             (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.SIGM_ELGA, te=546,
    para_in=((SP.PSIEFR, ECONTPG), ),
    para_out=((SP.PSIGMC, ECONTPC), (SP.PSIGMR, ECONTPG),
             ),
)

ele.addCalcul(OP.TOPOFA, te=510,
    para_in=((OP.TOPOFA.PAINTTO, LC.E55NEUTR), (OP.TOPOFA.PCNSETO, LC.E320NEUI),
             (SP.PDECOU, E1NEUTK), (SP.PGEOMER, NGEOMER),
             (SP.PGRADLN, LC.N3NEUT_R), (SP.PGRADLT, LC.N3NEUT_R),
             (OP.TOPOFA.PHEAVTO, LC.E32NEUTI), (OP.TOPOFA.PLONCHA, LC.E10NEUTI),
             (OP.TOPOFA.PLSN, LC.N1NEUT_R), (OP.TOPOFA.PLST, LC.N1NEUT_R),
             (OP.TOPOFA.PPINTTO, E33NEUTR), (OP.TOPOFA.PPMILTO, LC.E198NEUT),
             ),
    para_out=((OP.TOPOFA.PAINTER, LC.E170NEUT), (OP.TOPOFA.PBASECO, LC.E306NEUT),
             (OP.TOPOFA.PCFACE, LC.E90NEUTI), (SP.PGESCLA, LC.E102NEUT),
             (OP.TOPOFA.PGESCLO, LC.E102NEUT), (SP.PGMAITR, LC.E102NEUT),
             (OP.TOPOFA.PLONGCO, LC.E3NEUTI), (OP.TOPOFA.PPINTER, LC.E102NEUT),
             ),
)

ele.addCalcul(OP.TOPONO, te=120,
    para_in=((OP.TOPONO.PCNSETO, LC.E320NEUI), (OP.TOPONO.PHEAVTO, LC.E32NEUTI),
             (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
             ),
    para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, LC.E32NEUTI),
             ),
)

ele.addCalcul(OP.TOPOSE, te=514,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
             ),
    para_out=((OP.TOPOSE.PAINTTO, LC.E55NEUTR), (OP.TOPOSE.PCNSETO, LC.E320NEUI),
             (OP.TOPOSE.PHEAVTO, LC.E32NEUTI), (OP.TOPOSE.PLONCHA, LC.E10NEUTI),
             (OP.TOPOSE.PPINTTO, E33NEUTR), (OP.TOPOSE.PPMILTO, LC.E198NEUT),
             ),
)

ele.addCalcul(OP.TOU_INI_ELEM, te=99,
    para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA),
             (SP.PFACY_R, EFACY_R), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
             (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
             (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R), (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG),
             (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG), ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
)

ele.addCalcul(OP.XCVBCA, te=532,
    para_in=((OP.XCVBCA.PAINTER, LC.E170NEUT), (OP.XCVBCA.PBASECO, LC.E306NEUT),
             (OP.XCVBCA.PCFACE, LC.E90NEUTI), (SP.PDEPL_P, DDL_MECA),
             (SP.PDONCO, CONTX_R), (SP.PGEOMER, NGEOMER),
             (SP.PGLISS, LC.E1NEUTI), (SP.PINDCOI, LC.E1NEUTI),
             (OP.XCVBCA.PLONGCO, LC.E3NEUTI), (OP.XCVBCA.PLST, LC.N1NEUT_R),
             (SP.PMEMCON, LC.E1NEUTI), (OP.XCVBCA.PPINTER, LC.E102NEUT),
             ),
    para_out=((SP.PINCOCA, LC.E1NEUTI), (SP.PINDCOO, LC.E1NEUTI),
             (SP.PINDMEM, LC.E1NEUTI), ),
)

ele.addCalcul(OP.XFEM_XPG, te=46,
    para_in=((OP.XFEM_XPG.PCNSETO, LC.E320NEUI), (SP.PGEOMER, NGEOMER),
             (OP.XFEM_XPG.PHEAVTO, LC.E32NEUTI), (OP.XFEM_XPG.PLONCHA, LC.E10NEUTI),
             (OP.XFEM_XPG.PPINTTO, E33NEUTR), (OP.XFEM_XPG.PPMILTO, LC.E198NEUT),
             ),
    para_out=((OP.XFEM_XPG.PXFGEOM, XFGEOM_R), ),
)

ele.addCalcul(OP.XREACL, te=548,
    para_in=((OP.XREACL.PAINTER, LC.E170NEUT), (OP.XREACL.PBASECO, LC.E306NEUT),
             (OP.XREACL.PCFACE, LC.E90NEUTI), (SP.PDEPL_P, DDL_MECA),
             (SP.PDONCO, CONTX_R), (SP.PGEOMER, NGEOMER),
             (OP.XREACL.PLONGCO, LC.E3NEUTI), (OP.XREACL.PPINTER, LC.E102NEUT),
             ),
    para_out=((OP.XREACL.PSEUIL, LC.E1NEUTR), ),
)


#------------------------------------------------------------
MECA_XTC_HEXA8 = Element(modele=abstractElement)
ele = MECA_XTC_HEXA8
ele.meshType = MT.HEXA8
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','NOEU=NOEU','XFEM=XFEM480','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','GAUSS=FPG12','SIMP=SIMP',),),
    )


#------------------------------------------------------------
MECA_XTC_PENTA6 = Element(modele=abstractElement)
ele = MECA_XTC_PENTA6
ele.meshType = MT.PENTA6
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','NOEU=NOEU','XFEM=XFEM240','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','GAUSS=FPG12','SIMP=SIMP',),),
    )


#------------------------------------------------------------
MECA_XTC_PYRAM5 = Element(modele=abstractElement)
ele = MECA_XTC_PYRAM5
ele.meshType = MT.PYRAM5
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.PY5, gauss = ('RIGI=FPG5','NOEU=NOEU','XFEM=XFEM180','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','SIMP=SIMP','GAUSS=FPG12',),),
    )


#------------------------------------------------------------
MECA_XTC_TETRA4 = Element(modele=abstractElement)
ele = MECA_XTC_TETRA4
ele.meshType = MT.TETRA4
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG1','NOEU=NOEU','XINT=FPG15','FPG1=FPG1','XFEM=XFEM90',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','GAUSS=FPG12','SIMP=SIMP',),),
    )


#------------------------------------------------------------
MECA_XTC_HEXA20 = Element(modele=abstractElement)
ele = MECA_XTC_HEXA20
ele.meshType = MT.HEXA20
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        SetOfNodes('EN3', (9,10,11,12,13,14,15,16,17,18,19,20,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','NOEU=NOEU','XFEM=XFEM480','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','SIMP=SIMP','GAUSS=FPG12',),),
    )


#------------------------------------------------------------
MECA_XTC_PENTA15 = Element(modele=abstractElement)
ele = MECA_XTC_PENTA15
ele.meshType = MT.PENTA15
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,6,)),
        SetOfNodes('EN3', (7,8,9,10,11,12,13,14,15,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.P15, gauss = ('RIGI=FPG21','NOEU=NOEU','XFEM=XFEM240','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','SIMP=SIMP','GAUSS=FPG12',),),
    )


#------------------------------------------------------------
MECA_XTC_PYRAM13 = Element(modele=abstractElement)
ele = MECA_XTC_PYRAM13
ele.meshType = MT.PYRAM13
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,5,)),
        SetOfNodes('EN3', (6,7,8,9,10,11,12,13,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.P13, gauss = ('RIGI=FPG27','NOEU=NOEU','XFEM=XFEM180','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','SIMP=SIMP','GAUSS=FPG12',),),
    )


#------------------------------------------------------------
MECA_XTC_TETRA10 = Element(modele=abstractElement)
ele = MECA_XTC_TETRA10
ele.meshType = MT.TETRA10
ele.nodes = (
        SetOfNodes('EN1', (1,2,3,4,)),
        SetOfNodes('EN3', (5,6,7,8,9,10,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.T10, gauss = ('RIGI=FPG5','NOEU=NOEU','XFEM=XFEM90','FPG1=FPG1',), mater=('RIGI','XFEM',),),
        ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
        ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12','SIMP=SIMP','GAUSS=FPG12',),),
    )
