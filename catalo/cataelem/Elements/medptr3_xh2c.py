
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

#----------------
# Modes locaux :
#----------------


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN2',('DX','DY','H1X','H1Y','H2X',
          'H2Y','LAGS_C','LAGS_F1','LAG2_C','LAG2_F1',)),
    ('EN3',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','LAGS_C','LAGS_F1',
          'LAG2_C','LAG2_F1','LAG3_C','LAG3_F1',)),
    ('EN4',('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','H4X','H4Y',
          'LAGS_C','LAGS_F1','LAG2_C','LAG2_F1','LAG3_C',
          'LAG3_F1','LAG4_C','LAG4_F1',)),))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='XFEM',
    components=('DX','DY','H1X','H1Y','H2X',
          'H2Y','H3X','H3Y','H4X','H4Y',
          'LAGS_C','LAGS_F1','LAG2_C','LAG2_F1','LAG3_C',
          'LAG3_F1','LAG4_C','LAG4_F1',))


DDL_MECC = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


NFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELNO',
    components=('FX','FY',))


EKTHETA  = LocatedComponents(phys=PHY.G, type='ELEM',
    components=('GTHETA','FIC[2]','K[2]',))


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','W',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y',))


XFGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E8NEUTI  = LocatedComponents(phys=PHY.N240_I, type='ELEM',
    components=('X[8]',))


E24NEUI  = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[24]',))


E14NEUTI = LocatedComponents(phys=PHY.N720_I, type='ELEM',
    components=('X[14]',))


E16NEUTR = LocatedComponents(phys=PHY.N816_R, type='ELEM',
    components=('X[16]',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='XFEM',
    components=('X[30]',))


E1NEUTK  = LocatedComponents(phys=PHY.NEUT_K8, type='ELEM',
    components=('Z1',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='XFEM',
    components=('X[30]',))


EMNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELEM',
    components=('X[30]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES','CISA',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES','CISA',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='XFEM',
    components=('VARI',))


CONTX_R  = LocatedComponents(phys=PHY.XCONTAC, type='ELEM',
    components=('RHON','MU','RHOTK','INTEG','COECH',
          'COSTCO','COSTFR','COPECO','COPEFR',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MEDPTR3_XH2C(Element):
    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN2', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )
    calculs = (

        OP.CALC_G(te=288,
            para_in=((OP.CALC_G.PAINTER, LC.E40NEUTR), (OP.CALC_G.PBASECO, LC.E32NEUTR),
                     (OP.CALC_G.PBASLOR, LC.N6NEUT_R), (OP.CALC_G.PCFACE, E14NEUTI),
                     (OP.CALC_G.PCNSETO, LC.E72NEUI), (OP.CALC_G.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_G.PHEAVTO, E24NEUI),
                     (OP.CALC_G.PHEA_NO, LC.N5NEUTI), (OP.CALC_G.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_G.PLONGCO, LC.E3NEUTI), (OP.CALC_G.PLSN, LC.N1NEUT_R),
                     (OP.CALC_G.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_G.PPINTER, E16NEUTR),
                     (OP.CALC_G.PPINTTO, LC.E24NEUTR), (OP.CALC_G.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP(te=288,
            para_in=((OP.CALC_GTP.PAINTER, LC.E40NEUTR), (OP.CALC_GTP.PBASECO, LC.E32NEUTR),
                     (OP.CALC_GTP.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP.PCFACE, E14NEUTI),
                     (OP.CALC_GTP.PCNSETO, LC.E72NEUI), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP.PHEAVTO, E24NEUI),
                     (OP.CALC_GTP.PHEA_NO, LC.N5NEUTI), (OP.CALC_GTP.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_GTP.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP.PLSN, LC.N1NEUT_R),
                     (OP.CALC_GTP.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP.PPINTER, E16NEUTR),
                     (OP.CALC_GTP.PPINTTO, LC.E24NEUTR), (OP.CALC_GTP.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_GTP.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP_F(te=288,
            para_in=((OP.CALC_GTP_F.PAINTER, LC.E40NEUTR), (OP.CALC_GTP_F.PBASECO, LC.E32NEUTR),
                     (OP.CALC_GTP_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP_F.PCFACE, E14NEUTI),
                     (OP.CALC_GTP_F.PCNSETO, LC.E72NEUI), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP_F.PHEAVTO, E24NEUI),
                     (OP.CALC_GTP_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_GTP_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_GTP_F.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_GTP_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP_F.PPINTER, E16NEUTR),
                     (OP.CALC_GTP_F.PPINTTO, LC.E24NEUTR), (OP.CALC_GTP_F.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=288,
            para_in=((OP.CALC_G_F.PAINTER, LC.E40NEUTR), (OP.CALC_G_F.PBASECO, LC.E32NEUTR),
                     (OP.CALC_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_G_F.PCFACE, E14NEUTI),
                     (OP.CALC_G_F.PCNSETO, LC.E72NEUI), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_G_F.PHEAVTO, E24NEUI),
                     (OP.CALC_G_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_G_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_G_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_G_F.PPINTER, E16NEUTR),
                     (OP.CALC_G_F.PPINTTO, LC.E24NEUTR), (OP.CALC_G_F.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_K_G(te=297,
            para_in=((OP.CALC_K_G.PAINTER, LC.E40NEUTR), (OP.CALC_K_G.PBASECO, LC.E32NEUTR),
                     (OP.CALC_K_G.PBASLOR, LC.N6NEUT_R), (OP.CALC_K_G.PCFACE, E14NEUTI),
                     (OP.CALC_K_G.PCNSETO, LC.E72NEUI), (OP.CALC_K_G.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_K_G.PHEAVTO, E24NEUI),
                     (OP.CALC_K_G.PHEA_NO, LC.N5NEUTI), (OP.CALC_K_G.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_K_G.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G.PLSN, LC.N1NEUT_R),
                     (OP.CALC_K_G.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G.PPINTER, E16NEUTR),
                     (OP.CALC_K_G.PPINTTO, LC.E24NEUTR), (OP.CALC_K_G.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_K_G.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=297,
            para_in=((OP.CALC_K_G_F.PAINTER, LC.E40NEUTR), (OP.CALC_K_G_F.PBASECO, LC.E32NEUTR),
                     (OP.CALC_K_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_K_G_F.PCFACE, E14NEUTI),
                     (OP.CALC_K_G_F.PCNSETO, LC.E72NEUI), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_K_G_F.PHEAVTO, E24NEUI),
                     (OP.CALC_K_G_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_K_G_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_K_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_K_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G_F.PPINTER, E16NEUTR),
                     (OP.CALC_K_G_F.PPINTTO, LC.E24NEUTR), (OP.CALC_K_G_F.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CHAR_MECA_CONT(te=534,
            para_in=((OP.CHAR_MECA_CONT.PAINTER, LC.E40NEUTR), (OP.CHAR_MECA_CONT.PBASECO, LC.E32NEUTR),
                     (OP.CHAR_MECA_CONT.PCFACE, E14NEUTI), (SP.PCOHES, LC.E1NEUTR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (OP.CHAR_MECA_CONT.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, LC.FISNO_I),
                     (OP.CHAR_MECA_CONT.PHEA_FA, E8NEUTI), (OP.CHAR_MECA_CONT.PHEA_NO, LC.N5NEUTI),
                     (SP.PINDCOI, LC.I1NEUT_I), (OP.CHAR_MECA_CONT.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_CONT.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_CONT.PPINTER, E16NEUTR), (OP.CHAR_MECA_CONT.PSEUIL, LC.E1NEUTR),
                     (OP.CHAR_MECA_CONT.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FROT(te=534,
            para_in=((OP.CHAR_MECA_FROT.PAINTER, LC.E40NEUTR), (OP.CHAR_MECA_FROT.PBASECO, LC.E32NEUTR),
                     (OP.CHAR_MECA_FROT.PCFACE, E14NEUTI), (SP.PCOHES, LC.E1NEUTR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (OP.CHAR_MECA_FROT.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, LC.FISNO_I),
                     (OP.CHAR_MECA_FROT.PHEA_FA, E8NEUTI), (OP.CHAR_MECA_FROT.PHEA_NO, LC.N5NEUTI),
                     (SP.PINDCOI, LC.I1NEUT_I), (OP.CHAR_MECA_FROT.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_FROT.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_FROT.PPINTER, E16NEUTR), (OP.CHAR_MECA_FROT.PSEUIL, LC.E1NEUTR),
                     (OP.CHAR_MECA_FROT.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=580,
            para_in=((SP.PPRESSF, CPRESSF), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=580,
            para_in=((SP.PPRESSR, EPRESNO), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=441,
            para_in=((OP.CHAR_MECA_ROTA_R.PCNSETO, LC.E72NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_ROTA_R.PHEAVTO, E24NEUI), (OP.CHAR_MECA_ROTA_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_ROTA_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_ROTA_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_ROTA_R.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_ROTA_R.PPINTTO, LC.E24NEUTR), (OP.CHAR_MECA_ROTA_R.PPMILTO, LC.E22NEUTR),
                     (SP.PROTATR, LC.CROTATR), (OP.CHAR_MECA_ROTA_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHVOIS_XFEM(te=400,
            para_in=((OP.CHVOIS_XFEM.PCNSETO, LC.E72NEUI), (OP.CHVOIS_XFEM.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.CHVOIS_XFEM.PCVOISX, LC.E18NEUI), ),
        ),

        OP.COOR_ELGA(te=481,
            para_in=((OP.COOR_ELGA.PCNSETO, LC.E72NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.COOR_ELGA.PLONCHA, LC.E10NEUTI), (OP.COOR_ELGA.PPINTTO, LC.E24NEUTR),
                     (OP.COOR_ELGA.PPMILTO, LC.E22NEUTR), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEPL_XPG(te=566,
            para_in=((OP.DEPL_XPG.PBASLOR, LC.N6NEUT_R), (SP.PDEPLNO, DDL_MECA),
                     (OP.DEPL_XPG.PHEAVTO, E24NEUI), (OP.DEPL_XPG.PHEA_NO, LC.N5NEUTI),
                     (OP.DEPL_XPG.PLONCHA, LC.E10NEUTI), (OP.DEPL_XPG.PLSN, LC.N1NEUT_R),
                     (OP.DEPL_XPG.PLST, LC.N1NEUT_R), (OP.DEPL_XPG.PXFGEOM, XFGEOM_R),
                     ),
            para_out=((SP.PDEPLPG, EDEPLPG), ),
        ),

        OP.ENEL_ELEM(te=565,
            para_in=((OP.ENEL_ELEM.PCNSETO, LC.E72NEUI), (OP.ENEL_ELEM.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELEM.PCONTPR, ECONTPG), (SP.PDEPLR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.ENEL_ELEM.PLONCHA, LC.E10NEUTI),
                     (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PPINTTO, LC.E24NEUTR),
                     (OP.ENEL_ELEM.PPMILTO, LC.E22NEUTR), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.FORC_NODA(te=542,
            para_in=((OP.FORC_NODA.PBASLOR, LC.N6NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E72NEUI),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (OP.FORC_NODA.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.FORC_NODA.PHEAVTO, E24NEUI),
                     (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI), (OP.FORC_NODA.PLONCHA, LC.E10NEUTI),
                     (OP.FORC_NODA.PLSN, LC.N1NEUT_R), (OP.FORC_NODA.PLST, LC.N1NEUT_R),
                     (OP.FORC_NODA.PPINTTO, LC.E24NEUTR), (OP.FORC_NODA.PPMILTO, LC.E22NEUTR),
                     (OP.FORC_NODA.PSTANO, STANO_I), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=539,
            para_in=((OP.FULL_MECA.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCNSETO, LC.E72NEUI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (OP.FULL_MECA.PFISNO, LC.FISNO_I), (SP.PGEOMER, NGEOMER),
                     (SP.PHEAVNO, LC.FISNO_I), (OP.FULL_MECA.PHEAVTO, E24NEUI),
                     (OP.FULL_MECA.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.FULL_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.FULL_MECA.PLSN, LC.N1NEUT_R), (OP.FULL_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.FULL_MECA.PPINTTO, LC.E24NEUTR),
                     (OP.FULL_MECA.PPMILTO, LC.E22NEUTR), (OP.FULL_MECA.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.GEOM_FAC(te=519,
            para_in=((SP.NOMFIS, E1NEUTK), (SP.PDEPLA, DDL_MECA),
                     (OP.GEOM_FAC.PFISNO, LC.FISNO_I), (OP.GEOM_FAC.PGESCLO, E16NEUTR),
                     (OP.GEOM_FAC.PHEA_FA, E8NEUTI), (OP.GEOM_FAC.PHEA_NO, LC.N5NEUTI),
                     (OP.GEOM_FAC.PLONGCO, LC.E3NEUTI), (OP.GEOM_FAC.PLST, LC.N1NEUT_R),
                     (OP.GEOM_FAC.PPINTER, E16NEUTR), ),
            para_out=((SP.PNEWGEM, E16NEUTR), (SP.PNEWGES, E16NEUTR),
                     ),
        ),

        OP.INIT_MAIL_VOIS(te=99,
            para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N6NEUT_R), (OP.INI_XFEM_ELNO.PFISNO, LC.FISNO_I),
                     (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PSTANO, STANO_I), ),
        ),

        OP.NORME_L2(te=563,
            para_in=((SP.PCALCI, LC.EMNEUT_I), (SP.PCHAMPG, EGNEUT_R),
                     (SP.PCOEFR, EMNEUT_R), (OP.NORME_L2.PCOORPG, EGGEOP_R),
                     ),
            para_out=((SP.PNORME, LC.ENORME), ),
        ),

        OP.NSPG_NBVA(te=496,
            para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
            para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
        ),

        OP.RAPH_MECA(te=539,
            para_in=((OP.RAPH_MECA.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCNSETO, LC.E72NEUI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RAPH_MECA.PHEAVTO, E24NEUI),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (OP.RAPH_MECA.PLONCHA, LC.E10NEUTI), (OP.RAPH_MECA.PLSN, LC.N1NEUT_R),
                     (OP.RAPH_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PPINTTO, LC.E24NEUTR), (OP.RAPH_MECA.PPMILTO, LC.E22NEUTR),
                     (OP.RAPH_MECA.PSTANO, STANO_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.RIGI_CONT(te=533,
            para_in=((OP.RIGI_CONT.PAINTER, LC.E40NEUTR), (OP.RIGI_CONT.PBASECO, LC.E32NEUTR),
                     (OP.RIGI_CONT.PCFACE, E14NEUTI), (SP.PCOHES, LC.E1NEUTR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (OP.RIGI_CONT.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, LC.FISNO_I),
                     (OP.RIGI_CONT.PHEA_FA, E8NEUTI), (OP.RIGI_CONT.PHEA_NO, LC.N5NEUTI),
                     (SP.PINDCOI, LC.I1NEUT_I), (OP.RIGI_CONT.PLONGCO, LC.E3NEUTI),
                     (OP.RIGI_CONT.PLSN, LC.N1NEUT_R), (OP.RIGI_CONT.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_CONT.PPINTER, E16NEUTR),
                     (OP.RIGI_CONT.PSEUIL, LC.E1NEUTR), (OP.RIGI_CONT.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_FROT(te=533,
            para_in=((OP.RIGI_FROT.PAINTER, LC.E40NEUTR), (OP.RIGI_FROT.PBASECO, LC.E32NEUTR),
                     (OP.RIGI_FROT.PCFACE, E14NEUTI), (SP.PCOHES, LC.E1NEUTR),
                     (SP.PDEPL_M, DDL_MECA), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (OP.RIGI_FROT.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PHEAVNO, LC.FISNO_I),
                     (OP.RIGI_FROT.PHEA_FA, E8NEUTI), (OP.RIGI_FROT.PHEA_NO, LC.N5NEUTI),
                     (SP.PINDCOI, LC.I1NEUT_I), (OP.RIGI_FROT.PLONGCO, LC.E3NEUTI),
                     (OP.RIGI_FROT.PLSN, LC.N1NEUT_R), (OP.RIGI_FROT.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_FROT.PPINTER, E16NEUTR),
                     (OP.RIGI_FROT.PSEUIL, LC.E1NEUTR), (OP.RIGI_FROT.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_MECA(te=539,
            para_in=((OP.RIGI_MECA.PBASLOR, LC.N6NEUT_R), (OP.RIGI_MECA.PCNSETO, LC.E72NEUI),
                     (OP.RIGI_MECA.PFISNO, LC.FISNO_I), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_MECA.PHEAVTO, E24NEUI), (OP.RIGI_MECA.PHEA_NO, LC.N5NEUTI),
                     (OP.RIGI_MECA.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA.PPINTTO, LC.E24NEUTR), (OP.RIGI_MECA.PPMILTO, LC.E22NEUTR),
                     (OP.RIGI_MECA.PSTANO, STANO_I), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_TANG(te=539,
            para_in=((OP.RIGI_MECA_TANG.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCNSETO, LC.E72NEUI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (OP.RIGI_MECA_TANG.PFISNO, LC.FISNO_I), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_MECA_TANG.PHEAVTO, E24NEUI), (OP.RIGI_MECA_TANG.PHEA_NO, LC.N5NEUTI),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (OP.RIGI_MECA_TANG.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA_TANG.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_MECA_TANG.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PPINTTO, LC.E24NEUTR), (OP.RIGI_MECA_TANG.PPMILTO, LC.E22NEUTR),
                     (OP.RIGI_MECA_TANG.PSTANO, STANO_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG), ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.SIGM_ELGA(te=546,
            para_in=((SP.PSIEFR, ECONTPG), ),
            para_out=((SP.PSIGMC, ECONTPC), (SP.PSIGMR, ECONTPG),
                     ),
        ),

        OP.TOPOFA(te=510,
            para_in=((OP.TOPOFA.PAINTTO, LC.E60NEUTR), (OP.TOPOFA.PCNSETO, LC.E72NEUI),
                     (SP.PDECOU, E1NEUTK), (SP.PFISCO, LC.FISCO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PGRADLN, LC.N2NEUT_R),
                     (SP.PGRADLT, LC.N2NEUT_R), (OP.TOPOFA.PHEAVTO, E24NEUI),
                     (OP.TOPOFA.PLONCHA, LC.E10NEUTI), (OP.TOPOFA.PLSN, LC.N1NEUT_R),
                     (OP.TOPOFA.PLST, LC.N1NEUT_R), (OP.TOPOFA.PPINTTO, LC.E24NEUTR),
                     (OP.TOPOFA.PPMILTO, LC.E22NEUTR), ),
            para_out=((OP.TOPOFA.PAINTER, LC.E40NEUTR), (OP.TOPOFA.PBASECO, LC.E32NEUTR),
                     (OP.TOPOFA.PCFACE, E14NEUTI), (SP.PGESCLA, E16NEUTR),
                     (OP.TOPOFA.PGESCLO, E16NEUTR), (SP.PGMAITR, E16NEUTR),
                     (OP.TOPOFA.PHEAVFA, LC.E8NEUI), (OP.TOPOFA.PLONGCO, LC.E3NEUTI),
                     (OP.TOPOFA.PPINTER, E16NEUTR), ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, LC.E72NEUI), (SP.PFISCO, LC.FISCO_I),
                     (OP.TOPONO.PFISNO, LC.FISNO_I), (OP.TOPONO.PHEAVFA, LC.E8NEUI),
                     (OP.TOPONO.PHEAVTO, E24NEUI), (SP.PLEVSET, LC.N1NEUT_R),
                     (OP.TOPONO.PLONCHA, LC.E10NEUTI), (OP.TOPONO.PLONGCO, LC.E3NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_FA, E8NEUTI), (OP.TOPONO.PHEA_NO, LC.N5NEUTI),
                     (OP.TOPONO.PHEA_SE, E24NEUI), ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PFISCO, LC.FISCO_I), (SP.PGEOMER, NGEOMER),
                     (SP.PLEVSET, LC.N1NEUT_R), ),
            para_out=((OP.TOPOSE.PAINTTO, LC.E60NEUTR), (OP.TOPOSE.PCNSETO, LC.E72NEUI),
                     (OP.TOPOSE.PHEAVTO, E24NEUI), (OP.TOPOSE.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOSE.PPINTTO, LC.E24NEUTR), (OP.TOPOSE.PPMILTO, LC.E22NEUTR),
                     ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA),
                     (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
                     (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.XCVBCA(te=532,
            para_in=((OP.XCVBCA.PAINTER, LC.E40NEUTR), (OP.XCVBCA.PBASECO, LC.E32NEUTR),
                     (OP.XCVBCA.PCFACE, E14NEUTI), (SP.PCOHES, LC.E1NEUTR),
                     (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
                     (OP.XCVBCA.PFISNO, LC.FISNO_I), (SP.PGEOMER, NGEOMER),
                     (SP.PGLISS, LC.I1NEUT_I), (SP.PHEAVNO, LC.FISNO_I),
                     (OP.XCVBCA.PHEA_FA, E8NEUTI), (OP.XCVBCA.PHEA_NO, LC.N5NEUTI),
                     (SP.PINDCOI, LC.I1NEUT_I), (OP.XCVBCA.PLONGCO, LC.E3NEUTI),
                     (OP.XCVBCA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PMEMCON, LC.I1NEUT_I), (OP.XCVBCA.PPINTER, E16NEUTR),
                     ),
            para_out=((OP.XCVBCA.PCOHESO, LC.E1NEUTR), (SP.PINCOCA, LC.I1NEUT_I),
                     (SP.PINDCOO, LC.I1NEUT_I), (SP.PINDMEM, LC.I1NEUT_I),
                     ),
        ),

        OP.XFEM_XPG(te=46,
            para_in=((OP.XFEM_XPG.PCNSETO, LC.E72NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.XFEM_XPG.PHEAVTO, E24NEUI), (OP.XFEM_XPG.PLONCHA, LC.E10NEUTI),
                     (OP.XFEM_XPG.PPINTTO, LC.E24NEUTR), (OP.XFEM_XPG.PPMILTO, LC.E22NEUTR),
                     ),
            para_out=((OP.XFEM_XPG.PXFGEOM, XFGEOM_R), ),
        ),

        OP.XREACL(te=548,
            para_in=((OP.XREACL.PAINTER, LC.E40NEUTR), (OP.XREACL.PBASECO, LC.E32NEUTR),
                     (OP.XREACL.PCFACE, E14NEUTI), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (SP.PGEOMER, NGEOMER),
                     (OP.XREACL.PLONGCO, LC.E3NEUTI), (OP.XREACL.PLST, LC.N1NEUT_R),
                     (OP.XREACL.PPINTER, E16NEUTR), ),
            para_out=((OP.XREACL.PSEUIL, LC.E1NEUTR), ),
        ),

    )


#------------------------------------------------------------
class MEDPTR3_XH3C(MEDPTR3_XH2C):
    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN3', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )


#------------------------------------------------------------
class MEDPTR3_XH4C(MEDPTR3_XH2C):
    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN4', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )


#------------------------------------------------------------
class MEDPQU4_XH2C(MEDPTR3_XH2C):
    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN2', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )


#------------------------------------------------------------
class MEDPQU4_XH3C(MEDPTR3_XH2C):
    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN3', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )


#------------------------------------------------------------
class MEDPQU4_XH4C(MEDPTR3_XH2C):
    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN4', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72','FPG1=FPG1',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG4',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )
