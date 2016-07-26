# coding=utf-8
# CATALOGUES DES ELEMENTS 2D_DP X-FEM CRACKTIP AVEC CONTACT (QUE LINEAIRES)

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


CCAMASS  = LocatedComponents(phys=PHY.CAMASS, type='ELEM',
    components=('C','ALPHA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','K1','K2',
          'LAGS_C','LAGS_F1',)),
    ('EN3',('DX','DY','K1','K2',)),))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='XFEM',
    components=('DX','DY','K1','K2',
          'LAGS_C','LAGS_F1',))


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




XFGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


ECONTSE  = LocatedComponents(phys=PHY.N1920R, type='ELEM',
    components=('X[144]',))


E6NEUTI  = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[6]',))


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


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MEDPTR3_XTC(Element):
    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG12','FPG1=FPG1','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )
    calculs = (

        OP.CALC_G(te=288,
            para_in=((OP.CALC_G.PAINTER, LC.E35NEUTR), (OP.CALC_G.PBASECO, LC.E28NEUTR),
                     (OP.CALC_G.PBASLOR, LC.N6NEUT_R), (OP.CALC_G.PCFACE, LC.E9NEUTI),
                     (OP.CALC_G.PCNSETO, LC.E36NEUI), (OP.CALC_G.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_G.PHEAVTO, E6NEUTI),
                     (OP.CALC_G.PLONCHA, LC.E10NEUTI), (OP.CALC_G.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_G.PLSN, LC.N1NEUT_R), (OP.CALC_G.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_G.PPINTER, LC.E14NEUTR), (OP.CALC_G.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.CALC_G.PSTANO, STANO_I),),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP(te=288,
            para_in=((OP.CALC_GTP.PAINTER, LC.E35NEUTR), (OP.CALC_GTP.PBASECO, LC.E28NEUTR),
                     (OP.CALC_GTP.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP.PCFACE, LC.E9NEUTI),
                     (OP.CALC_GTP.PCNSETO, LC.E36NEUI), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP.PHEAVTO, E6NEUTI),
                     (OP.CALC_GTP.PLONCHA, LC.E10NEUTI), (OP.CALC_GTP.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_GTP.PLSN, LC.N1NEUT_R), (OP.CALC_GTP.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_GTP.PPINTER, LC.E14NEUTR), (OP.CALC_GTP.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_GTP.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG),),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP_F(te=288,
            para_in=((OP.CALC_GTP_F.PAINTER, LC.E35NEUTR), (OP.CALC_GTP_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_GTP_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_GTP_F.PCNSETO, LC.E36NEUI), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP_F.PHEAVTO, E6NEUTI),
                     (OP.CALC_GTP_F.PLONCHA, LC.E10NEUTI), (OP.CALC_GTP_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_GTP_F.PLSN, LC.N1NEUT_R), (OP.CALC_GTP_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_GTP_F.PPINTER, LC.E14NEUTR), (OP.CALC_GTP_F.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=288,
            para_in=((OP.CALC_G_F.PAINTER, LC.E35NEUTR), (OP.CALC_G_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_G_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_G_F.PCNSETO, LC.E36NEUI), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_G_F.PHEAVTO, E6NEUTI),
                     (OP.CALC_G_F.PLONCHA, LC.E10NEUTI), (OP.CALC_G_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_G_F.PLSN, LC.N1NEUT_R), (OP.CALC_G_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_G_F.PPINTER, LC.E14NEUTR), (OP.CALC_G_F.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_K_G(te=297,
            para_in=((OP.CALC_K_G.PAINTER, LC.E35NEUTR), (OP.CALC_K_G.PBASECO, LC.E28NEUTR),
                     (OP.CALC_K_G.PBASLOR, LC.N6NEUT_R), (OP.CALC_K_G.PCFACE, LC.E9NEUTI),
                     (OP.CALC_K_G.PCNSETO, LC.E36NEUI), (OP.CALC_K_G.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_K_G.PHEAVTO, E6NEUTI),
                     (OP.CALC_K_G.PLONCHA, LC.E10NEUTI), (OP.CALC_K_G.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_K_G.PLSN, LC.N1NEUT_R), (OP.CALC_K_G.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_K_G.PPINTER, LC.E14NEUTR), (OP.CALC_K_G.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.CALC_K_G.PSTANO, STANO_I),
                     (OP.CALC_K_G.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=297,
            para_in=((OP.CALC_K_G_F.PAINTER, LC.E35NEUTR), (OP.CALC_K_G_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_K_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_K_G_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_K_G_F.PCNSETO, LC.E36NEUI), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_K_G_F.PHEAVTO, E6NEUTI),
                     (OP.CALC_K_G_F.PLONCHA, LC.E10NEUTI), (OP.CALC_K_G_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R), (OP.CALC_K_G_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_K_G_F.PPINTER, LC.E14NEUTR), (OP.CALC_K_G_F.PPINTTO, LC.E6NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.CALC_K_G.PSTANO, STANO_I), 
                     (SP.PSIGISE, ECONTSE), (OP.CALC_K_G_F.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CHAR_MECA_CONT(te=534,
            para_in=((OP.CHAR_MECA_CONT.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_CONT.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_CONT.PCFACE, LC.E9NEUTI), (SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
                     (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
                     (OP.CHAR_MECA_CONT.PLONGCO, LC.E3NEUTI), (OP.CHAR_MECA_CONT.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_CONT.PPINTER, LC.E14NEUTR), (OP.CHAR_MECA_CONT.PSEUIL, LC.E1NEUTR),
                     (OP.CHAR_MECA_CONT.PSTANO, STANO_I), (OP.CHAR_MECA_CONT.PBASLOR, LC.N6NEUT_R),
                     (SP.PMATERC, LC.CMATERC), ( OP.CHAR_MECA_CONT.PLSN, LC.N1NEUT_R),),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FF2D2D(te=440,
            para_in=((SP.PFF2D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D2D(te=440,
            para_in=((OP.CHAR_MECA_FR2D2D.PCNSETO, LC.E36NEUI), (SP.PFR2D2D, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR2D2D.PHEAVTO, E6NEUTI),
                     (OP.CHAR_MECA_FR2D2D.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_FR2D2D.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FR2D2D.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_FR2D2D.PPINTTO, LC.E6NEUTR),
                     (OP.CHAR_MECA_FR2D2D.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FROT(te=534,
            para_in=((OP.CHAR_MECA_FROT.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_FROT.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_FROT.PCFACE, LC.E9NEUTI), (SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
                     (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
                     (OP.CHAR_MECA_FROT.PLONGCO, LC.E3NEUTI), (OP.CHAR_MECA_FROT.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FROT.PPINTER, LC.E14NEUTR), (OP.CHAR_MECA_FROT.PSEUIL, LC.E1NEUTR),
                     (OP.CHAR_MECA_FROT.PSTANO, STANO_I), (OP.CHAR_MECA_FROT.PBASLOR, LC.N6NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_FROT.PLSN, LC.N1NEUT_R),),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=37,
            para_in=((OP.CHAR_MECA_PRES_F.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_F.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_PRES_F.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTER, LC.E14NEUTR),
                     (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_PRES_F.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_F.PBASLOR, LC.N6NEUT_R),),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=37,
            para_in=((OP.CHAR_MECA_PRES_R.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_R.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_PRES_R.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTER, LC.E14NEUTR),
                     (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),  (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_PRES_R.PBASLOR, LC.N6NEUT_R), (OP.CHAR_MECA_PRES_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PRES_R.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=441,
            para_in=((OP.CHAR_MECA_ROTA_R.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_ROTA_R.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_ROTA_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_ROTA_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_ROTA_R.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_ROTA_R.PPINTTO, LC.E6NEUTR),
                     (SP.PROTATR, LC.CROTATR), (OP.CHAR_MECA_ROTA_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.COOR_ELGA(te=481,
            para_in=((OP.COOR_ELGA.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.COOR_ELGA.PLONCHA, LC.E10NEUTI), (OP.COOR_ELGA.PPINTTO, LC.E6NEUTR),
                     (OP.COOR_ELGA.PPMILTO, LC.E22NEUTR), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEPL_XPG(te=566,
            para_in=((OP.DEPL_XPG.PBASLOR, LC.N6NEUT_R), (SP.PDEPLNO, DDL_MECA),
                     (OP.DEPL_XPG.PHEAVTO, E6NEUTI), (OP.DEPL_XPG.PLONCHA, LC.E10NEUTI),
                     (OP.DEPL_XPG.PLSN, LC.N1NEUT_R), (OP.DEPL_XPG.PLST, LC.N1NEUT_R),
                     (OP.DEPL_XPG.PXFGEOM, XFGEOM_R), (SP.PMATERC, LC.CMATERC),
                     (OP.DEPL_XPG.PSTANO, STANO_I), (SP.PGEOMER, NGEOMER),),
            para_out=((SP.PDEPLPG, EDEPLPG), ),
        ),

        OP.ENEL_ELEM(te=565,
            para_in=((OP.ENEL_ELEM.PCNSETO, LC.E36NEUI), (OP.ENEL_ELEM.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELEM.PCONTPR, ECONTPG), (SP.PDEPLR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.ENEL_ELEM.PLONCHA, LC.E10NEUTI),
                     (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PPINTTO, LC.E6NEUTR),
                     (OP.ENEL_ELEM.PPMILTO, LC.E22NEUTR), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.FORC_NODA(te=542,
            para_in=((OP.FORC_NODA.PBASLOR, LC.N6NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E36NEUI),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.FORC_NODA.PHEAVTO, E6NEUTI), (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI),
                     (OP.FORC_NODA.PLONCHA, LC.E10NEUTI), (OP.FORC_NODA.PLSN, LC.N1NEUT_R),
                     (OP.FORC_NODA.PLST, LC.N1NEUT_R), (OP.FORC_NODA.PPINTTO, LC.E6NEUTR),
                     (OP.FORC_NODA.PSTANO, STANO_I), (OP.FORC_NODA.PVARCPR, LC.ZVARCPG),
                     (OP.FORC_NODA.PPMILTO, LC.E22NEUTR), (SP.PMATERC, LC.CMATERC),),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=539,
            para_in=((OP.FULL_MECA.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCNSETO, LC.E36NEUI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.FULL_MECA.PHEAVTO, E6NEUTI),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (OP.FULL_MECA.PLONCHA, LC.E10NEUTI), (OP.FULL_MECA.PLSN, LC.N1NEUT_R),
                     (OP.FULL_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.FULL_MECA.PPINTTO, LC.E6NEUTR), (OP.FULL_MECA.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), (OP.FULL_MECA.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     (OP.FULL_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.GEOM_FAC(te=519,
            para_in=((SP.NOMFIS, E1NEUTK), (SP.PDEPLA, DDL_MECA),
                     (OP.GEOM_FAC.PGESCLO, LC.E14NEUTR), (OP.GEOM_FAC.PLONGCO, LC.E3NEUTI),
                     (OP.GEOM_FAC.PLST, LC.N1NEUT_R), (OP.GEOM_FAC.PPINTER, LC.E14NEUTR),
                     (OP.GEOM_FAC.PBASLOR, LC.N6NEUT_R),
                     (OP.GEOM_FAC.PLSN, LC.N1NEUT_R), (OP.GEOM_FAC.PSTANO, STANO_I),
                     (SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),),
            para_out=((SP.PBASESC, LC.E28NEUTR), (SP.PBASMAI, LC.E28NEUTR),
                     (SP.PNEWGEM, LC.E14NEUTR), (SP.PNEWGES, LC.E14NEUTR),
                     ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N6NEUT_R), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
                     ),
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
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCNSETO, LC.E36NEUI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RAPH_MECA.PHEAVTO, E6NEUTI),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (OP.RAPH_MECA.PLONCHA, LC.E10NEUTI), (OP.RAPH_MECA.PLSN, LC.N1NEUT_R),
                     (OP.RAPH_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.RAPH_MECA.PPINTTO, LC.E6NEUTR), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     (OP.RAPH_MECA.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.RIGI_CONT(te=533,
            para_in=((OP.RIGI_CONT.PAINTER, LC.E35NEUTR), (OP.RIGI_CONT.PBASECO, LC.E28NEUTR),
                     (OP.RIGI_CONT.PCFACE, LC.E9NEUTI), (SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
                     (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
                     (OP.RIGI_CONT.PLONGCO, LC.E3NEUTI), (OP.RIGI_CONT.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_CONT.PLST, LC.N1NEUT_R), (OP.RIGI_CONT.PPINTER, LC.E14NEUTR),
                     (OP.RIGI_CONT.PSEUIL, LC.E1NEUTR), (OP.RIGI_CONT.PSTANO, STANO_I),
                     (OP.RIGI_CONT.PBASLOR, LC.N6NEUT_R), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_FROT(te=533,
            para_in=((OP.RIGI_FROT.PAINTER, LC.E35NEUTR), (OP.RIGI_FROT.PBASECO, LC.E28NEUTR),
                     (OP.RIGI_FROT.PCFACE, LC.E9NEUTI), (SP.PDEPL_M, DDL_MECA),
                     (SP.PDEPL_P, DDL_MECA), (SP.PDONCO, CONTX_R),
                     (SP.PGEOMER, NGEOMER), (SP.PINDCOI, LC.E1NEUTI),
                     (OP.RIGI_FROT.PLONGCO, LC.E3NEUTI), (OP.RIGI_FROT.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_FROT.PLST, LC.N1NEUT_R), (OP.RIGI_FROT.PPINTER, LC.E14NEUTR),
                     (OP.RIGI_FROT.PSEUIL, LC.E1NEUTR), (OP.RIGI_FROT.PSTANO, STANO_I),
                     (OP.RIGI_FROT.PBASLOR, LC.N6NEUT_R), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.RIGI_MECA(te=81,
            para_in=((SP.PCAMASS, CCAMASS), (SP.PGEOMER, NGEOMER),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_TANG(te=539,
            para_in=((OP.RIGI_MECA_TANG.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCNSETO, LC.E36NEUI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA_TANG.PHEAVTO, E6NEUTI),
                     (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
                     (OP.RIGI_MECA_TANG.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA_TANG.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_MECA_TANG.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.RIGI_MECA_TANG.PPINTTO, LC.E6NEUTR), (OP.RIGI_MECA_TANG.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     (OP.RIGI_MECA_TANG.PPMILTO, LC.E22NEUTR),),
            para_out=((SP.PMATUNS, MMATUNS), (SP.PMATUUR, MMATUUR),
                     ),
        ),

        OP.SIGM_ELGA(te=546,
            para_in=((SP.PSIEFR, ECONTPG), ),
            para_out=((SP.PSIGMC, ECONTPC), (SP.PSIGMR, ECONTPG),
                     ),
        ),

        OP.TOPOFA(te=510,
            para_in=((OP.TOPOFA.PAINTTO, LC.E15NEUTR), (OP.TOPOFA.PCNSETO, LC.E36NEUI),
                     (SP.PDECOU, E1NEUTK), (SP.PGEOMER, NGEOMER),
                     (SP.PGRADLN, LC.N2NEUT_R), (SP.PGRADLT, LC.N2NEUT_R),
                     (OP.TOPOFA.PHEAVTO, E6NEUTI), (OP.TOPOFA.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOFA.PLSN, LC.N1NEUT_R), (OP.TOPOFA.PLST, LC.N1NEUT_R),
                     (OP.TOPOFA.PPINTTO, LC.E6NEUTR), (OP.TOPOFA.PPMILTO, LC.E22NEUTR),
                     ),
            para_out=((OP.TOPOFA.PAINTER, LC.E35NEUTR), (OP.TOPOFA.PBASECO, LC.E28NEUTR),
                     (OP.TOPOFA.PCFACE, LC.E9NEUTI), (SP.PGESCLA, LC.E14NEUTR),
                     (OP.TOPOFA.PGESCLO, LC.E14NEUTR), (SP.PGMAITR, LC.E14NEUTR),
                     (OP.TOPOFA.PLONGCO, LC.E3NEUTI), (OP.TOPOFA.PPINTER, LC.E14NEUTR),
                     ),
        ),

        OP.TOPONO(te=120,
            para_in=((OP.TOPONO.PCNSETO, LC.E36NEUI), (OP.TOPONO.PHEAVTO, E6NEUTI),
                     (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, E6NEUTI),
                     ),
        ),

        OP.TOPOSE(te=514,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
                     ),
            para_out=((OP.TOPOSE.PAINTTO, LC.E15NEUTR), (OP.TOPOSE.PCNSETO, LC.E36NEUI),
                     (OP.TOPOSE.PHEAVTO, E6NEUTI), (OP.TOPOSE.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOSE.PPINTTO, LC.E6NEUTR), (OP.TOPOSE.PPMILTO, LC.E22NEUTR),
                     ),
        ),

        OP.TOU_INI_ELEM(te=99,
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, LC.CGEOM2D), ),
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
            para_in=((OP.XCVBCA.PAINTER, LC.E35NEUTR), (OP.XCVBCA.PBASECO, LC.E28NEUTR),
                     (OP.XCVBCA.PCFACE, LC.E9NEUTI), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (SP.PGEOMER, NGEOMER),
                     (SP.PGLISS, LC.E1NEUTI), (SP.PINDCOI, LC.E1NEUTI),
                     (OP.XCVBCA.PLONGCO, LC.E3NEUTI), (OP.XCVBCA.PLST, LC.N1NEUT_R),
                     (SP.PMEMCON, LC.E1NEUTI), (OP.XCVBCA.PPINTER, LC.E14NEUTR),
                     (SP.PMATERC, LC.CMATERC), (OP.XCVBCA.PBASLOR, LC.N6NEUT_R),
                     (OP.XCVBCA.PLSN, LC.N1NEUT_R), (OP.XCVBCA.PSTANO, STANO_I),),
            para_out=((SP.PINCOCA, LC.E1NEUTI), (SP.PINDCOO, LC.E1NEUTI),
                     (SP.PINDMEM, LC.E1NEUTI), ),
        ),

        OP.XFEM_XPG(te=46,
            para_in=((OP.XFEM_XPG.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.XFEM_XPG.PHEAVTO, E6NEUTI), (OP.XFEM_XPG.PLONCHA, LC.E10NEUTI),
                     (OP.XFEM_XPG.PPINTTO, LC.E6NEUTR), ),
            para_out=((OP.XFEM_XPG.PXFGEOM, XFGEOM_R), ),
        ),

        OP.XREACL(te=548,
            para_in=((OP.XREACL.PAINTER, LC.E35NEUTR), (OP.XREACL.PBASECO, LC.E28NEUTR),
                     (OP.XREACL.PCFACE, LC.E9NEUTI), (SP.PDEPL_P, DDL_MECA),
                     (SP.PDONCO, CONTX_R), (SP.PGEOMER, NGEOMER),
                     (OP.XREACL.PLONGCO, LC.E3NEUTI), (OP.XREACL.PPINTER, LC.E14NEUTR),
                     ),
            para_out=((OP.XREACL.PSEUIL, LC.E1NEUTR), ),
        ),

    )


#------------------------------------------------------------
class MEDPQU4_XTC(MEDPTR3_XTC):
    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','FPG1=FPG1','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72',), mater=('RIGI','XFEM',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG12',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
        )
#------------------------------------------------------------
class MEDPTR6_XTC(MEDPTR3_XTC):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN3', (4,5,6,)),
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG3','XINT=FPG12','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6','MASS=FPG6','XINT=FPG12',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
            )

    calculs = (
        OP.CHAR_MECA_CONT_M(te=-1),

        OP.PILO_PRED_ELAS_M(te=-1),

        OP.RIGI_CONT_M(te=-1),

        OP.XCVBCA_MORTAR(te=-1),
    )


#------------------------------------------------------------
class MEDPQU8_XTC(MEDPTR3_XTC):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN3', (5,6,7,8,)),
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM144','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','XINT=FPG12',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6','MASS=FPG6','XINT=FPG12',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG2','MASS=FPG3','FPG2=FPG2','FPG3=FPG3','FPG4=FPG4','NOEU=NOEU','GAUSS=FPG3','SIMP=SIMP','COTES=COTES','SIMP1=SIMP1','COTES1=COTES1','COTES2=COTES2',),),
            )


