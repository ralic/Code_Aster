# coding=utf-8
# CATALOGUES DES ELEMENTS AXI X-FEM HEAVISIDE SANS CONTACT

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
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','H1X','H1Y',)),))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='XFEM',
    components=('DX','DY','H1X','H1Y',))


DDL_MECC = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EERREUR  = LocatedComponents(phys=PHY.ERRE_R, type='ELEM',
    components=('ERREST','NUEST','SIGCAL','TERMRE','TERMR2',
          'TERMNO','TERMN2','TERMSA','TERMS2','TAILLE',))


CFORCEF  = LocatedComponents(phys=PHY.FORC_F, type='ELEM',
    components=('FX','FY',))


EFORCER  = LocatedComponents(phys=PHY.FORC_R, type='ELGA', location='XFEM',
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


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY',))


ETEMXPG  = LocatedComponents(phys=PHY.TEMP_R, type='ELGA', location='MATER',
    components=('TEMP','DTX','DTY',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='XFEM',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=DDL_MECA)

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=DDL_MECA)


#------------------------------------------------------------
class MEAXTR3_XH(Element):
    """Please document this element"""
    meshType = MT.TRIA3
    nodes = (
            SetOfNodes('EN1', (1,2,3,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG12','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM36','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3',),),
        )
    calculs = (

        OP.CALC_G(te=288,
            para_in=((OP.CALC_G.PAINTER, LC.E35NEUTR), (OP.CALC_G.PBASECO, LC.E28NEUTR),
                     (OP.CALC_G.PBASLOR, LC.N6NEUT_R), (OP.CALC_G.PCFACE, LC.E9NEUTI),
                     (OP.CALC_G.PCNSETO, LC.E36NEUI), (OP.CALC_G.PCOMPOR, CCOMPOR),
                     (OP.CALC_G.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFRVOLU, NFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_G.PHEAVTO, E6NEUTI), (OP.CALC_G.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_G.PLONCHA, LC.E10NEUTI), (OP.CALC_G.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_G.PLSN, LC.N1NEUT_R), (OP.CALC_G.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_G.PPINTER, LC.E14NEUTR), (OP.CALC_G.PPINTTO, LC.E6NEUTR),
                     (OP.CALC_G.PPMILTO, LC.E22NEUTR), (SP.PPRESSR, EPRESNO),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP(te=288,
            para_in=((OP.CALC_GTP.PAINTER, LC.E35NEUTR), (OP.CALC_GTP.PBASECO, LC.E28NEUTR),
                     (OP.CALC_GTP.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP.PCFACE, LC.E9NEUTI),
                     (OP.CALC_GTP.PCNSETO, LC.E36NEUI), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP.PHEAVTO, E6NEUTI),
                     (OP.CALC_GTP.PHEA_NO, LC.N5NEUTI), (OP.CALC_GTP.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_GTP.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP.PLSN, LC.N1NEUT_R),
                     (OP.CALC_GTP.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP.PPINTER, LC.E14NEUTR),
                     (OP.CALC_GTP.PPINTTO, LC.E6NEUTR), (OP.CALC_GTP.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP_F(te=288,
            para_in=((OP.CALC_GTP_F.PAINTER, LC.E35NEUTR), (OP.CALC_GTP_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_GTP_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_GTP_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_GTP_F.PCNSETO, LC.E36NEUI), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP_F.PHEAVTO, E6NEUTI),
                     (OP.CALC_GTP_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_GTP_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_GTP_F.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_GTP_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP_F.PPINTER, LC.E14NEUTR),
                     (OP.CALC_GTP_F.PPINTTO, LC.E6NEUTR), (OP.CALC_GTP_F.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=288,
            para_in=((OP.CALC_G_F.PAINTER, LC.E35NEUTR), (OP.CALC_G_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_G_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_G_F.PCNSETO, LC.E36NEUI), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
                     (OP.CALC_G_F.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_G_F.PHEAVTO, E6NEUTI), (OP.CALC_G_F.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_G_F.PLONCHA, LC.E10NEUTI), (OP.CALC_G_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_G_F.PLSN, LC.N1NEUT_R), (OP.CALC_G_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_G_F.PPINTER, LC.E14NEUTR), (OP.CALC_G_F.PPINTTO, LC.E6NEUTR),
                     (OP.CALC_G_F.PPMILTO, LC.E22NEUTR), (SP.PPRESSF, CPRESSF),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
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
                     (OP.CALC_K_G.PHEA_NO, LC.N5NEUTI), (OP.CALC_K_G.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_K_G.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G.PLSN, LC.N1NEUT_R),
                     (OP.CALC_K_G.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G.PPINTER, LC.E14NEUTR),
                     (OP.CALC_K_G.PPINTTO, LC.E6NEUTR), (OP.CALC_K_G.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSR, EPRESNO), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=297,
            para_in=((OP.CALC_K_G_F.PAINTER, LC.E35NEUTR), (OP.CALC_K_G_F.PBASECO, LC.E28NEUTR),
                     (OP.CALC_K_G_F.PBASLOR, LC.N6NEUT_R), (OP.CALC_K_G_F.PCFACE, LC.E9NEUTI),
                     (OP.CALC_K_G_F.PCNSETO, LC.E36NEUI), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_K_G_F.PHEAVTO, E6NEUTI),
                     (OP.CALC_K_G_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_K_G_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_K_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_K_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_K_G_F.PPINTER, LC.E14NEUTR),
                     (OP.CALC_K_G_F.PPINTTO, LC.E6NEUTR), (OP.CALC_K_G_F.PPMILTO, LC.E22NEUTR),
                     (SP.PPRESSF, CPRESSF), (SP.PPULPRO, LC.CFREQR),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CHAR_MECA_FF2D2D(te=440,
            para_in=((SP.PFF2D2D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR2D2D(te=440,
            para_in=((OP.CHAR_MECA_FR2D2D.PCNSETO, LC.E36NEUI), (SP.PFR2D2D, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR2D2D.PHEAVTO, E6NEUTI),
                     (OP.CHAR_MECA_FR2D2D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FR2D2D.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_FR2D2D.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_FR2D2D.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FR2D2D.PPINTTO, LC.E6NEUTR), (OP.CHAR_MECA_FR2D2D.PPMILTO, LC.E22NEUTR),
                     (OP.CHAR_MECA_FR2D2D.PSTANO, STANO_I), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=441,
            para_in=((OP.CHAR_MECA_PESA_R.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PESA_R.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_PESA_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_PESA_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_PESA_R.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CHAR_MECA_PESA_R.PPINTTO, LC.E6NEUTR), (OP.CHAR_MECA_PESA_R.PPMILTO, LC.E22NEUTR),
                     (OP.CHAR_MECA_PESA_R.PSTANO, STANO_I), (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=37,
            para_in=((OP.CHAR_MECA_PRES_F.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_F.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_PRES_F.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTER, LC.E14NEUTR),
                     (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=37,
            para_in=((OP.CHAR_MECA_PRES_R.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_R.PBASECO, LC.E28NEUTR),
                     (OP.CHAR_MECA_PRES_R.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTER, LC.E14NEUTR),
                     (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=441,
            para_in=((OP.CHAR_MECA_ROTA_R.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_ROTA_R.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_ROTA_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_ROTA_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_ROTA_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_ROTA_R.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_ROTA_R.PPINTTO, LC.E6NEUTR), (OP.CHAR_MECA_ROTA_R.PPMILTO, LC.E22NEUTR),
                     (SP.PROTATR, LC.CROTATR), (OP.CHAR_MECA_ROTA_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=541,
            para_in=((OP.CHAR_MECA_TEMP_R.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (OP.CHAR_MECA_TEMP_R.PCNSETO, LC.E36NEUI), (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_TEMP_R.PHEAVTO, E6NEUTI),
                     (OP.CHAR_MECA_TEMP_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_TEMP_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_TEMP_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_TEMP_R.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_TEMP_R.PPINTTO, LC.E6NEUTR),
                     (OP.CHAR_MECA_TEMP_R.PPMILTO, LC.E22NEUTR), (OP.CHAR_MECA_TEMP_R.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRT, ECONTPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.CHVOIS_XFEM(te=400,
            para_in=((OP.CHVOIS_XFEM.PCNSETO, LC.E36NEUI), (OP.CHVOIS_XFEM.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.CHVOIS_XFEM.PCVOISX, LC.E18NEUI), ),
        ),

        OP.COOR_ELGA(te=481,
            para_in=((OP.COOR_ELGA.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.COOR_ELGA.PLONCHA, LC.E10NEUTI), (OP.COOR_ELGA.PPINTTO, LC.E6NEUTR),
                     (OP.COOR_ELGA.PPMILTO, LC.E22NEUTR), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEPL_XPG(te=566,
            para_in=((OP.DEPL_XPG.PBASLOR, LC.N6NEUT_R), (SP.PDEPLNO, DDL_MECA),
                     (OP.DEPL_XPG.PHEAVTO, E6NEUTI), (OP.DEPL_XPG.PHEA_NO, LC.N5NEUTI),
                     (OP.DEPL_XPG.PLONCHA, LC.E10NEUTI), (OP.DEPL_XPG.PLSN, LC.N1NEUT_R),
                     (OP.DEPL_XPG.PLST, LC.N1NEUT_R), (OP.DEPL_XPG.PXFGEOM, XFGEOM_R),
                     ),
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

        OP.ERME_ELEM(te=382,
            para_in=((OP.ERME_ELEM.PCNSETO, LC.E36NEUI), (SP.PCONTNO, ECONTNO),
                     (OP.ERME_ELEM.PCONTSER, ECONTSE), (OP.ERME_ELEM.PCVOISX, LC.E18NEUI),
                     (SP.PFFVOLU, CFORCEF), (SP.PFORCE, LC.CREFERI),
                     (SP.PFRVOLU, EFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.ERME_ELEM.PLONCHA, LC.E10NEUTI), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.ERME_ELEM.PPINTTO, LC.E6NEUTR),
                     (OP.ERME_ELEM.PPMILTO, LC.E22NEUTR), (SP.PPRESS, LC.CREFERI),
                     (SP.PROTATR, LC.CROTATR), (SP.PTEMPSR, CTEMPSR),
                     (OP.ERME_ELEM.PVOISIN, LC.EVOISIN), ),
            para_out=((OP.ERME_ELEM.PERREUR, EERREUR), ),
        ),

        OP.FORC_NODA(te=542,
            para_in=((OP.FORC_NODA.PBASLOR, LC.N6NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E36NEUI),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.FORC_NODA.PHEAVTO, E6NEUTI), (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI),
                     (OP.FORC_NODA.PLONCHA, LC.E10NEUTI), (OP.FORC_NODA.PLSN, LC.N1NEUT_R),
                     (OP.FORC_NODA.PLST, LC.N1NEUT_R), (OP.FORC_NODA.PPINTTO, LC.E6NEUTR),
                     (OP.FORC_NODA.PPMILTO, LC.E22NEUTR), (OP.FORC_NODA.PSTANO, STANO_I),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=539,
            para_in=((OP.FULL_MECA.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCNSETO, LC.E36NEUI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.FULL_MECA.PHEAVTO, E6NEUTI),
                     (OP.FULL_MECA.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.FULL_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.FULL_MECA.PLSN, LC.N1NEUT_R), (OP.FULL_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.FULL_MECA.PPINTTO, LC.E6NEUTR),
                     (OP.FULL_MECA.PPMILTO, LC.E22NEUTR), (OP.FULL_MECA.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.INIT_MAIL_VOIS(te=99,
            para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), (OP.INIT_VARC.PVARCNO, LC.ZVARCNO), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N6NEUT_R), (OP.INI_XFEM_ELNO.PFISNO, LC.FISNO_I),
                     (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PSTANO, STANO_I), ),
        ),

        OP.MASS_MECA(te=538,
            para_in=((OP.MASS_MECA.PBASLOR, LC.N6NEUT_R), (OP.MASS_MECA.PCNSETO, LC.E36NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.MASS_MECA.PHEAVTO, E6NEUTI),
                     (OP.MASS_MECA.PHEA_NO, LC.N5NEUTI), (OP.MASS_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.MASS_MECA.PLSN, LC.N1NEUT_R), (OP.MASS_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA.PPINTTO, LC.E6NEUTR),
                     (OP.MASS_MECA.PPMILTO, LC.E22NEUTR), (OP.MASS_MECA.PSTANO, STANO_I),
                     (OP.MASS_MECA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMATUUR, MMATUUR), ),
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
                     (OP.RAPH_MECA.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.RAPH_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.RAPH_MECA.PLSN, LC.N1NEUT_R), (OP.RAPH_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RAPH_MECA.PPINTTO, LC.E6NEUTR),
                     (OP.RAPH_MECA.PPMILTO, LC.E22NEUTR), (OP.RAPH_MECA.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.RAPH_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.RIGI_MECA(te=539,
            para_in=((OP.RIGI_MECA.PBASLOR, LC.N6NEUT_R), (OP.RIGI_MECA.PCNSETO, LC.E36NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA.PHEAVTO, E6NEUTI),
                     (OP.RIGI_MECA.PHEA_NO, LC.N5NEUTI), (OP.RIGI_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.RIGI_MECA.PLSN, LC.N1NEUT_R), (OP.RIGI_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PPINTTO, LC.E6NEUTR),
                     (OP.RIGI_MECA.PPMILTO, LC.E22NEUTR), (OP.RIGI_MECA.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_GE(te=536,
            para_in=((OP.RIGI_MECA_GE.PBASLOR, LC.N6NEUT_R), (OP.RIGI_MECA_GE.PCNSETO, LC.E36NEUI),
                     (OP.RIGI_MECA_GE.PCONTRR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_MECA_GE.PHEAVTO, E6NEUTI), (OP.RIGI_MECA_GE.PHEA_NO, LC.N5NEUTI),
                     (OP.RIGI_MECA_GE.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA_GE.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_MECA_GE.PLST, LC.N1NEUT_R), (OP.RIGI_MECA_GE.PPINTTO, LC.E6NEUTR),
                     (OP.RIGI_MECA_GE.PPMILTO, LC.E22NEUTR), (OP.RIGI_MECA_GE.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_TANG(te=539,
            para_in=((OP.RIGI_MECA_TANG.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCNSETO, LC.E36NEUI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA_TANG.PHEAVTO, E6NEUTI),
                     (OP.RIGI_MECA_TANG.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.RIGI_MECA_TANG.PLONCHA, LC.E10NEUTI),
                     (OP.RIGI_MECA_TANG.PLSN, LC.N1NEUT_R), (OP.RIGI_MECA_TANG.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA_TANG.PPINTTO, LC.E6NEUTR),
                     (OP.RIGI_MECA_TANG.PPMILTO, LC.E22NEUTR), (OP.RIGI_MECA_TANG.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=261,
            para_in=((OP.SIEF_ELGA.PBASLOR, LC.N6NEUT_R), (SP.PCAMASS, CCAMASS),
                     (OP.SIEF_ELGA.PCNSETO, LC.E36NEUI), (OP.SIEF_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (OP.SIEF_ELGA.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.SIEF_ELGA.PHEAVTO, E6NEUTI),
                     (OP.SIEF_ELGA.PHEA_NO, LC.N5NEUTI), (OP.SIEF_ELGA.PLONCHA, LC.E10NEUTI),
                     (OP.SIEF_ELGA.PLSN, LC.N1NEUT_R), (OP.SIEF_ELGA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PPINTTO, LC.E6NEUTR),
                     (OP.SIEF_ELGA.PPMILTO, LC.E22NEUTR), (OP.SIEF_ELGA.PSTANO, STANO_I),
                     (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.SIEF_ELGA.PCONTRR, ECONTPG), ),
        ),

        OP.SIEF_ELNO(te=289,
            para_in=((OP.SIEF_ELNO.PCNSETO, LC.E36NEUI), (OP.SIEF_ELNO.PCONTRR, ECONTPG),
                     (OP.SIEF_ELNO.PLONCHA, LC.E10NEUTI), (OP.SIEF_ELNO.PVARCPR, LC.ZVARCPG),
                     ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIEF_ELNO.PSIEFNOR, ECONTNO),
                     ),
        ),

        OP.SIGM_ELGA(te=546,
            para_in=((SP.PSIEFR, ECONTPG), ),
            para_out=((SP.PSIGMC, ECONTPC), (SP.PSIGMR, ECONTPG),
                     ),
        ),

        OP.SIGM_ELNO(te=289,
            para_in=((OP.SIGM_ELNO.PCNSETO, LC.E36NEUI), (OP.SIGM_ELNO.PCONTRR, ECONTPG),
                     (OP.SIGM_ELNO.PLONCHA, LC.E10NEUTI), ),
            para_out=((SP.PSIEFNOC, ECONTNC), (OP.SIGM_ELNO.PSIEFNOR, ECONTNO),
                     ),
        ),

        OP.SISE_ELNO(te=289,
            para_in=((OP.SISE_ELNO.PCONTRR, ECONTPG), (OP.SISE_ELNO.PLONCHA, LC.E10NEUTI),
                     ),
            para_out=((OP.SISE_ELNO.PCONTSER, ECONTSE), ),
        ),

        OP.TOPOFA(te=510,
            para_in=((OP.TOPOFA.PAINTTO, LC.E15NEUTR), (OP.TOPOFA.PCNSETO, LC.E36NEUI),
                     (SP.PDECOU, E1NEUTK), (SP.PGEOMER, NGEOMER),
                     (SP.PGRADLN, LC.N2NEUT_R), (SP.PGRADLT, LC.N2NEUT_R),
                     (OP.TOPOFA.PHEAVTO, E6NEUTI), (OP.TOPOFA.PLONCHA, LC.E10NEUTI),
                     (OP.TOPOFA.PLSN, LC.N1NEUT_R), (OP.TOPOFA.PLST, LC.N1NEUT_R),
                     (OP.TOPOFA.PPINTTO, LC.E6NEUTR), (OP.TOPOFA.PPMILTO, LC.E22NEUTR),
                     (SP.PTYPDIS, LC.E1NEUTI), ),
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
                     (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (SP.PTEMP_R, ETEMXPG),
                     (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG), ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.XFEM_XPG(te=46,
            para_in=((OP.XFEM_XPG.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.XFEM_XPG.PHEAVTO, E6NEUTI), (OP.XFEM_XPG.PLONCHA, LC.E10NEUTI),
                     (OP.XFEM_XPG.PPINTTO, LC.E6NEUTR), (OP.XFEM_XPG.PPMILTO, LC.E22NEUTR),
                     ),
            para_out=((OP.XFEM_XPG.PXFGEOM, XFGEOM_R), ),
        ),

    )


#------------------------------------------------------------
class MEAXQU4_XH(MEAXTR3_XH):
    """Please document this element"""
    meshType = MT.QUAD4
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','XINT=FPG12',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3',),),
        )


#------------------------------------------------------------
class MEAXTR6_XH(MEAXTR3_XH):
    """Please document this element"""
    meshType = MT.TRIA6
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','XINT=FPG12','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM72','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG4',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3',),),
        )


#------------------------------------------------------------
class MEAXQU8_XH(MEAXTR3_XH):
    """Please document this element"""
    meshType = MT.QUAD8
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','NOEU_S=NOEU_S','NOEU=NOEU','XFEM=XFEM144','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','XINT=FPG12',),),
            ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG3',),),
            ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG2','MASS=FPG3',),),
        )
