# coding=utf-8
# CATALOGUES DES ELEMENTS X-FEM HEAVISIDE-CRACKTIP SANS CONTACT (LINEAIRES ET QUADRATIQUES)

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
    components=('C','ALPHA','BETA','KAPPA','X',
          'Y','Z',))


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
    ('EN1',('DX','DY','DZ','H1X','H1Y',
          'H1Z','E1X','E1Y','E1Z','E2X',
          'E2Y','E2Z','E3X','E3Y','E3Z',
          'E4X','E4Y','E4Z',)),))


EDEPLPG  = LocatedComponents(phys=PHY.DEPL_R, type='ELGA', location='XFEM',
    components=('DX','DY','DZ','H1X','H1Y',
          'H1Z','E1X','E1Y','E1Z','E2X',
          'E2Y','E2Z','E3X','E3Y','E3Z',
          'E4X','E4Y','E4Z',))


DDL_MECC = LocatedComponents(phys=PHY.DEPL_R, type='ELNO',
    components=('DX','DY','DZ',))


EENERR   = LocatedComponents(phys=PHY.ENER_R, type='ELEM',
    components=('TOTALE',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


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


EGGEOP_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z','W',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y','Z',))


CGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELEM',
    components=('X','Y','Z',))


XFGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y','Z',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E33NEUTR = LocatedComponents(phys=PHY.N132_R, type='ELEM',
    components=('X[33]',))


ECONTSE  = LocatedComponents(phys=PHY.N1920R, type='ELEM',
    components=('X[1920]',))


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


ECONTNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ETEMXPG  = LocatedComponents(phys=PHY.TEMP_R, type='ELGA', location='MATER',
    components=('TEMP','DTX','DTY','DTZ',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='XFEM',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
class MECA_XHT_HEXA8(Element):
    """Please document this element"""
    meshType = MT.HEXA8
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,)),
        )
    elrefe =(
            ElrefeLoc(MT.HE8, gauss = ('RIGI=FPG8','MASS=FPG8','NOEU=NOEU','XFEM=XFEM480','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )
    calculs = (

#       -- te0580 : ne resout que les cas triviaux : 0.
        OP.AMOR_MECA(te=580,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.CALC_G(te=288,
            para_in=((OP.CALC_G.PAINTER, LC.E170NEUT), (OP.CALC_G.PBASECO, LC.E306NEUT),
                     (OP.CALC_G.PBASLOR, LC.N9NEUT_R), (OP.CALC_G.PCFACE, LC.E90NEUTI),
                     (OP.CALC_G.PCNSETO, LC.E320NEUI), (OP.CALC_G.PCOMPOR, CCOMPOR),
                     (OP.CALC_G.PCONTRR, ECONTPG), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFRVOLU, NFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_G.PHEAVTO, LC.E32NEUTI), (OP.CALC_G.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_G.PLONCHA, LC.E10NEUTI), (OP.CALC_G.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_G.PLSN, LC.N1NEUT_R), (OP.CALC_G.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_G.PPINTER, LC.E102NEUT), (OP.CALC_G.PPINTTO, E33NEUTR),
                     (OP.CALC_G.PPMILTO, LC.E198NEUT), (SP.PPRESSR, EPRESNO),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_G.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP(te=288,
            para_in=((OP.CALC_GTP.PAINTER, LC.E170NEUT), (OP.CALC_GTP.PBASECO, LC.E306NEUT),
                     (OP.CALC_GTP.PBASLOR, LC.N9NEUT_R), (OP.CALC_GTP.PCFACE, LC.E90NEUTI),
                     (OP.CALC_GTP.PCNSETO, LC.E320NEUI), (OP.CALC_GTP.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFRVOLU, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_GTP.PHEAVTO, LC.E32NEUTI),
                     (OP.CALC_GTP.PHEA_NO, LC.N5NEUTI), (OP.CALC_GTP.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_GTP.PLONGCO, LC.E3NEUTI), (OP.CALC_GTP.PLSN, LC.N1NEUT_R),
                     (OP.CALC_GTP.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_GTP.PPINTER, LC.E102NEUT),
                     (OP.CALC_GTP.PPINTTO, E33NEUTR), (OP.CALC_GTP.PPMILTO, LC.E198NEUT),
                     (SP.PPRESSR, EPRESNO), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_GTP.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_GTP_F(te=288,
            para_in=((OP.CALC_GTP_F.PAINTER, LC.E170NEUT), (OP.CALC_GTP_F.PBASECO, LC.E306NEUT),
                     (OP.CALC_GTP_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_GTP_F.PCFACE, LC.E90NEUTI),
                     (OP.CALC_GTP_F.PCNSETO, LC.E320NEUI), (OP.CALC_GTP_F.PCOMPOR, CCOMPOR),
                     (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_GTP_F.PHEAVTO, LC.E32NEUTI), (OP.CALC_GTP_F.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_GTP_F.PLONCHA, LC.E10NEUTI), (OP.CALC_GTP_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_GTP_F.PLSN, LC.N1NEUT_R), (OP.CALC_GTP_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_GTP_F.PPINTER, LC.E102NEUT), (OP.CALC_GTP_F.PPINTTO, E33NEUTR),
                     (OP.CALC_GTP_F.PPMILTO, LC.E198NEUT), (SP.PPRESSF, CPRESSF),
                     (SP.PROTATR, LC.CROTATR), (SP.PSIGISE, ECONTSE),
                     (SP.PTEMPSR, CTEMPSR), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_GTP_F.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_G_F(te=288,
            para_in=((OP.CALC_G_F.PAINTER, LC.E170NEUT), (OP.CALC_G_F.PBASECO, LC.E306NEUT),
                     (OP.CALC_G_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_G_F.PCFACE, LC.E90NEUTI),
                     (OP.CALC_G_F.PCNSETO, LC.E320NEUI), (OP.CALC_G_F.PCOMPOR, CCOMPOR),
                     (OP.CALC_G_F.PCONTRR, ECONTPG), (SP.PCOURB, LC.G27NEUTR),
                     (SP.PDEPLAR, DDL_MECA), (SP.PFFVOLU, CFORCEF),
                     (SP.PGEOMER, NGEOMER), (OP.CALC_G_F.PHEAVTO, LC.E32NEUTI),
                     (OP.CALC_G_F.PHEA_NO, LC.N5NEUTI), (OP.CALC_G_F.PLONCHA, LC.E10NEUTI),
                     (OP.CALC_G_F.PLONGCO, LC.E3NEUTI), (OP.CALC_G_F.PLSN, LC.N1NEUT_R),
                     (OP.CALC_G_F.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CALC_G_F.PPINTER, LC.E102NEUT),
                     (OP.CALC_G_F.PPINTTO, E33NEUTR), (OP.CALC_G_F.PPMILTO, LC.E198NEUT),
                     (SP.PPRESSF, CPRESSF), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_G_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, LC.EGTHETA), ),
        ),

        OP.CALC_K_G(te=297,
            para_in=((OP.CALC_K_G.PAINTER, LC.E170NEUT), (OP.CALC_K_G.PBASECO, LC.E306NEUT),
                     (OP.CALC_K_G.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G.PCFACE, LC.E90NEUTI),
                     (OP.CALC_K_G.PCNSETO, LC.E320NEUI), (OP.CALC_K_G.PCOMPOR, CCOMPOR),
                     (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFRVOLU, NFORCER), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_K_G.PHEAVTO, LC.E32NEUTI), (OP.CALC_K_G.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_K_G.PLONCHA, LC.E10NEUTI), (OP.CALC_K_G.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_K_G.PLSN, LC.N1NEUT_R), (OP.CALC_K_G.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_K_G.PPINTER, LC.E102NEUT), (OP.CALC_K_G.PPINTTO, E33NEUTR),
                     (OP.CALC_K_G.PPMILTO, LC.E198NEUT), (SP.PPRESSR, EPRESNO),
                     (SP.PPULPRO, LC.CFREQR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTHETAR, DDL_MECC),
                     (OP.CALC_K_G.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CALC_K_G_F(te=297,
            para_in=((OP.CALC_K_G_F.PAINTER, LC.E170NEUT), (OP.CALC_K_G_F.PBASECO, LC.E306NEUT),
                     (OP.CALC_K_G_F.PBASLOR, LC.N9NEUT_R), (OP.CALC_K_G_F.PCFACE, LC.E90NEUTI),
                     (OP.CALC_K_G_F.PCNSETO, LC.E320NEUI), (OP.CALC_K_G_F.PCOMPOR, CCOMPOR),
                     (SP.PCOURB, LC.G27NEUTR), (SP.PDEPLAR, DDL_MECA),
                     (SP.PFFVOLU, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (OP.CALC_K_G_F.PHEAVTO, LC.E32NEUTI), (OP.CALC_K_G_F.PHEA_NO, LC.N5NEUTI),
                     (OP.CALC_K_G_F.PLONCHA, LC.E10NEUTI), (OP.CALC_K_G_F.PLONGCO, LC.E3NEUTI),
                     (OP.CALC_K_G_F.PLSN, LC.N1NEUT_R), (OP.CALC_K_G_F.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (SP.PPESANR, LC.CPESANR),
                     (OP.CALC_K_G_F.PPINTER, LC.E102NEUT), (OP.CALC_K_G_F.PPINTTO, E33NEUTR),
                     (OP.CALC_K_G_F.PPMILTO, LC.E198NEUT), (SP.PPRESSF, CPRESSF),
                     (SP.PPULPRO, LC.CFREQR), (SP.PROTATR, LC.CROTATR),
                     (SP.PSIGISE, ECONTSE), (SP.PTEMPSR, CTEMPSR),
                     (SP.PTHETAR, DDL_MECC), (OP.CALC_K_G_F.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PGTHETA, EKTHETA), ),
        ),

        OP.CHAR_MECA_FF3D3D(te=440,
            para_in=((SP.PFF3D3D, CFORCEF), (SP.PGEOMER, NGEOMER),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_FR3D3D(te=440,
            para_in=((OP.CHAR_MECA_FR3D3D.PCNSETO, LC.E320NEUI), (SP.PFR3D3D, NFORCER),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_FR3D3D.PHEAVTO, LC.E32NEUTI),
                     (OP.CHAR_MECA_FR3D3D.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_FR3D3D.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_FR3D3D.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_FR3D3D.PLST, LC.N1NEUT_R),
                     (OP.CHAR_MECA_FR3D3D.PPINTTO, E33NEUTR), (OP.CHAR_MECA_FR3D3D.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PESA_R(te=441,
            para_in=((OP.CHAR_MECA_PESA_R.PCNSETO, LC.E320NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PESA_R.PHEAVTO, LC.E32NEUTI), (OP.CHAR_MECA_PESA_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_PESA_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PESA_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_PESA_R.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PPINTTO, E33NEUTR),
                     (OP.CHAR_MECA_PESA_R.PPMILTO, LC.E198NEUT), (OP.CHAR_MECA_PESA_R.PSTANO, STANO_I),
                     (OP.CHAR_MECA_PESA_R.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_F(te=37,
            para_in=((OP.CHAR_MECA_PRES_F.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_PRES_F.PBASECO, LC.E306NEUT),
                     (OP.CHAR_MECA_PRES_F.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTER, LC.E102NEUT),
                     (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_PRES_R(te=37,
            para_in=((OP.CHAR_MECA_PRES_R.PAINTER, LC.E170NEUT), (OP.CHAR_MECA_PRES_R.PBASECO, LC.E306NEUT),
                     (OP.CHAR_MECA_PRES_R.PCFACE, LC.E90NEUTI), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PLONGCO, LC.E3NEUTI),
                     (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTER, LC.E102NEUT),
                     (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_ROTA_R(te=441,
            para_in=((OP.CHAR_MECA_ROTA_R.PCNSETO, LC.E320NEUI), (SP.PDEPLMR, DDL_MECA),
                     (SP.PDEPLPR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.CHAR_MECA_ROTA_R.PHEAVTO, LC.E32NEUTI), (OP.CHAR_MECA_ROTA_R.PHEA_NO, LC.N5NEUTI),
                     (OP.CHAR_MECA_ROTA_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_ROTA_R.PLSN, LC.N1NEUT_R),
                     (OP.CHAR_MECA_ROTA_R.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
                     (OP.CHAR_MECA_ROTA_R.PPINTTO, E33NEUTR), (OP.CHAR_MECA_ROTA_R.PPMILTO, LC.E198NEUT),
                     (SP.PROTATR, LC.CROTATR), (OP.CHAR_MECA_ROTA_R.PSTANO, STANO_I),
                     ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.CHAR_MECA_TEMP_R(te=541,
            para_in=((OP.CHAR_MECA_TEMP_R.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
                     (OP.CHAR_MECA_TEMP_R.PCNSETO, LC.E320NEUI), (OP.CHAR_MECA_TEMP_R.PCOMPOR, CCOMPOR),
                     (SP.PGEOMER, NGEOMER), (OP.CHAR_MECA_TEMP_R.PHEAVTO, LC.E32NEUTI),
                     (OP.CHAR_MECA_TEMP_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_TEMP_R.PLONCHA, LC.E10NEUTI),
                     (OP.CHAR_MECA_TEMP_R.PLSN, LC.N1NEUT_R), (OP.CHAR_MECA_TEMP_R.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.CHAR_MECA_TEMP_R.PPINTTO, E33NEUTR),
                     (OP.CHAR_MECA_TEMP_R.PPMILTO, LC.E198NEUT), (OP.CHAR_MECA_TEMP_R.PSTANO, STANO_I),
                     (SP.PTEMPSR, CTEMPSR), (OP.CHAR_MECA_TEMP_R.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), ),
            para_out=((SP.PCONTRT, ECONTPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.COOR_ELGA(te=481,
            para_in=((OP.COOR_ELGA.PCNSETO, LC.E320NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.COOR_ELGA.PLONCHA, LC.E10NEUTI), (OP.COOR_ELGA.PPINTTO, E33NEUTR),
                     (OP.COOR_ELGA.PPMILTO, LC.E198NEUT), ),
            para_out=((OP.COOR_ELGA.PCOORPG, EGGEOP_R), ),
        ),

        OP.DEPL_XPG(te=566,
            para_in=((OP.DEPL_XPG.PBASLOR, LC.N9NEUT_R), (SP.PDEPLNO, DDL_MECA),
                     (OP.DEPL_XPG.PHEAVTO, LC.E32NEUTI), (OP.DEPL_XPG.PHEA_NO, LC.N5NEUTI),
                     (OP.DEPL_XPG.PLONCHA, LC.E10NEUTI), (OP.DEPL_XPG.PLSN, LC.N1NEUT_R),
                     (OP.DEPL_XPG.PLST, LC.N1NEUT_R), (OP.DEPL_XPG.PXFGEOM, XFGEOM_R),
                     ),
            para_out=((SP.PDEPLPG, EDEPLPG), ),
        ),

        OP.ENEL_ELEM(te=565,
            para_in=((OP.ENEL_ELEM.PCNSETO, LC.E320NEUI), (OP.ENEL_ELEM.PCOMPOR, CCOMPOR),
                     (OP.ENEL_ELEM.PCONTPR, ECONTPG), (SP.PDEPLR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.ENEL_ELEM.PLONCHA, LC.E10NEUTI),
                     (SP.PMATERC, LC.CMATERC), (OP.ENEL_ELEM.PPINTTO, E33NEUTR),
                     (OP.ENEL_ELEM.PPMILTO, LC.E198NEUT), (OP.ENEL_ELEM.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.ENEL_ELEM.PVARIPR, ZVARIPG),
                     ),
            para_out=((SP.PENERD1, EENERR), ),
        ),

        OP.EPSI_ELGA(te=-2),

        OP.EPSP_ELGA(te=-2),

        OP.EPSP_ELNO(te=-2),

        OP.FORC_NODA(te=542,
            para_in=((OP.FORC_NODA.PBASLOR, LC.N9NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E320NEUI),
                     (OP.FORC_NODA.PCOMPOR, CCOMPOR), (OP.FORC_NODA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PGEOMER, NGEOMER),
                     (OP.FORC_NODA.PHEAVTO, LC.E32NEUTI), (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI),
                     (OP.FORC_NODA.PLONCHA, LC.E10NEUTI), (OP.FORC_NODA.PLSN, LC.N1NEUT_R),
                     (OP.FORC_NODA.PLST, LC.N1NEUT_R), (OP.FORC_NODA.PPINTTO, E33NEUTR),
                     (OP.FORC_NODA.PPMILTO, LC.E198NEUT), (OP.FORC_NODA.PSTANO, STANO_I),
                     (OP.FORC_NODA.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PVECTUR, MVECTUR), ),
        ),

        OP.FULL_MECA(te=539,
            para_in=((OP.FULL_MECA.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.FULL_MECA.PCNSETO, LC.E320NEUI),
                     (OP.FULL_MECA.PCOMPOR, CCOMPOR), (OP.FULL_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.FULL_MECA.PHEAVTO, LC.E32NEUTI),
                     (OP.FULL_MECA.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.FULL_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.FULL_MECA.PLSN, LC.N1NEUT_R), (OP.FULL_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.FULL_MECA.PPINTTO, E33NEUTR),
                     (OP.FULL_MECA.PPMILTO, LC.E198NEUT), (OP.FULL_MECA.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.FULL_MECA.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (SP.PVARIMP, ZVARIPG),
                     (OP.FULL_MECA.PVARIMR, ZVARIPG), ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
                     (SP.PMATUUR, MMATUUR), (OP.FULL_MECA.PVARIPR, ZVARIPG),
                     (SP.PVECTUR, MVECTUR), ),
        ),

        OP.GRAD_NEUT9_R(te=398,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PNEUTER, LC.N9NEUT_R),
                     ),
            para_out=((OP.GRAD_NEUT9_R.PGNEUTR, LC.G27NEUTR), ),
        ),

        OP.INIT_VARC(te=99,
            para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), (OP.INIT_VARC.PVARCNO, LC.ZVARCNO), ),
        ),

        OP.INI_XFEM_ELNO(te=99,
            para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N9NEUT_R), (OP.INI_XFEM_ELNO.PFISNO, LC.FISNO_I),
                     (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R),
                     (OP.INI_XFEM_ELNO.PSTANO, STANO_I), ),
        ),

        OP.MASS_INER(te=285,
            para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
                     (OP.MASS_INER.PVARCPR, LC.ZVARCPG), ),
            para_out=((SP.PMASSINE, LC.EMASSINE), ),
        ),

        OP.MASS_MECA(te=538,
            para_in=((OP.MASS_MECA.PBASLOR, LC.N9NEUT_R), (OP.MASS_MECA.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.MASS_MECA.PHEAVTO, LC.E32NEUTI),
                     (OP.MASS_MECA.PHEA_NO, LC.N5NEUTI), (OP.MASS_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.MASS_MECA.PLSN, LC.N1NEUT_R), (OP.MASS_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.MASS_MECA.PPINTTO, E33NEUTR),
                     (OP.MASS_MECA.PSTANO, STANO_I), (OP.MASS_MECA.PVARCPR, LC.ZVARCPG),
                     ),
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
            para_in=((OP.RAPH_MECA.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RAPH_MECA.PCNSETO, LC.E320NEUI),
                     (OP.RAPH_MECA.PCOMPOR, CCOMPOR), (OP.RAPH_MECA.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RAPH_MECA.PHEAVTO, LC.E32NEUTI),
                     (OP.RAPH_MECA.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.RAPH_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.RAPH_MECA.PLSN, LC.N1NEUT_R), (OP.RAPH_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RAPH_MECA.PPINTTO, E33NEUTR),
                     (OP.RAPH_MECA.PSTANO, STANO_I), (SP.PVARCMR, LC.ZVARCPG),
                     (OP.RAPH_MECA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     (SP.PVARIMP, ZVARIPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
                     (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
                     ),
        ),

        OP.RIGI_MECA(te=539,
            para_in=((OP.RIGI_MECA.PBASLOR, LC.N9NEUT_R), (OP.RIGI_MECA.PCNSETO, LC.E320NEUI),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA.PHEAVTO, LC.E32NEUTI),
                     (OP.RIGI_MECA.PHEA_NO, LC.N5NEUTI), (OP.RIGI_MECA.PLONCHA, LC.E10NEUTI),
                     (OP.RIGI_MECA.PLSN, LC.N1NEUT_R), (OP.RIGI_MECA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA.PPINTTO, E33NEUTR),
                     (OP.RIGI_MECA.PPMILTO, LC.E198NEUT), (OP.RIGI_MECA.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_GE(te=536,
            para_in=((OP.RIGI_MECA_GE.PBASLOR, LC.N9NEUT_R), (OP.RIGI_MECA_GE.PCNSETO, LC.E320NEUI),
                     (OP.RIGI_MECA_GE.PCONTRR, ECONTPG), (SP.PGEOMER, NGEOMER),
                     (OP.RIGI_MECA_GE.PHEAVTO, LC.E32NEUTI), (OP.RIGI_MECA_GE.PHEA_NO, LC.N5NEUTI),
                     (OP.RIGI_MECA_GE.PLONCHA, LC.E10NEUTI), (OP.RIGI_MECA_GE.PLSN, LC.N1NEUT_R),
                     (OP.RIGI_MECA_GE.PLST, LC.N1NEUT_R), (OP.RIGI_MECA_GE.PPINTTO, E33NEUTR),
                     (OP.RIGI_MECA_GE.PPMILTO, LC.E198NEUT), (OP.RIGI_MECA_GE.PSTANO, STANO_I),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.RIGI_MECA_TANG(te=539,
            para_in=((OP.RIGI_MECA_TANG.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
                     (SP.PCARCRI, CCARCRI), (OP.RIGI_MECA_TANG.PCNSETO, LC.E320NEUI),
                     (OP.RIGI_MECA_TANG.PCOMPOR, CCOMPOR), (OP.RIGI_MECA_TANG.PCONTMR, ECONTPG),
                     (SP.PDEPLMR, DDL_MECA), (SP.PDEPLPR, DDL_MECA),
                     (SP.PGEOMER, NGEOMER), (OP.RIGI_MECA_TANG.PHEAVTO, LC.E32NEUTI),
                     (OP.RIGI_MECA_TANG.PHEA_NO, LC.N5NEUTI), (SP.PINSTMR, CTEMPSR),
                     (SP.PINSTPR, CTEMPSR), (OP.RIGI_MECA_TANG.PLONCHA, LC.E10NEUTI),
                     (OP.RIGI_MECA_TANG.PLSN, LC.N1NEUT_R), (OP.RIGI_MECA_TANG.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.RIGI_MECA_TANG.PPINTTO, E33NEUTR),
                     (OP.RIGI_MECA_TANG.PPMILTO, LC.E198NEUT), (OP.RIGI_MECA_TANG.PSTANO, STANO_I),
                     (SP.PVARCMR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARCPR, LC.ZVARCPG),
                     (SP.PVARCRR, LC.ZVARCPG), (OP.RIGI_MECA_TANG.PVARIMR, ZVARIPG),
                     ),
            para_out=((SP.PMATUUR, MMATUUR), ),
        ),

        OP.SIEF_ELGA(te=261,
            para_in=((OP.SIEF_ELGA.PBASLOR, LC.N9NEUT_R), (SP.PCAMASS, CCAMASS),
                     (OP.SIEF_ELGA.PCNSETO, LC.E320NEUI), (OP.SIEF_ELGA.PCOMPOR, CCOMPOR),
                     (SP.PDEPLAR, DDL_MECA), (OP.SIEF_ELGA.PFISNO, LC.FISNO_I),
                     (SP.PGEOMER, NGEOMER), (OP.SIEF_ELGA.PHEAVTO, LC.E32NEUTI),
                     (OP.SIEF_ELGA.PHEA_NO, LC.N5NEUTI), (OP.SIEF_ELGA.PLONCHA, LC.E10NEUTI),
                     (OP.SIEF_ELGA.PLSN, LC.N1NEUT_R), (OP.SIEF_ELGA.PLST, LC.N1NEUT_R),
                     (SP.PMATERC, LC.CMATERC), (OP.SIEF_ELGA.PPINTTO, E33NEUTR),
                     (OP.SIEF_ELGA.PPMILTO, LC.E198NEUT), (OP.SIEF_ELGA.PSTANO, STANO_I),
                     (OP.SIEF_ELGA.PVARCPR, LC.ZVARCPG), (SP.PVARCRR, LC.ZVARCPG),
                     ),
            para_out=((OP.SIEF_ELGA.PCONTRR, ECONTPG), ),
        ),

        OP.SIEF_ELNO(te=289,
            para_in=((OP.SIEF_ELNO.PCNSETO, LC.E320NEUI), (OP.SIEF_ELNO.PCONTRR, ECONTPG),
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
            para_in=((OP.SIGM_ELNO.PCNSETO, LC.E320NEUI), (OP.SIGM_ELNO.PCONTRR, ECONTPG),
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
            para_out=((OP.TOU_INI_ELEM.PGEOM_R, CGEOMER), ),
        ),

        OP.TOU_INI_ELGA(te=99,
            para_out=((OP.TOU_INI_ELGA.PDEPL_R, EDEPLPG), (OP.TOU_INI_ELGA.PDOMMAG, LC.EDOMGGA),
                     (SP.PFACY_R, EFACY_R), (OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R),
                     (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R), (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F),
                     (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R), (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG),
                     (SP.PTEMP_R, ETEMXPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
                     ),
        ),

        OP.TOU_INI_ELNO(te=99,
            para_out=((OP.TOU_INI_ELNO.PGEOM_R, NGEOMER), ),
        ),

        OP.VARI_ELNO(te=-2),

        OP.XFEM_XPG(te=46,
            para_in=((OP.XFEM_XPG.PCNSETO, LC.E320NEUI), (SP.PGEOMER, NGEOMER),
                     (OP.XFEM_XPG.PHEAVTO, LC.E32NEUTI), (OP.XFEM_XPG.PLONCHA, LC.E10NEUTI),
                     (OP.XFEM_XPG.PPINTTO, E33NEUTR), (OP.XFEM_XPG.PPMILTO, LC.E198NEUT),
                     ),
            para_out=((OP.XFEM_XPG.PXFGEOM, XFGEOM_R), ),
        ),

    )


#------------------------------------------------------------
class MECA_XHT_PENTA6(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PENTA6
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,)),
        )
    elrefe =(
            ElrefeLoc(MT.PE6, gauss = ('RIGI=FPG6','MASS=FPG6','NOEU=NOEU','XFEM=XFEM240','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_PYRAM5(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PYRAM5
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,)),
        )
    elrefe =(
            ElrefeLoc(MT.PY5, gauss = ('RIGI=FPG5','MASS=FPG5','NOEU=NOEU','XFEM=XFEM180','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_TETRA4(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.TETRA4
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,)),
        )
    elrefe =(
            ElrefeLoc(MT.TE4, gauss = ('RIGI=FPG1','MASS=FPG1','NOEU=NOEU','XINT=FPG15','FPG1=FPG1','XFEM=XFEM90',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_HEXA20(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.HEXA20
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,)),
        )
    elrefe =(
            ElrefeLoc(MT.H20, gauss = ('RIGI=FPG27','MASS=FPG27','NOEU=NOEU','XFEM=XFEM480','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.T10, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('XINT=FPG12','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_PENTA15(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PENTA15
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,)),
        )
    elrefe =(
            ElrefeLoc(MT.P15, gauss = ('RIGI=FPG21','MASS=FPG21','NOEU=NOEU','XFEM=XFEM240','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.T10, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('XINT=FPG12','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_PYRAM13(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.PYRAM13
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,9,10,11,12,13,)),
        )
    elrefe =(
            ElrefeLoc(MT.P13, gauss = ('RIGI=FPG27','MASS=FPG27','NOEU=NOEU','XFEM=XFEM180','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.T10, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('XINT=FPG12','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
            ElrefeLoc(MT.TR3, gauss = ('FPG4=FPG4','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )


#------------------------------------------------------------
class MECA_XHT_TETRA10(MECA_XHT_HEXA8):
    """Please document this element"""
    meshType = MT.TETRA10
    nodes = (
            SetOfNodes('EN1', (1,2,3,4,5,6,7,8,9,10,)),
        )
    elrefe =(
            ElrefeLoc(MT.T10, gauss = ('RIGI=FPG5','MASS=FPG5','NOEU=NOEU','XINT=FPG15','XFEM=XFEM90','FPG1=FPG1',), mater=('XFEM','NOEU',),),
            ElrefeLoc(MT.TE4, gauss = ('XINT=FPG15','NOEU=NOEU',),),
            ElrefeLoc(MT.TR6, gauss = ('XINT=FPG12','NOEU=NOEU','FPG6=FPG6','FPG7=FPG7','XCON=FPG12',),),
        )
