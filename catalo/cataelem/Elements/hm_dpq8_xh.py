
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
    components=('C','ALPHA',))


CCARCRI  = LocatedComponents(phys=PHY.CARCRI, type='ELEM',
    components=('ITECREL','MACOMP','RESCREL','THETA','ITEDEC',
          'INTLOC','PERTURB','TOLDEBO','ITEDEBO','TSSEUIL',
          'TSAMPL','TSRETOUR','POSTITER','LC_EXT[3]','MODECALC',
          'ALPHA','LC_EXT2[2]',))


CCOMPOR  = LocatedComponents(phys=PHY.COMPOR, type='ELEM',
    components=('RELCOM','NBVARI','DEFORM','INCELA','C_PLAN',
          'NUME_LC','SD_COMP','KIT[9]','NVI_C','NVI_T',
          'NVI_H','NVI_M',))


DDL_MECA = LocatedComponents(phys=PHY.DEPL_R, type='ELNO', diff=True,
    components=(
    ('EN1',('DX','DY','PRE1','H1X','H1Y',
          'H1PRE1',)),
    ('EN2',('DX','DY','H1X','H1Y',)),))


EDEFONC  = LocatedComponents(phys=PHY.EPSI_C, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFOPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDEFONO  = LocatedComponents(phys=PHY.EPSI_R, type='ELNO',
    components=('EPXX','EPYY','EPZZ','EPXY','EPXZ',
          'EPYZ',))


EDFEQPG  = LocatedComponents(phys=PHY.EPSI_R, type='ELGA', location='RIGI',
    components=('INVA_2','PRIN_[3]','INVA_2SG','VECT_1_X','VECT_1_Y',
          'VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z','VECT_3_X',
          'VECT_3_Y','VECT_3_Z',))


NGEOMER  = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


EGGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELGA', location='XFEM',
    components=('X','Y',))


ENGEOM_R = LocatedComponents(phys=PHY.GEOM_R, type='ELNO',
    components=('X','Y',))


CTEMPSR  = LocatedComponents(phys=PHY.INST_R, type='ELEM',
    components=('INST',))


STANO_I  = LocatedComponents(phys=PHY.N120_I, type='ELNO',
    components=('X1',))


E6NEUTI  = LocatedComponents(phys=PHY.N512_I, type='ELEM',
    components=('X[6]',))


EGNEUT_F = LocatedComponents(phys=PHY.NEUT_F, type='ELGA', location='XFEM',
    components=('X[30]',))


E1NEUTK  = LocatedComponents(phys=PHY.NEUT_K24, type='ELEM',
    components=('Z1',))


EGNEUT_R = LocatedComponents(phys=PHY.NEUT_R, type='ELGA', location='XFEM',
    components=('X[30]',))


CPRESSF  = LocatedComponents(phys=PHY.PRES_F, type='ELEM',
    components=('PRES','CISA',))


EPRESNO  = LocatedComponents(phys=PHY.PRES_R, type='ELNO',
    components=('PRES','CISA',))


ESIGMPC  = LocatedComponents(phys=PHY.SIEF_C, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ESIGMNC  = LocatedComponents(phys=PHY.SIEF_C, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ESIGMPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ESIGMNO  = LocatedComponents(phys=PHY.SIEF_R, type='ELNO',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ',))


ECONTPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='XFEM',
    components=('SIXX','SIYY','SIZZ','SIXY','SIXZ',
          'SIYZ','SIPXX','SIPYY','SIPZZ','SIPXY',
          'SIPXZ','SIPYZ','M11','FH11X','FH11Y',))


ECOEQPG  = LocatedComponents(phys=PHY.SIEF_R, type='ELGA', location='RIGI',
    components=('VMIS','TRESCA','PRIN_[3]','VMIS_SG','VECT_1_X',
          'VECT_1_Y','VECT_1_Z','VECT_2_X','VECT_2_Y','VECT_2_Z',
          'VECT_3_X','VECT_3_Y','VECT_3_Z','TRSIG','TRIAX',))


ZVARIPG  = LocatedComponents(phys=PHY.VARI_R, type='ELGA', location='XFEM',
    components=('VARI',))


MVECTUR  = ArrayOfComponents(phys=PHY.VDEP_R, locatedComponents=(DDL_MECA,))

MMATUUR  = ArrayOfComponents(phys=PHY.MDEP_R, locatedComponents=(DDL_MECA,DDL_MECA))

MMATUNS  = ArrayOfComponents(phys=PHY.MDNS_R, locatedComponents=(DDL_MECA,DDL_MECA))


#------------------------------------------------------------
abstractElement = AbstractElement()
ele = abstractElement

ele.addCalcul(OP.ADD_SIGM, te=581,
    para_in=((SP.PEPCON1, ECONTPG), (SP.PEPCON2, ECONTPG),
             ),
    para_out=((SP.PEPCON3, ECONTPG), ),
)

ele.addCalcul(OP.CARA_GEOM, te=285,
    para_in=((SP.PGEOMER, NGEOMER), ),
    para_out=((SP.PCARAGE, LC.ECARAGE), ),
)

ele.addCalcul(OP.CHAR_MECA_PESA_R, te=588,
    para_in=((OP.CHAR_MECA_PESA_R.PCNSETO, LC.E36NEUI), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_PESA_R.PHEAVTO, E6NEUTI), (OP.CHAR_MECA_PESA_R.PHEA_NO, LC.N5NEUTI),
             (OP.CHAR_MECA_PESA_R.PLONCHA, LC.E10NEUTI), (OP.CHAR_MECA_PESA_R.PLSN, LC.N1NEUT_R),
             (OP.CHAR_MECA_PESA_R.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (SP.PPESANR, LC.CPESANR), (OP.CHAR_MECA_PESA_R.PPINTTO, LC.E6NEUTR),
             (OP.CHAR_MECA_PESA_R.PPMILTO, LC.E22NEUTR), (OP.CHAR_MECA_PESA_R.PSTANO, STANO_I),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_F, te=37,
    para_in=((OP.CHAR_MECA_PRES_F.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_F.PBASECO, LC.E28NEUTR),
             (OP.CHAR_MECA_PRES_F.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_PRES_F.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_F.PLONGCO, LC.E3NEUTI),
             (OP.CHAR_MECA_PRES_F.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_F.PPINTER, LC.E14NEUTR),
             (SP.PPRESSF, CPRESSF), (OP.CHAR_MECA_PRES_F.PSTANO, STANO_I),
             (SP.PTEMPSR, CTEMPSR), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.CHAR_MECA_PRES_R, te=37,
    para_in=((OP.CHAR_MECA_PRES_R.PAINTER, LC.E35NEUTR), (OP.CHAR_MECA_PRES_R.PBASECO, LC.E28NEUTR),
             (OP.CHAR_MECA_PRES_R.PCFACE, LC.E9NEUTI), (SP.PGEOMER, NGEOMER),
             (OP.CHAR_MECA_PRES_R.PHEA_NO, LC.N5NEUTI), (OP.CHAR_MECA_PRES_R.PLONGCO, LC.E3NEUTI),
             (OP.CHAR_MECA_PRES_R.PLST, LC.N1NEUT_R), (OP.CHAR_MECA_PRES_R.PPINTER, LC.E14NEUTR),
             (SP.PPRESSR, EPRESNO), (OP.CHAR_MECA_PRES_R.PSTANO, STANO_I),
             ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.EPEQ_ELGA, te=335,
    para_in=((OP.EPEQ_ELGA.PDEFORR, EDEFOPG), ),
    para_out=((OP.EPEQ_ELGA.PDEFOEQ, EDFEQPG), ),
)

ele.addCalcul(OP.EPEQ_ELNO, te=335,
    para_in=((OP.EPEQ_ELNO.PDEFORR, EDEFONO), ),
    para_out=((OP.EPEQ_ELNO.PDEFOEQ, LC.EDFEQNO), ),
)

ele.addCalcul(OP.EPSI_ELNO, te=4,
    para_in=((OP.EPSI_ELNO.PDEFOPG, EDEFOPG), ),
    para_out=((SP.PDEFONC, EDEFONC), (SP.PDEFONO, EDEFONO),
             ),
)

ele.addCalcul(OP.FORC_NODA, te=588,
    para_in=((OP.FORC_NODA.PBASLOR, LC.N6NEUT_R), (OP.FORC_NODA.PCNSETO, LC.E36NEUI),
             (OP.FORC_NODA.PCONTMR, ECONTPG), (SP.PGEOMER, NGEOMER),
             (OP.FORC_NODA.PHEAVTO, E6NEUTI), (OP.FORC_NODA.PHEA_NO, LC.N5NEUTI),
             (SP.PINSTMR, CTEMPSR), (SP.PINSTPR, CTEMPSR),
             (OP.FORC_NODA.PLONCHA, LC.E10NEUTI), (OP.FORC_NODA.PLSN, LC.N1NEUT_R),
             (OP.FORC_NODA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (OP.FORC_NODA.PPINTTO, LC.E6NEUTR), (OP.FORC_NODA.PPMILTO, LC.E22NEUTR),
             (OP.FORC_NODA.PSTANO, STANO_I), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.FULL_MECA, te=588,
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
             (SP.PVARCRR, LC.ZVARCPG), (OP.FULL_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.FULL_MECA.PCONTPR, ECONTPG),
             (SP.PMATUNS, MMATUNS), (OP.FULL_MECA.PVARIPR, ZVARIPG),
             (SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.INDL_ELGA, te=30,
    para_in=((OP.INDL_ELGA.PCOMPOR, CCOMPOR), (OP.INDL_ELGA.PCONTPR, ESIGMPG),
             (SP.PMATERC, LC.CMATERC), (OP.INDL_ELGA.PVARIPR, ZVARIPG),
             ),
    para_out=((SP.PINDLOC, LC.EGINDLO), ),
)

ele.addCalcul(OP.INIT_MAIL_VOIS, te=99,
    para_out=((OP.INIT_MAIL_VOIS.PVOISIN, LC.EVOISIN), ),
)

ele.addCalcul(OP.INIT_VARC, te=99,
    para_out=((OP.INIT_VARC.PVARCPR, LC.ZVARCPG), ),
)

ele.addCalcul(OP.INI_XFEM_ELNO, te=99,
    para_out=((OP.INI_XFEM_ELNO.PBASLOR, LC.N6NEUT_R), (OP.INI_XFEM_ELNO.PLSN, LC.N1NEUT_R),
             (OP.INI_XFEM_ELNO.PLST, LC.N1NEUT_R), (OP.INI_XFEM_ELNO.PSTANO, STANO_I),
             ),
)

ele.addCalcul(OP.MASS_INER, te=285,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PMASSINE, LC.EMASSINE), ),
)

ele.addCalcul(OP.MASS_MECA, te=82,
    para_in=((OP.MASS_MECA.PBASLOR, LC.N6NEUT_R), (OP.MASS_MECA.PCNSETO, LC.E36NEUI),
             (SP.PGEOMER, NGEOMER), (OP.MASS_MECA.PHEAVTO, E6NEUTI),
             (OP.MASS_MECA.PLONCHA, LC.E10NEUTI), (OP.MASS_MECA.PLSN, LC.N1NEUT_R),
             (OP.MASS_MECA.PLST, LC.N1NEUT_R), (SP.PMATERC, LC.CMATERC),
             (OP.MASS_MECA.PPINTTO, LC.E6NEUTR), (OP.MASS_MECA.PPMILTO, LC.E22NEUTR),
             (OP.MASS_MECA.PSTANO, STANO_I), ),
    para_out=((SP.PMATUUR, MMATUUR), ),
)

ele.addCalcul(OP.M_GAMMA, te=82,
    para_in=((SP.PACCELR, DDL_MECA), (SP.PGEOMER, NGEOMER),
             (SP.PMATERC, LC.CMATERC), ),
    para_out=((SP.PVECTUR, MVECTUR), ),
)

ele.addCalcul(OP.NSPG_NBVA, te=496,
    para_in=((OP.NSPG_NBVA.PCOMPOR, LC.CCOMPO2), ),
    para_out=((SP.PDCEL_I, LC.EDCEL_I), ),
)

ele.addCalcul(OP.PAS_COURANT, te=404,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PMATERC, LC.CMATERC),
             ),
    para_out=((SP.PCOURAN, LC.ECOURAN), ),
)

ele.addCalcul(OP.PDIL_ELGA, te=511,
    para_in=((OP.PDIL_ELGA.PCOMPOR, CCOMPOR), (OP.PDIL_ELGA.PCONTPR, ESIGMPG),
             (SP.PMATERC, LC.CMATERC), (OP.PDIL_ELGA.PVARIPR, ZVARIPG),
             ),
    para_out=((SP.PPDIL, LC.EPDILPG), ),
)

ele.addCalcul(OP.RAPH_MECA, te=588,
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
             (SP.PVARCRR, LC.ZVARCPG), (OP.RAPH_MECA.PVARIMR, ZVARIPG),
             ),
    para_out=((SP.PCODRET, LC.ECODRET), (OP.RAPH_MECA.PCONTPR, ECONTPG),
             (OP.RAPH_MECA.PVARIPR, ZVARIPG), (SP.PVECTUR, MVECTUR),
             ),
)

ele.addCalcul(OP.RIGI_MECA_TANG, te=588,
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
    para_out=((SP.PMATUNS, MMATUNS), ),
)

ele.addCalcul(OP.SIEQ_ELGA, te=335,
    para_in=((OP.SIEQ_ELGA.PCONTRR, ESIGMPG), ),
    para_out=((OP.SIEQ_ELGA.PCONTEQ, ECOEQPG), ),
)

ele.addCalcul(OP.SIEQ_ELNO, te=335,
    para_in=((OP.SIEQ_ELNO.PCONTRR, ESIGMNO), ),
    para_out=((OP.SIEQ_ELNO.PCONTEQ, LC.ECOEQNO), ),
)

ele.addCalcul(OP.SIGM_ELGA, te=546,
    para_in=((SP.PSIEFR, ESIGMPG), ),
    para_out=((SP.PSIGMC, ESIGMPC), (SP.PSIGMR, ESIGMPG),
             ),
)

ele.addCalcul(OP.SIGM_ELNO, te=4,
    para_in=((OP.SIGM_ELNO.PCONTRR, ESIGMPG), ),
    para_out=((SP.PSIEFNOC, ESIGMNC), (OP.SIGM_ELNO.PSIEFNOR, ESIGMNO),
             ),
)

ele.addCalcul(OP.TOPOFA, te=510,
    para_in=((OP.TOPOFA.PAINTTO, LC.E15NEUTR), (OP.TOPOFA.PCNSETO, LC.E36NEUI),
             (SP.PDECOU, LC.E1NEUK8), (SP.PGEOMER, NGEOMER),
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
)

ele.addCalcul(OP.TOPONO, te=120,
    para_in=((OP.TOPONO.PCNSETO, LC.E36NEUI), (OP.TOPONO.PHEAVTO, E6NEUTI),
             (SP.PLEVSET, LC.N1NEUT_R), (OP.TOPONO.PLONCHA, LC.E10NEUTI),
             ),
    para_out=((OP.TOPONO.PHEA_NO, LC.N5NEUTI), (OP.TOPONO.PHEA_SE, E6NEUTI),
             ),
)

ele.addCalcul(OP.TOPOSE, te=514,
    para_in=((SP.PGEOMER, NGEOMER), (SP.PLEVSET, LC.N1NEUT_R),
             ),
    para_out=((OP.TOPOSE.PAINTTO, LC.E15NEUTR), (OP.TOPOSE.PCNSETO, LC.E36NEUI),
             (OP.TOPOSE.PHEAVTO, E6NEUTI), (OP.TOPOSE.PLONCHA, LC.E10NEUTI),
             (OP.TOPOSE.PPINTTO, LC.E6NEUTR), (OP.TOPOSE.PPMILTO, LC.E22NEUTR),
             ),
)

ele.addCalcul(OP.TOU_INI_ELGA, te=99,
    para_out=((OP.TOU_INI_ELGA.PGEOM_R, EGGEOM_R), (OP.TOU_INI_ELGA.PINST_R, LC.EGINST_R),
             (OP.TOU_INI_ELGA.PNEUT_F, EGNEUT_F), (OP.TOU_INI_ELGA.PNEUT_R, EGNEUT_R),
             (OP.TOU_INI_ELGA.PSIEF_R, ECONTPG), (OP.TOU_INI_ELGA.PVARI_R, ZVARIPG),
             ),
)

ele.addCalcul(OP.TOU_INI_ELNO, te=99,
    para_out=((OP.TOU_INI_ELNO.PGEOM_R, ENGEOM_R), (OP.TOU_INI_ELNO.PINST_R, LC.ENINST_R),
             (OP.TOU_INI_ELNO.PNEUT_F, LC.ENNEUT_F), (OP.TOU_INI_ELNO.PNEUT_R, LC.ENNEUT_R),
             ),
)

ele.addCalcul(OP.VAEX_ELGA, te=549,
    para_in=((OP.VAEX_ELGA.PCOMPOR, CCOMPOR), (SP.PNOVARI, E1NEUTK),
             (SP.PVARIGR, ZVARIPG), ),
    para_out=((SP.PVARIGS, LC.E1GNEUT), ),
)

ele.addCalcul(OP.VAEX_ELNO, te=549,
    para_in=((OP.VAEX_ELNO.PCOMPOR, CCOMPOR), (SP.PNOVARI, E1NEUTK),
             (OP.VAEX_ELNO.PVARINR, LC.ZVARINO), ),
    para_out=((SP.PVARINS, LC.E1NNEUT), ),
)


#------------------------------------------------------------
HM_DPQ8_XH = Element(modele=abstractElement)
ele = HM_DPQ8_XH
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG9','MASS=FPG9','XFEM=XFEM144','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','XINT=FPG12',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG9','MASS=FPG9',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG4',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG4','MASS=FPG4',),),
    )


#------------------------------------------------------------
HM_DPTR6_XH = Element(modele=abstractElement)
ele = HM_DPTR6_XH
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG6','MASS=FPG6','XINT=FPG12','XFEM=XFEM72','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG6','MASS=FPG6',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG4',),),
        ElrefeLoc(MT.SE2, gauss = ('RIGI=FPG4','MASS=FPG4',),),
    )


#------------------------------------------------------------
HM_DPQ8M_XH = Element(modele=abstractElement)
ele = HM_DPQ8M_XH
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG4','MASS=FPG9','XFEM=XFEM144','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4','MASS=FPG9',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG4',),),
    )


#------------------------------------------------------------
HM_DPTR6M_XH = Element(modele=abstractElement)
ele = HM_DPTR6M_XH
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3','MASS=FPG6','XFEM=XFEM72','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3','MASS=FPG6',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4','MASS=FPG4',),),
    )


#------------------------------------------------------------
HM_DPQ8D_XH = Element(modele=abstractElement)
ele = HM_DPQ8D_XH
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=NOEU_S','MASS=NOEU_S','XFEM=XFEM144','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=NOEU_S',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
HM_DPTR6D_XH = Element(modele=abstractElement)
ele = HM_DPTR6D_XH
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=NOEU_S','MASS=NOEU_S','XFEM=XFEM72','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=NOEU_S',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
HM_DPQ8S_XH = Element(modele=abstractElement)
ele = HM_DPQ8S_XH
ele.meshType = MT.QUAD8
ele.nodes = (
        SetOfNodes('EN2', (5,6,7,8,)),
        SetOfNodes('EN1', (1,2,3,4,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.QU8, gauss = ('RIGI=FPG4NOS','MASS=FPG4','XFEM=XFEM144','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.QU4, gauss = ('RIGI=FPG4NOS','MASS=FPG4','NOEU_S=NOEU_S',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )


#------------------------------------------------------------
HM_DPTR6S_XH = Element(modele=abstractElement)
ele = HM_DPTR6S_XH
ele.meshType = MT.TRIA6
ele.nodes = (
        SetOfNodes('EN2', (4,5,6,)),
        SetOfNodes('EN1', (1,2,3,)),
    )
ele.elrefe=(
        ElrefeLoc(MT.TR6, gauss = ('RIGI=FPG3NOS','MASS=FPG3','XFEM=XFEM72','NOEU_S=NOEU_S','FPG1=FPG1',), mater=('XFEM',),),
        ElrefeLoc(MT.TR3, gauss = ('RIGI=FPG3NOS','MASS=FPG3','NOEU_S=NOEU_S',),),
        ElrefeLoc(MT.SE3, gauss = ('RIGI=FPG4',),),
    )
