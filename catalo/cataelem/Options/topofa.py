# coding=utf-8
# person_in_charge: samuel.geniaut at edf.fr


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

from cataelem.Tools.base_objects import InputParameter, OutputParameter, Option, CondCalcul
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.parameters as SP
import cataelem.Commons.attributes as AT




PLSN     = InputParameter(phys=PHY.NEUT_R)


PLST     = InputParameter(phys=PHY.NEUT_R)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PPINTTO  = InputParameter(phys=PHY.N132_R)


PCNSETO  = InputParameter(phys=PHY.N1280I)


PLONCHA  = InputParameter(phys=PHY.N120_I)


PHEAVTO  = InputParameter(phys=PHY.N512_I)


PAINTTO  = InputParameter(phys=PHY.N480_R)


PSTANO   = InputParameter(phys=PHY.N120_I)


PPINTER  = OutputParameter(phys=PHY.N816_R, type='ELEM')


PAINTER  = OutputParameter(phys=PHY.N1360R, type='ELEM')


PCFACE   = OutputParameter(phys=PHY.N720_I, type='ELEM')


PLONGCO  = OutputParameter(phys=PHY.N120_I, type='ELEM')


PBASECO  = OutputParameter(phys=PHY.N2448R, type='ELEM')


PGESCLO  = OutputParameter(phys=PHY.N816_R, type='ELEM')


PHEAVFA  = OutputParameter(phys=PHY.N960_I, type='ELEM')


TOPOFA = Option(
    para_in=(
           PAINTTO,
           PCNSETO,
        SP.PDECOU,
        SP.PFISCO,
        SP.PGEOMER,
        SP.PGRADLN,
        SP.PGRADLT,
           PHEAVTO,
           PLONCHA,
           PLSN,
           PLST,
           PPINTTO,
           PPMILTO,
           PSTANO,
        SP.PTYPDIS,
    ),
    para_out=(
           PAINTER,
           PBASECO,
           PCFACE,
        SP.PGESCLA,
           PGESCLO,
        SP.PGMAITR,
           PHEAVFA,
           PLONGCO,
           PPINTER,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.LXFEM,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),(AT.BORD,'0'),(AT.LXFEM,'OUI'),)),
    ),
    comment=""" TOPOFA (MOT-CLE: CONTACT): CALCUL DU DE LA TOPOLOGIE DES
           FACETTES DE CONTACT AVEC X-FEM ET LE METHODE CONTINUE """,
)
