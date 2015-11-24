# coding=utf-8
# person_in_charge: josselin.delmas at edf.fr


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




PCONTRR  = InputParameter(phys=PHY.SIEF_R, container='RESU!SIGM_ELGA!N',
comment="""  PCONTRR : CONTRAINTES REELLES AUX POINTS DE GAUSS """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  PLONCHA : XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS """)


PCONTSER = OutputParameter(phys=PHY.N1920R, type='ELEM',
comment="""  PCONTSER : CONTRAINTES REELLES PAR SOUS-ELEMENT AUX NOEUDS """)


SISE_ELNO = Option(
    para_in=(
           PCONTRR,
           PLONCHA,
    ),
    para_out=(
           PCONTSER,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.LXFEM,'OUI'),(AT.BORD,'0'),)),
    ),
    comment="""  SISE_ELNO : CALCUL DES CONTRAINTES ET DES EFFORTS
                       PAR SOUS-ELEMENT AUX NOEUDS """,
)
