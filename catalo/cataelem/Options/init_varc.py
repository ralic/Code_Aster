# coding=utf-8
# person_in_charge: jacques.pellet at edf.fr


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

from cataelem.Tools.base_objects import InputParameter, OutputParameter, Option, CondCalcul
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.parameters as SP
import cataelem.Commons.attributes as AT




PVARCPR  = OutputParameter(phys=PHY.VARI_R, type='ELGA',
comment="""  PVARCPR : VARIABLES DE COMMANDE POUR LE MATERIAU """)

PVARCNO  = OutputParameter(phys=PHY.VARI_R, type='ELNO',
comment="""  PVARCNO : VARIABLES DE COMMANDE POUR LE MATERIAU """)

INIT_VARC = Option(
    para_in=(
    ),
    para_out=(
           PVARCPR,
           PVARCNO,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
      CondCalcul('+', ((AT.PHENO,'TH'),(AT.BORD,'0'),)),
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD_ISO,'OUI'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FLS'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'3FL'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'2FL'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'FS2'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'3DA'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'DPA'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'3FA'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'2FA'),)),
      CondCalcul('-', ((AT.PHENO,'ME'),(AT.MODELI,'2FP'),)),
    ),
    comment=""" OPTION NE CALCULANT RIEN.
   ELLE SERT A ALLOUER LES CHAMPS DE VARIABLES DE COMMANDE POUR LE MATERIAU.

   ATTENTION :
      UN ELEMENT QUI CALCULE INIT_VARC DOIT AUSSI CALCULER L'OPTION NSPG_NBVA.
""",
)
