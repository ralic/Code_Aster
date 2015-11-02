
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

from cataelem.Tools.base_objects import InputParameter, OutputParameter, Option, CondCalcul
import cataelem.Commons.physical_quantities as PHY
import cataelem.Commons.parameters as SP




PVARCPR  = OutputParameter(phys=PHY.VARI_R, type='ELGA',
comment="""  PVARCPR : VARIABLES DE COMMANDE POUR LE MATERIAU """)


INIT_VARC = Option(
    para_in=(
    ),
    para_out=(
           PVARCPR,
    ),
    condition=(
      CondCalcul('+', (('PHENO','ME'),('BORD','0'),)),
      CondCalcul('+', (('PHENO','TH'),('BORD','0'),)),
      CondCalcul('+', (('PHENO','ME'),('BORD_ISO','OUI'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','FLS'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','3FL'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','2FL'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','FS2'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','3DA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','DPA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','3FA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','2FA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','2FP'),)),
    ),
    comment=""" OPTION NE CALCULANT RIEN.
   ELLE SERT A ALLOUER LES CHAMPS DE VARIABLES DE COMMANDE POUR LE MATERIAU.

   ATTENTION :
      UN ELEMENT QUI CALCULE INIT_VARC DOIT AUSSI CALCULER L'OPTION NSPG_NBVA.
""",
)
