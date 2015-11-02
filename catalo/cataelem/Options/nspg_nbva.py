
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




PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment=""" LE COMPORTEMENT PERMET DE DETERMINER NCMP_DYN """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I,
comment=""" LE CHAMP DE NBSP_I PERMET DE DETERMINER NPG_DYN """)


NSPG_NBVA = Option(
    para_in=(
           PCOMPOR,
           PNBSP_I,
    ),
    para_out=(
        SP.PDCEL_I,
    ),
    condition=(
      CondCalcul('+', (('PHENO','ME'),('BORD','0'),)),
      CondCalcul('+', (('PHENO','TH'),('BORD','0'),)),
      CondCalcul('+', (('PHENO','ME'),('BORD_ISO','OUI'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','FLS'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','FS2'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','3DA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','DPA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','3FA'),)),
      CondCalcul('-', (('PHENO','ME'),('MODELI','2FA'),)),
    ),
    comment=""" OPTION SERVANT A CALCULER LES 2 NOMBRES :
   NPG_DYN  : NOMBRE DE SOUS-POINTS (POUR LES ELEMENTS DE STRUCTURE EN GENERAL)
   NCMP_DYN : NOMBRE DE COMPOSANTES POUR LA GRANDEUR VARI_R
""",
)
