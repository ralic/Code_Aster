

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




PVARCPR  = InputParameter(phys=PHY.VARI_R)


PCAORIE  = InputParameter(phys=PHY.CAORIE, container='CARA!.CARORIEN',
comment="""  PCAORIE : ORIENTATION LOCALE D'UN ELEMENT DE POUTRE OU DE TUYAU  """)


PNBSP_I  = InputParameter(phys=PHY.NBSP_I, container='CARA!.CANBSP',
comment="""  PNBSP_I :  NOMBRE DE SOUS_POINTS  """)


PCOMPOR  = InputParameter(phys=PHY.COMPOR,
comment=""" POUR LES PMF""")


MASS_MECA_EXPLI = Option(
    para_in=(
        SP.PCACOQU,
        SP.PCADISM,
        SP.PCAGNBA,
        SP.PCAGNPO,
           PCAORIE,
        SP.PCINFDI,
           PCOMPOR,
        SP.PFIBRES,
        SP.PGEOMER,
        SP.PMATERC,
           PNBSP_I,
           PVARCPR,
    ),
    para_out=(
        SP.PMATUUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
    ),
    comment=""" matrice de masse "diagonale" adaptee a la commande DYNA_TRAN_EXPLI.

   L'option MASS_MECA_EXPLI a ete introduite en raison des ddls DRZ des
   elements de coque (DKT).
   Dans le repere local de l'element, la "masse" est theoriquement nulle.
   cela entraine souvent au niveau global, des frequences artificielles tres
   elevees et cela conduit a des pas de temps tres petits.

   Dans l'operateur DYNA_TRAN_EXPLI et pour les elements de coque (DKT),
   il a ete juge que :
     - la masse des ddls de rotation  etait presque negligeable par rapport
       a celle des ddls de translation
     - on peut, en consequence, systematiquement affceter sur les ddls DRZ :
       masse(DRZ) = 1/2*(masse(DRX)+masse(DRY))

   Pour les elements qui ne sont pas concernes par le ddl fictif DRZ, l'option
   MASS_MECA_EXPLI est la meme que MASS_MECA_DIAG.
""",
)
