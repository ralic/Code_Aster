

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




PXFGEOM  = InputParameter(phys=PHY.GEOM_R)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I)


PBASLOR  = InputParameter(phys=PHY.NEUT_R)


PLSN     = InputParameter(phys=PHY.NEUT_R)


PLST     = InputParameter(phys=PHY.NEUT_R)


PHEA_NO  = InputParameter(phys=PHY.N120_I,
comment="""  XFEM - IDENTIFIANT HEAVISIDE AU NOEUD XFEM  """)


DEPL_XPG = Option(
    para_in=(
           PBASLOR,
        SP.PDEPLNO,
           PHEAVTO,
           PHEA_NO,
           PLONCHA,
           PLSN,
           PLST,
           PXFGEOM,
    ),
    para_out=(
        SP.PDEPLPG,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.LXFEM,'OUI'),)),
    ),
    comment=""" CALCUL DES COORDONNEES DES POINTS DE GAUSS DES FAMILLES XFEM_... """,
)
