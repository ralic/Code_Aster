# coding=utf-8


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




PVOISIN  = InputParameter(phys=PHY.VOISIN,
comment="""  PVOISIN : VOISINS DE L ELEMENT  """)


PPINTTO  = InputParameter(phys=PHY.N132_R,
comment="""  PPINTTO : XFEM, POINTS D INTERSECTION  """)


PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PCVOISX  = InputParameter(phys=PHY.N120_I,
comment="""  PCVOISX : XFEM, SD VOISIN POUR LES SOUS-ELEMENTS  """)


PCONTSER = InputParameter(phys=PHY.N1920R,
comment="""  PCONTSER : XFEM, CONTRAINTES AUX NOEUDS DES SOUS-ELEMENTS  """)


PPMILTO  = InputParameter(phys=PHY.N792_R)


PERREUR  = OutputParameter(phys=PHY.ERRE_R, type='ELEM',
comment="""  PERREUR : ESTIMATEUR D ERREUR  """)


ERME_ELEM = Option(
    para_in=(
           PCNSETO,
        SP.PCONTNM,
        SP.PCONTNO,
           PCONTSER,
           PCVOISX,
        SP.PDEPLAR,
        SP.PDEPLMR,
        SP.PERREM,
        SP.PFFVOLU,
        SP.PFORCE,
        SP.PFRVOLU,
        SP.PGEOMER,
        SP.PGRDCA,
           PLONCHA,
        SP.PMATERC,
        SP.PPESANR,
           PPINTTO,
           PPMILTO,
        SP.PPRESS,
        SP.PROTATR,
        SP.PTEMPSR,
           PVOISIN,
    ),
    para_out=(
           PERREUR,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),)),
    ),
    comment="""  ERME_ELEM :
    ESTIMATEUR D ERREUR EN RESIDU
    PRODUIT UN CHAMP PAR ELEMENT  """,
)
