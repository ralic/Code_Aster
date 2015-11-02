
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




PCNSETO  = InputParameter(phys=PHY.N1280I, container='MODL!.TOPOSE.CNS',
comment="""  XFEM - CONNECTIVITE DES SOUS-ELEMENTS  """)


PHEAVTO  = InputParameter(phys=PHY.N512_I, container='MODL!.TOPOSE.HEA',
comment="""  XFEM - SIGNE HEAVISIDE PAR SOUS-ELEMENTS  """)


PLONCHA  = InputParameter(phys=PHY.N120_I, container='MODL!.TOPOSE.LON',
comment="""  XFEM - NBRE DE TETRAEDRES ET DE SOUS-ELEMENTS  """)


PHEAVFA  = InputParameter(phys=PHY.N960_I)


PLONGCO  = InputParameter(phys=PHY.N120_I)


PFISNO   = InputParameter(phys=PHY.NEUT_I,
comment=""" PFISNO : CONNECTIVITE DES FISSURES ET DES DDL HEAVISIDE """)


PHEA_NO  = OutputParameter(phys=PHY.N120_I, type='ELNO',
comment="""  XFEM - IDENTIFIANT HEAVISIDE AU NOEUD XFEM  """)


PHEA_SE  = OutputParameter(phys=PHY.N512_I, type='ELEM',
comment="""  XFEM - IDENTIFIANT HEAVISIDE SUR LES SOUS-ELEMENTS XFEM  """)


PHEA_FA  = OutputParameter(phys=PHY.N240_I, type='ELEM',
comment="""  XFEM - IDENTIFIANT HEAVISIDE POUR LES FACETTES DE CONTACT XFEM  """)


TOPONO = Option(
    para_in=(
           PCNSETO,
        SP.PFISCO,
           PFISNO,
           PHEAVFA,
           PHEAVTO,
        SP.PLEVSET,
           PLONCHA,
           PLONGCO,
    ),
    para_out=(
           PHEA_FA,
           PHEA_NO,
           PHEA_SE,
    ),
    condition=(
      CondCalcul('+', (('LXFEM','OUI'),)),
    ),
    comment=""" TOPONO : CALCUL DU SIGNE HEAVISIDE PAR NOEUD
           POUR LES ELEMENTS X-FEM """,
)
