# coding=utf-8
# person_in_charge: jean-luc.flejou at edf.fr


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


# Cette option initialise un champ de NEUT_R sur la famille MATER
# Elle est utilisee par PROJ_CHAMP/METHODE = 'SOUS_POINT'
# On ne peut pas utiliser TOU_INI_ELGA car il y aurait conflit
# entre les deux champs NEUT_R


INI_SP_MATER = Option(
    para_in=(
    ),
    para_out=(
        SP.PHYDMAT,
        SP.PNEUMAT,
        SP.PTEMMAT,
    ),
    condition=(
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.COQUE,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.TUYAU,'OUI'),)),
      CondCalcul('+', ((AT.PHENO,'ME'),(AT.BORD,'0'),(AT.TYPMOD2,'PMF'),)),
    ),
)
