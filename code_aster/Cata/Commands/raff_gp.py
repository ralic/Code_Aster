# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: david.haboussa at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


RAFF_GP =MACRO(nom="RAFF_GP",
                   op=OPS('Macro.raff_gp_ops.raff_gp_ops'),
                   sd_prod=maillage_sdaster,
                   reentrant='n',
                   fr=tr("Preparation du maillage pour calcul du Gp en 2D"),
         MAILLAGE_N   = SIMP(statut='o',typ=maillage_sdaster,
                      fr=tr("Maillage avant adaptation"),
                      ),
         TRANCHE_2D  = FACT(statut='o',max = 1,
                           CENTRE           =SIMP(statut='o',typ='R',max=2),
                           RAYON       =SIMP(statut='o',typ='R',max=1),
                           ANGLE            =SIMP(statut='o',typ='R',max=1),
                           TAILLE          =SIMP(statut='o',typ='R',max=1),
                           NB_ZONE        =SIMP(statut='o',typ='I',),
                             ),
         NB_RAFF      = SIMP(statut='f',typ='I',defaut=4),
)
