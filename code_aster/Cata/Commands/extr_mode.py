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
# person_in_charge: harinaivo.andriambololona at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


def extr_mode_prod(FILTRE_MODE,**args):
  vale=FILTRE_MODE[0]['MODE']
  if AsType(vale) == mode_meca   : return mode_meca
  if AsType(vale) == mode_meca_c : return mode_meca_c
  if AsType(vale) == mode_gene   : return mode_gene
  raise AsException("type de concept resultat non prevu")

EXTR_MODE=OPER(nom="EXTR_MODE",op= 168,sd_prod=extr_mode_prod,
               reentrant='n',fr=tr("Extraire séléctivement des modes des structures de données modales"),
         FILTRE_MODE     =FACT(statut='o',max='**',
           regles=(UN_PARMI('TOUT_ORDRE','NUME_ORDRE','NUME_MODE','NUME_MODE_EXCLU','FREQ_MIN','CRIT_EXTR',),),
           MODE            =SIMP(statut='o',typ=(mode_meca,mode_meca_c,mode_gene ) ),
           TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI","NON") ),
           NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
           NUME_MODE       =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
           NUME_MODE_EXCLU =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
           FREQ_MIN        =SIMP(statut='f',typ='R' ),
           CRIT_EXTR       =SIMP(statut='f',typ='TXM',into=("MASS_EFFE_UN","MASS_GENE") ),
           b_freq_min      =BLOC(condition = """exists("FREQ_MIN")""",
             FREQ_MAX        =SIMP(statut='o',typ='R' ),
             PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-3 ),
           ),
           b_crit_extr     =BLOC(condition = """exists("CRIT_EXTR")""",
             regles=(AU_MOINS_UN('SEUIL','SEUIL_X','SEUIL_Y','SEUIL_Z'),),
             SEUIL           =SIMP(statut='f',typ='R'),
             SEUIL_X         =SIMP(statut='f',typ='R'),
             SEUIL_Y         =SIMP(statut='f',typ='R'),
             SEUIL_Z         =SIMP(statut='f',typ='R'),
           ),
         ),
         TITRE           =SIMP(statut='f',typ='TXM'),
         IMPRESSION      =FACT(statut='f',
           CUMUL           =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON") ),
           CRIT_EXTR       =SIMP(statut='f',typ='TXM',defaut="MASS_EFFE_UN",into=("MASS_EFFE_UN","MASS_GENE") ),
         ),
)  ;
