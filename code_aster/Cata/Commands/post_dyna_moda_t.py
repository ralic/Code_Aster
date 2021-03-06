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


POST_DYNA_MODA_T=OPER(nom="POST_DYNA_MODA_T",op= 130,sd_prod=table_sdaster,
                      fr=tr("Post-traiter les résultats en coordonnées généralisées produit par DYNA_TRAN_MODAL"),
                      reentrant='n',
        regles=(UN_PARMI('CHOC','RELA_EFFO_DEPL', ),),
         RESU_GENE       =SIMP(statut='o',typ=tran_gene ),
         CHOC            =FACT(statut='f',max='**',
                               fr=tr("Analyse des non linéarités de choc"),
           INST_INIT       =SIMP(statut='f',typ='R',defaut= -1. ),
           INST_FIN        =SIMP(statut='f',typ='R',defaut= 999. ),
           NB_BLOC         =SIMP(statut='f',typ='I',defaut= 1 ),
           SEUIL_FORCE     =SIMP(statut='f',typ='R',defaut= 0.E+0 ),
           DUREE_REPOS     =SIMP(statut='f',typ='R',defaut= 0.E+0 ),
           OPTION          =SIMP(statut='f',typ='TXM',defaut="USURE",into=("IMPACT","USURE") ),
           NB_CLASSE       =SIMP(statut='f',typ='I',defaut= 10 ),
         ),
         RELA_EFFO_DEPL  =FACT(statut='f',
                               fr=tr("Analyse des relationsnon linéaires effort-déplacement"),
        regles=(UN_PARMI('NOEUD','GROUP_NO'),
                EXCLUS('NOEUD','GROUP_NO'),),
           NOEUD           =SIMP(statut='c',typ=no),
           GROUP_NO        =SIMP(statut='f',typ=grno),
           NOM_CMP         =SIMP(statut='o',typ='TXM' ),
         ),
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=(1,2) ),
         TITRE           =SIMP(statut='f',typ='TXM' ),
)  ;
