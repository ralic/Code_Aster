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


def matr_asse_gene_prod(MATR_ASSE,MATR_ASSE_GENE,**args):
  if AsType(MATR_ASSE) == matr_asse_depl_r  : return matr_asse_gene_r
  if AsType(MATR_ASSE_GENE) == matr_asse_gene_r  : return matr_asse_gene_r
  if AsType(MATR_ASSE) == matr_asse_depl_c  : return matr_asse_gene_c
  if AsType(MATR_ASSE_GENE) == matr_asse_gene_c  : return matr_asse_gene_c
  raise AsException("type de concept resultat non prevu")

PROJ_MATR_BASE=OPER(nom="PROJ_MATR_BASE",op=  71,sd_prod=matr_asse_gene_prod,
                    fr=tr("Projection d'une matrice assemblée sur une base (modale ou de RITZ)"),
                    reentrant='n',
         regles=(UN_PARMI('MATR_ASSE','MATR_ASSE_GENE'),),
         BASE            =SIMP(statut='o',typ=(mode_meca,mode_gene ) ),
         NUME_DDL_GENE   =SIMP(statut='o',typ=nume_ddl_gene ),
         MATR_ASSE       =SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_depl_c) ),
         MATR_ASSE_GENE  =SIMP(statut='f',typ=(matr_asse_gene_r,matr_asse_gene_c) ),
)  ;
