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

# person_in_charge: georges-cc.devesa at edf.fr
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


def rest_cond_tran_prod(RESULTAT,TYPE_RESU,**args ):

  if AsType(RESULTAT) == dyna_trans  : return dyna_trans
  if (AsType(RESULTAT) == evol_noli and TYPE_RESU == "DYNA_TRANS") : return dyna_trans
  if (AsType(RESULTAT) == evol_noli and TYPE_RESU == "EVOL_NOLI") : return evol_noli

  raise AsException("type de concept resultat non prevu")

REST_COND_TRAN=OPER(nom="REST_COND_TRAN",op=  78,sd_prod=rest_cond_tran_prod,
                    fr=tr("Restituer dans la base physique des résultats issus d'un calcul"
                         "non-lineaire avec projection modale ou d'un calcul transitoire linear"
                         "avec condensation dynamique"),
                    reentrant='f',
        regles=(
                EXCLUS('TOUT_ORDRE','NUME_ORDRE','INST','LIST_INST','TOUT_INST'),
                EXCLUS('MACR_ELEM_DYNA','BASE_MODALE'),),
         reuse=SIMP(statut='c', typ=CO),
         RESULTAT        =SIMP(statut='f',typ=(evol_noli,dyna_trans) ),
         TYPE_RESU       =SIMP(statut='f',typ='TXM',defaut="DYNA_TRANS",
                          into=("DYNA_TRANS","EVOL_NOLI") ),
         BASE_MODALE     =SIMP(statut='f',typ=mode_meca),
#         NUME_DDL        =SIMP(statut='f',typ=nume_ddl_sdaster ),
         MACR_ELEM_DYNA  =SIMP(statut='f',typ=macr_elem_dyna),
         TOUT_INST       =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**' ),
         LIST_INST       =SIMP(statut='f',typ=listr8_sdaster ),
         TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**' ),
         CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("ABSOLU","RELATIF") ),
         b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
             PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
         b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
             PRECISION       =SIMP(statut='o',typ='R',),),
         INTERPOL        =SIMP(statut='f',typ='TXM',defaut="NON",into=("NON","LIN") ),
         TOUT_CHAM       =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         b_nom_cham=BLOC(condition="""not exists("TOUT_CHAM")""",
             NOM_CHAM        =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max=3,defaut="ACCE",into=("DEPL",
                                   "VITE","ACCE",),),),
         b_base_moda=BLOC(condition="""exists("BASE_MODALE")""",
             CHAM_MATER      =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM       =SIMP(statut='f',typ=cara_elem),),
             RESU_FINAL      =SIMP(statut='f',typ=(evol_noli,dyna_trans) ),
         TITRE           =SIMP(statut='f',typ='TXM' ),
)  ;
