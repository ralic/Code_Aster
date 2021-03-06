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
# person_in_charge: mathieu.courtois at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


def info_fonction_prod(self,ECART_TYPE,RMS,NOCI_SEISME,MAX,NORME, **args):
   if (RMS         != None): return table_sdaster
   if (MAX         != None): return table_sdaster
   if (NOCI_SEISME != None): return table_sdaster
   if (ECART_TYPE  != None): return table_sdaster
   if (NORME       != None): return table_sdaster
   raise AsException("type de concept resultat non prevu")

INFO_FONCTION=MACRO(nom="INFO_FONCTION",
                    op=OPS('Macro.info_fonction_ops.info_fonction_ops'),
                    sd_prod=info_fonction_prod,
                    fr=tr("Opérations mathématiques sur des concepts de type fonction, "
                         "fonction_c ou nappe"),
                    reentrant='n',
         regles=(UN_PARMI('MAX','RMS','NOCI_SEISME','NORME','ECART_TYPE',),),
         RMS             =FACT(statut='f',fr=tr("Valeur RMS d'une fonction"),max='**',
           METHODE         =SIMP(statut='f',typ='TXM',defaut="TRAPEZE",into=("SIMPSON","TRAPEZE") ),
           FONCTION        =SIMP(statut='o',typ=fonction_sdaster ),
           INST_INIT       =SIMP(statut='f',typ='R',fr=tr("Instant initial définissant le début du signal") ),
           INST_FIN        =SIMP(statut='f',typ='R',fr=tr("Instant final définissant la fin du signal") ),
           CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",) ),
           b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
              PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,val_min=0.E+0),),
           b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
              PRECISION       =SIMP(statut='o',typ='R'),),
         ),
         NOCI_SEISME     =FACT(statut='f',
           regles=(UN_PARMI('FONCTION','SPEC_OSCI',),),
           FONCTION        =SIMP(statut='f',typ=fonction_sdaster ),
           SPEC_OSCI       =SIMP(statut='f',typ=nappe_sdaster ),
           b_option_f      =BLOC(condition="""exists("FONCTION")""",
             OPTION          =SIMP(statut='f',typ='TXM',defaut="TOUT",validators=NoRepeat(),
                                   into=("INTE_ARIAS","POUV_DEST","INTE_SPEC","VITE_ABSO_CUMU",
                                         "DUREE_PHAS_FORT","MAXI","ACCE_SUR_VITE","ASA","TOUT",), ),
            b_asa          =BLOC(condition="""is_in('OPTION', ("TOUT", "ASA"))""",
                FREQ_FOND         =SIMP(statut='o',typ='R',val_min=0.E+0),
                FREQ_PAS     =SIMP(statut='f',typ='R',defaut = 0.01 ),
                NORME        =SIMP(statut='f',typ='R',defaut = 1.E+0 ),
                RATIO     =SIMP(statut='f',typ='R',defaut = 0.40),),
             b_amor_red          =BLOC(condition="""is_in('OPTION', ("TOUT", "INTE_SPEC","ASA" )) """,
                AMOR_REDUIT     =SIMP(statut='o',typ='R'),),
             b_pesanteur         =BLOC(condition="""is_in('OPTION', ("TOUT", "INTE_ARIAS", "POUV_DEST", "DUREE_PHAS_FORT")) """,
                PESANTEUR       =SIMP(statut='o',typ='R'),),
           ),
           b_option_n      =BLOC(condition="""exists("SPEC_OSCI")""",
             OPTION          =SIMP(statut='f',typ='TXM',defaut="INTE_SPEC",into=("INTE_SPEC",), ),
             NATURE          =SIMP(statut='o',typ='TXM',into=("DEPL","VITE","ACCE") ),
             AMOR_REDUIT     =SIMP(statut='o',typ='R'), ),
           INST_INIT       =SIMP(statut='f',typ='R'),
           INST_FIN        =SIMP(statut='f',typ='R'),
           COEF            =SIMP(statut='f',typ='R',defaut= 0.E+0 ),
           FREQ_INIT       =SIMP(statut='f',typ='R',defaut= 4.E-1 ),
           FREQ_FIN        =SIMP(statut='f',typ='R',defaut= 10.E+0 ),
           LIST_FREQ       =SIMP(statut='f',typ=listr8_sdaster ),
           FREQ            =SIMP(statut='f',typ='R',max='**'),
           NORME           =SIMP(statut='f',typ='R',defaut= 1.E+0 ),
           BORNE_INF       =SIMP(statut='f',typ='R',defaut= 0.05E+0 ),
           BORNE_SUP       =SIMP(statut='f',typ='R',defaut= 0.95E+0 ),
           b_acce_reel     =BLOC(condition="""(exists("INST_INIT"))or(exists("INST_FIN"))or(exists("FREQ_INIT"))or(exists("FREQ_FIN"))""",
                  CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",) ),
                  b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
                      PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,val_min=0.E+0),),
                  b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
                      PRECISION       =SIMP(statut='o',typ='R'),),
           ),
         ),
         MAX             =FACT(statut='f',fr=tr("Extrémas locaux d'une fonction"),
           FONCTION        =SIMP(statut='o',typ=(fonction_sdaster,nappe_sdaster),max='**' ),
           INTERVALLE      =SIMP(statut='f',typ='R',min=2,max='**',
                               fr =tr("définition des bornes des intervalles sous forme de couples (xi_i1,xs_i1,xi_i2,xs_i2)")),
         ),
         NORME           =FACT(statut='f',fr=tr("Norme L2 d'une fonction"),
            FONCTION      =SIMP(statut='o', typ=nappe_sdaster),
         ),
         ECART_TYPE      =FACT(statut='f',fr=tr("Ecart-type d'une fonction"),
            METHODE       =SIMP(statut='f',typ='TXM',defaut="TRAPEZE",into=("SIMPSON","TRAPEZE") ),
            FONCTION      =SIMP(statut='o',typ=fonction_sdaster),
            INST_INIT     =SIMP(statut='f',typ='R',fr=tr("Instant initial définissant le début du signal") ),
            INST_FIN      =SIMP(statut='f',typ='R',fr=tr("Instant final définissant la fin du signal") ),
            CRITERE       =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",) ),
            b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
                PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,val_min=0.E+0),),
            b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
                PRECISION       =SIMP(statut='o',typ='R'),),
         ),
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
)
