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
# person_in_charge: natacha.bereux at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


def factoriser_prod(MATR_ASSE,**args):
  if AsType(MATR_ASSE) == matr_asse_depl_r : return matr_asse_depl_r
  if AsType(MATR_ASSE) == matr_asse_depl_c : return matr_asse_depl_c
  if AsType(MATR_ASSE) == matr_asse_temp_r : return matr_asse_temp_r
  if AsType(MATR_ASSE) == matr_asse_temp_c : return matr_asse_temp_c
  if AsType(MATR_ASSE) == matr_asse_pres_r : return matr_asse_pres_r
  if AsType(MATR_ASSE) == matr_asse_pres_c : return matr_asse_pres_c
  raise AsException("type de concept resultat non prevu")

FACTORISER=OPER(nom="FACTORISER",op=14,sd_prod=factoriser_prod,
               fr=tr("Factoriser une matrice assemblée en un produit de deux matrices triangulaires"
                  "ou construire une matrice de préconditionnement pour une résolution par gradient conjugué"),
               reentrant='f',
         reuse=SIMP(statut='c', typ=CO),
         MATR_ASSE       =SIMP(statut='o',typ=(matr_asse_depl_r,matr_asse_depl_c,matr_asse_temp_r,
                                               matr_asse_temp_c,matr_asse_pres_r,matr_asse_pres_c) ),

         METHODE         =SIMP(statut='f',typ='TXM',defaut="MULT_FRONT",into=("MULT_FRONT","LDLT","GCPC","PETSC","MUMPS") ),
         b_mult_front    =BLOC(condition="""equal_to("METHODE", 'MULT_FRONT')""",fr=tr("paramètres associés à la méthode multifrontale"),
           RENUM           =SIMP(statut='f',typ='TXM',into=("MD","MDA","METIS"),defaut="METIS" ),
           STOP_SINGULIER  =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
           NPREC           =SIMP(statut='f',typ='I',defaut=8),
         ),
         b_ldlt          =BLOC(condition="""equal_to("METHODE", 'LDLT')""",fr=tr("paramètres associés à la méthode LDLT"),
           RENUM           =SIMP(statut='f',typ='TXM',into=("RCMK",),defaut="RCMK"  ),
           STOP_SINGULIER  =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
           NPREC           =SIMP(statut='f',typ='I',defaut=8),
           BLOC_DEBUT      =SIMP(statut='f',typ='I',val_min=1,),
           DDL_DEBUT       =SIMP(statut='f',typ='I',val_min=1,),
           BLOC_FIN        =SIMP(statut='f',typ='I',val_min=1,),
           DDL_FIN         =SIMP(statut='f',typ='I',val_min=1,),
           regles=(EXCLUS('BLOC_DEBUT','DDL_DEBUT'),
                   EXCLUS('BLOC_FIN','DDL_FIN'),),
         ),
         b_mumps        =BLOC(condition = """equal_to("METHODE", 'MUMPS') """,fr=tr("Paramètres de la méthode MUMPS"),
           RENUM        =SIMP(statut='f',typ='TXM',defaut="AUTO",into=("AMD","AMF","PORD","METIS","QAMD","SCOTCH","AUTO","PARMETIS","PTSCOTCH")),
           STOP_SINGULIER  =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
           NPREC           =SIMP(statut='f',typ='I',defaut=8),
           TYPE_RESOL      =SIMP(statut='f',typ='TXM',defaut='AUTO',into=('NONSYM','SYMGEN','SYMDEF','AUTO')),
           PRETRAITEMENTS  =SIMP(statut='f',typ='TXM',defaut="AUTO",into=("SANS","AUTO")),
           PCENT_PIVOT     =SIMP(statut='f',typ='I',defaut=20,),
           ELIM_LAGR       =SIMP(statut='f',typ='TXM',defaut="LAGR2",into=("LAGR2","NON")),
           GESTION_MEMOIRE =SIMP(statut='f',typ='TXM',defaut="IN_CORE",into=("IN_CORE","OUT_OF_CORE","EVAL")),
           ACCELERATION=SIMP(statut='f', typ='TXM', defaut='AUTO',into=('AUTO','FR','FR+','LR','LR+')),
           LOW_RANK_SEUIL=SIMP(statut='f', typ='R', defaut=0.0, )
         ),
         b_iterat        =BLOC(condition="""equal_to("METHODE", 'GCPC') or equal_to("METHODE", 'PETSC')""",fr=tr("paramètres associés à la GCPC ou PETSc"),
           PRE_COND        =SIMP(statut='f',typ='TXM',into=("LDLT_INC","JACOBI","SOR","LDLT_SP"),defaut="LDLT_INC" ),
           b_ldlt_inc     =BLOC(condition = """equal_to("PRE_COND", 'LDLT_INC') """,
             NIVE_REMPLISSAGE = SIMP(statut='f',typ='I',defaut= 0 ),
             REMPLISSAGE      = SIMP(statut='f',typ='R',defaut= 1.0),
             RENUM           =SIMP(statut='f',typ='TXM',into=("RCMK",),defaut="RCMK"  ),
           ),
           b_ldlt_sp      =BLOC(condition = """equal_to("PRE_COND", 'LDLT_SP') """, fr=tr("Paramètres de la factorisation simple précision"),
             GESTION_MEMOIRE     =SIMP(statut='f',typ='TXM',defaut="AUTO",into=("IN_CORE","AUTO")),
             PCENT_PIVOT         =SIMP(statut='f',typ='I',defaut=20,),
             REAC_PRECOND        =SIMP(statut='f',typ='I',defaut=30, ),
             RENUM               =SIMP(statut='f',typ='TXM',defaut="SANS",into=("SANS",) ),
           ),
           b_jacobi_sor   =BLOC(condition = """equal_to("PRE_COND", 'JACOBI') or equal_to("PRE_COND", 'SOR')""",
                                                                           fr=tr("Paramètres des préconditionneurs JACOBI et SOR"),
             RENUM               =SIMP(statut='f',typ='TXM',defaut="SANS",into=("SANS",) ),
           ),
         ),

         TITRE           =SIMP(statut='f',typ='TXM'),
         INFO            =SIMP(statut='f',typ='I',into=(1,2) ),
)  ;
