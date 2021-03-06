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


def calc_fonc_interp_prod(FONCTION, NOM_PARA_FONC, **args):
   if   AsType(FONCTION) == nappe_sdaster:
      return nappe_sdaster
   elif AsType(FONCTION) == fonction_sdaster:
      return fonction_sdaster
   elif AsType(FONCTION) == fonction_c:
      return fonction_c
   elif AsType(FONCTION) == formule_c:
      return fonction_c
   elif AsType(FONCTION) == formule:
      if NOM_PARA_FONC != None:
         return nappe_sdaster
      return fonction_sdaster
   raise AsException("type de concept resultat non prevu")

CALC_FONC_INTERP=OPER(nom="CALC_FONC_INTERP",op= 134,sd_prod=calc_fonc_interp_prod,
                      docu="U4.32.01",reentrant='n',
           fr=tr("Définit une fonction (ou une nappe) à partir d'une fonction FORMULE à 1 ou 2 variables"),
         regles=(UN_PARMI('VALE_PARA','LIST_PARA'),),
         FONCTION        =SIMP(statut='o',typ=(formule,fonction_sdaster,nappe_sdaster,fonction_c) ),
         VALE_PARA       =SIMP(statut='f',typ='R',max='**'),
         LIST_PARA       =SIMP(statut='f',typ=listr8_sdaster ),
         NOM_RESU        =SIMP(statut='f',typ='TXM'),
         NOM_PARA        =SIMP(statut='f',typ='TXM'),
         INTERPOL        =SIMP(statut='f',typ='TXM',max=2,into=("NON","LIN","LOG") ),
         PROL_DROITE     =SIMP(statut='f',typ='TXM',into=("EXCLU","CONSTANT","LINEAIRE") ),
         PROL_GAUCHE     =SIMP(statut='f',typ='TXM',into=("EXCLU","CONSTANT","LINEAIRE") ),
         NOM_PARA_FONC   =SIMP(statut='f',typ='TXM'),
         b_eval_nappe    =BLOC(condition = """exists("NOM_PARA_FONC")""",
            regles=(UN_PARMI('VALE_PARA_FONC','LIST_PARA_FONC'),),
            VALE_PARA_FONC  =SIMP(statut='f',typ='R',max='**'),
            LIST_PARA_FONC  =SIMP(statut='f',typ=listr8_sdaster ),
            INTERPOL_FONC   =SIMP(statut='f',typ='TXM',max=2,into=("NON","LIN","LOG")),
            PROL_DROITE_FONC=SIMP(statut='f',typ='TXM',into=("EXCLU","CONSTANT","LINEAIRE") ),
            PROL_GAUCHE_FONC=SIMP(statut='f',typ='TXM',into=("EXCLU","CONSTANT","LINEAIRE") ),
         ),
         TITRE           =SIMP(statut='f',typ='TXM'),
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2 ) ),
)  ;
