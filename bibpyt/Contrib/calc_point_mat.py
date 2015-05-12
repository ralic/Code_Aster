# -*- coding: utf-8 -*-
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
# person_in_charge: jean-michel.proix at edf.fr


from Cata import cata
from Cata.cata import *


CALC_POINT_MAT=OPER(nom="CALC_POINT_MAT",op=33,sd_prod=table_sdaster,reentrant='f',
            UIinfo={"groupes":("CACHE",)},
            fr=tr("Intégrer une loi de comportement"),
     MATER           =SIMP(statut='o',typ=mater_sdaster,max=30),
     COMPORTEMENT    =C_COMPORTEMENT('CALC_POINT_MAT'),
     INCREMENT       =C_INCREMENT('MECANIQUE'),
     NEWTON          =C_NEWTON(),
     CONVERGENCE     =C_CONVERGENCE(),

    # --MASSIF : orientation du materiau (monocristal, orthotropie)
     MASSIF          =FACT(statut='f',max='**',
                    regles=(UN_PARMI('ANGL_REP','ANGL_EULER'),),
         ANGL_REP        =SIMP(statut='f',typ='R',min=1,max=3),
         ANGL_EULER      =SIMP(statut='f',typ='R',min=1,max=3),
      ),
    ## ANGLE : rotation de ANGLE autour de Z uniquement, et seulement pour les déformations imposées.
     ANGLE      =SIMP(statut='f',typ='R',max=1, defaut=0.),
     INFO            =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2)),

     regles=(
             EXCLUS('SIXX','EPXX',),
             EXCLUS('SIYY','EPYY',),
             EXCLUS('SIZZ','EPZZ',),
             EXCLUS('SIXY','EPXY',),
             EXCLUS('SIXZ','EPXZ',),
             EXCLUS('SIYZ','EPYZ',),

             ENSEMBLE('F11','F12','F13','F21','F22','F23','F31','F32','F33',),),

     SIXX = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     SIYY = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     SIZZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     SIXY = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     SIXZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     SIYZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),

     EPXX = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     EPYY = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     EPZZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     EPXY = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     EPXZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     EPYZ = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),

     F11 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F12 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F13 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F21 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F22 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F23 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F31 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F32 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
     F33 = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),

     MATR_C1=FACT(statut='f',max='**',
           VALE          =SIMP(statut='o',typ='R',max=1, ),
           NUME_LIGNE    =SIMP(statut='o',typ='I',max=1,val_min=1,val_max=6 ),
           NUME_COLONNE  =SIMP(statut='o',typ='I',max=1,val_min=1,val_max=12 ),
                              ),
     MATR_C2=FACT(statut='f',max='**',
           VALE          =SIMP(statut='o',typ='R',max=1, ),
           NUME_LIGNE    =SIMP(statut='o',typ='I',max=1,val_min=1,val_max=6 ),
           NUME_COLONNE  =SIMP(statut='o',typ='I',max=1,val_min=1,val_max=12 ),
                              ),
     VECT_IMPO=FACT(statut='f',max=6,
           VALE          =SIMP(statut='o',typ=(fonction_sdaster,nappe_sdaster,formule),max=1, ),
           NUME_LIGNE    =SIMP(statut='o',typ='I',max=1,val_min=1,val_max=6 ),
                              ),
     SIGM_INIT=FACT(statut='f',
            SIXX = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
            SIYY = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
            SIZZ = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
            SIXY = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
            SIXZ = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
            SIYZ = SIMP(statut='f',typ='R',max=1,defaut=0.0E+0),
                       ),
     EPSI_INIT=FACT(statut='f',
            EPXX = SIMP(statut='o',typ='R',max=1),
            EPYY = SIMP(statut='o',typ='R',max=1),
            EPZZ = SIMP(statut='o',typ='R',max=1),
            EPXY = SIMP(statut='o',typ='R',max=1),
            EPXZ = SIMP(statut='o',typ='R',max=1),
            EPYZ = SIMP(statut='o',typ='R',max=1),
                       ),
     VARI_INIT=FACT(statut='f',
            VALE = SIMP(statut='o',typ='R',max='**'),
                       ),
     FORMAT_TABLE   =SIMP(statut='f',typ='TXM',max=1,into=("CMP_COLONNE","CMP_LIGNE",),defaut=("CMP_COLONNE"),),

     NB_VARI_TABLE  =SIMP(statut='f',typ='I',max=1,),

     OPER_TANGENT   =SIMP(statut='f',typ='TXM',max=1,into=("OUI","NON",),defaut="NON",),

     ARCHIVAGE      =FACT(statut='f',
       LIST_INST       =SIMP(statut='f',typ=(listr8_sdaster) ),
       INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**' ),
       PAS_ARCH        =SIMP(statut='f',typ='I' ),
       PRECISION       =SIMP(statut='f',typ='R',defaut= 1.0E-6),
                           ),

    # variables de commandes scalaires, définies par une fonction du temps

     AFFE_VARC    = FACT(statut='f',max='**',
        NOM_VARC    = SIMP(statut='o',typ='TXM', into=("TEMP","CORR","IRRA","HYDR","SECH","NEUT1","NEUT2",
                                                 "PFERRITE","PPERLITE","PBAINITE","PMARTENS",  
                                                 "ALPHPUR","ALPHBETA", #"M_ZIRC",
                                                 "EPSA",
                                                  )),
        VALE_FONC   = SIMP(statut='f',typ=(fonction_sdaster,formule) ),
 
        B_VALE_REF          =BLOC(condition="NOM_VARC in ('TEMP', 'SECH')",
            VALE_REF         =SIMP(statut='o',typ='R'), ),
                        ),
);