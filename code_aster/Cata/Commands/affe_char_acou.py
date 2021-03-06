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
#
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


AFFE_CHAR_ACOU=OPER(nom="AFFE_CHAR_ACOU",op=  68,sd_prod=char_acou,
                    fr=tr("Affectation de charges et conditions aux limites acoustiques constantes"),
                    reentrant='n',
         regles=(AU_MOINS_UN('PRES_IMPO','VITE_FACE','IMPE_FACE','LIAISON_UNIF' ),),
         MODELE          =SIMP(statut='o',typ=modele_sdaster ),
         PRES_IMPO       =FACT(statut='f',max='**',
           regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE','GROUP_NO','NOEUD'),),
             TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
             NOEUD           =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
             GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
             MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
             SANS_GROUP_MA   =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
             SANS_MAILLE     =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
             SANS_GROUP_NO   =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
             SANS_NOEUD      =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
             PRES            =SIMP(statut='o',typ='C' ),
         ),
         VITE_FACE       =FACT(statut='f',max='**',
             regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE'),
                     PRESENT_ABSENT('TOUT','GROUP_MA','MAILLE'),),
           TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
           GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
           MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
           VNOR            =SIMP(statut='o',typ='C' ),
         ),
         IMPE_FACE       =FACT(statut='f',max='**',
             regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE'),
                     PRESENT_ABSENT('TOUT','GROUP_MA','MAILLE'),),
           TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
           GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
           MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
           IMPE            =SIMP(statut='o',typ='C' ),
         ),
         LIAISON_UNIF    =FACT(statut='f',max='**',
           regles=(UN_PARMI('GROUP_NO','NOEUD','GROUP_MA','MAILLE' ),),
           GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
           NOEUD           =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
           GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
           MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
           DDL             =SIMP(statut='o',typ='TXM',max='**'),
         ),
)  ;
