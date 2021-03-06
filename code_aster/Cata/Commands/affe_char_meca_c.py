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
# person_in_charge: mickael.abbas at edf.fr
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


AFFE_CHAR_MECA_C=OPER(nom="AFFE_CHAR_MECA_C",op=   7,sd_prod=char_meca,
                     fr=tr("Affectation de charges et conditions aux limites mécaniques complexes"),
                     reentrant='n',
         regles=(AU_MOINS_UN('DDL_IMPO','FORCE_POUTRE','LIAISON_DDL', ),),
         MODELE          =SIMP(statut='o',typ=modele_sdaster ),
         DDL_IMPO        =FACT(statut='f',max='**',
           fr=tr("Impose à des noeuds une ou plusieurs valeurs de déplacement (ou de certaines grandeurs asscociées)"),
           regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE','GROUP_NO','NOEUD',),
                   AU_MOINS_UN('DX','DY','DZ','DRX','DRY','DRZ','GRX','PRES','PHI','LIAISON','GLIS' ),),
             TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
             NOEUD           =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
             GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
             MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
             SANS_GROUP_MA   =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
             SANS_MAILLE     =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
             SANS_GROUP_NO   =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
             SANS_NOEUD      =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
           LIAISON         =SIMP(statut='f',typ='TXM',into=('ENCASTRE',)),
           DX              =SIMP(statut='f',typ='C' ),
           DY              =SIMP(statut='f',typ='C' ),
           DZ              =SIMP(statut='f',typ='C' ),
           DRX             =SIMP(statut='f',typ='C' ),
           DRY             =SIMP(statut='f',typ='C' ),
           DRZ             =SIMP(statut='f',typ='C' ),
           GRX             =SIMP(statut='f',typ='C' ),
           PRES            =SIMP(statut='f',typ='C' ),
           PHI             =SIMP(statut='f',typ='C' ),
           GLIS            =SIMP(statut='f',typ='C' ),
         ),
         FORCE_POUTRE    =FACT(statut='f',max='**',
           fr=tr("Applique des forces linéiques sur des éléments de type poutre"),
           regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE'),
                   PRESENT_ABSENT('TOUT','GROUP_MA','MAILLE'),
                   AU_MOINS_UN('FX','FY','FZ','N','VY','VZ',),
                   PRESENT_ABSENT('FX','N','VY','VZ',),
                   PRESENT_ABSENT('FY','N','VY','VZ',),
                   PRESENT_ABSENT('FZ','N','VY','VZ',),
                   PRESENT_ABSENT('N','FX','FY','FZ',),
                   PRESENT_ABSENT('VY', 'FX','FY','FZ',),
                   PRESENT_ABSENT('VZ','FX','FY','FZ', ),),
           TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
           GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
           MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
           TYPE_CHARGE     =SIMP(statut='f',typ='TXM',defaut="FORCE",into=("VENT","FORCE") ),
           FX              =SIMP(statut='f',typ='C' ),
           FY              =SIMP(statut='f',typ='C' ),
           FZ              =SIMP(statut='f',typ='C' ),
           N               =SIMP(statut='f',typ='C' ),
           VY              =SIMP(statut='f',typ='C' ),
           VZ              =SIMP(statut='f',typ='C' ),
         ),
         LIAISON_DDL     =FACT(statut='f',max='**',
           fr=tr("Définit une relation linéaire entre les DDLs de deux ou plusieurs noeuds"),
           regles=(UN_PARMI('GROUP_NO','NOEUD', ),),
           GROUP_NO        =SIMP(statut='f',typ=grno,max='**'),
           NOEUD           =SIMP(statut='c',typ=no  ,max='**'),
           DDL             =SIMP(statut='o',typ='TXM',max='**'),
           COEF_MULT       =SIMP(statut='o',typ='R'  ,max='**'),
           COEF_IMPO       =SIMP(statut='o',typ='C' ),
         ),
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2) ),
)  ;
