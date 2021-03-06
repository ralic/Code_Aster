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
# person_in_charge: mathieu.corus at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


MACR_ELEM_DYNA=OPER(nom="MACR_ELEM_DYNA",op=  81,sd_prod=macr_elem_dyna,
                    fr=tr("Definition d'un macro element pour analyse modale ou harmonique par sous structuration dynamique"),
                    reentrant='n',
         regles=(
                 # AMOR_REDUIT et MATR_AMOR sont redondants
                 EXCLUS('MATR_AMOR','AMOR_REDUIT' ),
                 
                 # Si MODELE_MESURE, on ne rentre pas de donnees pour le calcul
                 EXCLUS('MODELE_MESURE','MATR_RIGI' ),
                 EXCLUS('MODELE_MESURE','MATR_MASS' ),
                 EXCLUS('MODELE_MESURE','MATR_AMOR' ),
                 EXCLUS('MODELE_MESURE','AMOR_REDUIT' ),
                 EXCLUS('MODELE_MESURE','MATR_IMPE' ),
                 EXCLUS('MODELE_MESURE','MATR_IMPE_RIGI' ),
                 EXCLUS('MODELE_MESURE','MATR_IMPE_MASS' ),
                 EXCLUS('MODELE_MESURE','MATR_IMPE_AMOR' ),
                 
                 PRESENT_ABSENT('MATR_IMPE','MATR_IMPE_RIGI'),
                 PRESENT_ABSENT('MATR_IMPE','MATR_IMPE_MASS'),
                 PRESENT_ABSENT('MATR_IMPE','MATR_IMPE_AMOR'),
                 PRESENT_ABSENT('MATR_IMPE','MATR_RIGI','MATR_MASS'),
                 PRESENT_ABSENT('MATR_IMPE_MASS','MATR_RIGI','MATR_MASS'),
                 PRESENT_ABSENT('MATR_IMPE_RIGI','MATR_RIGI','MATR_MASS'),
                 PRESENT_ABSENT('MATR_IMPE_AMOR','MATR_RIGI','MATR_MASS'),),
         BASE_MODALE     =SIMP(statut='o',typ=mode_meca ),
         MATR_RIGI       =SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_depl_c),),
         MATR_MASS       =SIMP(statut='f',typ=matr_asse_depl_r ),
         MATR_AMOR       =SIMP(statut='f',typ=matr_asse_depl_r ),
         AMOR_REDUIT     =SIMP(statut='f',typ='R',max='**'), 
         SANS_GROUP_NO   =SIMP(statut='f',typ=grno ),
         MATR_IMPE       =SIMP(statut='f',typ=matr_asse_gene_c ),
         MATR_IMPE_RIGI  =SIMP(statut='f',typ=matr_asse_gene_c ),
         MATR_IMPE_MASS  =SIMP(statut='f',typ=matr_asse_gene_c ),
         MATR_IMPE_AMOR  =SIMP(statut='f',typ=matr_asse_gene_c ),
         MODELE_MESURE   =FACT(statut='f',
           FREQ            =SIMP(statut='o',typ='R',max='**' ),
           MASS_GENE       =SIMP(statut='o',typ='R',max='**' ),
           AMOR_REDUIT     =SIMP(statut='f',typ='R',max='**' ),
                              ),
         b_matr_impe     =BLOC(condition = """exists("MATR_IMPE")""",
             FREQ_EXTR       =SIMP(statut='o',typ='R' ),
             AMOR_SOL        =SIMP(statut='f',typ='R',defaut=0.E+0 ),
             MATR_IMPE_INIT  =SIMP(statut='f',typ=matr_asse_gene_c ),
           ),
         CAS_CHARGE      =FACT(statut='f',max='**',
           NOM_CAS         =SIMP(statut='o',typ='TXM'),
           VECT_ASSE_GENE  =SIMP(statut='o',typ=vect_asse_gene ),
         ),
)  ;
