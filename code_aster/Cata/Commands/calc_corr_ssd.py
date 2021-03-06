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


CALC_CORR_SSD=OPER(nom="CALC_CORR_SSD",op=  91,sd_prod=table_container,
                    fr=tr("Qualite d'un modele reduit en dynamique"),
                    reentrant='n',
         MODELE_GENE    =SIMP(statut='o',typ=modele_gene),
         RESU_GENE      =SIMP(statut='o',typ=mode_gene ),
         UNITE          =SIMP(statut='f',typ=UnitType(),defaut=6, inout='in'), 
         SHIFT          =SIMP(statut='f',typ='R',defaut= 1. ),
         VERIF          =FACT(statut='f',max='**',
           STOP_ERREUR     =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
           PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-3 ),
           CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU") ),
         ),
         SOLVEUR =C_SOLVEUR('CALC_CORR_SSD'),
         TITRE          =SIMP(statut='f',typ='TXM'),
)  ;
