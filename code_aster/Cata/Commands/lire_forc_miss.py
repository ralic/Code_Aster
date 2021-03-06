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


LIRE_FORC_MISS=OPER(nom="LIRE_FORC_MISS",op= 179,sd_prod=vect_asse_gene,
                    fr=tr("Création d'un vecteur assemblé à partir d'une base modale"),
                    reentrant='n',
         BASE            =SIMP(statut='o',typ=mode_meca),
         NUME_DDL_GENE   =SIMP(statut='o',typ=nume_ddl_gene ),
         FREQ_EXTR       =SIMP(statut='o',typ='R',max=1),
         NOM_CMP         =SIMP(statut='f',typ='TXM',into=("DX","DY","DZ") ),
         NOM_CHAM        =SIMP(statut='f',typ='TXM',into=("DEPL","VITE","ACCE"),defaut="DEPL"),
         NUME_CHAR       =SIMP(statut='f',typ='I' ),
         ISSF            =SIMP(statut='f',typ='TXM',defaut="NON",into=("NON","OUI") ),
         UNITE_RESU_FORC =SIMP(statut='f',typ=UnitType(),defaut=30, inout='in',),
         NOM_RESU_FORC   =SIMP(statut='f',typ='TXM' ),
)  ;
