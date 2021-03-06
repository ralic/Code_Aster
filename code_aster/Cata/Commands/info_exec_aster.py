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
# person_in_charge: j-pierre.lefebvre at edf.fr
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


INFO_EXEC_ASTER=OPER(nom="INFO_EXEC_ASTER",op=35,sd_prod=table_sdaster,
                    fr=tr("Récupère différentes informations propres à l'exécution en cours"),
                    reentrant='n',

         regles=(),
         LISTE_INFO     =SIMP(statut='o',typ='TXM',validators=NoRepeat(),max=3,
                              into=("TEMPS_RESTANT","UNITE_LIBRE","ETAT_UNITE"),),
         b_etat_unite   =BLOC(condition = """is_in('LISTE_INFO', 'ETAT_UNITE')""",
            regles=(UN_PARMI('UNITE','FICHIER'),),
            UNITE          =SIMP(statut='f',typ=UnitType(),val_min=1,val_max=99,max=1,  inout='inout',
                                 fr=tr("Unité logique dont on veut obtenir l'état"),),
            FICHIER        =SIMP(statut='f',typ='TXM',validators=LongStr(1,255),
                                 fr=tr("Nom du fichier dont on veut obtenir l'état"),),
         ),
         TITRE          =SIMP(statut='f',typ='TXM'),
         INFO           =SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
)  ;
