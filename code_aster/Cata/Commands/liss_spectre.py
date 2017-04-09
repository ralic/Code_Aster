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
# person_in_charge: adrien.guilloux at edf.fr


from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *

LISS_SPECTRE=MACRO(nom="LISS_SPECTRE",
                   op=OPS('Macro.liss_spectre_ops.liss_spectre_ops'),
                   reentrant='n',
                   fr=tr("Lissage de spectre, post-traitement de séisme"),
         
         SPECTRE     =FACT(statut='o',max='**',
             regles=(UN_PARMI('TABLE','NAPPE'),),
             TABLE         =SIMP(statut='f',typ=table_sdaster),
             NAPPE         =SIMP(statut='f',typ=nappe_sdaster),
             ELARG         =SIMP(statut='f',typ='R'),
             
             b_nappe=BLOC( condition = """exists("NAPPE")""",
                   DIRECTION     =SIMP(statut='o',typ='TXM',into=('X','Y','Z','H'),),
                   NOM           =SIMP(statut='o',typ='TXM',),
                   BATIMENT      =SIMP(statut='f',typ='TXM',),
                   COMMENTAIRE   =SIMP(statut='f',typ='TXM',),
             ), # fin b_nappe
         ),
         
         OPTION          =SIMP(statut='o',typ='TXM' ,into=('VERIFICATION','CONCEPTION')),
         FREQ_MIN        =SIMP(statut='f',typ='R'),
         FREQ_MAX        =SIMP(statut='f',typ='R'),
         NB_FREQ_LISS    =SIMP(statut='f',typ='I',max=2, val_min=1, defaut=10, fr=tr("Nb de points pour le lissage ") ), 
         ZPA             =SIMP(statut='f',typ='R'), 
         
         # format du graphique
         BORNE_X         =SIMP(statut='f',typ='R',min=2,max=2,
                               fr=tr("Intervalles de variation des abscisses")),
         BORNE_Y         =SIMP(statut='f',typ='R',min=2,max=2,
                                fr=tr("Intervalles de variation des ordonnées")),
         ECHELLE_X       =SIMP(statut='f',typ='TXM',defaut="LIN",into=("LIN","LOG"),
                                fr=tr("Type d'échelle pour les abscisses") ),
         ECHELLE_Y       =SIMP(statut='f',typ='TXM',defaut="LIN",into=("LIN","LOG"),
                                fr=tr("Type d'échelle pour les ordonnées") ),
         LEGENDE_X       =SIMP(statut='f',typ='TXM',
                                fr=tr("Légende associée à l'axe des abscisses") ),
         LEGENDE_Y       =SIMP(statut='f',typ='TXM',
                                fr=tr("Légende associée à l'axe des ordonnées") ),
)
