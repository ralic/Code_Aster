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
# person_in_charge: romeo.fernandes at edf.fr

from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


POST_MAC3COEUR = MACRO(nom="POST_MAC3COEUR",
                       sd_prod=table_sdaster,
                       op=OPS("Mac3coeur.post_mac3coeur_ops.post_mac3coeur_ops"),

           TYPE_COEUR   = SIMP(statut='o',typ='TXM',into=("MONO","MONO_FROID","TEST","900","1300","N4","LIGNE900","LIGNE1300","LIGNEN4")),
           RESULTAT     = SIMP(statut='o',typ=evol_noli),                             # SD_RESULTAT
           TABLE        = SIMP(statut='o',typ=table_sdaster),                         # TABLE DES DAMAC A L INSTANT N
           INST         = SIMP(statut='o',typ='R', max=1),                            # INSTANT

           LAME    = FACT(statut='f',max='**',
                          fr=tr("Post-traitement des lames d'eau, par grille ou valeurs min/max"),

                 FORMAT       = SIMP(statut='o',typ='TXM',into=("GRACE","TABLE")),
                 UNITE        = SIMP(statut='o',typ=UnitType(), inout='out',
                                     fr=tr("Numéro de l'unité logique pour le post-traitement")),

                 b_lame_grace  = BLOC(condition = """equal_to("FORMAT", 'GRACE') """,fr=tr("Paramètres pour le format GRACE"),
                       regles = UN_PARMI('NUME_GRILLE','TYPE_RESU',),
                       NUME_GRILLE  = SIMP(statut='f',typ='I', max=1),                      # NUMERO DE LA GRILLE A POST-TRAITER
                       TYPE_RESU    = SIMP(statut='f',typ='TXM',into=("MINI","MAXI")),      # EXTREMA POUR LE POST
                                   ),

                 b_lame_table  = BLOC(condition = """equal_to("FORMAT", 'TABLE') """,fr=tr("Paramètres pour le format TABLE"),
                                   ),


           ),

           DEFORMATION = FACT(statut='f',max='**',
                              fr=tr("Post-traitement des deformations, par grille ou valeurs min/max"),

                 UNITE        = SIMP(statut='o',typ=UnitType(), inout='out'),
                 FORMAT       = SIMP(statut='o',typ='TXM',into=("GRACE","TABLE")),

                 b_def_grace  = BLOC(condition = """equal_to("FORMAT", 'GRACE') """,fr=tr("Paramètres pour le format GRACE"),
                       regles=UN_PARMI('NUME_GRILLE','TYPE_RESU','POSITION'),
                       TYPE_VISU    = SIMP(statut='o',typ='TXM',into=("AMPLITUDE","MODULE","VECTEUR","DEFORME")),
                       TYPE_RESU    = SIMP(statut='f',typ='TXM',into=("MINI","MAXI")),
                       NUME_GRILLE  = SIMP(statut='f',typ='I', max=1), # NUMERO DE LA GRILLE A POST-TRAITER
                       POSITION     = SIMP(statut='f',typ='TXM', max=1),
                       CONCEPTION   = SIMP(statut='f',typ='TXM', max=1),
                                   ),

                 b_def_table  = BLOC(condition = """equal_to("FORMAT", 'TABLE') """,fr=tr("Paramètres pour le format TABLE"),
                        NOM_SITE     = SIMP(statut='o',typ='TXM', max=1),
                        FORMAT_R     = SIMP(statut='f',typ='TXM', into=("DAMAC","STANDARD"), defaut="DAMAC")

                                   ),


           ),
)
