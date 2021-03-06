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
# person_in_charge: jacques.pellet at edf.fr
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


MECA_STATIQUE=OPER(nom="MECA_STATIQUE",op=46,sd_prod=evol_elas,
                   fr=tr("Résoudre un problème de mécanique statique linéaire"),reentrant='f',
         regles=(EXCLUS("INST","LIST_INST"),
                 AU_MOINS_UN('CHAM_MATER','CARA_ELEM',),),
         reuse=SIMP(statut='c', typ=CO),
         MODELE          =SIMP(statut='o',typ=modele_sdaster),
         CHAM_MATER      =SIMP(statut='f',typ=cham_mater,
         fr=tr("le CHAM_MATER est nécessaire, sauf si le modèle ne contient que des éléments discrets (modélisations DIS_XXX)"),
         ),
         CARA_ELEM       =SIMP(statut='f',typ=cara_elem,
         fr=tr("le CARA_ELEM est nécessaire dès que le modèle contient des éléments de structure : coques, poutres, ..."),
         ),
         TITRE           =SIMP(statut='f',typ='TXM'),
         EXCIT           =FACT(statut='o',max='**',
           CHARGE          =SIMP(statut='o',typ=(char_meca,char_cine_meca)),
           FONC_MULT       =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
           TYPE_CHARGE     =SIMP(statut='f',typ='TXM',defaut="FIXE_CSTE",into=("FIXE_CSTE",) ),
         ),
         INST            =SIMP(statut='f',typ='R'),
         LIST_INST       =SIMP(statut='f',typ=listr8_sdaster),
         INST_FIN        =SIMP(statut='f',typ='R'),
         OPTION          =SIMP(statut='f',typ='TXM',into=("SIEF_ELGA","SANS"),defaut="SIEF_ELGA",max=1,
             fr=tr("Seule option : contraintes aux points de Gauss. Utilisez CALC_CHAMP pour les autres options."),
                          ),

#-------------------------------------------------------------------
#        Catalogue commun SOLVEUR
         SOLVEUR         =C_SOLVEUR('MECA_STATIQUE'),
#-------------------------------------------------------------------
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
         translation={
            "MECA_STATIQUE": "Static mechanical analysis",
         }
)  ;
