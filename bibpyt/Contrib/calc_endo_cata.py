# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

from calc_endo_ops import calc_endo_ops
from Cata.cata import *

CALC_ENDO=MACRO(
    nom       = "CALC_ENDO",
    op        = calc_endo_ops,
    sd_prod   = evol_noli,
    reentrant = 'n',
    UIinfo    = {"groupes":("Résolution","Mécanique",)},
    fr        = "Calcul d'endommagement automatisé à partir d'un état initial",

    MODELE          = SIMP(statut='o',typ=modele_sdaster),
    CHAM_MATER      = SIMP(statut='o',typ=cham_mater),
    CARA_ELEM       = SIMP(statut='f',typ=cara_elem),
    EXCIT           = FACT(statut='o',max='**',
        CHARGE          = SIMP(statut='o',typ=(char_meca,char_cine_meca)),
        FONC_MULT       = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
        TYPE_CHARGE     = SIMP(statut='f',typ='TXM',defaut="FIXE_CSTE",
                              into=("FIXE_CSTE","FIXE_PILO","SUIV","DIDI")),
        ),
#-------------------------------------------------------------------
    COMPORTEMENT    = C_COMPORTEMENT('STAT_NON_LINE'),
#-------------------------------------------------------------------
    ETAT_INIT       = FACT(statut='f',max=1,
        UNITE           = SIMP(statut='F', typ='I', defaut=20),
        NOM_CHAM_MED    = SIMP(statut='F', typ='TXM', defaut='VARI'),
        ),
#-------------------------------------------------------------------
    INCREMENT       = FACT(statut='d', max=1,
        NOMBRE          = SIMP(statut='F', typ='I', defaut=5),
        ),
#-------------------------------------------------------------------
    PILOTAGE        = FACT(statut='o', max=1,
        regles          = (PRESENT_ABSENT('TOUT','GROUP_MA'),),
        TYPE            = SIMP(statut='c',typ='TXM',defaut='PRED_ELAS'),
        COEF_MULT       = SIMP(statut='c',typ='R',defaut=10.0),
        ETA_PILO_MAX    = SIMP(statut='f',typ='R'),
        ETA_PILO_MIN    = SIMP(statut='f',typ='R'),
        ETA_PILO_R_MAX  = SIMP(statut='f',typ='R'),
        ETA_PILO_R_MIN  = SIMP(statut='f',typ='R'),
        PROJ_BORNES     = SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON")),
        SELECTION       = SIMP(statut='f',typ='TXM',defaut="MIXTE",
                            into=("RESIDU","MIXTE","ANGL_INCR_DEPL","NORM_INCR_DEPL")),
        TOUT            = SIMP(statut='f',typ='TXM',into=("OUI",) ),
        GROUP_MA        = SIMP(statut='f',typ=grma ,validators=NoRepeat(),max='**'),
        ),
#--------------------------------------------------------------
    CONVERGENCE     = FACT(statut='o', max=1,
        SIGM_REFE       =SIMP(statut='o',typ='R'),
        LAGR_REFE       =SIMP(statut='f',typ='R'),
        VARI_REFE       =SIMP(statut='f',typ='R',defaut=1.0),
        RESI_REFE_RELA  =SIMP(statut='f',typ='R',defaut=1.e-3),
        ITER_GLOB_MAXI  =SIMP(statut='f',typ='I',defaut=20),
        ITER_GLOB_ELAS  =SIMP(statut='f',typ='I',defaut=100),
        ARRET           =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON")),
        ),
#--------------------------------------------------------------
    SOLVEUR         = C_SOLVEUR('STAT_NON_LINE'),
#--------------------------------------------------------------
    ARCHIVAGE       = C_ARCHIVAGE(),
#--------------------------------------------------------------
    IMPR            = FACT(statut='f', max=1,
        UNITE           = SIMP(statut='f',typ='I',defaut=80),
        FILTRE          = SIMP(statut='f',typ='TXM',defaut='OUI',into=('OUI','NON')),
        ),
#--------------------------------------------------------------
    INFO            = SIMP(statut='f',typ='I',into=(1,2) ),
    TITRE           = SIMP(statut='f',typ='TXM',max='**' ),
    )
