# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: jean-luc.flejou at edf.fr

DEFI_MATER_GC=MACRO(nom="DEFI_MATER_GC",
    op=OPS('Macro.defi_mater_gc_ops.defi_mater_gc_ops'),
    sd_prod=mater_sdaster,
    reentrant='n',
    UIinfo={"groupes":("Modélisation",) },
    fr=tr("Définir des lois matériaux spécifique au Génie Civil"),
    #
    regles = (UN_PARMI('MAZARS','ACIER','ENDO_FISS_EXP'), ),
    #
    # ============================================================================
    MAZARS      =FACT(statut= 'f',max= 1,
        fr=tr("Paramètres matériaux de MAZARS unilatéral en 1D à partir des caractéristiques du béton"),
        CODIFICATION =SIMP(statut='o',typ='TXM', into=('ESSAI','BAEL91','EC2'),),
        b_BAEL91 = BLOC(condition = " CODIFICATION == 'BAEL91'",
            UNITE_CONTRAINTE =SIMP(statut='o',typ='TXM',   into=("MPa","Pa"), fr=tr("Unité des contraintes du problème.")),
            FCJ =SIMP(statut='o',typ='R', val_min=0.0E+0, fr=tr("Contrainte au pic en compression [Unité]"),),
        ),
        b_EC2 = BLOC(condition = " CODIFICATION == 'EC2'",
            UNITE_CONTRAINTE =SIMP(statut='o',typ='TXM',   into=("MPa","Pa"), fr=tr("Unité des contraintes du problème.")),
            CLASSE =SIMP(statut='o',typ='TXM', fr=tr("Classe de résistance du béton, selon Eurocode 2"),
                            into=(  "C12/15", "C16/20", "C20/25", "C25/30", "C30/37", "C35/45", "C40/50",
                                    "C45/55", "C50/60", "C55/67", "C60/75", "C70/85", "C80/95", "C90/105"),),
        ),
        b_ESSAI = BLOC(condition = " CODIFICATION == 'ESSAI'",
            FCJ            =SIMP(statut='o',typ='R', val_min=0.0E+0,                 fr=tr("Contrainte au pic en compression [Unité]")),
            EIJ            =SIMP(statut='o',typ='R', val_min=0.0E+0,                 fr=tr("Module d'Young [Unité]")),
            EPSI_C         =SIMP(statut='o',typ='R', val_min=0.0E+0,                 fr=tr("Déformation au pic en compression")),
            FTJ            =SIMP(statut='o',typ='R', val_min=0.0E+0,                 fr=tr("Contrainte au pic en traction [Unité]")),
            NU             =SIMP(statut='f',typ='R', val_min=0.0E+0, val_max=0.5E+0, fr=tr("Coefficient de poisson")),
            EPSD0          =SIMP(statut='f',typ='R', val_min=0.0E+0,                 fr=tr("Déformation, seuil d'endommagement")),
            K              =SIMP(statut='f',typ='R', val_min=0.0E+0,                 fr=tr("Asymptote en cisaillement pur")),
            AC             =SIMP(statut='f',typ='R', val_min=0.0E+0,                 fr=tr("Paramètre de décroissance post-pic en compression")),
            BC             =SIMP(statut='f',typ='R', val_min=0.0E+0,                 fr=tr("Paramètre de décroissance post-pic en compression")),
            AT             =SIMP(statut='f',typ='R', val_min=0.0E+0, val_max=1.0E+0, fr=tr("Paramètre de décroissance post-pic en traction")),
            BT             =SIMP(statut='f',typ='R', val_min=0.0E+0,                 fr=tr("Paramètre de décroissance post-pic en traction")),
            # Pour post-traitement ELS et ELU
            SIGM_LIM       =SIMP(statut='f',typ='R',                                 fr=tr("Contrainte limite, post-traitement")),
            EPSI_LIM       =SIMP(statut='f',typ='R',                                 fr=tr("Déformation limite, post-traitement")),
        ),
    ),
    # ============================================================================
    ACIER       =FACT(statut= 'f',max= 1,
        fr=tr("Définir les paramètres matériaux de l'acier pour le Génie Civil"),
        E              =SIMP(statut='o',typ='R',  val_min=0.0E+0,                    fr=tr("Module d'Young")),
        SY             =SIMP(statut='o',typ='R',                                     fr=tr("Limite élastique")),
        NU             =SIMP(statut='f',typ='R',  val_min=0.0E+0, val_max=0.5E+0,    fr=tr("Coefficient de poisson")),
        D_SIGM_EPSI    =SIMP(statut='f',typ='R',                                     fr=tr("Module plastique")),
        # Pour post-traitement ELS et ELU
        SIGM_LIM       =SIMP(statut='f',typ='R',                                     fr=tr("Contrainte limite, post-traitement")),
        EPSI_LIM       =SIMP(statut='f',typ='R',                                     fr=tr("Déformation limite, post-traitement")),
    ),
    # ============================================================================
    ENDO_FISS_EXP       =FACT(statut= 'f',max= 1,
        fr=tr("Définir les paramètres matériaux du béton pour la loi ENDO_FISS_EXP"),
        regles = (
            UN_PARMI('FT','FT_FENDAGE'),
            UN_PARMI('P','DSIG_DU'),
            EXCLUS('Q','Q_REL'),
            ),
        E              =SIMP(statut='o',typ='R',  val_min=0.0E+0,                     fr=tr("Module d'Young")),
        NU             =SIMP(statut='o',typ='R',  val_min=0.0E+0, val_max=0.5E+0,     fr=tr("Coefficient de poisson")),
        FT             =SIMP(statut='f',typ='R',  val_min=0.0E+0                ,     fr=tr("Limite en traction simple")),
        FT_FENDAGE     =SIMP(statut='f',typ='R',  val_min=0.0E+0                ,     fr=tr("Limite en traction obtenue via un essai brésilien")),
        FC             =SIMP(statut='o',typ='R',  val_min=0.0E+0                ,     fr=tr("Limite en compression simple")),
        GF             =SIMP(statut='o',typ='R',  val_min=0.0E+0                ,     fr=tr("Energie de fissuration")),
        P              =SIMP(statut='f',typ='R',  val_min=1.0E+0                ,     fr=tr("Parametre dominant de la loi cohésive asymptotique")),
        DSIG_DU        =SIMP(statut='f',typ='R',  val_min=1.102658              ,     fr=tr("Pente initiale (au signe pres) de la loi cohesive asymptotique")),
        Q              =SIMP(statut='f',typ='R',  val_min=0.0E+0                ,     fr=tr("Parametre secondaire de la loi cohesive asymptotique")),
        Q_REL          =SIMP(statut='f',typ='R',  val_min=0.0E+0, val_max=1.0   ,     fr=tr("Parametre Q exprime de maniere relative par rapport a Qmax(P)")),
        LARG_BANDE     =SIMP(statut='o',typ='R',  val_min=0.0E+0                ,     fr=tr("Largeur de bande d'endommagement (2*D)")),
        COEF_RIGI_MINI =SIMP(statut='f',typ='R',  val_min=0.0, defaut = 0.0     ,     fr=tr("Rigidite minimale dans la matrice tangente")),
    ),
    # ============================================================================
    INFO        =SIMP(statut='f',typ='I',     into=(1,2,),     defaut=1),
    RHO         =SIMP(statut='f',typ='R',     fr=tr("Masse volumique")),
    ALPHA       =SIMP(statut='f',typ='R',     fr=tr("Coefficient de dilatation")),
    AMOR_ALPHA  =SIMP(statut='f',typ='R'),
    AMOR_BETA   =SIMP(statut='f',typ='R'),
    AMOR_HYST   =SIMP(statut='f',typ='R'),
)
