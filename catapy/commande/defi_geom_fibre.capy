# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
DEFI_GEOM_FIBRE=OPER(
    nom="DEFI_GEOM_FIBRE", op=119, sd_prod=gfibre_sdaster,
    fr=tr("Definition des groupes de fibres pour les elements multifibres"),
    reentrant='n', UIinfo={"groupes":("Modélisation",)},
    regles=(AU_MOINS_UN('SECTION','FIBRE'),),
    INFO=SIMP(statut='f',typ='I', defaut= 1 ,into=(1,2)),
# ============================================================================
    SECTION             =FACT(statut='f',max='**',
        regles=(AU_MOINS_UN('TOUT_SECT','GROUP_MA_SECT','MAILLE_SECT'),
                PRESENT_ABSENT('TOUT_SECT','GROUP_MA_SECT','MAILLE_SECT'),),

        GROUP_FIBRE       =SIMP(statut='o',typ='TXM',min=1,max=1),
        TOUT_SECT         =SIMP(statut='f',typ='TXM',into=("OUI",) ),
        GROUP_MA_SECT     =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
        MAILLE_SECT       =SIMP(statut='f',typ=ma  ,validators=NoRepeat(),max='**'),

        MAILLAGE_SECT     =SIMP(statut='o',typ=maillage_sdaster),
        COOR_AXE_POUTRE   =SIMP(statut='o',typ='R',min=2,max=2),
    ),
# ============================================================================
    FIBRE               =FACT(statut='f',max='**',
        GROUP_FIBRE       =SIMP(statut='o',typ='TXM',min=1,max=1),
        CARA              =SIMP(statut='f',typ='TXM',defaut='SURFACE',into=('SURFACE','DIAMETRE',)),
        VALE              =SIMP(statut='o',typ='R',max='**'),
        COOR_AXE_POUTRE   =SIMP(statut='o',typ='R',min=2,max=2),
    ),
# ============================================================================
    ASSEMBLAGE_FIBRE    =FACT(statut='f',max='**',
        GROUP_ASSE_FIBRE  =SIMP(statut='o',typ='TXM',min=1,max=1),
        GROUP_FIBRE       =SIMP(statut='o',typ='TXM',min=1,max='**'),
        COOR_GROUP_FIBRE  =SIMP(statut='o',typ='R',  min=2,max='**'),
        GX_GROUP_FIBRE    =SIMP(statut='o',typ='R',  min=1,max='**'),
    ),
)
