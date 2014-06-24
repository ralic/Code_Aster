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
# person_in_charge: sylvie.michel-ponnelle at edf.fr

DEFI_CABLE_OP=OPER(nom="DEFI_CABLE_OP",op= 180,sd_prod=cabl_precont,reentrant='n',
            fr=tr("Définit les profils initiaux de tension d'une structure en béton le long des cables de précontrainte"
                " (utilisée par la macro DEFI_CABLE_BP)"),
            UIinfo={"groupes":("Modélisation",)},
         MODELE          =SIMP(statut='o',typ=modele_sdaster ),
         CHAM_MATER      =SIMP(statut='o',typ=cham_mater ),
         CARA_ELEM       =SIMP(statut='o',typ=cara_elem ),
         GROUP_MA_BETON  =SIMP(statut='o',typ=grma,max='**'),
         DEFI_CABLE      =FACT(statut='o',max='**',
           regles=(UN_PARMI('MAILLE','GROUP_MA'),
                   UN_PARMI('NOEUD_ANCRAGE','GROUP_NO_ANCRAGE'),),
           MAILLE          =SIMP(statut='f',typ=ma,min=2,validators=NoRepeat(),max='**'),
           GROUP_MA        =SIMP(statut='f',typ=grma),
           NOEUD_ANCRAGE   =SIMP(statut='f',typ=no  ,validators=NoRepeat(),max=2),
           GROUP_NO_ANCRAGE=SIMP(statut='f',typ=grno,validators=NoRepeat(),max=2),
           GROUP_NO_FUT    =SIMP(statut='f',typ=grno,validators=NoRepeat(),max=2),
           TENSION_CT      =SIMP(statut='f',typ=table_sdaster),
         ),
         ADHERENT        =SIMP(statut='o',typ='TXM',defaut='OUI',into=("OUI","NON") ),
         TYPE_ANCRAGE    =SIMP(statut='o',typ='TXM',min=2,max=2,into=("ACTIF","PASSIF") ),
         TENSION_INIT    =SIMP(statut='o',typ='R',val_min=0.E+0 ),
         RECUL_ANCRAGE   =SIMP(statut='o',typ='R',val_min=0.E+0 ),
         TYPE_RELAX      =SIMP(statut='o',typ='TXM',into=("SANS","BPEL","ETCC_DIRECT","ETCC_REPRISE"),defaut="SANS",),
         R_J             =SIMP(statut='f',typ='R',val_min=0.E+0),
         NBH_RELAX       =SIMP(statut='f',typ='R',val_min=0.E+0),
#         PERT_ELAS       =SIMP(statut='o',typ='TXM',into=("OUI","NON"),defaut="NON"),
#         EP_BETON        =SIMP(statut='f',typ='R',val_min=0.E+0),
#         ESP_CABLE       =SIMP(statut='f',typ='R',val_min=0.E+0),
         TITRE           =SIMP(statut='f',typ='TXM',max='**' ),
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2) ),
         CONE            =FACT(statut='f',min=0,
           RAYON             =SIMP(statut='o',typ='R',val_min=0.E+0 ),
           LONGUEUR          =SIMP(statut='o',typ='R',val_min=0.E+0, defaut=0.E+0 ),
           PRESENT           =SIMP(statut='o',typ='TXM',min=2,max=2,into=("OUI","NON") ),
         ),
)  ;
