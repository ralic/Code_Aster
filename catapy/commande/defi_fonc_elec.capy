# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: nicolas.relun at edf.fr


DEFI_FONC_ELEC=MACRO(nom="DEFI_FONC_ELEC",
                     op=OPS('Macro.defi_fonc_elec_ops.defi_fonc_elec_ops'),
                     sd_prod=fonction_sdaster,
                     reentrant='n',
                     UIinfo={"groupes":("Outils-métier",)},
                     fr=tr("Définir une fonction du temps intervenant dans le calcul des "
                          "forces de LAPLACE"),
         regles=(UN_PARMI('COUR_PRIN','COUR'),
                 EXCLUS('COUR','COUR_SECO'), ),
         FREQ            =SIMP(statut='f',typ='R',defaut= 50.),
         SIGNAL          =SIMP(statut='f',typ='TXM',defaut="COMPLET",into=("COMPLET","CONTINU") ),
         COUR            =FACT(statut='f',max='**',
           fr=tr("Définition du courant de court-circuit"),
           regles=(UN_PARMI('PHI_CC_1','INTC_CC_1'),
                   UN_PARMI('PHI_CC_2','INTC_CC_2'),),
           INTE_CC_1       =SIMP(statut='o',typ='R'),
           TAU_CC_1        =SIMP(statut='o',typ='R'),
           PHI_CC_1        =SIMP(statut='f',typ='R'),
           INTC_CC_1       =SIMP(statut='f',typ='R'),
           INTE_CC_2       =SIMP(statut='o',typ='R'),
           TAU_CC_2        =SIMP(statut='o',typ='R'),
           PHI_CC_2        =SIMP(statut='f',typ='R'),
           INTC_CC_2       =SIMP(statut='f',typ='R'),
           INST_CC_INIT    =SIMP(statut='o',typ='R'),
           INST_CC_FIN     =SIMP(statut='o',typ='R'),
         ),
         COUR_PRIN       =FACT(statut='f',
         fr=tr("Définition du courant de court-circuit avec réenclenchement"),
           regles=(UN_PARMI('PHI_CC_1','INTC_CC_1'),),
           INTE_CC_1       =SIMP(statut='o',typ='R'),
           TAU_CC_1        =SIMP(statut='o',typ='R'),
           PHI_CC_1        =SIMP(statut='f',typ='R'),
           INTC_CC_1       =SIMP(statut='f',typ='R'),
           INTE_RENC_1     =SIMP(statut='f',typ='R'),
           TAU_RENC_1      =SIMP(statut='f',typ='R'),
           PHI_RENC_1      =SIMP(statut='f',typ='R'),
           INST_CC_INIT    =SIMP(statut='o',typ='R'),
           INST_CC_FIN     =SIMP(statut='o',typ='R'),
           INST_RENC_INIT  =SIMP(statut='f',typ='R',defaut= 0.0E+0),
           INST_RENC_FIN   =SIMP(statut='f',typ='R',defaut= 0.0E+0),
         ),
         COUR_SECO       =FACT(statut='f',max='**',
         fr=tr("Définition du courant de court-circuit avec un intervalle de temps différent de celui de COUR_PRIN"),
           regles=(UN_PARMI('PHI_CC_2','INTC_CC_2'),),
           INTE_CC_2       =SIMP(statut='o',typ='R'),
           TAU_CC_2        =SIMP(statut='o',typ='R'),
           PHI_CC_2        =SIMP(statut='f',typ='R'),
           INTC_CC_2       =SIMP(statut='f',typ='R'),
           INTE_RENC_2     =SIMP(statut='f',typ='R'),
           TAU_RENC_2      =SIMP(statut='f',typ='R'),
           PHI_RENC_2      =SIMP(statut='f',typ='R'),
           DIST            =SIMP(statut='f',typ='R',defaut=1.0E+0),
         ),
         INFO            =SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
)
