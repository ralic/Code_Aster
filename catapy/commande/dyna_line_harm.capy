# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: harinaivo.andriambololona at edf.fr
def dyna_line_harm_prod(MATR_RIGI,**args):
   if (AsType(MATR_RIGI) == matr_asse_depl_r) : return dyna_harmo
   elif (AsType(MATR_RIGI) == matr_asse_depl_c) : return dyna_harmo
   elif (AsType(MATR_RIGI) == matr_asse_pres_c) : return acou_harmo
   elif (AsType(MATR_RIGI) == matr_asse_gene_r) : return harm_gene
   elif (AsType(MATR_RIGI) == matr_asse_gene_c) : return harm_gene
   raise AsException("type de concept resultat non prevu")

DYNA_LINE_HARM=OPER(nom="DYNA_LINE_HARM",op=  60,sd_prod=dyna_line_harm_prod,
                    fr=tr("Calcul de la réponse dynamique complexe d'un système à une excitation harmonique"),
                    reentrant='f',
            UIinfo={"groupes":("Résolution","Dynamique",)},
         regles=(PRESENT_ABSENT('MATR_AMOR','AMOR_MODAL'),
                 UN_PARMI('FREQ','LIST_FREQ'),),
         CHAM_MATER      =SIMP(statut='f',typ=cham_mater ),
         CARA_ELEM       =SIMP(statut='f',typ=cara_elem ),
         MATR_MASS       =SIMP(statut='o',typ=(matr_asse_depl_r,matr_asse_pres_c,matr_asse_gene_r ) ),
         MATR_RIGI       =SIMP(statut='o',typ=(matr_asse_depl_r,matr_asse_depl_c,matr_asse_pres_c
                                              ,matr_asse_gene_r,matr_asse_gene_c ) ),
         MATR_AMOR       =SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_pres_c,matr_asse_gene_r ) ),
         AMOR_MODAL      =FACT(statut='f', max=1,
                    regles=(EXCLUS('AMOR_REDUIT','LIST_AMOR'),),
                AMOR_REDUIT     =SIMP(statut='f',typ='R',max='**'),
                LIST_AMOR       =SIMP(statut='f',typ=listr8_sdaster ),
         ),
         MATR_IMPE_PHI   =SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_gene_r) ),
         FREQ            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
         LIST_FREQ       =SIMP(statut='f',typ=listr8_sdaster ),
         TOUT_CHAM       =SIMP(statut='f',typ='TXM',into=("OUI",)),
         NOM_CHAM        =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max=3,into=("DEPL","VITE","ACCE") ),
         b_reuse =BLOC(condition = "reuse",fr=tr("en mode concept reentrant : RESULTAT obligatoire"),
             RESULTAT      =SIMP(statut='o',typ=(dyna_harmo,harm_gene)),
         ),
         EXCIT           =FACT(statut='o',max='**',
           regles=(UN_PARMI('VECT_ASSE','VECT_ASSE_GENE','CHARGE'),
                   UN_PARMI('FONC_MULT','FONC_MULT_C','COEF_MULT','COEF_MULT_C'),
                  ),
           VECT_ASSE       =SIMP(statut='f',position='global',typ=cham_no_sdaster),
           VECT_ASSE_GENE  =SIMP(statut='f',position='global',typ=vect_asse_gene),
           CHARGE          =SIMP(statut='f',position='global', typ=char_meca ),
           FONC_MULT_C     =SIMP(statut='f',typ=(fonction_c,formule_c) ),
           COEF_MULT_C     =SIMP(statut='f',typ='C' ),
           FONC_MULT       =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule) ),
           COEF_MULT       =SIMP(statut='f',typ='R' ),
           PHAS_DEG        =SIMP(statut='f',typ='R',defaut= 0.E+0 ),
           PUIS_PULS       =SIMP(statut='f',typ='I',defaut= 0 ),
         ),
         b_modele_char =BLOC(condition = " CHARGE != None ",
                       MODELE    =SIMP(statut='o',typ=modele_sdaster ),
                       ),
         b_modele_vect =BLOC(condition = " VECT_ASSE != None ",
                       MODELE    =SIMP(statut='f',typ=modele_sdaster ),
                       ),
         EXCIT_RESU      =FACT(statut='f',max='**',
           RESULTAT        =SIMP(statut='o',typ=(dyna_harmo,harm_gene)),
           COEF_MULT_C     =SIMP(statut='o',typ='C' ),
         ),
#-------------------------------------------------------------------
#        Catalogue commun SOLVEUR
         b_matr_gene =BLOC(condition = "AsType(MATR_MASS) in (matr_asse_gene_r,)",
                           fr=tr("Methode de resolution matrice generalisee"),
          SOLVEUR         =C_SOLVEUR('DYNA_LINE_HARM','GENE'),
         ),

         b_matr_phys =BLOC(condition = "AsType(MATR_MASS) in (matr_asse_depl_r,matr_asse_pres_c,)",
                           fr=tr("Methode de resolution matrice sur ddl physique"),
          SOLVEUR         =C_SOLVEUR('DYNA_LINE_HARM','PHYS'),
         ),
#-------------------------------------------------------------------

         TITRE           =SIMP(statut='f',typ='TXM',max='**'),
         INFO            =SIMP(statut='f',typ='I',into=(1,2) ),
)  ;
