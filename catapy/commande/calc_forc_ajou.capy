# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: nicolas.greffet at edf.fr
CALC_FORC_AJOU=OPER(nom="CALC_FORC_AJOU",op=199,sd_prod=vect_asse_gene,
                   fr=tr("Calculer l'effet de surpression hydrodynamique due au mouvement d'entrainement de la structure"
                         " en analyse sismique"),
                   reentrant ='n',
            UIinfo={"groupes":("Matrices et vecteurs",)},

        regles=(EXCLUS('MODE_MECA','MODELE_GENE'),
                PRESENT_PRESENT( 'MODELE_GENE','NUME_DDL_GENE'),
                UN_PARMI('MONO_APPUI', 'NOEUD','GROUP_NO'),
                UN_PARMI('MONO_APPUI','MODE_STAT')),

         MODELE_FLUIDE   =SIMP(statut='o',typ=modele_sdaster ),
         MODELE_INTERFACE=SIMP(statut='o',typ=modele_sdaster ),
         CHAM_MATER      =SIMP(statut='o',typ=cham_mater ),
         CHARGE          =SIMP(statut='o',typ=char_ther ),
         MODE_MECA       =SIMP(statut='f',typ=mode_meca ),
         MODELE_GENE     =SIMP(statut='f',typ=modele_gene ),
         NUME_DDL_GENE   =SIMP(statut='f',typ=nume_ddl_gene ),
         DIST_REFE       =SIMP(statut='f',typ='R',defaut= 1.E-2 ),
         AVEC_MODE_STAT  =SIMP(statut='f',typ='TXM',defaut="OUI",into=("OUI","NON") ),
         NUME_MODE_MECA  =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
         POTENTIEL       =SIMP(statut='f',typ=evol_ther ),
         NOEUD_DOUBLE    =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON") ),

         DIRECTION       =SIMP(statut='o',typ='R',max=3),
         MONO_APPUI      =SIMP(statut='f',typ='TXM',into=("OUI",),),
         NOEUD           =SIMP(statut='f',typ=no  ,validators=NoRepeat(),max='**'),
         GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
         MODE_STAT       =SIMP(statut='f',typ=mode_meca,),
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2 ) ),
#-------------------------------------------------------------------
#        Catalogue commun SOLVEUR
         SOLVEUR         =C_SOLVEUR('CALC_FORC_AJOU'),
#-------------------------------------------------------------------
     ) ;
