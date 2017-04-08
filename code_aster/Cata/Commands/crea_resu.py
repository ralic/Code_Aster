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
# person_in_charge: j-pierre.lefebvre at edf.fr
from code_aster.Cata.Syntax import *
from code_aster.Cata.DataStructure import *
from code_aster.Cata.Commons import *


def crea_resu_prod(TYPE_RESU,**args):
    if TYPE_RESU == "EVOL_ELAS"    : return evol_elas
    if TYPE_RESU == "EVOL_NOLI"    : return evol_noli
    if TYPE_RESU == "EVOL_THER"    : return evol_ther
    if TYPE_RESU == "MULT_ELAS"    : return mult_elas
    if TYPE_RESU == "MODE_MECA"    : return mode_meca
    if TYPE_RESU == "MODE_MECA_C"  : return mode_meca_c
    if TYPE_RESU == "DYNA_TRANS"   : return dyna_trans
    if TYPE_RESU == "DYNA_HARMO"   : return dyna_harmo
    if TYPE_RESU == "FOURIER_ELAS" : return fourier_elas
    if TYPE_RESU == "FOURIER_THER" : return fourier_ther
    if TYPE_RESU == "EVOL_VARC"    : return evol_varc
    if TYPE_RESU == "EVOL_CHAR"    : return evol_char
    raise AsException("type de concept resultat non prevu")

CREA_RESU=OPER(nom="CREA_RESU",op=124,sd_prod=crea_resu_prod,reentrant='f',
               fr=tr("Creer ou enrichir une structure de donnees resultat a partir de champs aux noeuds"),

         reuse=SIMP(statut='c', typ=CO),
         OPERATION =SIMP(statut='o',typ='TXM',into=("AFFE","ASSE","ECLA_PG","PERM_CHAM","PROL_RTZ","PREP_VRC1","PREP_VRC2",),
                         fr=tr("choix de la fonction a activer"),),

         TYPE_RESU    =SIMP(statut='o',typ='TXM',
                            into=(
                                  # pour bloc AFFE
                                    "MODE_MECA","MODE_MECA_C","MULT_ELAS",
                                    "EVOL_ELAS","EVOL_NOLI",
                                    "DYNA_HARMO","DYNA_TRANS",
                                    "FOURIER_ELAS","FOURIER_THER",
                                    "EVOL_THER","EVOL_VARC","EVOL_CHAR"
                                  # pour bloc ASSE
                                  # "EVOL_THER "
                                  # pour bloc ECLA_PG
                                  # "EVOL_ELAS","EVOL_NOLI","EVOL_THER"
                                  # pour bloc PERM_CHAM
                                  # "EVOL_NOLI"
                                  # pour bloc PROL_RTZ
                                  # "EVOL_THER"
                                  # pour bloc PREP_VRC1
                                  # "EVOL_THER"
                                  # pour bloc PREP_VRC2
                                  # "EVOL_THER"
                                  ),
                           ),

         b_affe_mult_elas = BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'MULT_ELAS')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             NOM_CAS       =SIMP(statut='f',typ='TXM' ),
             CHARGE        =SIMP(statut='f',typ=(char_meca),max='**'),
           ),
         ), # fin bloc b_affe_mult_elas
         
         b_affe_evol_dyn= BLOC(condition = """equal_to("OPERATION", 'AFFE') and is_in('TYPE_RESU', ('EVOL_ELAS', 'EVOL_NOLI', 'EVOL_THER', 'EVOL_VARC', 'EVOL_CHAR', 'DYNA_TRANS'))""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             regles=(UN_PARMI('INST','LIST_INST'),),
             INST          =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
             LIST_INST     =SIMP(statut='f',typ=listr8_sdaster),
             NUME_INIT     =SIMP(statut='f',typ='I', val_min=1),
             NUME_FIN      =SIMP(statut='f',typ='I', val_min=1),
             PRECISION     =SIMP(statut='f',typ='R',defaut= 0.0 ),
             CRITERE       =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU") ),
           ),
         ), # fin bloc b_affe_evol_dyn

         b_affe_fourier_elas= BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'FOURIER_ELAS')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             NUME_MODE     =SIMP(statut='f',typ='I'),
             TYPE_MODE     =SIMP(statut='f',typ='TXM',defaut="SYME",into=("SYME","ANTI","TOUS") ),
             CHARGE        =SIMP(statut='f',typ=(char_meca),max='**'),
           ),
         ), # fin bloc b_affe_fourier_elas

         b_affe_fourier_ther= BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'FOURIER_THER')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             NUME_MODE     =SIMP(statut='f',typ='I'),
             TYPE_MODE     =SIMP(statut='f',typ='TXM',defaut="SYME",into=("SYME","ANTI","TOUS") ),
           ),
         ), # fin bloc b_affe_fourier_ther

         # Creation par affectation de champs :
         #-------------------------------------
         b_evol_char =BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'EVOL_CHAR')""",
             NOM_CHAM     =SIMP(statut='o',typ='TXM',validators=NoRepeat(),into=("PRES","FORC_NODA",
                                    "FSUR_2D","FSUR_3D","FVOL_2D","FVOL_3D","VITE_VENT","T_EXT","COEF_H")),
             COMPORTEMENT     =C_COMPORTEMENT(),
         ), # fin bloc b_evol_char
         
         b_cham_no    =BLOC(condition = """equal_to("OPERATION", 'AFFE') and not equal_to("TYPE_RESU", 'EVOL_CHAR')""",
             NOM_CHAM     =SIMP(statut='o',typ='TXM',validators=NoRepeat(),into=C_NOM_CHAM_INTO()),
             COMPORTEMENT     =C_COMPORTEMENT(),
         ),  # fin bloc b_cham_no
                   
         b_mode_meca =BLOC(condition = """equal_to("OPERATION", 'AFFE') and is_in('TYPE_RESU', ('MODE_MECA', 'MODE_MECA_C', 'DYNA_HARMO', 'DYNA_TRANS'))""", 
             MATR_RIGI    =SIMP(statut='f',typ=matr_asse_depl_r,),
             MATR_MASS    =SIMP(statut='f',typ=matr_asse_depl_r,),
         ), # fin bloc b_mode_meca
        
         b_evol_elas =BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'EVOL_ELAS')""",
             EXCIT        =FACT(statut='f',max='**',
                CHARGE          =SIMP(statut='o',typ=(char_meca,char_cine_meca)),
                FONC_MULT       =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
                TYPE_CHARGE     =SIMP(statut='f',typ='TXM',defaut="FIXE_CSTE",into=("FIXE_CSTE",) ),),
         ), # fin bloc b_evol_elas
        
         b_evol_ther =BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'EVOL_THER')""",
             EXCIT        =FACT(statut='f',max='**',
                CHARGE          =SIMP(statut='o',typ=(char_ther,char_cine_ther)),
                FONC_MULT       =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),),
         ), # fin bloc b_evol_ther

         b_evol_noli  = BLOC(condition="""equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'EVOL_NOLI')""",
             EXCIT           =FACT(statut='f',max='**',
                CHARGE          =SIMP(statut='o',typ=(char_meca,char_cine_meca)),
                FONC_MULT       =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
                TYPE_CHARGE     =SIMP(statut='f',typ='TXM',defaut="FIXE_CSTE",
                                  into=("FIXE_CSTE","FIXE_PILO","SUIV","DIDI")),
                DEPL            =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
                ACCE            =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
                VITE            =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
                MULT_APPUI      =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON") ),
                DIRECTION       =SIMP(statut='f',typ='R',max='**'),
                NOEUD           =SIMP(statut='f',typ=no  ,validators=NoRepeat(),max='**'),
                GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),),
         ), # fin bloc b_evol_noli
   
         b_affe_mode_meca= BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'MODE_MECA')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             NUME_MODE     =SIMP(statut='o',typ='I'),
             FREQ          =SIMP(statut='f',typ='R'),
             AXE           =SIMP(statut='f',typ='TXM',into=("X","Y","Z") ),
           ),  
         ), # fin bloc b_affe_mode_meca
           
         b_affe_mode_meca_c= BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'MODE_MECA_C')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             NUME_MODE     =SIMP(statut='o',typ='I'),
             FREQ          =SIMP(statut='f',typ='R'),
             AXE           =SIMP(statut='f',typ='TXM',into=("X","Y","Z") ),
             AMOR_REDUIT    =SIMP(statut='f',typ='R'),
           ),  
         ), # fin bloc b_affe_mode_meca_c
           
         b_affe_dyna_harmo= BLOC(condition = """equal_to("OPERATION", 'AFFE') and equal_to("TYPE_RESU", 'DYNA_HARMO')""",

           AFFE         =FACT(statut='o',max='**',
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)),
             MODELE        =SIMP(statut='f',typ=modele_sdaster),
             CHAM_MATER    =SIMP(statut='f',typ=cham_mater),
             CARA_ELEM     =SIMP(statut='f',typ=cara_elem),
             regles=(UN_PARMI('FREQ','LIST_FREQ',),),
             FREQ          =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
             LIST_FREQ     =SIMP(statut='f',typ=listr8_sdaster),
             CRITERE       =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU") ),
             PRECISION     =SIMP(statut='f',typ='R',defaut=0.0),

           ),
         ), # fin b_affe_dyna_harmo

         # Creation par assemblage d'evol_ther :
         #-----------------------------------------
         b_asse       =BLOC(condition = """equal_to("OPERATION", 'ASSE')""",
           ASSE         =FACT(statut='o',max='**',
             RESULTAT       =SIMP(statut='o',typ=evol_ther),
             TRANSLATION    =SIMP(statut='f',typ='R',defaut= 0. ),
           ),
         ),


         b_ecla_pg    =BLOC(condition = """equal_to("OPERATION", 'ECLA_PG')""",

           ECLA_PG         =FACT(statut='o',
             regles=(EXCLUS('TOUT_ORDRE','NUME_ORDRE','INST','LIST_INST','LIST_ORDRE'),),
             NOM_CHAM        =SIMP(statut='o',typ='TXM',validators=NoRepeat(),max='**',into=C_NOM_CHAM_INTO('ELGA'),),
             MODELE_INIT     =SIMP(statut='o',typ=modele_sdaster),
             TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             MAILLE          =SIMP(statut='f',typ=ma  ,validators=NoRepeat(),max='**'),
             GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
             RESU_INIT       =SIMP(statut='o',typ=resultat_sdaster),
             MAILLAGE        =SIMP(statut='o',typ=maillage_sdaster),
             TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
             LIST_ORDRE      =SIMP(statut='f',typ=listis_sdaster),
             INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
             LIST_INST       =SIMP(statut='f',typ=listr8_sdaster),
             CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",),),
             b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
                 PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
             b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
                 PRECISION       =SIMP(statut='o',typ='R',),),
           ),
         ),


         b_perm_cham =BLOC(condition = """equal_to("OPERATION", 'PERM_CHAM')""",

           NOM_CHAM        =SIMP(statut='f',typ='TXM',into=("DEPL","SIEF_ELGA","VARI_ELGA","STRX_ELGA"),
                                 validators=NoRepeat(),max='**'),
           RESU_INIT       =SIMP(statut='o',typ=evol_noli),
           INST_INIT       =SIMP(statut='f',typ='R'),
           CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",),),
           b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
               PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
           b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
               PRECISION       =SIMP(statut='o',typ='R',),),
           MAILLAGE_INIT   =SIMP(statut='o',typ=maillage_sdaster,),
           RESU_FINAL      =SIMP(statut='o',typ=evol_noli,),
           MAILLAGE_FINAL  =SIMP(statut='o',typ=maillage_sdaster,),
           PERM_CHAM       =FACT(statut='o',max='**',
              GROUP_MA_FINAL =SIMP(statut='o',typ=grma),
              GROUP_MA_INIT  =SIMP(statut='o',typ=grma),
              TRAN           =SIMP(statut='o',typ='R',min=3,max=3),
              PRECISION      =SIMP(statut='f',typ='R',defaut=1.0E-3),
           ),
         ),

         b_prol_rtz   =BLOC(condition = """equal_to("OPERATION", 'PROL_RTZ')""",

           PROL_RTZ        =FACT(statut='o',
              regles=(EXCLUS('INST','LIST_INST'),),
              MAILLAGE_FINAL  =SIMP(statut='o',typ=maillage_sdaster,),
              TABLE           =SIMP(statut='o',typ=table_sdaster,fr=tr("Table issue de post_releve_t")),
              INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
              LIST_INST       =SIMP(statut='f',typ=listr8_sdaster),
              b_acce_reel     =BLOC(condition="""(exists("INST"))or(exists("LIST_INST"))""",
                 CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",),),
                 b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
                     PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
                 b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
                     PRECISION       =SIMP(statut='o',typ='R',),),
              ),
              PROL_DROITE     =SIMP(statut='f',typ='TXM',defaut="EXCLU",into=("CONSTANT","LINEAIRE","EXCLU",),),
              PROL_GAUCHE     =SIMP(statut='f',typ='TXM',defaut="EXCLU",into=("CONSTANT","LINEAIRE","EXCLU",),),
              REPERE          =SIMP(statut='o',typ='TXM',into=("CYLINDRIQUE",),),
              ORIGINE         =SIMP(statut='o',typ='R',min=3,max=3),
              AXE_Z           =SIMP(statut='o',typ='R',min=3,max=3),
           ),
         ),

         b_prep_vrc1      =BLOC(condition = """equal_to("OPERATION", 'PREP_VRC1')""",
           # calculer la temperature dans les couches des coques multicouche a partir d'un champ de fonctions
           # de fonctions du temps et de l'espace (epaisseur)

           PREP_VRC1        =FACT(statut='o',max=1,
             CHAM_GD       =SIMP(statut='o',typ=(cham_gd_sdaster)), # carte de fonctions du temps et de l'epaisseur
             MODELE        =SIMP(statut='o',typ=modele_sdaster),    # modele mecanique contenant les coques multicouche
             CARA_ELEM     =SIMP(statut='o',typ=cara_elem),         # CARA_ELEM pour connaitre EPAIS et COQU_NCOU
             INST          =SIMP(statut='o',typ='R',validators=NoRepeat(),max='**'),
           ),
         ),

         b_prep_vrc2      =BLOC(condition = """equal_to("OPERATION", 'PREP_VRC2')""",
           # calculer la temperature dans les couches des coques multicouche a partir d'un evol_ther "coque"
           # contenant TEMP_MIL/TEMP_INF/TEMP_SUP

           PREP_VRC2        =FACT(statut='o',max=1,
             EVOL_THER     =SIMP(statut='o',typ=(evol_ther)),       # evol_ther de type "coque" (TEMP_MIL/TEMP_INF/TEMP_SUP)
             MODELE        =SIMP(statut='o',typ=modele_sdaster),    # modele mecanique contenant les coques multicouche
             CARA_ELEM     =SIMP(statut='o',typ=cara_elem),         # CARA_ELEM pour connaitre EPAIS et COQU_NCOU

             TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
             MAILLE          =SIMP(statut='f',typ=ma  ,validators=NoRepeat(),max='**'),
             GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),

           ),
         ),
)  ;
