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


def crea_champ_prod(TYPE_CHAM,**args):
  if TYPE_CHAM[0:5] == "CART_" :
     return carte_sdaster
  elif TYPE_CHAM[0:5] == "NOEU_" :
     return cham_no_sdaster
  elif TYPE_CHAM[0:2] == "EL"    :
     return cham_elem
  else :
     raise AsException("type de concept resultat_sdaster non prevu")


CREA_CHAMP=OPER(nom="CREA_CHAMP",op= 195,sd_prod=crea_champ_prod,
                fr=tr("Création d'un champ "),reentrant='f',

         reuse=SIMP(statut='c', typ=CO),
       # TYPE_CHAM doit etre de la forme : CART_xx, NOEU_xx, ELEM_xx, ELGA_xx ou ELNO_xx
       # ou xx est le nom d'une grandeur définie dans le catalogue des grandeurs
         TYPE_CHAM       =SIMP(statut='o',typ='TXM',into=C_TYPE_CHAM_INTO()),

#        SI CREATION D'UN CHAM_NO, POUR IMPOSER LA NUMEROTATION DES DDLS :
#        ------------------------------------------------------------------
         regles=(EXCLUS('NUME_DDL','CHAM_NO',)),
         NUME_DDL        =SIMP(statut='f',typ=(nume_ddl_sdaster) ),
         CHAM_NO         =SIMP(statut='f',typ=(cham_no_sdaster) ),


#        AUTORISE-T-ON LE PROLONGEMENT DU CHAMP PAR ZERO ?
#        ------------------------------------------------------------------
#        CE MOT CLE N'A DE SENS QUE DANS 2 CAS DE FIGURE :
#          - POUR LES CHAM_ELEM (AVEC LE MOT CLE MODELE)
#          - POUR LES CHAM_NO SI ON IMPOSE LEUR NUMEROTATION
         b_prol_zero  =BLOC(condition = """exists("NUME_DDL") or exists("CHAM_NO") or (exists("TYPE_CHAM") and TYPE_CHAM[0:2] == 'EL')""",
                 PROL_ZERO       =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON",)),),


#        SI CREATION D'UN CHAM_ELEM, POUR POUVOIR AIDER A L'ALLOCATION DU CHAMP :
#        (PAR DEFAUT : TOU_INI_ELNO/_ELGA/_ELEM)
#        ------------------------------------------------------------------
         OPTION         =SIMP(statut='f',typ='TXM',validators=NoRepeat(),),


#        Si creation d'un cham_elem avec sous-points, pour que tous les sous-points
#        soient affectes : on duplique la valeur sur tous les sous-points
#        ------------------------------------------------------------------
         AFFE_SP         =FACT(statut='f',max=1,
             CARA_ELEM          =SIMP(statut='o',typ=cara_elem,min=1,max=1),
                             ),


#        LE MOT-CLE OPERATION EST OBLIGATOIRE. IL PERMET LE BON AIGUILLAGE.
#        ------------------------------------------------------------------
         OPERATION       =SIMP(statut='o',typ='TXM',into=("AFFE","ASSE","ASSE_DEPL","EVAL","EXTR","DISC","NORMALE","R2C","C2R","COMB") ),

#        ------------------------------------------------------------------
         b_norm          =BLOC(condition = """equal_to("OPERATION", 'NORMALE')""",
                               regles=(AU_MOINS_UN('GROUP_MA','MAILLE',),),
             MODELE          =SIMP(statut='o',typ=(modele_sdaster) ),
             GROUP_MA        =SIMP(statut='f',typ=grma ,validators=NoRepeat(),max='**'),
             MAILLE          =SIMP(statut='c',typ=ma   ,validators=NoRepeat(),max='**'),
                             ),
#        ------------------------------------------------------------------
         b_affe          =BLOC(condition = """equal_to("OPERATION", 'AFFE')""",
             regles=(UN_PARMI('MAILLAGE','MODELE'),),
             MAILLAGE        =SIMP(statut='f',typ=(maillage_sdaster) ),
             MODELE          =SIMP(statut='f',typ=(modele_sdaster) ),
             AFFE            =FACT(statut='o',max='**',
                regles=(AU_MOINS_UN('TOUT','GROUP_MA','MAILLE','GROUP_NO','NOEUD',),
                        UN_PARMI('VALE','VALE_I','VALE_C','VALE_F', ),),
                TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
                GROUP_MA        =SIMP(statut='f',typ=grma ,validators=NoRepeat(),max='**'),
                MAILLE          =SIMP(statut='c',typ=ma   ,validators=NoRepeat(),max='**'),
                GROUP_NO        =SIMP(statut='f',typ=grno ,validators=NoRepeat(),max='**'),
                NOEUD           =SIMP(statut='c',typ=no   ,validators=NoRepeat(),max='**'),
                NOM_CMP         =SIMP(statut='o',typ='TXM',max='**'),
                VALE            =SIMP(statut='f',typ='R',max='**' ),
                VALE_I          =SIMP(statut='f',typ='I',max='**' ),
                VALE_C          =SIMP(statut='f',typ='C',max='**' ),
                VALE_F          =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule),max='**'),
                                   ),
                             ),
#        ------------------------------------------------------------------
         b_asse          =BLOC(condition = """equal_to("OPERATION", 'ASSE')""",
             regles=(UN_PARMI('MAILLAGE','MODELE'),),
             MAILLAGE        =SIMP(statut='f',typ=(maillage_sdaster) ),
             MODELE          =SIMP(statut='f',typ=(modele_sdaster) ),
             ASSE            =FACT(statut='o',max='**',
                regles=(AU_MOINS_UN('TOUT','GROUP_MA','GROUP_NO','MAILLE','NOEUD',),
                PRESENT_PRESENT('NOM_CMP_RESU','NOM_CMP', ),),
                TOUT            =SIMP(statut='f',typ='TXM',into=("OUI",) ),
                GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
                GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
                MAILLE          =SIMP(statut='c',typ=ma  ,validators=NoRepeat(),max='**'),
                NOEUD           =SIMP(statut='c',typ=no  ,validators=NoRepeat(),max='**'),
                CHAM_GD         =SIMP(statut='o',typ=cham_gd_sdaster),
                NOM_CMP         =SIMP(statut='f',typ='TXM',max='**' ),
                NOM_CMP_RESU    =SIMP(statut='f',typ='TXM',max='**' ),
                CUMUL           =SIMP(statut='f',typ='TXM',defaut="NON",into=("OUI","NON",) ),
                COEF_R          =SIMP(statut='f',typ='R',defaut= 1. ),
                COEF_C          =SIMP(statut='f',typ='C',max=1),
                                    ),
                             ),
#        ------------------------------------------------------------------
         b_comb          =BLOC(condition = """equal_to("OPERATION", 'COMB')""",
                               fr=tr("Pour faire une combinaison linéaire de cham_no ayant meme profil"),
             COMB            =FACT(statut='o',max='**',
                CHAM_GD         =SIMP(statut='o',typ=cham_no_sdaster),
                COEF_R          =SIMP(statut='o',typ='R'),
                                   ),
                             ),
#        ------------------------------------------------------------------
         b_eval          =BLOC(condition = """equal_to("OPERATION", 'EVAL')""",
             CHAM_F          =SIMP(statut='o',typ=cham_gd_sdaster),
             CHAM_PARA       =SIMP(statut='o',typ=cham_gd_sdaster,max='**'),
                             ),
#        ------------------------------------------------------------------
         b_xfem          =BLOC(condition = """equal_to("OPERATION", 'ASSE_DEPL')""",
             CHAM_GD          =SIMP(statut='o',typ=cham_gd_sdaster),
             MODELE           =SIMP(statut='o',typ=(modele_sdaster) ),
             CHAM_MATER       =SIMP(statut='f',typ=cham_mater),
                             ),
#        ------------------------------------------------------------------
         b_r2c           =BLOC(condition = """equal_to("OPERATION", 'R2C')""",
             CHAM_GD          =SIMP(statut='o',typ=cham_gd_sdaster),
                             ),
#        ------------------------------------------------------------------
         b_c2r           =BLOC(condition = """equal_to("OPERATION", 'C2R')""",
             CHAM_GD          =SIMP(statut='o',typ=cham_gd_sdaster),
             PARTIE           =SIMP(statut='o',typ='TXM',into=('REEL','IMAG','MODULE','PHASE'),),
                             ),
#        ------------------------------------------------------------------
         b_disc          =BLOC(condition = """equal_to("OPERATION", 'DISC')""",
             MODELE          =SIMP(statut='f',typ=(modele_sdaster) ),
             CHAM_GD         =SIMP(statut='o',typ=cham_gd_sdaster),
                             ),
#        ------------------------------------------------------------------
         b_extr          =BLOC(condition = """equal_to("OPERATION", 'EXTR')""",
             regles=(AU_MOINS_UN('MAILLAGE','FISSURE','RESULTAT','TABLE','CARA_ELEM','CHARGE'),
                     PRESENT_ABSENT('MAILLAGE','FISSURE','RESULTAT','CARA_ELEM','CHARGE'),
                     PRESENT_ABSENT('FISSURE','MAILLAGE','RESULTAT','TABLE','CARA_ELEM','CHARGE'),
                     PRESENT_ABSENT('RESULTAT','FISSURE','MAILLAGE','TABLE','CARA_ELEM','CHARGE'),
                     PRESENT_ABSENT('TABLE','RESULTAT','FISSURE','CARA_ELEM','CHARGE'),
                     PRESENT_ABSENT('CARA_ELEM','MAILLAGE','TABLE','RESULTAT','FISSURE','CHARGE'),
                     PRESENT_ABSENT('CHARGE','MAILLAGE','TABLE','RESULTAT','FISSURE','CARA_ELEM'),
                     ),
             MAILLAGE        =SIMP(statut='f',typ=(maillage_sdaster) ),
             FISSURE         =SIMP(statut='f',typ=(fiss_xfem) ),
             RESULTAT        =SIMP(statut='f',typ=(resultat_sdaster) ),
             TABLE           =SIMP(statut='f',typ=(table_sdaster),min=1,max=1),
             CARA_ELEM       =SIMP(statut='f',typ=(cara_elem),min=1,max=1),
             CHARGE          =SIMP(statut='f',typ=(char_meca),min=1,max=1),
             b_extr_maillage =BLOC(condition = """exists("MAILLAGE") and not exists("TABLE")""",
                 NOM_CHAM        =SIMP(statut='o',typ='TXM',validators=NoRepeat(),into=("GEOMETRIE","ABSC_CURV")),
             ),

             b_extr_cara_elem =BLOC(condition = """exists("CARA_ELEM")""",
                 NOM_CHAM        =SIMP(statut='o',typ='TXM',validators=NoRepeat(),
                 into=('.CARGENBA', '.CARMASSI', '.CARCABLE', '.CARCOQUE', '.CARGEOBA', '.CARDISCK',
                       '.CARARCPO', '.CARGENPO', '.CARDISCM', '.CARORIEN', '.CARDISCA', '.CVENTCXF',
                       '.CARPOUFL', '.CARGEOPO', '.CARDINFO', '.CAFIBR',   '.CANBSP',)),
             ),

             b_extr_charge =BLOC(condition = """exists("CHARGE")""",
                 NOM_CHAM        =SIMP(statut='o',typ='TXM',validators=NoRepeat(),
                 into=('.CHME.EPSIN', '.CHME.F1D1D', '.CHME.F1D2D', '.CHME.F1D3D', '.CHME.F2D2D',
                       '.CHME.F2D3D', '.CHME.F3D3D', '.CHME.FCO2D', '.CHME.FCO3D', '.CHME.FELEC',
                       '.CHME.FL101', '.CHME.FL102', '.CHME.FLUX',  '.CHME.FORNO', '.CHME.IMPE',
                       '.CHME.ONDE',  '.CHME.ONDPL', '.CHME.ONDPR', '.CHME.PESAN', '.CHME.PRESS',
                       '.CHME.ROTAT', '.CHME.SIGIN', '.CHME.SIINT', '.CHME.VNOR',)),
             ),

             b_extr_fissure  = BLOC(condition = """exists("FISSURE")""",
                 NOM_CHAM=SIMP(statut='o',typ='TXM',validators=NoRepeat(),into=("LTNO","LNNO",
                               "GRLTNO","GRLNNO","STNO","STNOR","BASLOC","GRI.LNNO","GRI.LTNO","GRI.GRLNNO","GRI.GRLTNO")),

             ),

             b_extr_table    =BLOC(condition = """exists("TABLE")""",
                      MODELE          =SIMP(statut='f',typ=(modele_sdaster),),
                 ),
             b_extr_resultat =BLOC(condition = """exists("RESULTAT")""",
                 NOM_CHAM        =SIMP(statut='o',typ='TXM',validators=NoRepeat(),into=C_NOM_CHAM_INTO()),
                 TYPE_MAXI       =SIMP(statut='f',typ='TXM',into=("MAXI","MINI","MAXI_ABS","MINI_ABS","NORM_TRAN",) ),

                 # si TYPE_MAXI, on spécifie en général plusieurs numéros d'ordre :
                 b_type_maxi =BLOC(condition = """exists("TYPE_MAXI")""",
                      TYPE_RESU       =SIMP(statut='o',typ='TXM',defaut="VALE",into=("VALE","INST",) ),

                      regles=(EXCLUS('TOUT_ORDRE','LIST_INST','LIST_FREQ','NUME_ORDRE','INST',
                                      'FREQ','NUME_MODE','NOEUD_CMP','NOM_CAS','ANGLE'),),
                      TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
                      LIST_INST       =SIMP(statut='f',typ=(listr8_sdaster) ),
                      LIST_FREQ       =SIMP(statut='f',typ=(listr8_sdaster) ),
                      NUME_ORDRE      =SIMP(statut='f',typ='I',max='**'),
                      INST            =SIMP(statut='f',typ='R',max='**'),
                      FREQ            =SIMP(statut='f',typ='R',max='**'),
                      NUME_MODE       =SIMP(statut='f',typ='I',max='**'),
                      NOEUD_CMP       =SIMP(statut='f',typ='TXM',max='**'),
                      NOM_CAS         =SIMP(statut='f',typ='TXM',max='**'),
                      ANGLE           =SIMP(statut='f',typ='R',max='**'),
                 ),

                 # si .not. TYPE_MAXI, on ne doit spécifier qu'un seul numéro d'ordre :
                 b_non_type_maxi =BLOC(condition = """not exists("TYPE_MAXI")""",
                      regles=(EXCLUS('NUME_ORDRE','INST','FREQ','NUME_MODE','NOEUD_CMP','NOM_CAS','ANGLE'),),
                      NUME_ORDRE      =SIMP(statut='f',typ='I'),
                      INST            =SIMP(statut='f',typ='R'),
                      FREQ            =SIMP(statut='f',typ='R'),
                      NUME_MODE       =SIMP(statut='f',typ='I'),
                      NOEUD_CMP       =SIMP(statut='f',typ='TXM',max=2),
                      NOM_CAS         =SIMP(statut='f',typ='TXM'),
                      ANGLE           =SIMP(statut='f',typ='R'),

                      INTERPOL        =SIMP(statut='f',typ='TXM',defaut="NON",into=("NON","LIN",) ),
                 ),

                 CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",) ),
                 b_prec_rela=BLOC(condition="""(equal_to("CRITERE", 'RELATIF'))""",
                     PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
                 b_prec_abso=BLOC(condition="""(equal_to("CRITERE", 'ABSOLU'))""",
                     PRECISION       =SIMP(statut='o',typ='R',),),

         ),  # fin bloc b_extr


               ),
# FIN DU CATALOGUE : INFO,TITRE ET TYPAGE DU RESULTAT :
#-----------------------------------------------------
         INFO            =SIMP(statut='f',typ='I',defaut= 1,into=(1,2,) ),
         TITRE           =SIMP(statut='f',typ='TXM' ),
)  ;
