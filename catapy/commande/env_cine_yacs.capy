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
# 
#  ENVOI DES CHAMPS CINEMATIQUES VIA YACS POUR COUPLAGE IFS 
#
ENV_CINE_YACS=PROC(nom             = "ENV_CINE_YACS",
                   op              = 111,
                   UIinfo          = {"groupes":("Résultats et champs",)},
                   fr              = tr("Envoi des champs de deplacement et vitesse via YACS pour Couplage de Code_Aster et Saturne"),
                   regles          = (EXCLUS('ETAT_INIT','RESULTAT',),),
                   MATR_PROJECTION = SIMP(statut='o', typ=corresp_2_mailla,),
                   VIS_A_VIS = FACT(statut='o', max='**',
                                   GROUP_MA_1=SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
                                   GROUP_NO_2=SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),),
                   RESULTAT  = FACT(statut='f',
                                   NUME_ORDRE=SIMP(statut='o', typ='I',              validators=NoRepeat()),
                                   RESU      =SIMP(statut='o', typ=resultat_sdaster, validators=NoRepeat()),),
                   ETAT_INIT = FACT(statut='f',
                                    DEPL=SIMP(statut='f', typ=cham_no_sdaster,  validators=NoRepeat()),
                                    VITE=SIMP(statut='f', typ=cham_no_sdaster,  validators=NoRepeat()),
                                    ACCE=SIMP(statut='f', typ=cham_no_sdaster,  validators=NoRepeat()),),
                   INST         = SIMP(statut='o',typ='R', ),
                   PAS             = SIMP(statut='o',typ='R', ),
                   NUME_ORDRE_YACS = SIMP(statut='o', typ='I',),
                   INFO            = SIMP(statut='f',typ='I',defaut=1,into=(1,2) ),
) ;
