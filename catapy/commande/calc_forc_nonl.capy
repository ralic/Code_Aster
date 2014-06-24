# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: Georges-cc.devesa at edf.fr

CALC_FORC_NONL=OPER(nom="CALC_FORC_NONL",op= 183,sd_prod=dyna_trans,reentrant='n',
            fr=tr("Créer un dyna_trans contenant des champs nommés 'DEPL' correspondant à 'FONL_NOEU' "),
            UIinfo={"groupes":("Post-traitements","Résultats et champs",)},
         RESULTAT        =SIMP(statut='o',typ=resultat_sdaster),

         regles=(EXCLUS('TOUT_ORDRE','NUME_ORDRE','INST','FREQ','NUME_MODE',
                        'NOEUD_CMP','LIST_INST','LIST_FREQ','LIST_ORDRE','NOM_CAS'),
                 ),
         TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
         NUME_MODE       =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**'),
         NOEUD_CMP       =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max='**'),
         NOM_CAS         =SIMP(statut='f',typ='TXM' ),
         INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
         FREQ            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**'),
         LIST_INST       =SIMP(statut='f',typ=listr8_sdaster),
         LIST_FREQ       =SIMP(statut='f',typ=listr8_sdaster),
         LIST_ORDRE      =SIMP(statut='f',typ=listis_sdaster),
         CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("RELATIF","ABSOLU",),),
         b_prec_rela=BLOC(condition="(CRITERE=='RELATIF')",
             PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
         b_prec_abso=BLOC(condition="(CRITERE=='ABSOLU')",
             PRECISION       =SIMP(statut='o',typ='R',),),
         OPTION          =SIMP(statut='o',typ='TXM',validators=NoRepeat(),max=1, defaut="FONL_NOEU",
                               into=("FONL_NOEU",) ),

         MODELE          =SIMP(statut='o',typ=modele_sdaster),
         CHAM_MATER      =SIMP(statut='f',typ=cham_mater),
         CARA_ELEM       =SIMP(statut='f',typ=cara_elem),

         COMPORTEMENT       =C_COMPORTEMENT(),
)  ;
