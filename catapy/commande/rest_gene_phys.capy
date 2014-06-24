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

# person_in_charge: emmanuel.boyere at edf.fr

def rest_gene_phys_prod(RESU_GENE,**args ):
  if AsType(RESU_GENE) == tran_gene : return dyna_trans
  if AsType(RESU_GENE) == mode_gene : return mode_meca
  if AsType(RESU_GENE) == harm_gene : return dyna_harmo
  
  raise AsException("type de concept resultat non prevu")

REST_GENE_PHYS=OPER(nom="REST_GENE_PHYS",op=  75,sd_prod=rest_gene_phys_prod,
                    fr=tr("Restituer dans la base physique des résultats en coordonnées généralisées"),
                    reentrant='n',
            UIinfo={"groupes":("Matrices et vecteurs",)},
        regles=(
                EXCLUS('INST','LIST_INST','TOUT_INST',
                       'TOUT_ORDRE','NUME_ORDRE','NUME_MODE',),
                EXCLUS('FREQ','LIST_FREQ'),
                EXCLUS('MULT_APPUI','CORR_STAT'),
                EXCLUS('MULT_APPUI','NOEUD','GROUP_NO'),
                EXCLUS('CORR_STAT','NOEUD','GROUP_NO'),             
                EXCLUS('NOEUD','GROUP_NO'),
                EXCLUS('MAILLE','GROUP_MA'),
                PRESENT_PRESENT('ACCE_MONO_APPUI','DIRECTION'),),
         RESU_GENE       =SIMP(statut='f',typ=(tran_gene,mode_gene,harm_gene) ), 
         MODE_MECA       =SIMP(statut='f',typ=mode_meca ),
         NUME_DDL        =SIMP(statut='f',typ=nume_ddl_sdaster ),
         TOUT_INST       =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         INST            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**' ), 
         LIST_INST       =SIMP(statut='f',typ=listr8_sdaster ),
         TOUT_ORDRE      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         NUME_ORDRE      =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**' ),  
         NUME_MODE       =SIMP(statut='f',typ='I',validators=NoRepeat(),max='**' ), 
         FREQ            =SIMP(statut='f',typ='R',validators=NoRepeat(),max='**' ),  
         LIST_FREQ       =SIMP(statut='f',typ=listr8_sdaster ),
         CRITERE         =SIMP(statut='f',typ='TXM',defaut="RELATIF",into=("ABSOLU","RELATIF") ),
         b_prec_rela=BLOC(condition="(CRITERE=='RELATIF')",
             PRECISION       =SIMP(statut='f',typ='R',defaut= 1.E-6,),),
         b_prec_abso=BLOC(condition="(CRITERE=='ABSOLU')",
             PRECISION       =SIMP(statut='o',typ='R',),),
         INTERPOL        =SIMP(statut='f',typ='TXM',defaut="NON",into=("NON","LIN") ),
         MULT_APPUI      =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         CORR_STAT       =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         TOUT_CHAM       =SIMP(statut='f',typ='TXM',into=("OUI",) ),
         b_nom_cham=BLOC(condition="TOUT_CHAM == None",
             NOM_CHAM        =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max=8,defaut="ACCE",into=("DEPL",
                                   "VITE","ACCE","ACCE_ABSOLU","EFGE_ELNO","SIPO_ELNO","SIGM_ELNO","FORC_NODA",),),),
         GROUP_NO        =SIMP(statut='f',typ=grno,validators=NoRepeat(),max='**'),
         NOEUD           =SIMP(statut='f',typ=no  ,validators=NoRepeat(),max='**'),
         GROUP_MA        =SIMP(statut='f',typ=grma,validators=NoRepeat(),max='**'),
         MAILLE          =SIMP(statut='f',typ=ma  ,validators=NoRepeat(),max='**'),
         ACCE_MONO_APPUI =SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster,formule)),
         DIRECTION       =SIMP(statut='f',typ='R',min=3,max=3 ),
         TITRE           =SIMP(statut='f',typ='TXM',max='**' ),  
)  ;
