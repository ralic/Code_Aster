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
# person_in_charge: nicolas.brie at edf.fr

def norm_mode_prod(MODE,**args ):
  if AsType(MODE) == mode_meca   : return mode_meca
  if AsType(MODE) == mode_meca_c : return mode_meca_c
  if AsType(MODE) == mode_flamb  : return mode_flamb
  raise AsException("type de concept resultat non prevu")

NORM_MODE=OPER(nom="NORM_MODE",op=  37,sd_prod=norm_mode_prod,
               fr=tr("Normer des modes propres en fonction d'un critère choisi par l'utilisateur"),
               reentrant='f',
            UIinfo={"groupes":("Résolution","Dynamique",)},
         regles=(UN_PARMI('NORME','GROUP_NO','NOEUD','AVEC_CMP','SANS_CMP'),),
         MODE       =SIMP(statut='o',typ=(mode_meca,mode_flamb) ),
         NORME      =SIMP(statut='f',typ='TXM',fr=tr("Norme prédéfinie : masse généralisée, euclidienne,..."),
                          into=("MASS_GENE","RIGI_GENE","EUCL","EUCL_TRAN","TRAN","TRAN_ROTA") ),
         NOEUD      =SIMP(statut='f',typ=no, fr=tr("Composante donnée d'un noeud spécifié égale à 1")),
         GROUP_NO   =SIMP(statut='f',typ=grno,fr=tr("Composante donnée d'un groupe contenant un seul noeud spécifié égale à 1")),
         b_noeud    =BLOC(condition = "NOEUD != None or GROUP_NO != None",
           NOM_CMP    =SIMP(statut='o',typ='TXM' ),
         ),
         AVEC_CMP   =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max='**'),
         SANS_CMP   =SIMP(statut='f',typ='TXM',validators=NoRepeat(),max='**'),
         MODE_SIGNE =FACT(statut='f',fr=tr("Imposer un signe sur une des composantes des modes"),
                  regles=(UN_PARMI('GROUP_NO','NOEUD'),),
           NOEUD      =SIMP(statut='f',typ=no,fr=tr("Noeud où sera imposé le signe")),
           GROUP_NO   =SIMP(statut='f',typ=grno,fr=tr("Groupe d'un seul noeud où sera imposé le signe")),
           NOM_CMP    =SIMP(statut='o',typ='TXM',fr=tr("Composante du noeud où sera imposé le signe") ),
           SIGNE      =SIMP(statut='f',typ='TXM',defaut="POSITIF",into=("NEGATIF","POSITIF"),
                            fr=tr("Choix du signe") ),
         ),

         MASSE = SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_gene_r,matr_asse_pres_r ), ),
         RAIDE = SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_depl_c,matr_asse_gene_r,matr_asse_pres_r ), ),
         AMOR  = SIMP(statut='f',typ=(matr_asse_depl_r,matr_asse_gene_r) ),
         TITRE      =SIMP(statut='f',typ='TXM',max='**'),
         INFO       =SIMP(statut='f',typ='I',defaut= 1,into=( 1 , 2) ),
)  ;
