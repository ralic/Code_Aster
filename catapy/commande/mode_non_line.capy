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
MODE_NON_LINE=OPER(nom="MODE_NON_LINE",op=  61,sd_prod=table_container,
                     fr=tr("Calcul des modes non-linéaires"),
                     reentrant='f',
           UIinfo={"groupes":("Résolution","Dynamique",)},

        reuse =SIMP(statut='f',typ='table_container'),

        ETAT_INIT       =FACT(statut='o',max=1,
                regles=( UN_PARMI('MODE_LINE','MODE_NON_LINE'),),
                       MODE_LINE = SIMP(statut='f',typ=mode_meca,max = 1),
                       MODE_NON_LINE = SIMP(statut='f',typ=table_container,max = 1),
                       NUME_ORDRE = SIMP(statut='o',typ='I' ),
                       DIR_EVOLUTION    = SIMP(statut='f',typ='I',defaut=-1, into=(-1,1)),
                       COEF_AMPL = SIMP(statut='f',typ='R',defaut=1,),
                  ),

        CHOC = FACT(statut='f',max='**',
                regles=( UN_PARMI('NOEUD','GROUP_NO'),),
                    OBSTACLE = SIMP(statut='f',typ='TXM', into=("PLAN","BI_PLAN","CERCLE",)),
                    b_cercle = BLOC(condition="OBSTACLE=='CERCLE'",
                                     NOM_CMP = SIMP(statut='o',typ='TXM',min=2,max=2,validators=NoRepeat(),
                                                    into=('DX','DY','DZ'),),
                                     ORIG_OBST = SIMP(statut='f',typ='R',defaut=(0.,0.,0.),min=3,max=3),
                                     ),
                    b_bi_plan = BLOC(condition="OBSTACLE=='BI_PLAN'",
                                   NOM_CMP = SIMP(statut='o',typ='TXM',min=1,max=1,into=('DX','DY','DZ'),),
                                   ),
                    b_plan = BLOC(condition="OBSTACLE=='PLAN'",
                                   NOM_CMP = SIMP(statut='o',typ='TXM',min=1,max=1,into=('DX','DY','DZ'),),
                                    ),
                    NOEUD = SIMP(statut='f', typ=no, max=1),
                    GROUP_NO = SIMP(statut='f', typ=grno, max=1),
                    JEU = SIMP(statut='o',typ='R',max=1 ),
                    RIGI_NOR = SIMP(statut='o',typ='R',max=1 ),
                    PARA_REGUL = SIMP(statut='f',typ='R',defaut=0.005 ),
                ),

        MATR_RIGI = SIMP(statut='o',typ=(matr_asse_depl_r,) ),
        MATR_MASS = SIMP(statut='o',typ=(matr_asse_depl_r,) ),

        RESOLUTION = FACT(statut='o',max=1,
                       METHODE = SIMP(statut='f',typ='TXM',defaut="EHMAN",into=("EHMAN",)),
                       b_ehman = BLOC(condition="METHODE=='EHMAN'",
                                    NB_HARM_LINE = SIMP(statut='o',typ='I',val_min=1,),
                                    NB_HARM_NONL = SIMP(statut='f',typ='I',defaut=201,val_min=1,),
                                    NB_BRANCHE = SIMP(statut='o',typ='I',val_min=0),
                                    NB_PAS_MAN = SIMP(statut='o',typ='I',val_min=1),
                                    NB_ORDRE_MAN = SIMP(statut='f',typ='I',defaut=20,val_min=2),
                                    PREC_MAN = SIMP(statut='f',typ='R',defaut=1.E-9,val_min=0.E+0),
                                    PREC_NEWTON = SIMP(statut='f',typ='R',defaut=1.E-8,val_min=0.E+0),
                                    ITER_NEWTON_MAXI = SIMP(statut='f',typ='I',defaut=15,val_min=1),
                                    CRIT_ORDR_BIFURCATION = SIMP(statut='f',typ='I',defaut=3,val_min=1),
                                    RESI_RELA_BIFURCATION = SIMP(statut='f',typ='R',defaut=1.E-4,val_min=0.E+0),
                                   ),
                       ),

        SOLVEUR = C_SOLVEUR('MODE_NON_LINE'),
        
        INFO = SIMP(statut='f',typ='I',defaut=1),

)  ;
