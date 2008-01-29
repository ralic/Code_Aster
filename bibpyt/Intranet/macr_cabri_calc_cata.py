#@ MODIF macr_cabri_calc_cata Intranet  DATE 28/01/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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

from Intranet.macr_cabri_calc_ops import macr_cabri_calc_ops
from Cata.cata import *
   
def macr_cabri_calc_prod(self,MODELE_THER,MODELE_MECA,CHAM_MATER,
                         CHAR_THER,CHAR_MECA,RESU_THER,**args):
  if MODELE_THER != None:
   self.type_sdprod(MODELE_THER,modele_sdaster)   
  if MODELE_MECA != None:
   self.type_sdprod(MODELE_MECA,modele_sdaster)  
  if RESU_THER != None:
   self.type_sdprod(RESU_THER,evol_ther)     
  if CHAM_MATER != None:
   self.type_sdprod(CHAM_MATER,cham_mater)     
  if CHAR_THER != None: 
    for m in CHAR_THER:
      self.type_sdprod(m['CHARGE'],char_ther)
  if CHAR_MECA != None: 
    for m in CHAR_MECA:
      self.type_sdprod(m['CHARGE'],char_meca)
  return evol_noli


MACR_CABRI_CALC=MACRO(nom="MACR_CABRI_CALC",
                      op=macr_cabri_calc_ops,
                      sd_prod=macr_cabri_calc_prod,
                      fr="Calcul thermo-mécanique d'une jonction boulonnée de tuyauterie",
                      reentrant='n',
                      UIinfo={"groupes":("Outils métier",)},
                      MAILLAGE   = SIMP(statut='o',typ=maillage_sdaster,),
                      AFFE_MATERIAU = FACT(statut='o',max='**',
                        regles=(UN_PARMI('TOUT','GROUP_MA',),),
                        TOUT     = SIMP(statut='f',typ='TXM',into=("OUI",) ),
                        GROUP_MA = SIMP(statut='f',typ='TXM',into=(
                                                                  "BRIDE",
                                                                  "GOUJON",
                                                                  "ROND",
                                                                  "ECROU",
                                                                  "JOINT",) ),
                        MATER    = SIMP(statut='o',typ=mater_sdaster),
                        TEMP_REF = SIMP(statut='d',typ='R',defaut= 25. ),
                      ),                      
                      CHAM_MATER = SIMP(statut = 'f',typ=CO,),
                      MODELE_THER= SIMP(statut = 'f',typ=CO,),
                      
                      DEFI_CHAR_THER = FACT(statut ='d',
                        TEMP_INIT     = SIMP(statut='d',typ='R',defaut= 25.,),
                        COEF_H_FLUI   = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        TEMP_EXT_FLUI = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        COEF_H_AIR    = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        TEMP_EXT_AIR  = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        LIST_INST     = SIMP(statut='f',typ=listr8_sdaster),
                      ),                      
                      
                      CHAR_THER  = FACT(statut = 'f',max=4,
                        CHARGE    = SIMP(statut='o',typ=CO),
                        TYPE      = SIMP(statut='o',typ='TXM',
                                 into=("BRIDE_FLUIDE","BRIDE_AIR","ECROU_GOUJON",
                                       "BRIDE_JOINT"),)
                                       ),

                      RESU_THER  = SIMP(statut = 'f',typ=CO,),                                       

                                       
                      MODELE_MECA= SIMP(statut = 'f',typ=CO,),

                      DEFI_CHAR_MECA   = FACT(statut='o',
                        PRETENS    = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        PRES_REP   = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                        EFFE_FOND  = SIMP(statut='f',typ=(fonction_sdaster,nappe_sdaster),),
                      ),                                                             

                      CHAR_MECA  = FACT(statut = 'f',max=11,
                        CHARGE    = SIMP(statut='o',typ=CO),
                        TYPE      = SIMP(statut='o',typ='TXM',
                                 into=("BLOC_BAS_GOUJ","BLOC_BAS_JOINT",
                                       "BLOC_LAT_ALES","BLOC_LAT_NALES",
                                       "PLAN_TUBE",
                                       "PRES_FLU","EFFET_FOND",
                                       "CONT_JOINT",
                                       "DEFO_THER",
                                       "SERR_ECROU_1","SERR_ECROU_2",),)
                                       ),
                     
                      RELATION = SIMP(statut='f',typ='TXM',
                                       into=('VMIS_ISOT_TRAC','ELAS','ELAS_VMIS_TRAC',),),
                        
                      SOLVEUR   = FACT(statut='d',
                        METHODE  = SIMP(statut='d',typ='TXM',defaut="MULT_FRONT",into=("MULT_FRONT",) ),
                        b_mult_front = BLOC(condition = "METHODE == 'MULT_FRONT' ",
                           fr="Paramètres de la méthode multi frontale",
                           RENUM           = SIMP(statut='d',typ='TXM',defaut="METIS",into=("MD","MDA","METIS") ),
                           NPREC           = SIMP(statut='d',typ='I',defaut=8),
                           STOP_SINGULIER  = SIMP(statut='d',typ='TXM',defaut="OUI",into=("OUI","NON") ),
                           ),                 
                      ),                                             
                      INCREMENT = FACT(statut='f',
                        regles=(EXCLUS('NUME_INST_INIT','INST_INIT'),
                                EXCLUS('NUME_INST_FIN','INST_FIN'),),
                        LIST_INST       =SIMP(statut='f',typ=listr8_sdaster),
                        EVOLUTION       =SIMP(statut='f',typ='TXM',defaut="CHRONOLOGIQUE",
                                 into=("CHRONOLOGIQUE",) ),                                 
                        NUME_INST_INIT  =SIMP(statut='f',typ='I'),
                        INST_INIT       =SIMP(statut='f',typ='R'),
                        NUME_INST_FIN   =SIMP(statut='f',typ='I'),
                        INST_FIN        =SIMP(statut='f',typ='R'),
                        PRECISION       =SIMP(statut='f',typ='R',defaut=1.0E-3 ),
           # DEBUT DE BLOC POUR LA SUBDIVISION DES PAS DE TEMPS
           SUBD_METHODE    =SIMP( statut='f',typ='TXM',
              into =("AUCUNE","UNIFORME","EXTRAPOLE"),
              defaut="AUCUNE",
              fr="Méthode de subdivision des pas de temps en cas de non-convergence"
           ),
           b_subd_unif=BLOC(condition = "SUBD_METHODE == 'UNIFORME'",
             regles=(AU_MOINS_UN('SUBD_NIVEAU','SUBD_PAS_MINI'),),
             SUBD_COEF_PAS_1=SIMP(statut='f',typ='R',defaut=1.0,val_min=0.0,
                fr="Coefficient multiplicateur de la 1ère subdivision"),
             SUBD_PAS       =SIMP(statut='f',typ='I',defaut=4,val_min=2,
                fr="Nombre de subdivision d'un pas de temps"),
             SUBD_NIVEAU=SIMP(statut='f',typ='I',val_min=2,
                fr="Nombre maximum de niveau de subdivision d'un pas de temps"),
             SUBD_PAS_MINI=SIMP(statut='f',typ='R',val_min=0.0,
                fr="Pas de temps en dessous duquel on ne subdivise plus"),
           ),
           b_subd_extr=BLOC(condition = "SUBD_METHODE == 'EXTRAPOLE'",
             regles=(AU_MOINS_UN('SUBD_NIVEAU','SUBD_PAS_MINI'),),
             SUBD_OPTION    =SIMP(statut='f',typ='TXM',
                into =("IGNORE_PREMIERES","GARDE_DERNIERES",), 
                defaut="IGNORE_PREMIERES",
                fr="Technique d'extrapolation : les 1ere itérations sont ignorées ou les dernières sont gardées"),
             SUBD_ITER_IGNO =SIMP(statut='c',typ='I',defaut=3,val_min=0,
                fr="Les n premières itérations sont ignorées pour l'extrapolation"),
             SUBD_ITER_FIN  =SIMP(statut='c',typ='I',defaut=8,val_min=3,
                fr="Seules les n dernières itérations sont prises pour l'extrapolation"),
             SUBD_PAS       =SIMP(statut='c',typ='I',defaut=4,val_min=2,
                fr="Nombre de subdivision d'un pas de temps en cas divergence"),
             SUBD_NIVEAU=SIMP(statut='f',typ='I',val_min=2,
                fr="Nombre maximum de niveau de subdivision d'un pas de temps"),
             SUBD_PAS_MINI=SIMP(statut='f',typ='R',val_min=0.0,
                fr="Pas de temps en dessous duquel on ne subdivise plus"),
             SUBD_ITER_PLUS =SIMP(statut='c',typ='I',defaut=50,val_min=20,
                fr="% itération autorisée en plus"),
           ),
           # FIN DE BLOC POUR LA SUBDIVISION DES PAS DE TEMPS 
                        OPTI_LIST_INST  =SIMP(statut='f',typ='TXM',into=("INCR_MAXI",),),
                        NOM_CHAM        =SIMP(statut='f',typ='TXM',),
                        NOM_CMP         =SIMP(statut='f',typ='TXM',),
                        VALE            =SIMP(statut='f',typ='R'),
                      ),
                      NEWTON          =FACT(statut='d',
                        REAC_INCR       =SIMP(statut='f',typ='I',defaut= 1 ),
                        PREDICTION      =SIMP(statut='f',typ='TXM',into=("TANGENTE","ELASTIQUE","EXTRAPOL") ),
                        MATRICE         =SIMP(statut='f',typ='TXM',defaut="TANGENTE",into=("TANGENTE","ELASTIQUE") ),
                        PAS_MINI_ELAS   =SIMP(statut='f',typ='R',defaut=0.0E+0),
                        REAC_ITER       =SIMP(statut='f',typ='I',defaut=0),
                        EVOL_NOLI       =SIMP(statut='f',typ=evol_noli),
                      ),
                      RESO_INTE       =SIMP(statut='f',typ='TXM',defaut="IMPLICITE",
                                into=("IMPLICITE",)),
                      CONVERGENCE     =FACT(statut='d',
                        regles=(PRESENT_ABSENT('RESI_REFE_RELA','RESI_GLOB_MAXI','RESI_GLOB_RELA'),),
                        SIGM_REFE       =SIMP(statut='f',typ='R'),
                        EPSI_REFE       =SIMP(statut='f',typ='R'),
                        FLUX_THER_REFE  =SIMP(statut='f',typ='R'),        
                        RESI_REFE_RELA  =SIMP(statut='f',typ='R'),
                        RESI_GLOB_MAXI  =SIMP(statut='f',typ='R'),
                        RESI_GLOB_RELA  =SIMP(statut='f',typ='R'),
                        ITER_GLOB_MAXI  =SIMP(statut='f',typ='I',defaut=10),
                        ITER_GLOB_ELAS  =SIMP(statut='f',typ='I',defaut=25),
                      ),
                     );
