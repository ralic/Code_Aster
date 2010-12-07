#@ MODIF czm_fat_mix Comportement  DATE 07/12/2010   AUTEUR GENIAUT S.GENIAUT 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE LAVERNE J.LAVERNE

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'CZM_FAT_MIX',
   doc = """Relation de comportement cohésive (Cohesive Zone Model FATigue MIXte) pour la fatigue (Cf. [R7.02.11]) modélisant l'ouverture et la 
   propagation d'une fissure sous chargement cyclique. Cette loi est utilisable avec l'élément fini d'interface basé sur une formulation mixte
   lagrangien augmenté (Cf. [R3.06.13]) """,
   num_lc         = 43,
   nb_vari        = 9,
   nom_vari       = ('SEUILDEP','INDIDISS','INDIENDO','PCENERDI','DISSIP','ENEL_RES','SAUT_N','SAUT_T1','SAUT_T2'),
   mc_mater       = ('RUPT_FRAG'),
   modelisation   = ('3D','PLAN','AXIS','INTERFAC'),
   deformation    = ('PETIT'),
   nom_varc       = None,
   algo_inte         = ('ANALYTIQUE'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ('PRED_ELAS'),
)
