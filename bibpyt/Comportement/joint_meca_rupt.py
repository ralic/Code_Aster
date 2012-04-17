#@ MODIF joint_meca_rupt Comportement  DATE 16/04/2012   AUTEUR PROIX J-M.PROIX 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE KAZYMYRENKO

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'JOINT_MECA_RUPT',
   doc = """Relation de comportement de contact, elastique avec resistance a la traction et rupture 
             pour modéliser les joints dans les barrages. Cette loi permet également de modéliser le clavage de plots.
             Enfin elle permet de modéliser, avec les éléments de joint HM, un couplage entre 
             la mécanique et l'écoulement de fluide dans la fissure""",
   num_lc         = 45,
   nb_vari        = 18,
   nom_vari       = ('SEUILDEP','INDIDISS','INDIENDN','PCENERDI','INDIENDT','PCENDOT','SAUT_N','SAUT_T1','SAUT_T2', 'EPAISSJO','SIGN_GLO','GRADP_X','GRADP_Y','GRADP_Z','FH_X','FH_Y','FH_Z','PRESF'),
   mc_mater       = ('JOINT_MECA_RUPT'),
   modelisation   = ('3D','PLAN','AXIS','ELEMJOINT','EJ_HYME'),
   deformation    = ('PETIT'),
   nom_varc       = ('TEMP'),
   algo_inte      = ('ANALYTIQUE'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ('PRED_ELAS'),
)

