#@ MODIF czm_exp Comportement  DATE 16/09/2008   AUTEUR PROIX J-M.PROIX 
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

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'CZM_EXP',
   doc = """Loi cohésive exponentielle  de type Barenblatt - R7.02.11""",
   num_lc         = 12,
   nb_vari        = 7,
   nom_vari       = ('SAUT_N', 'SAUT_T1', 'SEUIL_DEP','INDIENDO', 'PCENERDI', 'SIGM_N','SIGM_T1'),
   mc_mater       = ('RUPT_FRAG'),
   modelisation   = ('3D','PLAN','AXIS','ELEMJOIN'),
   deformation    = ('PETIT'),
   nom_varc       = ('TEMP'),
   schema         = ('IMPLICITE'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ('PRED_ELAS'),
)

