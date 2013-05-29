# coding=utf-8

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

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ENDO_SCALAIRE',
   doc = """Comportement elastique-fragile, a endommagement scalaire, seuil elliptique et 
   ecrouissage isotrope lineaire negatif - R5.03.18""",
   num_lc         = 46,
   nb_vari        = 3,
   nom_vari       = ('ENDO', 'INDIENDO', 'ENDORIGI'),
   mc_mater       = ('ELAS', 'ENDO_SCALAIRE', 'NON_LOCAL'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN', 'GRADVARI'),
   deformation    = ('PETIT',),
   nom_varc       = ('TEMP',),
   algo_inte      = ('NEWTON',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
