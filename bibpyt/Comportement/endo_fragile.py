#@ MODIF endo_fragile Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE MICHEL S.MICHEL

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ENDO_FRAGILE',
   doc = """Comportement élastique-fragile, à endommagement scalaire et 
   écrouissage isotrope linéaire négatif - R5.03.18""",
   num_lc         = 5,
   nb_vari        = 2,
   nom_vari       = ('ENDO', 'INDICAT'),
   mc_mater       = ('ELAS', 'ECRO_LINE', 'NON_LOCAL'),
   modelisation   = ('3D', 'AXIS', 'C_PLAN', 'D_PLAN','GRADVARI','GRADEPSI'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = ('TEMP', 'SECH', 'HYDR'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

