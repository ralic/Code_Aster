#@ MODIF vmis_cine_line Comportement  DATE 16/04/2012   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE PROIX J.M.PROIX

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'VMIS_CINE_LINE',
   doc = """Loi de Von Mises - Prager avec ecrouissage cinematique lineaire [R5.03.02]""",
   num_lc         = 2,
   nb_vari        = 7,
   nom_vari       = ('XCINXX', 'XCINYY', 'XCINZZ', 'XCINXY', 'XCINXZ', 'XCINYZ', 'INDIPLAS',),
   mc_mater       = ('ELAS', 'ECRO_LINE'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN', '1D'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP','GDEF_LOG','GDEF_HYPO_ELAS'),
   nom_varc       = ('TEMP',),
   algo_inte         = ('ANALYTIQUE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

