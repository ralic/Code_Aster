#@ MODIF vmis_john_cook Comportement  DATE 16/04/2012   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE SFAYOLLE S.FAYOLLE

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'VMIS_JOHN_COOK',
   doc = """Loi de plasticité de Von Mises à écrouissage de Johnson-Cook [R5.03.02]""",
   num_lc         = 54,
   nb_vari        = 5,
   nom_vari       = ('EPSPEQ', 'INDIPLAS','DEPSPEQ','DINSTM','DDISSM',),
   mc_mater       = ('ECRO_COOK'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN',),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP', 'GDEF_LOG','GDEF_HYPO_ELAS','GREEN_REAC'),
   nom_varc       = ('TEMP',),
   algo_inte      = ('DEKKER',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)


