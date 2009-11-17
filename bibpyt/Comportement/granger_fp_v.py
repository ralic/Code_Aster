#@ MODIF granger_fp_v Comportement  DATE 16/11/2009   AUTEUR DURAND C.DURAND 
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
   nom            = 'GRANGER_FP_V',
   doc = """Comportement de fluage propre du beton avec prise en compte du phénomène de vieillissement R7.01.01""",
   num_lc         = 26,
   nb_vari        = 55,
   nom_vari       = ('VG1','VG2','VG3','VG4','VG5','VG6','VG7','VG8','VG9','VG10','VG11','VG12','VG13','VG14','VG15','VG16','VG17','VG18','VG19','VG20','VG21','VG22','VG23','VG24','VG25','VG26','VG27','VG28','VG29','VG30','VG31','VG32','VG33','VG34','VG35','VG36','VG37','VG38','VG39','VG40','VG41','VG42','VG43','VG44','VG45','VG46','VG47','VG48','VG49','VG50','VG51','VG52','VG53','VG54','VG55'),
   mc_mater       = ('ELAS', 'V_GRANGER_FP'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN','C_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = ('TEMP', 'SECH', 'HYDR'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

