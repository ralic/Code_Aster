#@ MODIF visc_taheri Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE TAHERI S.TAHERI

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'VISC_TAHERI',
   doc = """Relation de comportement (visco)-plastique de S.Taheri modélisant la réponse de matériaux sous chargement plastique cyclique, 
   et en particulier permettant de représenter les effets de rochet. 
   Les données nécessaires sont fournies dans l'opérateur DEFI_MATERIAU [U4.43.01], 
   sous les mots clés TAHERI(_FO) pour la description de l'écrouissage, LEMAITRE(_FO) pour la viscosité 
   et ELAS(_FO) (Cf. [R5.03.05] pour plus de détails). 
   En l'absence de LEMAITRE, la loi est purement élasto-plastique.""",
   num_lc         = 18,
   nb_vari        = 9,
   nom_vari       = ('DEFPLCUM', 'SIGMAPIC','E_XX','E_YY','E_ZZ','E_XY','E_XZ','E_YZ','INDICAT'),
   mc_mater       = ('ELAS', 'TAHERI', 'LEMAITRE'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = ('TEMP',),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

