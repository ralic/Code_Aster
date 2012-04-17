#@ MODIF norton_hoff Comportement  DATE 16/04/2012   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE MICHEL S.MICHEL

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'NORTON_HOFF',
   doc = """Loi de visco-plasticité  indépendante de la température, régularisant la loi rigide-plastique de Von Mises 
   à utiliser pour le calcul de charges limites de structures, à seuil de VON MISES. 
   Le seul paramètre matériau est la limite d'élasticité à renseigner dans l'opérateur DEFI_MATERIAU [U4.43.01] 
   sous le mot-clé ECRO_LINE (Cf. [R7.07.01] et [R5.03.12] pour plus de détails). 
   Pour le calcul de la charge limite, il existe un mot clé spécifique sous PILOTAGE pour ce modèle 
   (voir mot clé PILOTAGE='ANA_LIM' de STAT_NON_LINE [U4.51.03]). 
   Il est fortement conseillé d'employer de la recherche linéaire (voir mot clé RECH_LINEAIRE de STAT_NON_LINE [U4.51.03]). 
   En effet, le calcul de la charge limite requiert beaucoup d'itérations de recherche linéaire (de l'ordre de 50) 
   et d'itérations de Newton (de l'ordre de 50).""",
   num_lc         = 17,
   nb_vari        = 1,
   nom_vari       = ('VIDE',),
   mc_mater       = ('ECRO_LINE'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP',),
   algo_inte         = ('ANALYTIQUE',),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   nom_varc       = ('TEMP'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

