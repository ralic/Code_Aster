# coding=utf-8
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
# person_in_charge: sylvie.michel at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'MAZARS',
   doc = """Loi d'endommagement isotrope élastique-fragile du béton, suivant le modèle de Mazars. 
   Elle permet de prendre en comtpe l'adoucissement et distingue l'endommagemetn en traction et en compression. 
   Une seule variable d'endommagement scalaire est utilisée (cf [R7.01.08]). 
   En cas de chargement thermique, les coefficients matériau dépendent de la température maximale atteinte au point de Gauss considéré, 
   et la dilatation thermique, supposée linéaire, ne contribue pas à l'évolution de l'endommagement.""",
   num_lc         = 8,
   nb_vari        = 4,
   nom_vari       = ('ENDO', 'INDIENDO','TEMP_MAX','EPSEQ'),
   mc_mater       = ('ELAS', 'MAZARS', 'NON_LOCAL'),
   modelisation   = ('3D', 'AXIS', 'C_PLAN', 'D_PLAN','GRADEPSI',),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP', 'SECH', 'HYDR'),
   algo_inte         = ('ANALYTIQUE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
