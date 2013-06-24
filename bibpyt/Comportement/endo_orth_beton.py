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
   nom            = 'ENDO_ORTH_BETON',
   doc = """Relation de comportement anisotrope du béton avec endommagement [R7.01.09]. 
   Il s'agit d'une modélisation locale d'endommagement prenant en compte la refermeture des fissures.""",
   num_lc         = 7,
   nb_vari        = 7,
   nom_vari       = ('ENDOXX','ENDOYY','ENDOZZ','ENDOXY','ENDOXZ','ENDOYZ','ENDOCOMP'),
   mc_mater       = ('ELAS', 'ENDO_ORTH_BETON', 'NON_LOCAL'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN','GRADEPSI'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP', 'SECH', 'HYDR'),
   algo_inte         = ('NEWTON',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
