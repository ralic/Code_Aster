#@ MODIF glrc_dm Comportement  DATE 07/12/2010   AUTEUR GENIAUT S.GENIAUT 
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
# RESPONSABLE FAYOLLE S.FAYOLLE

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'GLRC_DM',
   doc = """Ce modèle global permet de représenter l'endommagement d'une plaque en béton armé pour des sollicitations modérées. 
   Contrairement aux modélisations locales où chaque constituant du matériau est modélisé à part, dans les modèles globaux, 
   la loi de comportement s'écrit directement en terme de contraintes et de déformations généralisées. 
   La modélisation jusqu'à la rupture n'est pas recommandée, puisque les phénomènes de plastification ne sont pas 
   pris en compte, mais le sont dans GLRC_DAMAGE. En revanche, la modélisation du couplage de l'endommagement entre les effets 
   de membrane et de flexion dans GLRC_DM est pris en compte, ce qui n'est pas le cas dans GLRC_DAMAGE. 
   Pour les précisions sur la formulation du modèle voir [R7.01.32]""",
   num_lc         = 9999,
   nb_vari        = 4,
   nom_vari       = ('ENDOFL+','ENDOFL-','INDIEND1','INDIEND2'),
   mc_mater       = ('GLRC_DM'),
   modelisation   = ('DKTG'),
   deformation    = ('PETIT', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('NEWTON',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

