#@ MODIF letk Comportement  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE ELGHARIB J.ELGHARIB

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'LETK',
   doc = """Relation de comportement pour la modélisation élasto visco plastique des roches suivant le modèle de Laigle et Kleine, cf. [R7.01.24].
   L'opérateur tangent n'étant pas validé, il est possible d'utiliser la matrice de perturbation sous le mot clé TYPE_MATR_TANG.
   L'opérateur relatif à la prédiction élastique est celui de l'élasticité non linéaire spécifique à la loi.""",
   num_lc         = 35,
   nb_vari        = 7,
   nom_vari       = ('EPSPEQ','EPSPDEV','EPSVPEQ','EPSVPDEV','INDICDIL','INDIVISC','INDIPLAS'),
   mc_mater       = ('ELAS','LETK'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN', 'THM'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   schema         = 'IMPLICITE',
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ' ',
)

