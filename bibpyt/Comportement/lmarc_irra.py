#@ MODIF lmarc_irra Comportement  DATE 04/01/2012   AUTEUR SELLENET N.SELLENET 
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
# RESPONSABLE FERNANDES R.FERNANDES

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'LMARC_IRRA',
   doc = """Relation de comportement de viscoplasticité du LMARC avec prise en compte de l'irradiation pour les assemblages combustibles.
   Le champ de fluence est défini par le mot-clé AFFE_VARC de la commande AFFE_MATERIAU. 
   Pour les poutres, le fluage n'a lieu que dans le sens axial de la poutre : dans les autres directions, le comportement est élastique.""",
   num_lc         = 30,
   nb_vari        = 21,
   nom_vari       = ('X_XX','X_YY','X_ZZ','X_XY','X_XZ','X_YZ',  'X1_XX','X1_YY','X1_ZZ','X1_XY','X1_XZ','X1_YZ',  'X2_XX','X2_YY','X2_ZZ','X2_XY','X2_XZ','X2_YZ','EPSPEQ','IRVECU','EPSGRD'),
   mc_mater       = ('ELAS','LMARC_IRRA'),
   modelisation   = ('POU_D_E','POU_D_T'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP','IRRA'),
   algo_inte         = ('NEWTON',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

