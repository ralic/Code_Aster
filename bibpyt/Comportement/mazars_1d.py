# coding=utf-8
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
# person_in_charge: jean-luc.flejou at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'MAZARS_1D',
   doc = """Loi d'endommagement isotrope élastique-fragile du béton, suivant le modèle de Mazars.
   Permet de prendre en comtpe l'adoucissement. Distingue l'endommagement en traction et en compression.
   Deux variables d'endommagement scalaire sont utilisée (cf [R7.01.08]) pour faire la distinction entre
   l'endommagement de traction et de compression.
   """,
   num_lc         = 9999,
   nb_vari        = 5,
   nom_vari       = ('CRITELS','CRITELU','ENDO_T','ENDO_C','DISSIP'),
   mc_mater       = ('ELAS', 'MAZARS', ),
   modelisation   = ('1D'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP', 'SECH', 'HYDR'),
   algo_inte      = ('ANALYTIQUE',),
   type_matr_tang = None,
   proprietes     = None,
)
