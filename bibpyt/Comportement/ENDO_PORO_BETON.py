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
# person_in_charge: etienne grimal at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ENDO_PORO_BETON',
   doc            = """Loi RAG pour le beton""",
   num_lc         = 66,
   nb_vari        = 116,
   nom_vari       = ('HYD0','SSG1','SSG2','SSG3','SSG4','SSG5','SSG6','EPG1','EPG2','EPG3','EPG4','EPG5','EPG6','DG1','DG2','DG3',
'PWAT','PGEL','SSW1','SSW2','SSW3','SSW4','SSW5','SSW6','DW1','DW2','DW3','DTH','PAS0','SES1','SES2','SES3',
'SES4','SES5','SES6','SSP1','SSP2','SSP3','SSP4','SSP5','SSP6','DTP1','DTP2','DTP3','SSL1','SSL2','SSL3','SSL4',
'SSL5','SSL6','DTL1','DTL2','DTL3','SPL1','SPL2','SPL3','SPL4','SPL5','SPL6','WLM1','WLM2','WLM3','WLM4','WLM5',
'WLM6','WL1','WL2','WL3','SSC','DC','DV','XNL','MSRD','EVE1','EVE2','EVE3','EVE4','EVE5','EVE6','SVE1','SVE2',
'SVE3','SVE4','SVE5','SVE6','XRTW','TAUW','VVE1','VVE2','VVE3','VVE4','VVE5','VVE6','VMA1','VMA2','VMA3','VMA4',
'VMA5','VMA6','ERRM','XGFW','TOEQ','IRTW','TEQU','TORF','XRTT','VT00','VT11','VT21','VT31','VT12','VT22','VT32',
'VT13','VT23','VT33'),
   mc_mater       = ('ELAS', 'ENDO3D'),
   modelisation   = ('3D',),
   deformation    = ('PETIT', 'PETIT_REAC'),
   nom_varc       = ('TEMP','HYDR'),
   algo_inte      = ('SPECIFIQUE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

