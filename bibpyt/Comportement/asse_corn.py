#@ MODIF asse_corn Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE PEYRARD C.PEYRARD

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ASSE_CORN',
   doc = """Relation de comportement élasto-plastique isotherme pour les assemblages boulonnés de cornières de pylônes [R5.03.32]""",
   num_lc         = 9999,
   nb_vari        = 7,
   nom_vari       = ('V1','V2','V3','V4','V5','V6','V7'),
   mc_mater       = ('ASSE_CORN'),
   modelisation   = ('DIS_TR','DIS_T'),
   deformation    = ('PETIT','PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM'),
   nom_varc       = None,
   schema         = ('IMPLICITE'),
   type_matr_tang = None,
   proprietes     = None,
)

