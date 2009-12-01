#@ MODIF dis_gouj2e_plas Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE ANGLES J.ANGLES

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'DIS_GOUJ2E_PLAS',
   doc = """Relation de comportement élastoplastique des filets des goujons pour des elements discrets""",
   num_lc         = 9999,
   nb_vari        = 2,
   nom_vari       = ('DIS1','DIS2'),
   modelisation   = ('DIS_T','DIS_TR','2D_DIS_T','2D_DIS_TR'),
   deformation    = ('PETIT','PETIT_REAC', 'EULER_ALMANSI'),
   nom_varc       = None,
   schema         = ('IMPLICITE'),
   type_matr_tang = None,
   proprietes     = None,
)

