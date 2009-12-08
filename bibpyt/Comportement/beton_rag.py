#@ MODIF beton_rag Comportement  DATE 08/12/2009   AUTEUR DEBONNIERES P.DE-BONNIERES 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
#            CONFIGURATION MANAGEMENT OF EDF VERSION

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'BETON_RAG',
   doc = """Loi RAG pour le beton""",
   num_lc         = 44,
   nb_vari        = 65,
   nom_vari       = ('DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT','DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT',
                     'DPDEVCUM','DPVOLCUM','INDICAT','DPVOLCUM','INDICAT'),
   mc_mater       = ('ELAS', 'BETON_RAG'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC'),
   nom_varc       = ('TEMP',),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

