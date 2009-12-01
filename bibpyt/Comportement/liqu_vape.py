#@ MODIF liqu_vape Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE GRANET S.GRANET

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'LIQU_VAPE',
   doc = """Loi de comportement pour un milieux poreux saturé par un composant présent sous forme liquide ou vapeur
   avec changement de phase (Cf. [R7.01.11] pour plus de détails).""",
   num_lc         = 9999,
   nb_vari        = 3,
   nom_vari       = ('LIQVAP1','LIQVAP2','LIQVAP3'),
   mc_mater       = ('THM_LIQ','THM_VAPE'),
   modelisation   = ('KIT_HH','KIT_HHM','KIT_HM','KIT_THHM','KIT_THH','KIT_THM','KIT_THV'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = ('TEMP'),
   schema         = 'IMPLICITE',
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ' ',
)

