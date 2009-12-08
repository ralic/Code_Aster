#@ MODIF irrad3m Comportement  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE FLEJOU J.L.FLEJOU

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'IRRAD3M',
   doc = """Relation de comportement élasto-plastique sous irradiation des aciers inoxydables 304 et 316, 
   matériaux dont sont constitués les structures internes de cuve des réacteurs nucléaires (cf. [R5.03.13]). 
   Le champ de fluence est défini par le mot-clé AFFE_VARC de la commande AFFE_MATERIAU. 
   Le modèle prend en compte la plasticité, le fluage sous irradiation, le gonflement sous flux neutronique.""",
   num_lc         = 30,
   nb_vari        = 5,
   nom_vari       = ('EPSPEQ','SEUIL','EPEQIRRA','GONF','INDIPLAS'),
   mc_mater       = ('ELAS','IRRAD3M'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN','C_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP','IRRA'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

