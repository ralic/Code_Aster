#@ MODIF visc_irra_log Comportement  DATE 12/07/2011   AUTEUR FERNANDES R.FERNANDES 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
   nom            = 'VISC_IRRA_LOG',
   doc = """Loi de fluage axial sous irradiation des assemblages combustibles. 
   Elle permet de modéliser le fluage primaire et secondaire, paramétrés par la fluence neutronique (cf. [R5.03.09]).
   Le champ de fluence est défini par le mot-clé AFFE_VARC de la commande AFFE_MATERIAU.""",
   num_lc         = 28,
   nb_vari        = 2,
   nom_vari       = ('EPSPEQ','IRVECU'),
   mc_mater       = ('VISC_IRRA_LOG'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN','CONT_1D'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP', 'IRRA'),
   algo_inte         = ('ANALYTIQUE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

