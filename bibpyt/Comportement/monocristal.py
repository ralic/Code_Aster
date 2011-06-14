#@ MODIF monocristal Comportement  DATE 14/06/2011   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE PROIX J.M.PROIX

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'MONOCRISTAL',
   doc = """Ce modèle permet de décrire le comportement d'un monocristal dont les relations de comportement 
            sont fournies via le concept compor, issu de DEFI_COMPOR.
            Le nombre de variables internes est fonction des choix effectués dans DEFI_COMPOR ; 
            pour plus de précisions consulter [R5.03.11].""",
   num_lc         = 32,
   nb_vari        = 0,
   nom_vari       = None,
   mc_mater       = None,
   modelisation   = ('3D','AXIS','D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'SIMO_MIEHE'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('NEWTON','NEWTON_RELI','RUNGE_KUTTA','NEWTON_PERT',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

