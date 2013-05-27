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
# person_in_charge: samuel.geniaut at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'VISCOCHAB',
   doc = """Modèle élastoviscoplastique de Lemaitre-Chaboche avec effet de mémoire et restauration.
   Ce modèle s'emploie avec les mots clés DEFORMATION = PETIT ou PETIT_REAC.""",
   num_lc         = 32,
   nb_vari        = 28,
   nom_vari       = ('VISCHA1','VISCHA2','VISCHA3','VISCHA4','VISCHA5','VISCHA6','VISCHA7','VISCHA8','VISCHA9','VISCHA10','VISCHA11','VISCHA12','VISCHA13','VISCHA14','VISCHA15','VISCHA16','VISCHA17','VISCHA18','VISCHA19','VISCHA20','VISCHA21','VISCHA22','VISCHA23','VISCHA24','VISCHA25','VISCHA26','VISCHA27','VISCHA28',),
   mc_mater       = ('ELAS','VISCOCHAB'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('NEWTON','NEWTON_RELI','RUNGE_KUTTA'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
