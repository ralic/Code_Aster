#@ MODIF gran_irra_log Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE FERNANDES R.FERNANDES

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'GRAN_IRRA_LOG',
   doc = """Relation de comportement de fluage et de grandissement sous irradiation pour les assemblages combustibles, 
   similaire à la loi VISC_IRRA_LOG pour la déformation viscoplastique, et intégrant en plus une déformation de grandissement 
   sous irradiation (cf. [R5.03.09]). Le champ de fluence est défini par le mot-clé AFFE_VARC de la commande AFFE_MATERIAU.
   Le grandissement ne se faisant que selon une direction, il est nécessaire dans les cas 3D et 2D de donner la direction du grandissement 
   par l'opérande ANGL_REP du mot clé MASSIF de l'opérateur AFFE_CARA_ELEM""",
   num_lc         = 28,
   nb_vari        = 1,
   nom_vari       = ('EPSPEQ'),
   mc_mater       = ('GRAN_IRRA_LOG'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN','CONT_1D'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = ('TEMP', 'IRRA'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

