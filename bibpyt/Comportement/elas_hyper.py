#@ MODIF elas_hyper Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE ABBAS M.ABBAS

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ELAS_HYPER',
   doc = """Relation de comportement hyper-élastique généralisant le modèle de Mooney-Rivlin généralisé
            Sous sa version incrémentale (COMP_INCR) : elle permet de prendre en compte des déplacements 
            et contraintes initiaux donnés sous le mot clé ETAT_INIT. 
            Cette relation n'est supportée qu'en grandes déformations (DEFORMATION='GREEN') cf.[R5.03.23]. """,
   num_lc         = 19,
   nb_vari        = 1,
   nom_vari       = ('VIDE'),
   mc_mater       = ('ELAS_HYPER'),
   modelisation   = ('3D', 'C_PLAN', 'D_PLAN'),
   deformation    = ('GREEN','GREEN_GR'),
   nom_varc       = ('TEMP'),
   schema         = 'IMPLICITE',
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ' ',
)

