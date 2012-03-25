#@ MODIF hayhurst Comportement  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 

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

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'HAYHURST',
   doc = """Modele viscoplastique couple a l'endommagement isotrope de Kachanov.
   Ce modele s'emploie avec les mots cles DEFORMATION = PETIT ou PETIT_REAC ou GDEF_HYPO_ELAS ou GDEF_LOG.""",
   num_lc         = 32,
   nb_vari        = 12,
   nom_vari       = ('EPSVP_XX','EPSVP_YY','EPSVP_ZZ','EPSVP_XY','EPSVP_XZ','EPSVP_YZ','EPSPEQ','H1','H2','PHI','D','H'),
   mc_mater       = ('ELAS','HAYHURST'),
   modelisation   = ('3D','AXIS','D_PLAN'),
   deformation    = ('PETIT','PETIT_REAC','GDEF_HYPO_ELAS','GDEF_LOG'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('RUNGE_KUTTA'),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

