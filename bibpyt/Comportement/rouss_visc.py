# coding=utf-8
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
# person_in_charge: renaud.bargellini at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'ROUSS_VISC',
   doc = """Relation de comportement élasto-visco-plastique de G.Rousselier, en petites déformations.
   Elle permet de rendre compte de la croissance des cavités et de décrire la rupture ductile.
   Pour faciliter l'intégration de ce modèle, il est conseillé d'utiliser le redécoupage automatique local du pas de temps (ITER_INTE_PAS). 
   Pour l'intégration de cette loi, une theta-méthode est disponible et on conseille d'utiliser une intégration semi-NEWTON_1D c'est-à-dire : PARM_THETA = 0.5.""",
   num_lc         = 30,
   nb_vari        = 5,
   nom_vari       = ('EPSPEQ','POROSITE','DISSIP','EBLOC','INDIPLAS'),
   mc_mater       = ('ELAS','ROUSSELIER','VISC_SINH'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   algo_inte         = ('NEWTON_1D',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
