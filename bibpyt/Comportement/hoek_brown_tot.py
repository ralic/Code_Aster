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
# person_in_charge: sylvie.granet at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'HOEK_BROWN_TOT',
   doc = """Relation de comportement de Hoek et Brown modifiée pour la modélisation du comportement
   des roches [R7.01.18] pour la mécanique pure. Le couplage est formulé en contraintes totales.
   Pour faciliter l'intégration de ce modèle, 
   on peut utiliser le re-découpage local du pas de temps (ITER_INTE_PAS).""",
   num_lc         = 9999,
   nb_vari        = 3,
   nom_vari       = ('GAMMAECR','EPSPVOL','INDIPLAS'),
   mc_mater       = ('ELAS', 'HOEK_BROWN'),
   modelisation   = ('THM'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP',),
   algo_inte         = ('NEWTON_1D',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)
