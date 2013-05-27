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
# person_in_charge: sylvie.granet at edf.fr

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'HYDR_ENDO',
   doc = """Loi de comportement hydraulique, si le comportement mécanique est endommageant  
   (donc si on utilise 'MAZARS' ou 'ENDO_ISOT_BETON') sous RELATION_KIT. Ce mot clé permet de renseigner la 
   courbe de saturation et sa dérivée en fonction de la pression capillaire 
   ainsi que la perméabilité relative et sa dérivée en fonction de la saturation.""",
   num_lc         = 9999,
   nb_vari        = 1,
   nom_vari       = ('HYDREND1'),
   mc_mater       = None,
   modelisation   = ('KIT_HH','KIT_HHM','KIT_HM','KIT_THHM','KIT_THH','KIT_THM','KIT_THV'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   algo_inte         = 'SANS_OBJET',
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = ' ',
)
