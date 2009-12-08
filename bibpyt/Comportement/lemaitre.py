#@ MODIF lemaitre Comportement  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE DEBONNIERES P.DEBONNIERES

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'LEMAITRE',
   doc = """Relation de comportement visco-plastique non linéaire de Lemaitre (sans seuil), cf. [R5.03.08]. 
   Un cas particulier de cette relation (en annulant le paramètre UN_SUR_M) donne une relation de NORTON. 
   La correspondance des variables internes permet le chaînage avec un calcul utilisant un comportement 
   élasto-plastique avec écrouissage isotrope (VMIS_ISOT_LINE, VMIS_ISOT_TRAC, VMIS_ISOT_PUIS). 
   L'ntégration de ce modèle est réalisée par une méthode semi-implicite (PARM_THETA=0.5) ou implicite (PARM_THETA=1)""",
   num_lc         = 29,
   nb_vari        = 2,
   nom_vari       = ('EPSPEQ','VIDE'),
   mc_mater       = ('ELAS','LEMAITRE'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP','GDEF_HYPO_ELAS'),
   nom_varc       = ('TEMP'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

