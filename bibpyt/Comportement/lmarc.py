#@ MODIF lmarc Comportement  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
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
# RESPONSABLE PARROT A.PARROT

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'LMARC',
   doc = """Modele phénoménologique développé au LMA-RC de Besancon pour obtenir une description fine du comportement
   des tubes de gaine en Zircaloy du crayon combustible des centrales REP qui présentent un comportement mécanique 
   anisotrope et fortement visqueux [R5.03.10]""",
   num_lc         = 30,
   nb_vari        = 20,
   nom_vari       = ('X_XX','X_YY','X_ZZ','X_XY','X_XZ','X_YZ',  'X1_XX','X1_YY','X1_ZZ','X1_XY','X1_XZ','X1_YZ',  'X2_XX','X2_YY','X2_ZZ','X2_XY','X2_XZ','X2_YZ','EPSPEQ','INDIPLAS'),
   mc_mater       = ('ELAS','LMARC'),
   modelisation   = ('3D', 'AXIS', 'D_PLAN'),
   deformation    = ('PETIT', 'PETIT_REAC', 'GROT_GDEP'),
   nom_varc       = ('TEMP'),
   schema         = ('IMPLICITE',),
   type_matr_tang = ('PERTURBATION', 'VERIFICATION'),
   proprietes     = None,
)

