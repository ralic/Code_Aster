#@ MODIF kit_thm Comportement  DATE 06/04/2009   AUTEUR DURAND C.DURAND 
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
# RESPONSABLE GRANET S.GRANET

from cata_comportement import LoiComportement

loi = LoiComportement(
   nom            = 'KIT_THM',
   doc = """KIT associé au comportement des milieux poreux (modélisations thermo-hydro-mécanique).
   Pour plus de détails sur les modélisations thermo-hydro-mécaniques et les modèles de comportement, 
   on pourra consulter les documents [R7.01.10] et [R7.01.11], ainsi que la notice d'utilisation [U2.04.05].
   Les relations KIT_XXXX permettent de résoudre simultanément de deux à quatre équations d'équilibre. 
   Les équations considérées dépendent du suffixe XXXX avec la règle suivante :
   - M désigne l'équation d'équilibre mécanique,
   - T désigne l'équation d'équilibre thermique,
   - H désigne une équation d'équilibre hydraulique.
   - V désigne la présence d'une phase sous forme vapeur (en plus du liquide)
   Les problèmes thermo-hydro-mécaniques associés sont traités de facon totalement couplée.
   Une seule lettre H signifie que le milieu poreux est saturé (une seule variable de pression p), 
   par exemple soit de gaz, soit de liquide, soit d'un mélange liquide/gaz (dont la pression du gaz est constante).
   Deux lettres H signifient que le milieu poreux est non saturé (deux variables de pression p), par exemple 
   un mélange liquide/vapeur/gaz. La présence des deux lettres HV signifie que le milieu poreux est saturé par 
   un composant (en pratique de l'eau), mais que ce composant peut être sous forme liquide ou vapeur. 
   Il n'y a alors qu'une équation de conservation de ce composant, donc un seul degré de liberté pression, 
   mais il y a un flux liquide et un flux vapeur.
   """,
   num_lc         = 9999,
   nb_vari        = 0,
   nom_vari       = None, # depend des modeles de comportement 
   mc_mater       = None,
   modelisation   = ('D_PLAN_THM','D_PLAN_THMS','D_PLAN_THMD','AXIS_THM','AXIS_THMS','AXIS_THMD','3D_THM','3D_THMS','3D_THMD'),
   deformation    = ('PETIT', 'PETIT_REAC', 'EULER_ALMANSI','REAC_GEOM', 'GREEN','GREEN_GR'),
   nom_varc       = None,
   schema         = ('IMPLICITE'),
   type_matr_tang = None,
   proprietes     = None,
)

