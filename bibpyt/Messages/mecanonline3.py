#@ MODIF mecanonline3 Messages  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

cata_msg = {

1 : _("""
 Il y a trop de colonnes à afficher dans le tableau de convergence.
 La largeur maximale affichable est de 256 caractères, donc 14 colonnes maximum.
 Or vous avez <%(i1)d> colonnes !
 Si vous avez des colonnes SUIVI_DDL, supprimez-en.
 Sinon, utiliser la commande AFFICHAGE pour choisir les colonnes à afficher.
"""),

2 : _("""
 Votre modèle contient des variables de commandes (température, irradiation, etc.)
 or on utilise une matrice élastique constante au cours du temps.
 Si vous faites de l'amortissement de Rayleigh, il y a un risque de résultats faux
 si l'amortissement dépend de cette variable de commande (via les coefficients élastiques).
 
 """),

70 : _("""
 macro_element statique et FETI incompatibles
"""),

71 : _("""
 chargement onde plane et FETI incompatibles
"""),

73 : _("""
 forces d'inertie et FETI incompatibles
"""),

75 : _("""
 forces d'inertie derivées et FETI incompatibles
"""),

78 : _("""
 FETI et contact discret incompatibles !
"""),

79 : _("""
 FETI et contact continu incompatibles !
"""),

89 : _("""
 contact et recherche linéaire peuvent poser des problèmes de convergence
"""),

90 : _("""
La combinaison: contact-frottement et solveur GCPC n'est pas disponible.
"""),

91 : _("""
Contact méthode continue et recherche linéaire sont incompatibles
"""),

92 : _("""
Contact méthode continue et pilotage sont incompatibles
"""),

93 : _("""
 la combinaison: méthode continue en contact et solveur GCPC n'est pas disponible.
"""),

94 : _("""
 LIAISON_UNILATER et PILOTAGE sont des fonctionnalités incompatibles
"""),

95 : _("""
 LIAISON_UNILATER et recherche linéaire peuvent poser des problèmes de convergence
"""),

96 : _("""
 la combinaison : LIAISON_UNILATER et solveur GCPC n'est pas disponible.
"""),
}
