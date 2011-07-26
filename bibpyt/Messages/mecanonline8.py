#@ MODIF mecanonline8 Messages  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
  Statistiques sur tout le transitoire.
    * Nombre de pas de temps                      : %(i1)d 
    * Nombre d'itérations de Newton               : %(i2)d 
    * Nombre de factorisations de la matrice      : %(i3)d 
    * Nombre d'intégrations de la LDC             : %(i4)d
    * Nombre de résolutions K.U=F                 : %(i5)d
"""),

2 : _("""
    * Nombre d'itérations de recherche linéaire   : %(i1)d 
"""),

3 : _("""
    * Nombre d'itérations du solveur FETI         : %(i1)d 
"""),

10 : _("""
    * Nombre d'itérations de résolution contact   : %(i1)d
"""),

11 : _("""
    * Nombre d'appariements contact               : %(i1)d
"""),

22 : _("""
    * Nombre de boucles de frottement             : %(i1)d
"""),

23 : _("""
    * Nombre de boucles de contact                : %(i1)d
"""),

30 :_("""
  Statistiques du contact sur tout le transitoire.
"""),

31 : _("""
    * Nombre de liaisons de contact               : %(i1)d 
"""),

32 : _("""
    * Nombre de liaisons de frottement            : %(i1)d 
"""),

50 : _("""
  Temps CPU consommé dans le transitoire          : %(k1)s.
"""),

51 : _("""
    * Temps assemblage matrice                    : %(k1)s
"""),

52 : _("""
    * Temps construction second membre            : %(k1)s
"""),

53 : _("""
    * Temps total factorisation matrice           : %(k1)s
"""),

54 : _("""
    * Temps total intégration LDC                 : %(k1)s
"""),

55 : _("""
    * Temps total résolution K.U=F                : %(k1)s
"""),

56 : _("""
    * Temps résolution contact                    : %(k1)s 
"""),

57 : _("""
    * Temps construction matrices contact         : %(k1)s
"""),

58 : _("""
    * Temps préparation données contact           : %(k1)s
"""),

59 : _("""
    * Temps frottement                            : %(k1)s
"""),

60 : _("""
    * Temps contact                               : %(k1)s 
"""),

61 : _("""
    * Temps appariement contact                   : %(k1)s 
"""),

62 : _("""
    * Temps post-traitement (flambement)          : %(k1)s
"""),

63 : _("""
    * Temps autres opérations                     : %(k1)s
"""),

}


