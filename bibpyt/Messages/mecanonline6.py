#@ MODIF mecanonline6 Messages  DATE 31/05/2011   AUTEUR GENIAUT S.GENIAUT 
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
 Instant de calcul: %(r1)19.12e 
"""),

2 : _("""
 Post-traitement: calcul d'un mode de flambement
"""),

3 : _("""
 Post-traitement: calcul d'un mode vibratoire
"""),

4 : _("""
 La gestion automatique du pas de temps (DEFI_LIST_INST/METHODE='AUTO') 
 avec le schéma IMPLEX (DEFI_LIST_INST/MODE_CALCUL_TPLUS='IMPLEX') nécessite 
 de traiter la résolution par la méthode IMPLEX (STAT/DYNA_NON_LINE/METHODE='IMPL_EX').
 Conseil :
 - Choisissez STAT/DYNA_NON_LINE/METHODE='IMPL_EX'
 - ou bien choisissez un autre schéma d'adaptation du pas de temps (DEFI_LIST_INST/MODE_CALCUL_TPLUS).
"""),

10 : _("""
  Le mode vibratoire de numéro d'ordre %(i1)d a pour fréquence %(r1)19.12e
"""),

11 : _("""
  Le mode de flambement de numéro d'ordre %(i1)d a pour charge critique %(r1)19.12e
"""),

12 : _("""
  Temps CPU consommé dans ce pas de temps: %(k1)s.
    * Temps par itération de Newton     : %(k2)s ( %(i1)d itérations )
    * Temps total archivage             : %(k3)s
    * Temps total création numérotation : %(k4)s ( %(i2)d créations )
    * Temps total factorisation matrice : %(k5)s ( %(i3)d factorisations )
    * Temps total intégration LDC       : %(k6)s ( %(i4)d intégrations )
    * Temps total résolution K.U=F      : %(k7)s ( %(i5)d résolutions )
"""),

13 : _("""
    * Temps total résolution contact    : %(k1)s ( %(i1)d itérations )
"""),

14 : _("""
  Statistiques de résolution du contact discret dans ce pas de temps.
    * Nombre d'itérations de contact          : %(i1)d 
    * Nombre de réactualisations géométriques : %(i2)d 
    * Temps total consommé par l'appariement  : %(k1)s
    * Temps total consommé par la résolution  : %(k2)s
    * Nombre final de liaisons de contact     : %(i3)d 
"""),

15 : _("""
    * Nombre final de liaisons de frottement  : %(i1)d 
"""),

16 : _("""
  Statistiques de résolution du contact continu dans ce pas de temps.
    * Nombre d'itérations de contact                       : %(i1)d 
    * Nombre d'itérations de réactualisations géométriques : %(i2)d 
"""),

17 : _("""
  Statistiques sur tout le transitoire.
    * Nombre de pas de temps                      : %(i1)d 
    * Nombre d'itérations de Newton               : %(i2)d 
    * Nombre de création numérotation             : %(i3)d
    * Nombre de factorisation matrice             : %(i4)d 
    * Nombre d'intégration LDC                    : %(i5)d
    * Nombre de résolution K.U=F                  : %(i6)d
"""),

18 : _("""
    * Nombre d'itérations de recherche linéaire   : %(i1)d 
"""),

19 : _("""
    * Nombre d'itérations du solveur FETI         : %(i1)d 
"""),

20 : _("""
    * Nombre d'itérations de contact              : %(i1)d 
    * Nombre de réactualisations géométriques     : %(i2)d 
"""),

21 : _("""
    * Nombre d'itérations de frottement           : %(i1)d 
"""),

22 : _("""
    * Temps total création numérotation           : %(k1)s 
    * Temps total factorisation matrice           : %(k2)s 
    * Temps total intégration LDC                 : %(k3)s
    * Temps total résolution K.U=F                : %(k4)s 
"""),

23 : _("""
    * Temps total pour le contact (appariement)   : %(k1)s
    * Temps total pour le contact (résolution)    : %(k2)s
"""),

24 : _("""
  Subdivision du pas de temps en %(i1)d sous-pas
"""),

25 : _("""
  Critère(s) de convergence atteint(s)
"""),

26 : _("""
  Pas de critère(s) de convergence
"""),

27 : _("""
  Convergence forcée (mode ARRET='NON')
"""),

28 : _("""
  <*> Attention ! Convergence atteinte avec RESI_GLOB_MAXI valant %(r1)19.12e pour cause de chargement presque nul.
"""),

29 : _("""
 <Erreur> Echec dans l'intégration de la loi de comportement
"""),

30 : _("""
 <Erreur> Echec dans le pilotage
"""),

31 : _("""
 <Erreur> Le nombre maximum d'itérations est atteint
"""),

32 : _("""
 <Erreur> Echec dans le traitement du contact discret
"""),

33 : _("""
 <Erreur> La matrice de contact est singulière
"""),

34 : _("""
 <Erreur> La matrice du système est singulière
"""),

35 : _("""
 Vérifiez votre modèle ou essayez de subdiviser le pas de temps
"""),

36 : _("""
    Le résidu de type <%(k1)s> vaut %(r1)19.12e au noeud et degré de liberté <%(k2)s> 
"""),

37 : _("""
  Le pilotage a échoué. On recommence en utilisant la solution rejetée initialement.
 """),

38 : _("""
    * Nombre d'itérations de frottement                    : %(i1)d 
"""),

}
