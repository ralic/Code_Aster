# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {


8 : _(u"""
 élément non traité
"""),

9 : _(u"""
 Vous avez choisie une méthode à pas de temps adaptatif. Il n'est pas possible de prendre en compte une
 liste d'instants de calcul définie a priori.
"""),

10 : _(u"""
  -> Contact avec DYNA_TRAN_MODAL : Il y a interpénétration d'une valeur supérieure à (DIST_MAIT + DIST_ESCL).
  -> Risque & Conseil :
     DIST_MAIT et DIST_ESCL permettent de tenir compte d'une épaisseur de matériau non représentée dans le maillage
     (rayon d'une poutre, épaisseur d'une coque ou simplement une bosse). Une trop forte interpénétration peut venir 
     d'une erreur dans le fichier de commande : RIGI_NOR trop faible ; noeuds de contact qui ne sont en vis à vis ; 
     OBSTACLE et NORM_OBSTACLE incohérents. Dans le cas de deux poutres aux fibres neutres confondues, elle peut 
     générer des erreurs dans l'orientation des forces de contact.
"""),

11 : _(u"""
 méthode à pas adaptatif : la donnée du pas est obligatoire 
"""),

12 : _(u"""
 le pas de temps ne peut pas être nul  
"""),

13 : _(u"""
 les matrices de masse élémentaires doivent obligatoirement avoir été calculées
 avec l'option MASS_MECA_DIAG
"""),

14 : _(u"""
 on archive au moins un champ.
"""),

15 : _(u"""
 La méthode d'intégration %(k1)s n'est pas disponible pour les analyses 
 transitoires sur base modale
"""),

16 : _(u"""
A l'instant %(r1)f, l'erreur vaut %(r2)f
Cette erreur est supérieure à 1.
Le pas de temps vaut %(r3)f
On arrête de le réduire, car le nombre de réductions a atteint %(i1)d, qui est le maximum possible.
"""),

17 : _(u"""
 méthode à pas adaptatif : pas de temps minimal atteint
"""),

18 : _(u"""
 La liste des instants de calcul ne doit contenir qu'un seul pas
 Conseil: si vous avez défini une liste d'instants manuellement par des valeurs discrètes,
 veillez à ce que le pas soit constant dans tout l'intervalle.
"""),

19 : _(u"""
 La méthode d'intégration %(k1)s n'est pas disponible pour les analyses 
 transitoires sur base physique
"""),

20 : _(u"""
 le chargement de type DIRICHLET nécessite la résolution par le schéma de NEWMARK
"""),

21 : _(u"""
Nombre de pas de calcul : %(i1)d
Nombre d'itérations     : %(i2)d
"""),

23 : _(u"""
 vous calculez une impédance absorbante
"""),

24 : _(u"""
 on n'a pas pu trouver le dernier instant sauvé.
"""),

25 : _(u"""
 le champ "DEPL" n'est pas trouvé dans le concept DYNA_TRANS  %(k1)s 
"""),

26 : _(u"""
 le champ "VITE" n'est pas trouvé dans le concept DYNA_TRANS  %(k1)s 
"""),

27 : _(u"""
 le champ "ACCE" n'est pas trouve dans le concept DYNA_TRANS  %(k1)s 
"""),

28 : _(u"""
 déplacements initiaux nuls.
"""),

29 : _(u"""
 vitesses initiales nulles.
"""),

36 : _(u"""
 NUME_INIT: on n'a pas trouvé le NUME_INIT dans le résultat  %(k1)s 
"""),

37 : _(u"""
 incohérence sur H, ALPHA, ELAS
"""),

40 : _(u"""
 le NOM_CHAM  %(k1)s n'appartient pas à la structure de données
"""),

41 : _(u"""
 erreur(s) dans les données
"""),

42 : _(u"""
 critère inconnu :  %(k1)s 
"""),

43 :_(u"""
 <DPMAT2> PLAS=2
"""),

55 : _(u"""
 ITER_INTE_MAXI insuffisant
"""),

56 : _(u"""
 la durée du transitoire est limitée par les possibilités de la transformée de Fourier rapide 
"""),

57 : _(u"""
 la durée de la simulation temporelle est insuffisante pour le passage du transitoire
"""),

58 : _(u"""
 changement de signe de la vitesse --> on prend VITG0(I)
"""),

60 : _(u"""
 la matrice interspectrale possède un pivot nul.
"""),

61 : _(u"""
 option non prévue !
"""),

62 : _(u"""
 Problème 1 test spectre fi par ARPACK
"""),

63 : _(u"""
 Problème 2 test spectre fi par ARPACK
"""),

64 : _(u"""
 valeur de STOGI incohérente
"""),

65 : _(u"""
 en parallèle STOGI=OUI obligatoire pour l'instant
"""),

66 : _(u"""
 option de calcul incohérente
"""),

67 : _(u"""
 Problème division par zéro dans la construction du BETA
"""),

72 : _(u"""
 donnée erronée, multiplicité nulle
"""),

78 : _(u"""
 pas de discrétisation de l'interspectre non constant.
"""),

80 : _(u"""
 "NB_POIN" n est pas une puissance de 2
 on prend la puissance de 2 supérieure
"""),

81 : _(u"""
 coefficient de dispersion trop grand
 consulter la documentation d'utilisation
"""),

82 : _(u"""
 matrice moyenne non définie positive
"""),

83 : _(u"""
 le pas tend vers 0 ...
"""),

86 : _(u"""
 pas d'interpolation possible pour les fréquences.
"""),

87 : _(u"""
 dérivée de F nulle
"""),

88 : _(u"""
 GM négatif
"""),

89 : _(u"""
 valeurs propres non ordonnées :
 %(k1)s  %(k2)s  %(k3)s 
"""),

90 : _(u"""
 coefficients paraboliques pas compatibles
"""),

92 : _(u"""
 modélisations C_PLAN et 1D pas autorisées
"""),








}
