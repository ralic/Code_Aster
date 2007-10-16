#@ MODIF algorith7 Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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
 couplage fluage/fissuration :
 il faut définir deux lois de comportement exactement. 
"""),

2 : _("""
 GRANGER et ENDO_ISOT_BETON ou MAZARS non encore développé
"""),

3 : _("""
 loi de comportement non autorisée dans le couplage fluage/fissuration
"""),

4 : _("""
 UMLV_FP et MAZARS non encore developpé
"""),

5 : _("""
 pas de C_PLAN pour ENDO_ISOT_BETON
 utiliser C_PLAN_DEBORST
"""),

6 : _("""
 loi de fluage non autorisee dans le couplage fluage/fissuration
"""),

7 : _("""
 pas d'orthotropie non linéaire
"""),

8 : _("""
 loi de comportement hyper-élastique non prevue
"""),

9 : _("""
 C_PLAN méthode DEBORST et SIMO_MIEHE incompatibles
"""),

10 : _("""
 COMP1D et SIMO_MIEHE incompatibles
"""),

11 : _("""
 couplage fluage/fissuration :
 la première loi doit etre une loi de fluage de type GRANGER_FP ou GRANGER_FP_V.
"""),

12 : _("""
 couplage fluage/fissuration :
 nombre total de variables internes incohérent <--> erreur de programmation. 
"""),

13 : _("""
 f(0)=  %(k1)s  > 0 
"""),

15 : _("""
  le concept EVOL_CHAR :  %(k1)s  n'en est pas un !
"""),

16 : _("""
  le concept EVOL_CHAR :  %(k1)s  ne contient aucun champ de type EVOL_CHAR.
"""),

17 : _("""
  on ne trouve pas le résultat derivé associé à  %(k1)s  et  %(k2)s 
"""),

18 : _("""
  le concept EVOL_THER :  %(k1)s  ne contient aucun champ.
"""),

19 : _("""
 erreur de type sur la charge thermique  %(k1)s 
"""),

20 : _("""
 le champ de déplacement DIDI n'est pas trouvé dans le concept  %(k1)s 
"""),

21 : _("""
  -> Critère de convergence est lache !
  -> Risque & Conseil : La valeur de RESI_GLOB_RELA est supérieure a 10-4.
     Cela peut nuire à la qualité de la solution. Vous ne vérifiez pas l'équilibre de manière rigoureuse.
"""),

22 : _("""
 la charge  %(k1)s  n'est pas mécanique
"""),

23 : _("""
 la charge  %(k1)s  ne peut etre suiveuse
"""),

24 : _("""
 la charge  %(k1)s  ne peut etre différentielle
"""),

25 : _("""
 il y a plusieurs charges thermiques 
"""),

27 : _("""
 la charge  %(k1)s  ne peut etre pilotée
"""),

28 : _("""
 on ne peut piloter une charge fonction du temps
"""),

29 : _("""
 la charge thermique  %(k1)s  ne peut etre pilotée
"""),

30 : _("""
 il y a plusieurs charges de séchage 
"""),

31 : _("""
 la charge de séchage  %(k1)s  ne peut etre pilotée
"""),

32 : _("""
 il y a plusieurs charges de déformations anélastiques 
"""),

33 : _("""
 la charge de déformations anélastiques  %(k1)s  ne peut etre pilotée
"""),

34 : _("""
 la charge de type EVOL_CHAR  %(k1)s  ne peut etre pilotée
"""),

35 : _("""
 une meme charge ne peut contenir à la fois
 le mot-cle "LIAISON_UNIL" et le mot-cle "CONTACT"
"""),

36 : _("""
 la charge de type liaison_unilatérale  %(k1)s  ne peut etre pilotée
"""),

37 : _("""
 la charge de type contact  %(k1)s  ne peut etre pilotée
"""),

38 : _("""
 la charge  %(k1)s  ne peut pas utiliser de fonction multiplicative FONC_MULT
 car elle est pilotée
"""),

39 : _("""
 on ne peut pas faire de pilotage en l'absence de forces de type "FIXE_PILO"
"""),

40 : _("""
 il ne peut pas y avoir de contact (mot-cle "contact") dans plus d'une charge
"""),

41 : _("""
 il y a au moins une charge non mécanique : vérifier le fichier de commandes
"""),

42 : _("""
  REAC_INCR négatif
"""),

43 : _("""
  reac_iter negatif
"""),

44 : _("""
  REAC_ITER_ELAS négatif
"""),

45 : _("""
 il faut préciser un concept EVOL_NOLI en prédiction 'DEPL_CALCULE'
"""),

46 : _("""
  -> La définition des paramètres rho_min et rho_excl est contradictoire.
     On choisit de prendre RHO_MIN à RHO_EXCL.
  -> Risque & Conseil :
     RHO_MIN ne doit pas etre compris entre -RHO_EXCL et RHO_EXCL

"""),

47 : _("""
 les valeurs des paramètres RHO_MAX et RHO_EXCL sont contradictoires,
 on prend RHO_MAX à -RHO_EXCL
"""),

48 : _("""
 ETA_PILO_MAX doit etre inférieur a ETA_PILO_R_MAX
"""),

49 : _("""
 ETA_PILO_MIN doit etre supérieur à ETA_PILO_R_MIN
"""),

50 : _("""
 il faut au plus 1 noeud pour le pilotage DDL_IMPO
"""),

51 : _("""
 il faut au plus 1 groupe de noeud pour le pilotage DDL_IMPO
"""),

52 : _("""
 il faut au plus un noeud dans le groupe pour le pilotage DDL_IMPO
"""),

53 : _("""
 il faut préciser un groupe de noeuds dans la méthode LONG_ARC
"""),

54 : _("""
 groupe de noeud  %(k1)s  vide
"""),

55 : _("""
 liste de composantes vide pour la methode LONG_ARC
"""),

56 : _("""
 liste RELATION_KIT vide
"""),

57 : _("""
 liste RELATION_KIT trop longue
"""),

58 : _("""
 1D ou C_PLAN ?
"""),

59 : _("""
 liste RELATION_KIT trop
### trop quoi ?
"""),

60 : _("""
  -> Le critère de convergence pour intégrer le comportement 'RESI_INTE_RELA'
     est lache (très supérieur à la valeur par défaut).
  -> Risque & Conseil :
     Cela peut nuire à la qualité de la solution et à la convergence.
"""),

61 : _("""
 option  %(k1)s  non traitee
"""),

62 : _("""
 matrice non inversible
"""),

63 : _("""
 pas existence de solution pour le saut
"""),

64 : _("""
 existence d'un élément à discontinuité trop grand
 non unicité du saut
"""),

65 : _("""
 non convergence du NEWTON pour le calcul du saut numéro 1
"""),

66 : _("""
 non convergence du NEWTON pour le calcul du saut numéro 2
"""),

67 : _("""
 non convergence du NEWTON pour le calcul du saut numéro 3
"""),

68 : _("""
 erreur dans le calcul du saut
"""),

69 : _("""
 loi %(k1)s  non implantee pour les elemdisc 
"""),

70 : _("""
 elements isoparamétriques 2D non disponibles en grandes rotations
"""),

71 : _("""
 elements isoparamétriques 3D non disponibles en grandes rotations
"""),

72 : _("""
 seule une loi de comportement élastique isotrope est valide pour
 le calcul de DG
"""),

73 : _("""
 le tenseur EPSEQ=0 on a donc une derivée DEPSEQ très grande !
"""),

74 : _("""
  valeur de D_SIGM_EPSI non trouvée
"""),

75 : _("""
  valeur de SY non trouvée
"""),

76 : _("""
 développement non implanté
"""),

78 : _("""
 FETI et frottement incompatibles !
"""),

79 : _("""
 N doit etre > 0
"""),

80 : _("""
 PHI_ZERO < ou = à zéro
"""),

81 : _("""
 1/k et l doivent etre >=0
"""),

82 : _("""
 PHI/KPHI0+L=0 et BETA<0
"""),

83 : _("""
 F(0) > 0 : erreur de conception
"""),

84 : _("""
 calcul flambement non linéaire HPP
"""),

85 : _("""
 flambement non linéaire GREEN
"""),

86 : _("""
 fonctionnalite modifiée (dvlp)
"""),

89 : _("""
 contact et recherche linéaire peuvent poser des problèmes de convergence
"""),

90 : _("""
 la combinaison: contact-frottement et solveur GCPC n'est pas disponible.
"""),

91 : _("""
 contact méthode continue et recherche linéaire sont incompatibles
"""),

92 : _("""
 contact méthode continue et pilotage sont incompatibles
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
 comportement ZMAT obligatoire
"""),

97 : _("""
 il faut declarer lam_visc                           pour le fluage de dessication intrinseque
"""),

98 : _("""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage de dessication
 intrinseque avec SECH comme paramètre
"""),

99 : _("""
 mauvais dimensionnement de GEOMM et GEOMP
"""),

}
