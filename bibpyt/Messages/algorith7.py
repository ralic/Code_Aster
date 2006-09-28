#@ MODIF algorith7 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={

1: _("""
 couplage fluage/fissuration : il faut definir deux lois de comportement exactement. 
"""),

2: _("""
 granger et endo_isot_betonou mazars non encore developpe
"""),

3: _("""
 loi de comportement non autorisee dans le couplage fluage/fissuration
"""),

4: _("""
 umlv_fp et mazars non encore developpe
"""),

5: _("""
 pas de c_plan pour eib utiliser c_plan_deborst
"""),

6: _("""
 loi de fluage non autorisee dans le couplage fluage/fissuration
"""),

7: _("""
 pas d'orthotropie non line
"""),

8: _("""
 loi de comportement hyper-elastique non prevue
"""),

9: _("""
 c_plan methode de borstet simo_miehe incompatibles
"""),

10: _("""
 comp1det simo_miehe incompatibles
"""),

11: _("""
 couplage fluage/fissuration : la premiere loi doit etre une loi de fluage de type granger_fp ou granger_fp_v.
"""),

12: _("""
 couplage fluage/fissuration : nombre total de variables internes incoherent <--> erreur de programmation. 
"""),

13: _("""
 f(0)=  %(k1)s  > 0 
"""),

14: _("""
  le concept vari_comm :  %(k1)s  ne contient aucun champ d irradiation
"""),

15: _("""
  le concept evol_char :  %(k1)s  n'en est pas un !
"""),

16: _("""
  le concept evol_char :  %(k1)s  ne contient aucun champ de type evol_char.
"""),

17: _("""
  on ne trouve pas le resultat derive associe a  %(k1)s  et  %(k2)s 
"""),

18: _("""
  le concept evol_ther :  %(k1)s  ne contient aucun champ.
"""),

19: _("""
 erreur de type sur la charge thermique  %(k1)s 
"""),

20: _("""
 le champ de deplacement didi n'est pas trouve dans le concept  %(k1)s 
"""),

21: _("""
 critere de convergence lache
"""),

22: _("""
 la charge  %(k1)s  n'est pas mecanique
"""),

23: _("""
 la charge  %(k1)s  ne peut etre suiveuse
"""),

24: _("""
 la charge  %(k1)s  ne peut etre differentielle
"""),

25: _("""
 il y a plusieurs charges thermiques 
"""),

26: _("""
 le modele contient des elements de structure il faut renseigner le mot-cle cara_elem pour les options forc_noda et reac_noda.
"""),

27: _("""
 la charge  %(k1)s  ne peut etre pilotee
"""),

28: _("""
 on ne peut piloter une charge fonction du temps
"""),

29: _("""
 la charge thermique  %(k1)s  ne peut etre pilotee
"""),

30: _("""
 il y a plusieurs charges sechage 
"""),

31: _("""
 la charge de sechage  %(k1)s  ne peut etre pilotee
"""),

32: _("""
 il y a plusieurs charges defo.anelastiques 
"""),

33: _("""
 la charge defo.anelastiques  %(k1)s  ne peut etre pilotee
"""),

34: _("""
 la charge de type evol_char  %(k1)s  ne peut etre pilotee
"""),

35: _("""
 une meme charge ne peut contenir a la fois le mot-cle "liaison_unil" et le mot-cle "contact"
"""),

36: _("""
 la charge de type liaison_unilaterale  %(k1)s  ne peut etre pilotee
"""),

37: _("""
 la charge de type contact  %(k1)s  ne peut etre pilotee
"""),

38: _("""
 la charge  %(k1)s  ne peut pas utiliser de fonction multiplicative fonc_mult car elle est pilotee
"""),

39: _("""
 on ne peut pas faire de pilotage en l'absence de forces de type "fixe_pilo"
"""),

40: _("""
 il ne peut pas y avoir de contact (mot-cle "contact") dans plus d'une charge
"""),

41: _("""
 il y a au moins une charge non mecanique : verifier le fichier de commandes
"""),

42: _("""
  reac_incr negatif
"""),

43: _("""
  reac_iter negatif
"""),

44: _("""
  reac_iter_elas negatif
"""),

45: _("""
 il faut preciser un concept evol_noli en prediction 'depl_calcule'
"""),

46: _("""
 la definition des parametres rho_min et rho_excl est contradictoire, on prend rho_min a rho_excl
"""),

47: _("""
 les valeurs des parametres rho_max et rho_excl sont contradictoires, on prend rho_max a -rho_excl
"""),

48: _("""
 eta_pilo_max doit etre inferieur a eta_pilo_r_max
"""),

49: _("""
 eta_pilo_min doit etre superieur a eta_pilo_r_min
"""),

50: _("""
 il faut au plus 1 noeud pour le pilotage ddl_impo
"""),

51: _("""
 il faut au plus 1 groupe de noeud pour le pilotage ddl_impo
"""),

52: _("""
 il faut au plus un noeud dans le groupe pour le pilotage ddl_impo
"""),

53: _("""
 il faut preciser un groupe de noeuds dans la methode long_arc
"""),

54: _("""
 groupe de noeud  %(k1)s  vide
"""),

55: _("""
 liste de composantes vide pour la methode long_arc
"""),

56: _("""
 liste relation_kit vide
"""),

57: _("""
 liste relation_kit trop longue
"""),

58: _("""
 1d ou c_plan ?
"""),

59: _("""
 liste relation_kit trop
"""),

60: _("""
 critere de convergence pour integrer le comportement resi_inte_rela lache
"""),

61: _("""
 option  %(k1)s  non traitee
"""),

62: _("""
 matrice non inversible
"""),

63: _("""
 pas existance de solution                                                     pour le saut
"""),

64: _("""
 existance d'un element a                                         discontinuite trop grand                                          non unicite du saut
"""),

65: _("""
 non convergence du newton                                         pour le calcul du saut no1
"""),

66: _("""
 non convergence du newton                                         pour le calcul du saut no2
"""),

67: _("""
 non convergence du newton                                         pour le calcul du saut no3
"""),

68: _("""
 erreur dans le calcul du saut
"""),

69: _("""
 loi %(k1)s  non implantee pour les elemdisc 
"""),

70: _("""
 elements isoparametriques 2d non disponibles en grandes rotations
"""),

71: _("""
 elements isoparametriques 3d non disponibles en grandes rotations
"""),

72: _("""
 seule une loi de comportement elastique isotrope est valide pour le calcul de dg !
"""),

73: _("""
 le tenseur epseq=0 on a donc une derivee depseq tres grande !
"""),

74: _("""
  valeur de d_sigm_epsi non trouvee
"""),

75: _("""
  valeur de sy non trouvee
"""),

76: _("""
 dvp : non implante
"""),

77: _("""
 n=0
"""),

78: _("""
 feti et frottement incompatibles !
"""),

79: _("""
 n doit etre > 0
"""),

80: _("""
 phi_zero < ou = a zero
"""),

81: _("""
 1/k et l doivent etre >=0
"""),

82: _("""
 phi/kphi0+l=0 et beta<0
"""),

83: _("""
 f(0) > 0 : erreur de conception
"""),

84: _("""
 calcul flambement non lineaire hpp
"""),

85: _("""
 flambement non lineaire green
"""),

86: _("""
 fonctionnalite modifiee (dvlp)
"""),

87: _("""
 pour le traitement du contact avec x-fem, le solveur mumps est vivement recommande.
"""),

88: _("""
 contact et pilotage sont des fonctionnalites incompatibles
"""),

89: _("""
 contact et rech. lin. peuvent poser des problemes de convergence
"""),

90: _("""
 la combinaison: contact-frottement et solveur gcpc n'est pas disponible.
"""),

91: _("""
 contact continue et rech. lin. sont incompatibles
"""),

92: _("""
 contact continue et pilotage sont incompatibles
"""),

93: _("""
 la combinaison: methode continue en contact et solveur gcpc n'est pas disponible.
"""),

94: _("""
 liaison_unilater et pilotage sont des fonctionnalites incompatibles
"""),

95: _("""
 liaison_unilater et rech. lin. peuvent poser des problemes de convergence
"""),

96: _("""
 comportement zmat obligatoire
"""),

97: _("""
 il faut declarer lam_visc                           pour le fluage de dessication intrinseque
"""),

98: _("""
 il faut declarer fonc_desorp           sous elas_fo pour le fluage de dessication intrinseque            avec sech comme parametre
"""),

99: _("""
 mauvais dimensionnementde geomm et geomp
"""),
}
