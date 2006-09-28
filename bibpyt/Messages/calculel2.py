#@ MODIF calculel2 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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

2: _("""
 le champ_s:  %(k1)s  est a la fois cham_elem_s et cham_no_s.
"""),

3: _("""
 le champ_s:  %(k1)s n'existe pas. 
"""),

4: _("""
 types scalaires(i/r/c/...) differents pour :  %(k1)s  et  %(k2)s 
"""),

5: _("""
 arret suite a l'alarme precedente.
"""),

6: _("""
 on ne sait pas renommer la cmp:  %(k1)s 
"""),

7: _("""
 trop d'antecedents. verifiez si le maillage de l'interface ne contient pas de noeuds coincidents ou  diminuez dist_refe.
"""),

8: _("""
  %(k1)s  valeurs de chamno de deplacement n'ont pas ete recopiees sur  %(k2)s  noeuds a affecter  ce qui peut entrainer des erreurs de calcul sur la masse ajoutee des sous structures deduites par rotation et translation definies dans le modele  generalise. augmentez dist_refe ou assurez vous de l' invariance du maillage de structure par la translation et la rotation definies dans le modele generalise.
"""),

9: _("""
 plus de 50 pour cent  des valeurs de chamno de deplacement n'ont pas ete recopiees  ce qui peut entrainer des erreurs graves de calcul sur la masse ajoutee des sous structures deduites par rotation et translation definies dans le modele  generalise. augmentez dist_refe!!!.
"""),

10: _("""
 trop de noeuds affectes
"""),

11: _("""
 seulement reel svp
"""),

12: _("""
 le cham_no est de longueur nulle.
"""),

13: _("""
 il manque la cmp: %(k1)s  sur le noeud: %(k2)s 
"""),

14: _("""
 ncmp doit etre >0
"""),

15: _("""
 grandeurs differentes.
"""),

16: _("""
 reel ou k8 svp
"""),

17: _("""
 nbno doit etre >=0
"""),

18: _("""
 arret suite a l'erreur precedente.
"""),

19: _("""
 maille non disponible
"""),

20: _("""
 maille indisponible
"""),

21: _("""
 grandeur :  %(k1)s  inexistante au catalogue
"""),

22: _("""
 composante :  %(k1)s  inexistante au catalogue pour la grandeur : %(k2)s 
"""),

23: _("""
 la grandeur : %(k1)s  n est pas de type reel.
"""),

24: _("""
 on traite un superelement  et le noeud courant n'est ni un noeud lagrange, ni un noeud physqiue du maillage.
"""),

25: _("""
 le ligrel :  %(k1)s  ne contient pas d elements finis
"""),

26: _("""
 l'option  %(k1)s  n'existe pas.
"""),

27: _("""
 le maillage associe au champ: %(k1)s  est different de celui associe au ligrel:  %(k2)s 
"""),

28: _("""
  erreur programmeur : appel a calcul, le champ: %(k1)s  est un champ "in" et un champ "out".
"""),

29: _("""
 la grandeur associee au champ  %(k1)s : %(k2)s  n est pas celle associee au parametre  %(k3)s : %(k4)s  (option: %(k5)s 
"""),

30: _("""
  on n'arrive pas a etendre la carte:  %(k1)s 
"""),

31: _("""
 maille  %(k1)s  indisponible
"""),

32: _("""
 probleme creation champ
"""),

33: _("""
 pour le modele  %(k1)s  on ne peut pas visualiser les champs ensemble  %(k2)s  ... car les familles de pg sont differentes
"""),

34: _("""
 bug
"""),

35: _("""
 aucun element du modele n'est visualisable avec ecla_pg.
"""),

36: _("""
 on ne trouve aucun point de gauss.
"""),

37: _("""
 le type de resu_init est different de celui du resultat.
"""),

38: _("""
 la liste de numeros d ordre est vide.
"""),

39: _("""
 les seuls champs autorises pour ecla_pg sont les champs reels.
"""),

40: _("""
 le champ:  %(k1)s  a des elements ayant des sous-points. ces elements ne seront pas traites.
"""),

41: _("""
 les seuls champs autorises sont elga.
"""),

42: _("""
 le type_elem:  %(k1)s  n'a pas le nombre de points de gauss declare dans la routine eclau1. nom_cham= %(k2)s 
"""),

43: _("""
 nombre de noeuds > 27 
"""),

44: _("""
 l impression des cham_no a  representation constante reste a faire!
"""),

45: _("""
 famille de pg "liste" interdite: %(k1)s 
"""),

46: _("""
  mode ligne  %(k1)s  /= mode colonne  %(k2)s 
"""),

47: _("""
  le mode  %(k1)s  de code  %(k2)s  reference le mode  %(k3)s  dont le code :  %(k4)s  > 3 
"""),

48: _("""
  pour le mode  %(k1)s  nombre de points  %(k2)s  < argument k :  %(k3)s 
"""),

49: _("""
 carte inexistante.
"""),

50: _("""
 3
"""),

51: _("""
 cham_elem etendu a faire ... 
"""),

52: _("""
 probleme noeud tardif pour un champ a representation constante
"""),

53: _("""
  inutile de comprimer 1 telle carte 
"""),

54: _("""
 on devrait avoir icode=3 pour des mailles tardives.
"""),

55: _("""
  erreur dans l'extraction d'un resuelem pour le grel:  %(k1)s le champ n'existe pas
"""),

56: _("""
  erreur lors d'une extraction: le champ associe au parametre :  %(k1)s  n'est pas dans la liste des champs parametres.
"""),

57: _("""
 sa21
"""),

58: _("""
 sa22
"""),

59: _("""
 sa23
"""),

60: _("""
 ******* erreur donnees *******
"""),

61: _("""
 l option que l on calcule ne connait pas le parametre :  %(k1)s erreur probable dans un catalogue(typelem)
"""),

62: _("""
  on ne trouve pas de routine ininpq npq doit etre compris entre 1 et 100 ici : npq = %(k1)s 
"""),

63: _("""
 la maille  %(k1)s  porte un element fini de bord. mais elle ne borde aucun element ayant une "rigidite". cela peut entrainer des problemes de "pivot nul" lors de la resolution.
"""),

64: _("""
 le modele  %(k1)s n'a pas d'elements calculant de la rigidite.)
"""),

65: _("""
 erreur programmeur 3
"""),

66: _("""
  type_scalaire inconnu:  %(k1)s 
"""),

67: _("""
 non programme.
"""),

68: _("""
 maille partiellement affectee.
"""),

69: _("""
 le parametre: %(k1)s  n'est pas un parametre de l'option: %(k2)s 
"""),

70: _("""
 le parametre: %(k1)s  n'est pas un parametre de l'option: %(k2)s  pour le type_element:  %(k3)s 
"""),

71: _("""
 erreur :on ne trouve pas dans les arguments de la routine calcul de champ a associer au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

72: _("""
 impossible...
"""),

73: _("""
 erreur :on n'a pas pu extraire toutes les cmps voulues du champ associe au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

74: _("""
 objet:  %(k1)s  introuvable.
"""),

75: _("""
 long=8,16 ou 24
"""),

76: _("""
 erreur pgmeur: lk3 pas assez grand.
"""),

77: _("""
 il manque .carcoque dans la sd cara_elem
"""),

78: _("""
 normale moyenne nulle : attention a l'orientation des mailles
"""),

79: _("""
 ce chargement n est pas prevu en lagrange
"""),

80: _("""
 le calcul lagrangien avec les temperatures n'est pas encore disponible
"""),

81: _("""
 pas de chgeom
"""),

82: _("""
 il faut un modele.
"""),

83: _("""
 il n'y a pas de rigidite sur le modele.
"""),

84: _("""
 il n'y a pas de masse sur le modele.
"""),

85: _("""
 g_bili : champ initial impossible
"""),

86: _("""
 compor svp!
"""),

87: _("""
 impossible lire  %(k1)s 
"""),

88: _("""
 option  %(k1)s  non disponible sur les elements du modele- pas de champ cree 
"""),

89: _("""
 le materiau depend de la temperature l'option calc_dg ne prend pas en compte cette possibilite
"""),

90: _("""
 le champ de theta sensibilite est inexistant dans la sd  %(k1)s  !
"""),

91: _("""
 exicha different de 0 et 1 
"""),

92: _("""
 votre chargement contient plus d'une charge repartie. le calcul n'est pas possible pour les modeles de poutre.
"""),

93: _("""
 vous avez renseigne un des mots-cles fonc_mult_*, coef_mult_*, phas_deg, puis_puls, or votre charge ne contient pas d'effort reparti sur des poutres. ces mots-cles seront donc ignores
"""),

94: _("""
 pour un modele comportant des elements de plaque ou de coque, il faut le "cara_elem"
"""),

95: _("""
 impossible de calculer un resultat derive pour le type  %(k1)s 
"""),

96: _("""
 impossible de trouver le resultat derive associe au resultat  %(k1)s  et au parametre sensible  %(k2)s 
"""),

97: _("""
  option  %(k1)s non licite pour un calcul hors plan du maillage
"""),

98: _("""
 erreur: la charge doit etre une charge mecanique !
"""),

99: _("""
  option  %(k1)s non licite pour un calcul non lineaire.
"""),
}
