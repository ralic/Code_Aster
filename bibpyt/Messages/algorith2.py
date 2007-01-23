#@ MODIF algorith2 Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
 operation d'appariement inconnue
"""),

3: _("""
 la liste des cham_no n'existe pas
"""),

4: _("""
 il n'y a aucun cham_no dans la liste
"""),

5: _("""
 les cham_no n'ont pas tous la meme longueur
"""),

6: _("""
 il faut definir nom_cmp
"""),

7: _("""
 il faut definir 3 angles nautiques.
"""),

8: _("""
 l origine doit etre definie par 3 coordonnees.
"""),

9: _("""
 l axe z est obligatoire en 3d.
"""),

10: _("""
 pour le 2d on ne prend que 2 coordonnees pour l origine.
"""),

11: _("""
 l axe z est n a pas de sens en 2d.
"""),

12: _("""
 le noeud se trouve sur l axe du repere cylindrique. on prend le noeud moyen des centres geometriques.
"""),

13: _("""
  -> Lors du passage au repère cylindrique, un noeud a été localisé sur l'axe
     du repère cylindrique. Code_Aster utilise dans ce cas le centre de gravité de
     l'élément pour le calcul de la matrice de passage en repère cylindrique.
  -> Risque & Conseil :
     Si ce centre de gravité se trouve également sur l'axe du repère, le calcul
     s'arrete en erreur fatale.
"""),

14: _("""
 charge non traitee:  %(k1)s 
"""),

15: _("""
 les modelisations autorisees sont 3d et d_plan et axis
"""),

16: _("""
 le choix des parametres ne correspond pas a l un des modeles cjs
"""),

17: _("""
 non converg.: essai normales
"""),

18: _("""
 non converg.: nb iter max atteint
"""),

19: _("""
 les modelisations autorisees sont 3d et d-plan et axis
"""),

20: _("""
 modelisation inconnue
"""),

21: _("""
  nvi > nvimax
"""),

22: _("""
 vecteur  de norme nulle  
"""),

23: _("""
 la maille doit etre de type tetra4, tetra10, penta6, penta15, hexa8 ou hexa20. or la maille est de type :  %(k1)s .
"""),

24: _("""
 la maille doit etre de type tetra4, tetra10, penta6, penta15, hexa8 ou hexa20. ou tria3-6 ou quad4-8 or la maille est de type :  %(k1)s .
"""),

25: _("""
 mauvaise face
"""),

26: _("""
  %(k1)s  groupe inexistant
"""),

27: _("""
 maille  %(k1)s  de type  %(k2)s  invalide pour le contact
"""),

28: _("""
 groupe de mailles de contact invalide
"""),

29: _("""
 mailles de contact 2d et 3d
"""),

30: _("""
 trois elements
"""),

31: _("""
 deux elements sur la meme face
"""),

32: _("""
 une reorientation a eu lieu pour le deuxieme appui
"""),

33: _("""
 pas de maille de reference trouvee
"""),

34: _("""
 stop_singulier=decoupe necessite la subdivision automatique du pas de temps (subd_pas)
"""),

35: _("""
 la methode  %(k1)s  est inadequate pour une resolution de type "ldlt"
"""),

36: _("""
 la methode  %(k1)s  est inadequate pour une resolution de type "gcpc"
"""),

37: _("""
 la methode  %(k1)s  etant inadequate pour une resolution de type "mult_front"
"""),

38: _("""
 la methode  %(k1)s  etant inadequate pour une resolution de type "feti"
"""),

39: _("""
 le solveur feti requiert un concept produit de type sd_feti en entree du mot-cle partition
"""),

40: _("""
 ! nombre de sous-domaines illicite !
"""),

41: _("""
 en parallele il faut au moins un sous-domaine par processeur !
"""),

42: _("""
 en parallele stogi=oui obligatoire pour limiter les msgs !
"""),

43: _("""
 pas de calcul sur le critere de rice disponible
"""),

44: _("""
 cette commande doit necessairement avoir le type evol_ther.
"""),

45: _("""
 seuls les champs de fonctions aux noeuds sont evaluables:  %(k1)s 
"""),

46: _("""
 nous traitons les champs de reels et de fonctions: . %(k1)s 
"""),

47: _("""
 le nom symbolique du champ chercher n est pas licite. %(k1)s 
"""),

48: _("""
 plusieurs instants correspondent a celui specifie sous affe 
"""),

49: _("""
 nume_fin inferieur a nume_init
"""),

50: _("""
 cmp non traitee
"""),

51: _("""
 il y a plusieurs charges contenant des liaisons unilaterales 
"""),

52: _("""
 debordement tableau (dvlp)
"""),

53: _("""
 erreur code dans affichage (dvlp)
"""),

54: _("""
  increment de deformation cumulee (dp) = - %(k1)s 
"""),

55: _("""
 erreur d integration- essai d integration  numero  %(k1)s - convergence vers  une solution non conforme - increment de deformation cumulee negative = - %(k2)s - redecoupage du pas de temps
"""),

56: _("""
  erreur - non convergence a iteration maxi  %(k1)s  - convergence reguliere mais trop lente - erreur >  %(k2)s - redecoupage du pas de temps
"""),

57: _("""
  erreur - non convergence a iteration maxi  %(k1)s  - convergence irreguliere & erreur >  %(k2)s  - redecoupage du pas de temps
"""),

58: _("""
  erreur - non convergence a iteration maxi  %(k1)s  - erreur >  %(k2)s  - redecoupage du pas de temps
"""),

59: _("""
  la transformation geometrique est singuliere pour la maille : %(k1)s  (jacobien = 0.)
"""),

60: _("""
  derivees secondes non etendues au 3d
"""),

61: _("""
 les listes des groupes de noeuds  a fournir doivent contenir le meme nombre de groupes de noeuds
"""),

62: _("""
  les listes des groupes de noeuds  doivent contenir le meme nombre de  noeuds
"""),

63: _("""
 on imprime que des champs reels
"""),

64: _("""
  %(k1)s cham_no deja existant
"""),

65: _("""
 appel errone a rsexch
"""),

66: _("""
 calcul du transitoire : choc en phase transitoire - pas de solution trouvee. utiliser l'option etat_stat = non.
"""),

67: _("""
 modele non local : projecteur singulier
"""),

68: _("""
 iter_dual_maxi trop eleve (<10000)
"""),

69: _("""
 fonction duale non convexe
"""),

70: _("""
 probleme recherche lineaire
"""),

71: _("""
 pas de geometrie associee au modele delocalise
"""),

72: _("""
 erreur transfo cham_elem_s
"""),

73: _("""
 mauvaise direction de descente
"""),

74: _("""
 pas de borne sup
"""),

75: _("""
 probleme recherche lineaire primal
"""),

76: _("""
 iterations primales insuffisantes
"""),

77: _("""
 mauvais dimensionnement de geomi
"""),

78: _("""
 dvp : energie non convexe
"""),

79: _("""
 pas de valeurs propres trouvees
"""),

80: _("""
 les champs de temperatureet d irradiation doivent etre specifiespour utiliser la force de serragesous forme de fonction
"""),

81: _("""
 les champs de temperatureet d irradiation doivent etre specifiespour utiliser les angles et les pentessous forme de fonctions
"""),

82: _("""
 nombre maximal de niveau de subdivision atteint
"""),

83: _("""
 detection divergence, force la subdivision.
"""),

84: _("""
 force la subdivision
"""),

85: _("""
 pas minimal de la subdivision atteint.
"""),

86: _("""
 il n'y a aucun instant de calcul ('list_inst')
"""),

87: _("""
 liste d'instants non croissante
"""),

88: _("""
 acces par instant sans evolution ordonnee interdit (increment)
"""),

89: _("""
 instant initial introuvable dans la liste d'instants (list_inst)
"""),

90: _("""
 instant final introuvable dans la liste d'instants (list_inst)
"""),

91: _("""
 nume_inst_init plus petit que nume_fin avec evolution: 'retrograde'
"""),

92: _("""
 nume_init plus grand que nume_fin
"""),

93: _("""
 nume_inst_init n'appartient pas a la liste d'instants
"""),

94: _("""
  -> Le numéro d'ordre correspondant à l'instant final de calcul NUME_INST_FIN
     n'appartient pas à la liste des numéros d'ordre.
     Dans ce cas, Aster considère pour numéro d'ordre final, le dernier de
     la liste fournie.
  -> Risque & Conseil :
     Afin d'éviter des pertes de résultats, assurez-vous que le numéro d'ordre
     associé à l'instant NUME_INST_FIN appartienne bien à la liste des numéros
     d'ordre.
"""),

95: _("""
 acces par instant sans evolution ordonnee interdit (archivage)
"""),

96: _("""
 impossible d'archiver l'etat initial : le concept est reentrant (archivage)
"""),

97: _("""
 l'archivage va ecraser des instants deja calcules (archivage)
"""),

98: _("""
 l'archivage va laisser des trous dans la sd evol_noli (archivage, nume_init)
"""),

99: _("""
 le nombre de niveau de subdivisions doit etre plus grand que 1 (subd_niveau)
"""),
}
