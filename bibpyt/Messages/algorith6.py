#@ MODIF algorith6 Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
 vecteurs tangents nuls lors  de la projection du point de contact  sur la maille maitre  %(k1)s 
"""),

2: _("""
 element inconnu  sur la maille maitre  %(k1)s 
"""),

3: _("""
 vecteur normal nul  sur le noeud maitre  %(k1)s 
"""),

4: _("""
 plus de 3 noeuds exclus  sur la maille esclave  %(k1)s  par l'option sans_group_no ou sans_noeud
"""),

5: _("""
 l'element porte par la   la maille esclave  %(k1)s  n'est pas du bon type pour un fond de fissure , elle est de type  %(k2)s 
"""),

6: _("""
 schema d'integration inconnu  sur la maille  %(k1)s 
"""),

7: _("""
 code erreur introuvable (dvlp)
"""),

8: _("""
 erreur point integration (dvlp)
"""),

9: _("""
 question inconnue (dvlp)
"""),

10: _("""
 element de contact incorrect (dvlp)
"""),

11: _("""
 pas d'elements tardifs ! (dvlp)
"""),

12: _("""
 la  methode d integration est gauss, le champ vale_cont n est pas cree
"""),

13: _("""
 dimension du probleme inconnu
"""),

14: _("""
 erreur dans la programmation, cette routine ne doit etre appele que dans le cas de la methode continue du contact
"""),

15: _("""
 le vecteur dire_appa est nul !
"""),

16: _("""
 le fond de fissure d'un maillage 2d ne peut etre defini par des mailles
"""),

17: _("""
 les mailles a modifier doivent etre de type "seg3" ou "poi1"
"""),

18: _("""
 le fond de fissure d'un maillage 2d est defini par un noeud unique
"""),

19: _("""
  -> Code Aster a détecté des mailles de type différent lors de la
     correspondance entre les maillages des deux modèles (mesuré/numérique).
     Ce cas n'est pas prévu, Code Aster initialise la correspondance au noeud
     le plus proche.
  -> Risque & Conseil :
     ???

"""),

20: _("""
 nb noeuds mesure superieur nb noeuds calcul
"""),

21: _("""
 noeu_calcul non trouve
"""),

22: _("""
 noeu_mesure non trouve
"""),

23: _("""
 nombre de noeuds different
"""),

24: _("""
 traitement manuel correspondance : un couple a la fois
"""),

25: _("""
 echec projection
"""),

26: _("""
 norme vecteur dir. nulle
"""),

27: _("""
 le nombre des coefficients de ponderation est superieur au nombre de vecteurs de base
"""),

28: _("""
 le nombre des coefficients de ponderation est inferieur au nombre de vecteurs de base , le dernier coefficient est affecte aux autres
"""),

29: _("""
 le nombre des fonctions de ponderation est superieur au nombre de vecteurs de base
"""),

30: _("""
 le nombre des fonctions de ponderation est inferieur au nombre de vecteurs de base la derniere fonction est affectee aux autres
"""),

31: _("""
 le nombre d abscisses d une des fonctions d interpolation n est pas identique au nombre d abscisses du premier point de mesure experimental 
"""),

32: _("""
  le critere d egalite de la liste d abscisses du premier  dataset 58 et de la liste d abscisses d une des fonctions  de ponderation n est pas verifie 
"""),

33: _("""
 incompatibilite nom_para et donnees mesurees 
"""),

34: _("""
 erreur
"""),

35: _("""
 surcharge d'un resultat sans definir d'etat initial : on prend un etat initial nul
"""),

36: _("""
 le concept dans etat_init n'est du type evol_noli
"""),

37: _("""
 pas de numero d'ordre pour le concept  %(k1)s 
"""),

38: _("""
 l'instant specifie sous 'etat_init' n'est pas trouve
"""),

39: _("""
 plusieurs instants correspondent a celui specifie sous 'etat_init'
"""),

41: _("""
 le champ de depl_r (ou derive) n'est pas trouve dans le concept  %(k1)s 
"""),

42: _("""
 le champ de sief_r (ou derive) n'est pas trouve dans le concept  %(k1)s 
"""),

43: _("""
 le champ de vite n'est pas trouve dans le concept  %(k1)s  on cree un champ de vitesses nulles
"""),

44: _("""
 le champ d'acce n'est pas trouve dans le concept  %(k1)s  on calcule un champ d'accelerations, ce qui est possible puisque les vitesses sont nulles
"""),

45: _("""
 le champ d'acce n'est pas trouve dans le concept  %(k1)s  on ne peut pas, pour l'instant, calculer le champ des accelerations car les vitesses ne sont pas nulles
"""),

46: _("""
 le champ de vari_r (ou derive) n'est pas trouve dans le concept  %(k1)s 
"""),

47: _("""
 le champ de vari_non_local (ou derive) n'est pas trouve dansle concept %(k1)s 
"""),

48: _("""
 le champ de lanl_elga n'est pas trouve dans le concept  %(k1)s 
"""),

49: _("""
 l'etat initial n'appartient pas a un evol_noli : on suppose qu'on part d'un etat a deplacements nuls
"""),

50: _("""
 l'etat initial n'appartient pas a un evol_noli : on suppose qu'on part d'un etat a vitesses nulles
"""),

52: _("""
 iterations cycliques : changement de configuration ou variation trop importante du deplacement physique a l'issue de la derniere iteration
"""),

53: _("""
 pas de convergence de l'algorithme de newton en  %(k1)s  iterations a l'instant  %(k2)s . il faut reduire la rigidite normale, ou le jeu.
"""),

54: _("""
 dvp : trop de noeuds
"""),

55: _("""
 theta = 1 ou 0.5 
"""),

56: _("""
 fluence commandee et flux_phi different de 1
"""),

57: _("""
 fluence decroissante (phi<0)
"""),

58: _("""
 relation asse_combu 1d sans loi de fluence appropriee
"""),

59: _("""
 erreur dir. grandissement
"""),

60: _("""
 cam_clay : la porosite donnee dans cam_clay doit etre la meme que dans thm_init
"""),

61: _("""
 barcelone : il faut que la contrainte hydrostatique soit superieure a la  pression de cohesion -kc*pc 
"""),

62: _("""
 iter_inte_maxi insuffisant lors du calcul de la borne
"""),

63: _("""
 cam_clay : le cas des contraintes planes n est pas traite pour ce modele.
"""),

64: _("""
 cam_clay : il faut que la contrainte hydrostatique soit superieure a la  pression initiale pa 
"""),

65: _("""
 type de selection inconnue
"""),

66: _("""
 pour l'instant on ne traite pas le cas des contraintes planes dans le modele de chaboche a une variable cinematique.
"""),

67: _("""
 n doit etre strictementpositif.
"""),

68: _("""
 parametre un_sur_k egal a zero cas incompatible avec visc_cinx_chab
"""),

69: _("""
 loi visc_cinx_chab on doit obligatoirement avoir un_sur_m = zero
"""),

70: _("""
 macro_element statique et feti incompatibles !
"""),

71: _("""
 chargement onde plane et feti incompatibles !
"""),

72: _("""
 forces fluides sur les grappes et feti incompatibles !
"""),

73: _("""
 forces d'inertie et feti incompatibles !
"""),

74: _("""
 forces explicites et feti incompatibles !
"""),

75: _("""
 forces d'inertie derivees et feti incompatibles !
"""),

76: _("""
 mode  %(k1)s  non reconnu (dvlp)
"""),

77: _("""
 coeff vic_cin1_chab tous nuls ?
"""),

78: _("""
 f reste toujours negative.
"""),

79: _("""
 f reste toujours positive.
"""),

80: _("""
 pb interp vari entiere ?? 
"""),

81: _("""
 utiliser algo_1d="deborst" sous comp_incr pour le comportement  %(k1)s 
"""),

82: _("""
 integration explicite du comportement non programmee
"""),

83: _("""
 rousselier à gradient sans simo miehe non programmé : utilisez la modélisation **_INCO
"""),

84: _("""
 loi non traitee pour les elemjoin : %(k1)s 
"""),

85: _("""
 integration explicite impossible
"""),

86: _("""
 erreur de programmation 1
"""),

87: _("""
 loi de comportement inexistante
"""),

88: _("""
 erreur dans le type de comportment
"""),

89: _("""
 erreur de programmation 2
"""),

90: _("""
 pas de c_plan pour vmis_cineutiliser c_plan_deborst
"""),

91: _("""
 pas de c_plan pour vmis_cin1utiliser c_plan_deborst
"""),

92: _("""
 pas de contraintes planes
"""),

93: _("""
 integration du comportement           poly_cfc uniquement explicite
"""),

94: _("""
 pas de c_plan pour bazant_fd  utiliser c_plan_deborst
"""),

95: _("""
 integration implicite du comportement non programmee
"""),

96: _("""
 green deformation required for elas_hyper material
"""),

97: _("""
  -> Les variables de commandes initiales induisent des contraintes
     incompatibles.
  -> Risque & Conseil : Ce message apparait si l'état initial
    (avant le premier instant de calcul) est tel que les variables de commande
    (température, hydratation, séchage...) conduisent à des contraintes
     non équilibrées. Dans le cas de la température, vérifiez que la valeur
     TEMP_REF correspond à la température de l'état initial.

"""),

98: _("""
 convergence atteinte avec resi_glob_maxi pour cause de chargement presque nul
"""),

99: _("""
 reac. geom. du contact supérieure a 5%%
"""),
}
