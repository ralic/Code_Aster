#@ MODIF algorith5 Messages  DATE 10/10/2006   AUTEUR MCOURTOI M.COURTOIS 
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
 le type de resultat dyna_transne supporte pas les donnees complexes
"""),

2: _("""
 le type de resultat dyna_harmone supporte pas les donnees reelles
"""),

3: _("""
 on ne traite pas actuellement les deformations complexes
"""),

4: _("""
 nombre de dataset 58 superieur a nbnoeud * nbcmp 
"""),

5: _("""
 erreur dans la lecture du fichier ideas
"""),

6: _("""
 seules les donnees de type deplacement, vitesse, acceleration, deformation ou contrainte sont actuellement traitees 
"""),

7: _("""
 donnees : complexes, incompatibles avec dyna_trans
"""),

8: _("""
 donnees : reelles, incompatibles avec dyna_harmo ou harm_gene
"""),

9: _("""
 on ne traite pas actuellement la redefinition des orientations pour les champs de contrainte 
"""),

10: _("""
 on ne traite pas actuellement la redefinition des orientations pour les champs de deformation 
"""),

11: _("""
 la condition gamma/ksi <= 1 n est pas respectee
"""),

12: _("""
 incoherence des relations sigma_c sigma_p1 m_pic a_pic a_e et m_e
"""),

13: _("""
 erreur d integration - essai d integration  numero  %(k1)s - divergence de l integration locale - redecoupage du pas de temps
"""),

14: _("""
  increment de deformation cumulee (dv) = - %(k1)s 
"""),

15: _("""
 type d'element fini pas traite
"""),

16: _("""
 le profil de la matrice n est surement pas plein. on  continue mais s'il vous arrive des problemes plus loin...
"""),

17: _("""
 le profil de la matrice n est surement pas plein. on  continue mais attention ....
"""),

18: _("""
 le profil de la matrice n est  pas plein. on arrete tout ....
"""),

19: _("""
 matrice singuliere
"""),

20: _("""
 inversion seulement en dimension 3
"""),

21: _("""
 type de maille inconnu (dvlp)
"""),

22: _("""
 la matrice masse est singuliere.
"""),

23: _("""
 pas de temps minimal atteint
"""),

24: _("""
 donnees erronees.
"""),

25: _("""
  gamma = 0 : valeur par defaut 
"""),

26: _("""
  dispositif anti-sismique :  la distance des noeuds 1 et 2 est nulle
"""),

27: _("""
 le noeud  %(k1)s  n'est pas un noeud du maillage  %(k2)s 
"""),

28: _("""
 on n'a pas trouve le ddl "dx" pour le noeud  %(k1)s 
"""),

29: _("""
 on n'a pas trouve le ddl "dy" pour le noeud  %(k1)s 
"""),

30: _("""
 on n'a pas trouve le ddl "dz" pour le noeud  %(k1)s 
"""),

31: _("""
 calcul non-lineaire par sous-structuration, le mot-cle sous_struc_1 est obligatoire
"""),

32: _("""
 argument du mot-cle "sous_struc_1" n'est pas un nom de sous-structure
"""),

33: _("""
 calcul non-lineaire par sous-structuration entre 2 structures mobiles, le mot-cle sous_struc_2 est obligatoire
"""),

34: _("""
 argument du mot-cle "sous_struc_2" n'est pas un nom de sous-structure
"""),

35: _("""
  obstacle bi_cerc_int : dist_2 doit etre superieure ou egale a dist_1
"""),

36: _("""
 calcul non-lineaire par sous-structuration, pas de dispositif anti-sismique ou de flambage possible 
"""),

37: _("""
 le multi-appui + sous-structuration n'est pas developpe - bon courage
"""),

38: _("""
 conflit entre choc et flambage au meme lieu de choc : le calcul sera de type flambage
"""),

39: _("""
 argument du mot-cle "repere" inconnu
"""),

40: _("""
 les rigidites de chocs doivent etre strictement positives
"""),

41: _("""
 incoherence dans les donnees de la loi de flambage : les caracteristiques introduites peuvent induire a un ecrasement residuel negatif 
"""),

42: _("""
 les bases utilisees pour la projection sont differentes.
"""),

43: _("""
 les bases utilisees n'ont pas le meme nombre de vecteurs.
"""),

44: _("""
 les numerotations des matrices sont differentes.
"""),

45: _("""
 les numerotations des vecteurs d'excitation sont differentes.
"""),

46: _("""
 on n'a pas pu trouve les deplacements initiaux 
"""),

47: _("""
 on n'a pas pu trouve les vitesses initiales 
"""),

48: _("""
 on n'a pas pu trouve les variables internes initiales : reprise  choc avec flambage 
"""),

49: _("""
 abscence de terme de forcage externe. l'algorithme itmi n'est pas prevu pour calculer la reponse libre d'une structure.
"""),

50: _("""
 abscence de non-linearites de choc. pour traiter le regime lineaire, preciser une non-linearites de choc avec un jeu important.
"""),

51: _("""
 impossible de traiter le type d obstacle choisi avec methode itmi, (obstacle de type  %(k1)s  au noeud  %(k2)s ).
"""),

52: _("""
 duree de la simulation temporelle apres transitoire inferieure a la duree demandee (excitation temporelle trop courte)
"""),

53: _("""
 variation du deplacement entre deux instants successifs superieure a la valeur de tolerance proposee
"""),

54: _("""
 le calcul de la reponse temporelle n'est pas possible pour le type de structure etudiee.
"""),

55: _("""
 le couplage fluide-structure n'a pas ete pris en compte en amont.
"""),

56: _("""
 nb_mode est superieur au nombre de modes du concept  %(k1)s . on impose donc nb_mode =  %(k2)s , i.e. egal au nombre de modes du concept  %(k3)s .
"""),

57: _("""
 nb_mode_diag est different de nb_mode nombre de modes de la base modale . complete l'option base reduite n'est plus disponible.
"""),

58: _("""
 le calcul des parametres du mode no %(k1)s  par l'operateur <calc_flui_stru> n'a pas converge pour la vitesse no %(k2)s . le calcul de la reponse dynamique de la sructure n'est donc pas possible.
"""),

59: _("""
 pas de mot-cle <nb_mode_flui>. les  %(k1)s  modes du concept  %(k2)s  sont pris en compte pour le calcul du saut de force fluidelastique d'amortissement au cours des phases de choc.
"""),

60: _("""
 nb_mode_flui est plus grand que le nombre de modes du concept  %(k1)s .  %(k2)s  modes sont pris en compte pour le calcul du saut de force fluidelastique d'amortissement au cours des phases de choc.
"""),

61: _("""
 la matrice ktilda est singuliere.
"""),

62: _("""
  instant initial non trouve  valeur prise : 0 
"""),

63: _("""
 rela_effo_depl par sous-structuration, le mot-cle sous_struc est obligatoire
"""),

64: _("""
 argument du mot-cle "sous_struc" n'est pas un nom de sous-structure
"""),

65: _("""
 type de base inconnu.
"""),

66: _("""
 le taux de souplesse negligee est superieur au seuil.
"""),

67: _("""
 algorithme de devoge: developpement "amor_gene" non implante.
"""),

68: _("""
 algorithme itmi : il faut renseigner obligatoirement l'un ou l'autre des mots cles <amor_reduit>, <amor_gene>
"""),

69: _("""
 algorithme itmi : il faut renseigner obligatoirement les mots-cles <base_elas_flui> et <nume_vite_flui> pour definir une base modale sous ecoulement
"""),

70: _("""
 algorithme itmi : il faut renseigner obligatoirement le mot cle <pas> , i.e. donner la valeur du pas de temps initial
"""),

71: _("""
 algorithme itmi : lorsque l on affecte "oui" a <etat_stat>, il faut renseigner <ts_reg_etab>
"""),

72: _("""
 calcul non-lineaire par sous-structuration, option <sous_struc_1>, non implante dans la methode itmi.
"""),

73: _("""
  l'option <noeud_2> n'est pas implantee dans la la methode itmi.
"""),

74: _("""
 calcul non-lineaire par sous-structuration, option <sous_struc_2>, non implante dans la methode itmi.
"""),

75: _("""
 algorithme de NEWMARK: developpement %(k1)s non implante.
"""),

76: _("""
 NUME_ORDRE plus grand que le nombre de modes de la base
"""),

78: _("""
 mauvaise definition de l excitation  mot cle : vect_gene non autorise pour itmi .
"""),

79: _("""
 ksib non inversible
"""),

80: _("""
 la sensibilite en meca ne fonctionne pas encore avec un chargement thermique
"""),

81: _("""
 option non prevue
"""),

82: _("""
 projection nulle sur la boule unite (dvlp)
"""),

83: _("""
 etat de contact inconnu
"""),

84: _("""
 champ de geometrie introuvable (dvlp)
"""),

85: _("""
 echec dans le traitement du contact, augmenter iter_cont_max
"""),

86: _("""
 convergence forcee  sur boucle contraintes actives lors du traitement du contact
"""),

87: _("""
 convergence forcee  sur boucle seuil frottement lors du traitement du contact
"""),

88: _("""
 convergence forcee  sur boucle de geometrie lors du traitement du contact
"""),

89: _("""
 element de contact inconnu (dvlp)
"""),

90: _("""
 nom de l'element inconnu
"""),

91: _("""
 schema integration non conforme
"""),

92: _("""
 element de contact non conforme (dvlp)
"""),

93: _("""
 dimension de l'espace incorrecte (dvlp)
"""),

94: _("""
 sd introuvable (dvlp)
"""),

95: _("""
 matrice singuliere lors du  calcul du repere local tangent au  noeud maitre  %(k1)s  sur la maille maitre  %(k2)s  vecteurs tangents colineaires ?
"""),

96: _("""
 matrice singuliere lors  de la projection du point de contact  sur la maille maitre  %(k1)s  vecteurs tangents colineaires ?
"""),

97: _("""
 newton echoue lors du calcul du repere local tangent au  noeud maitre  %(k1)s  sur la maille maitre  %(k2)s 
"""),

98: _("""
 newton echoue lors  de la projection du point de contact  sur la maille maitre  %(k1)s 
"""),

99: _("""
 vecteurs tangents nuls au noeud maitre  %(k1)s  sur la maille maitre  %(k2)s 
"""),
}
