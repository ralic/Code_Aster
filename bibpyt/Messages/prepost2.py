#@ MODIF prepost2 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""
  Il faut autant de nom pour NOM_CHAM_MED que pour NOM_CHAM.
"""),

2 : _(u"""
Modele inconnu, pas d'impression du champ  %(k1)s 
"""),

3 : _(u"""
On ne sait pas ecrire des champs par element aux points de gauss au format CASTEM
"""),

4 : _(u"""
 erreur programmation
"""),

19 : _(u"""
 Le nombre de cmps de la grandeur du champ d?passe 500.
 Augmenter la dimension de 4 tableaux statiques
"""),

20 : _(u"""
  impr_resu format ensight : la composante  %(k1)s  du cham_gd  %(k2)s  n'est portee par aucun noeud du maillage et ne sera pas imprimee
"""),

21 : _(u"""
  impr_resu format ensight : la composante  %(k1)s  du champ  %(k2)s  du concept  %(k3)s  n'est portee par aucun noeud du maillage et ne sera pas imprimee
"""),

22 : _(u"""
  impr_resu format ensight : les trois composantes  %(k1)s   %(k2)s   %(k3)s  du cham_gd  %(k4)s  ne sont portees par aucun noeud du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

23 : _(u"""
  impr_resu format ensight : les trois composantes  %(k1)s   %(k2)s   %(k3)s  du champ  %(k4)s  du concept  %(k5)s  ne sont portees par aucun noeud du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

24 : _(u"""
 impression de la composante  %(k1)s  du champ  %(k2)s  sous forme d'une variable scalaire ensight  %(k3)s . la composante  %(k4)s  n'est pas portee par certains noeuds du maillage et des valeurs nulles sont affectees a ces noeuds dans les fichiers  %(k5)s 
"""),

25 : _(u"""
 impression des 3 composantes  %(k1)s   %(k2)s  et  %(k3)s  du champ  %(k4)s  sous forme d'une variable vectorielle ensight  %(k5)s . une ou plusieurs de ces composantes ne sont pas portees par certains noeuds du maillage et des valeurs nulles sont affectees a ces noeuds pour ces composantes dans les fichiers  %(k6)s 
"""),

26 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie reelle du cham_gd  %(k2)s 
"""),

27 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie relle du champ  %(k2)s  du concept %(k3)s 
"""),

28 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie imaginaire du cham_gd  %(k2)s 
"""),

29 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie imaginaire du champ  %(k2)s  du concept %(k3)s 
"""),

30 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s .r : ce fichier existe deja !! l'impression du meme champ ouconcept resultat est demandee deux fois ou une meme composante est selectionnee 2 fois sous le mot-cle nom_cmp
"""),

31 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression du cham_gd  %(k2)s 
"""),

32 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression du champ  %(k2)s  du concept  %(k3)s 
"""),

33 : _(u"""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  : ce fichier existe deja !! l'impression du meme champ ouconcept resultat est demandee deux fois ou une meme composante est selectionnee 2 fois sous le mot-cle nom_cmp
"""),

35 : _(u"""
   desole on ne sait pas ecrire les champs aux noeuds de representation constante et a valeurs complexes au format  %(k1)s 
"""),

36 : _(u"""
   desole on ne sait pas ecrire le champ aux noeuds  %(k1)s  au format  %(k2)s 
"""),

37 : _(u"""
 le maillage  %(k1)s  associe au cham_gd  %(k2)s  est different du maillage  %(k3)s  en operande de la commande !!!! attention !!!!
"""),

38 : _(u"""
 le maillage  %(k1)s  associe au champ  %(k2)s  du concept  %(k3)s  est different du maillage  %(k4)s  en operande de la commande !!!! attention !!!!
"""),

39 : _(u"""
 probleme a l'ouverture du fichier d'avertissement  %(k1)s 
"""),

40 : _(u"""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du cham_gd  %(k1)s  n'est presente dans la grandeur  %(k2)s 
"""),

41 : _(u"""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du champ  %(k1)s  du concept  %(k2)s  n'est presente dans la grandeur  %(k3)s 
"""),

42 : _(u"""
  impr_resu format ensight : la composante  %(k1)s  du cham_gd  %(k2)s  n'est pas portee par les noeuds du maillage et ne sera pas imprimee
"""),

43 : _(u"""
  impr_resu format ensight : la composante  %(k1)s  du champ  %(k2)s  du concept  %(k3)s  n'est pas portee par les noeuds du maillage et ne sera pas imprimee
"""),

44 : _(u"""
  impr_resu format ensight : aucune des trois composantes  %(k1)s   %(k2)s   %(k3)s  du cham_gd  %(k4)s  n'est portee par les noeuds du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

45 : _(u"""
  impr_resu format ensight : aucune des trois composantes  %(k1)s   %(k2)s   %(k3)s  du champ  %(k4)s  du concept  %(k5)s  n'est portee par les noeuds du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

46 : _(u"""
  numero d'ordre  %(k1)s  non licite 
"""),

47 : _(u"""
  le numero d'ordre suivant est desormais considere comme le premier numero d'ordre demande
"""),

48 : _(u"""
  pour certains numeros d'ordre le champ  %(k1)s  n'est pas present dans la sd_resultat  %(k2)s ==> des fichiers de valeurs vides seront generes afin de respecter le format ensight.
"""),

49 : _(u"""
 le nombre de cmps de la grandeur du champ depasse 500 augmenter la dimension du tableau ivari
"""),

50 : _(u"""
 une meme composante est donnee 2 fois sous le mot-cle nom_cmp lors de l'impression au format ensight par impr_resu
"""),

51 : _(u"""
 type de structure non traite:  %(k1)s 
"""),

52 : _(u"""
 on imprime que des champs elno
"""),

53 : _(u"""
 nbcmp different
"""),

54 : _(u"""
 composante inconnue %(k1)s 
"""),

55 : _(u"""
 L'ordre des composantes ?tabli lorsque que vous avez renseign? le mot-cl? 
 NOM_CMP est diff?rent de celui du catalogue Aster:
    - ordre des composantes fournies     : %(k1)s %(k2)s %(k3)s %(k4)s %(k5)s %(k6)s
    - ordre des composantes du catalogue : %(k7)s %(k8)s %(k9)s %(k10)s %(k11)s %(k12)s
"""),

56 : _(u"""
 on imprime que des champs "elga" ou "elem"
"""),

57 : _(u"""
 nbsp different de 1
"""),

58 : _(u"""
 pas de correspondance
"""),

59 : _(u"""
 aucun champ trouve, pas d'impression au format "gmsh"
"""),

60 : _(u"""
 On ne sait pas imprimer au format "gmsh" le champ %(k1)s
 car il est de type %(k2)s.
"""),

61 : _(u"""
 erreur de programmation : nbcmp different de 1 ou 3.
"""),

62 : _(u"""
 on ne peut pas traiter des elements a plus de 99 noeuds !
"""),

63 : _(u"""
 erreur de programation
"""),

64 : _(u"""
 seg4 element inexistant dans castem, converti en seg2
"""),

65 : _(u"""
 tria7 element inexistant dans castem, converti en tria6
"""),

66 : _(u"""
 quad9 element inexistant dans castem, converti en quad8
"""),

67 : _(u"""
 les champs n'ont pas la meme grandeur
"""),

68 : _(u"""
 les champs n'ont pas la meme numerotation
"""),

69 : _(u"""
 les champs n'ont pas le meme type de valeurs
"""),

70 : _(u"""
   desole on ne sait pas ecrire les champs aux noeuds de representation constante au format ideas
"""),

71 : _(u"""
 element noeud non disponible dans ensight
"""),

72 : _(u"""
 element  %(k1)s  non disponible dans ensight
"""),

73 : _(u"""
 les elements tria7 seront reduits a des tria6
"""),

74 : _(u"""
 les elements quad9 seront reduits a des quad8
"""),

75 : _(u"""
 les elements penta15 seront reduits a des penta6
"""),

76 : _(u"""
 il y a des groupes de noeuds dans le maillage  %(k1)s  qui n'apparaitront pas dans le fichier geometrie ensight: seuls des groupes de mailles peuvent y etre integres
"""),

77 : _(u"""
 la dimension du probleme est invalide : il faut : 1d, 2d, 3d.
"""),

78 : _(u"""
 hexa27 element inexistant dans ideas, converti en hexa20
"""),

79 : _(u"""
 tria7 element inexistant dans ideas, converti en tria6
"""),

80 : _(u"""
 quad9 element inexistant dans ideas, converti en quad8
"""),

81 : _(u"""
 seg4 element inexistant dans ideas, converti en seg2
"""),

82 : _(u"""
 element pyram5 non disponible dans ideas
"""),

83 : _(u"""
 element pyram13 non disponible dans ideas
"""),

84 : _(u"""
 Le champ %(k1)s est un champ aux noeuds par éléments
 contenant des %(k2)s
 Or l'impression de ce type de champ n'est pas encore possible au format MED
 On n'imprimera donc pas ce champ dans le fichier MED
"""),

85 : _(u"""
 L'élément PENTA18 est inexistant dans ideas, il est converti en PENTA15.
"""),

86 : _(u"""
 L'élément PENTA18 est inexistant dans castem, il est converti en PENTA15.
"""),

93 : _(u"""
 on ne sait pas ecrire les mailles de type  %(k1)s 
"""),

}
