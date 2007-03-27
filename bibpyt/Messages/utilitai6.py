#@ MODIF utilitai6 Messages  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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

cata_msg={
1: _("""

 la grandeur introduite en operande  ne figure pas dans le catalogue des grandeurs
 grandeur %(k1)s
"""),

2: _("""

 incoherence dans les donnees:  on cherche a creer un champ de valeurs fonctions  alors que la grandeur n''est pas de type fonction
 grandeur de la commande :  %(k1)s
"""),

3: _("""

 la liste de composantes et la liste  des valeurs n''ont pas la meme dimension
 occurence de affe numero  %(i1)d
"""),

4: _("""
 une composante n''appartient pas a la grandeur
 occurence de affe numero  %(i1)d
 grandeur   :  %(k1)s
 composante :  %(k2)s
"""),

5: _("""

 le nume_ddl en entree ne s''appuie  pas sur la meme grandeur que celle de la commande
 grandeur associee au nume_ddl %(k1)s
 grandeur de la commande :  %(k2)s
"""),

6: _("""
 on a affecte la fonction  %(k1)s  pour la composante  %(k2)s
"""),

7: _("""
 on a affecte la valeur  %(r1)f  pour la composante  %(k1)s
"""),

8: _("""

 on cherche a affecter sur un noeud une  composante qui n''est pas dans le profil  noeud d''entree
 noeud : %(k1)s
 composante : %(k2)s
"""),

9: _("""

 on cherche a affecter sur un noeud une  composante qui n''est pas dans le profil  noeud d''entree
 noeud : %(k1)s
 composante : %(k2)s
"""),

10: _("""

 la grandeur introduite en operande  ne figure pas dans le catalogue des grandeurs
 grandeur:  %(k1)s
"""),

11: _("""
 une composante n''appartient pas a la grandeurgrandeur   :  %(k1)s
 composante :  %(k2)s
"""),

12: _("""
 variable inconnue: variable :  %(k1)s  pour le resultat :  %(k2)s
"""),

13: _("""
 probleme rencontre lors de la recherche de la variable :  %(k1)s
         debut :  %(k2)s
           fin :  %(k3)s
"""),

14: _("""
 interpolation non permise. valeur a interpoler: %(r1)f
     borne inferieure: %(r2)f
     borne superieure: %(r3)f
"""),

15: _("""
 il faut donner :   - une maille ou un group_ma %(k1)s
    - un noeud ou un group_no ou un point. %(k2)s
"""),

16: _("""
 interpolation impossible instant a interpoler:  %(r1)f
"""),

17: _("""
 interpolation impossible  instant a interpoler:  %(r1)f
  borne inferieure:  %(r2)f
"""),

18: _("""
 interpolation impossible  instant a interpoler:  %(r1)f  borne superieure: %(r2)f
"""),

19: _("""
 cham_no inexistant pour l''acces %(k1)s sur le resultat %(k2)s
 pour le nume_ordre %(i1)d
 instant a interpoler %(r1)f
"""),

20: _("""
 cham_no inexistant pour l''acces %(k1)s sur le resultat %(k2)s
 pour le nume_ordre %(i1)d
 instant a interpoler %(r1)f
"""),

21: _("""
 il faut donner :   - une maille ou un group_ma %(k1)s
    - un noeud ou un group_no ou un point. %(k2)s
"""),

22: _("""
 interpolation impossible instant a interpoler:  %(r1)f
"""),

23: _("""
 interpolation impossible  instant a interpoler:  %(r1)f
  borne inferieure:  %(r2)f
"""),

24: _("""
 interpolation impossible  instant a interpoler:  %(r1)f  borne superieure: %(r2)f
"""),

25: _("""
 cham_elem inexistant pour l''acces %(k1)s sur le resultat %(k2)s
 pour le nume_ordre %(i1)d
 instant a interpoler %(r1)f
"""),

26: _("""
 cham_elem inexistant pour l''acces %(k1)s sur le resultat %(k2)s
 pour le nume_ordre %(i1)d
 instant a interpoler %(r1)f
"""),

27: _("""
 il sera tronque:  %(k1)s
"""),

28: _("""
 erreur extrapolationl''extrapolation a gauche de  %(k1)s
 la fonction  %(k2)s
  est exclue %(k3)s
"""),

29: _("""
 erreur extrapolationextrapolation a gauche inconnue %(k1)s
"""),

30: _("""
 erreur interpolation.interpolation non permise %(k1)s
   valeur a interpoler: %(r1)f
   borne inferieure: %(r2)f
   borne superieure: %(r3)f
"""),

31: _("""
 erreur interpolation.interpolation inconnue %(k1)s
"""),

32: _("""
 erreur extrapolationl''extrapolation a droite de  %(k1)s
 la fonction  %(k2)s
  est exclue %(k3)s
"""),

33: _("""
 erreur extrapolationextrapolation a droite inconnue %(k1)s
"""),

34: _("""
 on deborde a gauche   valeur a interpolee:  %(r1)f
       borne inferieure:  %(r2)f
"""),

35: _("""
 on deborde a droite   valeur a interpolee:  %(r1)f
       borne superieure:  %(r2)f
"""),

36: _("""
 erreur de programmationtype de fonction inconnu  %(k1)s
"""),

37: _("""
 erreur  la fonction  %(k1)s  a  %(i1)d
  arguments, le maximum exploitable est  %(i2)d
"""),

38: _("""
 il y a   %(i1)d  parametre(s) identique(s) dans la  %(k1)s
 definition de la nappe. %(k2)s
"""),

39: _("""
 erreur dans les donnees   interface de type :  %(k1)s  non valable %(k2)s
"""),

40: _("""
 erreur dans les donneeson ne retrouve pas le noeud  %(k1)s
  dans la numerotation %(k2)s
"""),

41: _("""
 erreur dans les donnees   le noeud :  %(k1)s
  n''appartient pas au maillage  %(k2)s
"""),

42: _("""
  probleme recuperation chamno   concept resultat:  %(k1)s
    numero ordre:  %(i1)d
"""),

43: _("""
  probleme recuperation chamno   concept resultat:  %(k1)s
    numero ordre:  %(i1)d
"""),

44: _("""
 trop d''amortissements modaux   nombre d''amortissements :  %(i1)d
    nombre de modes :  %(i2)d
"""),

45: _("""
 sensibilite demandee par rapport au concept : %(k1)s
 son type est inconnu :  %(k2)s
"""),

46: _("""
 acces impossible  champ :  %(k1)s , nume_ordre :  %(i1)d
"""),

47: _("""
 erreur dans la recherche du noeud      nom du noeud :  %(k1)s
    nom du maillage :  %(k2)s
"""),

48: _("""
 methode de newtonexposant de la loi   = %(r1)f
 nombre d''iterations = %(i1)d
 residu fonction = %(r2)f
 residu f/df = %(r3)f
 precision = %(r4)f
"""),

49: _("""
 erreurs sur les donneesnombre de mots-cles :  %(i1)d
 nombre de valeurs   :  %(i2)d
"""),

50: _("""
 erreurs sur les donneesnombre de mots-cles :  %(i1)d
 nombre de mots-cles facteurs :  %(i2)d
"""),

51: _("""
 pas de champ correspondant  a l''instant demande.resultat  %(k1)s
 , acces "inst_init" : %(r1)f
"""),

52: _("""
 plusieurs champs correspondant  a l''instant demande.resultat  %(k1)s
 , acces "inst_init" : %(r1)f
 , nombre : %(i1)d
"""),

53: _("""

 le premier instant de rupture  n''est pas dans la liste des instants de calcul
 premier instant de rupture =  %(r1)f
 premier instant de calcul =  %(r2)f
"""),

54: _("""

 le dernier instant de rupture  n''est pas dans la liste des instants de calcul
 dernier instant de rupture =  %(r1)f
 dernier instant de calcul =  %(r2)f
"""),

55: _("""
 parametres initiaux de weibullexposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 contrainte de reference = %(r3)f
"""),

56: _("""
 statistiques recalage :nombre d''iterations = %(i1)d
 convergence atteinte = %(r1)f
"""),

57: _("""
 les abscisses  %(k1)s %(k2)s ne sont pas monotones. %(k3)s
"""),

58: _("""
 les abscisses  %(k1)s %(k2)s ont ete reordonnees. %(k3)s
"""),

59: _("""
 l'ordre des abscisses  %(k1)s %(k2)s a ete inverse. %(k3)s
"""),

60: _("""
 homogeneite du champ de materiaux pour weibull
 nombre de rc weibull trouvees =  %(i1)d
 les calculs sont valables pour  un seul comportement weibull %(k1)s
 on choisit la premiere relation du type weibull %(k2)s
"""),

61: _("""
 parametres de la rc weibull_foexposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 &contrainte de reference conventionnelle
      &= %(r3)f
"""),

62: _("""
 parametres de la rc weibullexposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 contrainte de reference = %(r3)f
"""),

63: _("""
 la chaine nomsd est de longueur :  %(i1)d
"""),

64: _("""
 probleme de declarationla chaine nomstr est de longueur  %(i1)d
 on veut y mettre  saux24  de longueur  %(i2)d
"""),

65: _("""
 mauvaise valeur pour typestil faut entre 0 et  %(i1)d mais on a donne  %(i2)d
"""),

66: _("""
 mais on a donne  %(i1)d
"""),

67: _("""
 probleme de declarationla chaine typeps est declaree a  %(i1)d
 &on veut y mettre  zk24(adraux+2)  qui en contient  %(i2)d
"""),

68: _("""
 type de numerotation non connue numerotation: %(k1)s
"""),

69: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s
"""),

70: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s
"""),

71: _("""
 il faut donner :   - une maille ou un group_ma %(k1)s
    - un noeud ou un group_noou un point %(k2)s
"""),

72: _("""
 trop de mailles dans le group_ma  maille utilisee:  %(k1)s
"""),

73: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s
"""),

74: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s
"""),

75: _("""
 trop de mailles dans le group_ma  maille utilisee:  %(k1)s
"""),

76: _("""
 type de numerotation non connue numerotation: %(k1)s
"""),

77: _("""
 le numero d''ordre n''existe pas    numero :  %(i1)d
"""),

78: _("""
  numero d archivage:  %(i1)d superieur au max:, %(i2)d
"""),

79: _("""
  numero de rangement :  %(i1)d superieur au max:, %(i2)d
"""),

80: _("""
 variable inconnue: variable :  %(k1)s pour le type :  %(k2)s
"""),

81: _("""
 parametre inconnu: parametre :  %(k1)s  pour le resultat :  %(k2)s
"""),

82: _("""
 pas de champs trouve pour la frequence  %(r1)f
"""),

83: _("""
 plusieurs champs trouves pour la frequence  %(r1)f
 nombre de champs trouves  %(i1)d
"""),

84: _("""
 le "nom_para_resu"  %(k1)s n''est pas un parametre du resultat  %(k2)s
"""),

85: _("""
 code retour de psrenc :  %(i1)d la derivee de :  %(k1)s par rapport a :  %(k2)s
 est introuvable. %(k3)s
"""),

86: _("""
 mauvaise valeur pour choixil faut 1,2 ou 3, mais pas  %(i1)d
"""),

87: _("""
 la chaine  %(k1)s est de longueur :  %(i1)d
"""),

88: _("""
 probleme de declarationla chaine nocomp est declaree a  %(i1)d
 &on veut y mettre  saux08  qui en contient  %(i2)d
"""),

89: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

90: _("""
 erreur dans les donneesparametre non trouve:  %(k1)s
"""),

91: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

92: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

93: _("""
 le parametre  %(k1)s n''existe pas dans la table %(k2)s
 .il est necessaire. %(k3)s
 veuillez consulter la documentaion  de la commande. %(k4)s
"""),

94: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

95: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

96: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

97: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

98: _("""
 erreur dans les donneesparametre n''existe pas:  %(k1)s
"""),

99: _("""
 erreur dans les donneesparametre :  %(k1)s   plusieurs valeurs trouvees %(k2)s
 pour le parametre  %(k3)s
 et le parametre  %(k4)s
"""),

}
