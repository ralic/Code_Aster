#@ MODIF algorith12 Messages  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
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
 nombre de mot-cle invalide numero liaison: %(i1)d mot-cle: %(k1)s
 vous en avez defini: %(i2)d il en faut exactement: %(i3)d
"""),

2: _("""
 interface inexistante numero liaison: %(i1)d nom sous-structure: %(k1)s
 nom macr_elem: %(k2)s
 nom interface inexistante: %(k3)s
"""),

3: _("""
 une sous-structure est sans connexion sous-structure -->  %(k1)s
"""),

4: _("""
  probleme stockage :   option de calcul :  %(k1)s    occurence : %(k2)s
    nom du champ :  %(k3)s
"""),

5: _("""
  probleme stockage :   option de calcul :  %(k1)s    nom du champ cumul :  %(k2)s
"""),

6: _("""
 donnees incompatibles :   pour la direction  %(k1)s
    nombre de blocage :  %(i1)d
    nombre d'excitations :  %(i2)d
"""),

7: _("""
 donnees incompatibles :   pour les modes mecaniques :  %(k1)s
    il manque l'option :  %(k2)s
"""),

8: _("""
 &coefficient de conditionnement des lagranges:  %(r1)f
"""),

9: _("""
  probleme stockage :   option de calcul :  %(k1)s    direction :  %(k2)s
    nom du champ :  %(k3)s
"""),

10: _("""
  probleme stockage :   option de calcul :  %(k1)s    direction :  %(k2)s
    nom du champ :  %(k3)s
"""),

11: _("""
 correction statique non prise en compte   pour l'option :  %(k1)s
"""),

12: _("""
 donnees incompatibles :   pour les mode_corr :  %(k1)s
    il manque le champ :  %(k2)s
"""),

13: _("""
 donnees incompatibles :   pour les mode_corr :  %(k1)s    pour le champ :  %(k2)s
    le type n'est pas  %(k3)s
"""),

14: _("""
 donnees incompatibles :   pour les statiques :  %(k1)s
    il manque le champ :  %(k2)s
"""),

15: _("""
 donnees incompatibles :   pour les statiques :  %(k1)s    pour le champ :  %(k2)s
    le type n'est pas  %(k3)s
"""),

16: _("""
 donnees incompatibles :   pour les mode_corr :  %(k1)s
    il manque le champ :  %(k2)s
"""),

17: _("""
 donnees incompatibles :   pour les mode_corr :  %(k1)s    pour le champ :  %(k2)s
    le type n'est pas  %(k3)s
"""),

18: _("""
 on ne sait pas bien traiter l'option de calcul demandee :  %(k1)s
"""),

19: _("""
 donnees incompatibles :   pour les modes mecaniques :  %(k1)s
    il manque l'option :  %(k2)s
"""),

20: _("""
 donnees incompatibles :   pour les modes mecaniques :  %(k1)s
    pour l'option :  %(k2)s
    il manque le champ d'ordre  %(i1)d
"""),

21: _("""
 donnees incompatibles :   pour les mode_corr :  %(k1)s
    il manque l'option :  %(k2)s
"""),

22: _("""
 donnees incompatibles :   pour les modes statiques :  %(k1)s
    il manque l'option :  %(k2)s
"""),

23: _("""
 arret sur question illicite pour le type de base type de base -->  %(k1)s
  question -->  %(k2)s
"""),

24: _("""
 arret sur question illicite pour le type de base type de base -->  %(k1)s
  question -->  %(k2)s
"""),

25: _("""
 arret sur question illicite pour le type de base type de base -->  %(k1)s
  question -->  %(k2)s
"""),

26: _("""
 arret sur manque argument base modale donnee -->  %(k1)s
  interf_dyna donnee -->  %(k2)s
"""),

27: _("""
 arret sur type de base incorrecte base modale donnee -->  %(k1)s
  type  base modale -->  %(k2)s
  type attendu -->  %(k3)s
"""),

28: _("""
 arret su incoherence donnees base modale donnee -->  %(k1)s
  interf_dyna correspondante -->  %(k2)s
  interf_dyna donnee -->  %(k3)s
"""),

29: _("""
 probleme arguments de definition interface nom interface donne %(k1)s
  numero interface donne %(i1)d
"""),

30: _("""
 arret sur base modale sans interf_dyna base modale  donnee -->  %(k1)s
"""),

31: _("""
 arret sur manque arguments base modale  donnee -->  %(k1)s
  interf_dyna  donnee -->  %(k2)s
"""),

32: _("""
 arret sur base modale sans interf_dyna base modale  donnee -->  %(k1)s
"""),

33: _("""
 arret sur manque arguments base modale  donnee -->  %(k1)s
  interf_dyna  donnee -->  %(k2)s
"""),

34: _("""
 probleme arguments de definition interface nom interface donne %(k1)s
  numero interface donne %(i1)d
"""),

35: _("""
 arret sur base modale sans interf_dyna base modale  donnee -->  %(k1)s
"""),

36: _("""
 arret sur manque arguments base modale  donnee -->  %(k1)s
  interf_dyna  donnee -->  %(k2)s
"""),

37: _("""
 affichage des coeff d'amortissement:
 premier coefficient d'amortissement %(r1)f
 second coefficient d'amortissement %(r2)f
"""),

38: _("""
 arret sur probleme coherence interface
"""),

39: _("""
 arret sur matrice inexistante matrice %(k1)s
"""),

40: _("""

  arret probleme de factorisation: presence probable de modes de corps rigide  la methode de mac-neal ne fonctionne pas en presence  de modes de corps rigide
"""),

41: _("""
 --- la taille bloc  : %(i1)d est < hauteur_max : %(i2)d
  changez la taille_bloc des profils: %(k1)s
  prenez au moins : %(i3)d
"""),

42: _("""
 le mot-cle  %(k1)s est incompatible avec le champ %(k2)s
 . utiliser 'group_ma' ou 'maille'  pour restreindre le changement de repere a certaines  mailles. %(k3)s
"""),

43: _("""
 etude 2d angle nautique unique :  %(r1)f
"""),

44: _("""
 noeud sur l axe_z noeud :  %(k1)s
"""),

45: _("""
 noeud sur l axe_z noeud :  %(k1)s
"""),

46: _("""
 etude 2d angle nautique unique :  %(r1)f
"""),

47: _("""
 noeud sur l'axe_z noeud :  %(k1)s
"""),

48: _("""
 noeud sur l'axe_z noeud :  %(k1)s
"""),

49: _("""
 probleme: sous-structure inconnue sous-structure -->  %(k1)s
"""),

50: _("""
 pas de sous-structure dans le squelette
"""),

51: _("""
 nom de sous-structure non trouve la sous-structure :  %(k1)s n existe pas  %(k2)s
"""),

52: _("""
 Nombre de modes propres calculés isuffisant.
 MODE_MECA :  %(k1)s
 Nombre de modes propres calculés limité a %(i1)d
"""),

53: _("""
 arret sur pivot nul ligne -->  %(i1)d
"""),

54: _("""
 arret sur pivot nul ligne -->  %(i1)d
"""),

55: _("""
 le maillage mail ne contient pas de group_ma mail=  %(k1)s
"""),

56: _("""
 le group_ma gp n'existe pas dans le maillage mail mail= %(k1)s gp= %(k2)s
"""),

57: _("""
 le maillage ne contient pas de group_nomaillage=  %(k1)s
"""),

58: _("""
 le group_no n'existe pas dansle maillage maillage= %(k1)s
 group_no= %(k2)s
"""),

59: _("""
 nombre noeuds communs nbnoco =  %(i1)d
"""),

60: _("""
 nombre noeuds communs nbnoco =  %(i1)d
"""),

61: _("""
 nombre noeuds communs nbnoco =  %(i1)d
"""),

62: _("""
 les deux numerotations n'ont pas meme maillage d'origine
  numerotation 1: %(k1)s
 maillage 1: %(k2)s
  numerotation 2: %(k3)s
 maillage 2: %(k4)s
"""),

63: _("""
 perte information sur ddl physique a la conversion de numerotation
 noeud numero:  %(i1)d
 type ddl numero:  %(i2)d
"""),

64: _("""
 arret sur perte information ddl physique
"""),

65: _("""
 les deux numerotations n'ont  pas meme maillage d'origine
  numerotation 1:  %(k1)s
  maillage 1:  %(k2)s
  numerotation 2:  %(k3)s
  maillage 2:  %(k4)s
"""),

66: _("""
 champ inexistant champ:  %(k1)s , nume_ordre:  %(i1)d , mode_meca:  %(k2)s
"""),

67: _("""
 arret sur probleme conditions interface
"""),

68: _("""
 le maillage final n'est pas 3d maillage :  %(k1)s
"""),

69: _("""
 l origine du maillage 1d  n est pas 0
"""),

70: _("""
 les noeuds du maillage sont  confondus
"""),

71: _("""

 le noeud se trouve en  dehors du domaine de definition avec  un profil gauche de type exclu
  noeud :  %(k1)s
"""),

72: _("""

 le noeud se trouve en  dehors du domaine de definition avec  un profil droit de type exclu
  noeud :  %(k1)s
"""),

73: _("""
 probleme pour stoker le champ dans le resultat :  %(k1)s
 , pour le nume_ordre :  %(i1)d
"""),

74: _("""
 *** champ deja existant ***il sera remplace par le champ %(k1)s
  pour le nume_ordre  %(i1)d
"""),

75: _("""
  composante inexistante sur  le noeud:  %(k1)s  composante:  %(k2)s
"""),

76: _("""
  probleme recuperation chamno concept resultat:  %(k1)s numero ordre:  %(i1)d
"""),

77: _("""
 &pas d' interface definie --> un gage!
"""),

78: _("""
 &arret sur interface deja definie mot-cle interface numero  -->  %(i1)d
  interface  -->  %(k1)s
"""),

79: _("""
 &les deux interfaces ont pas meme nombre de noeuds
 nombre noeuds interface droite -->  %(i1)d
 nombre noeuds interface gauche -->  %(i2)d
"""),

80: _("""
 &les deux interfaces ont pas meme nombre de degres de liberte
 nombre ddl interface droite -->  %(i1)d
 nombre ddl interface gauche -->  %(i2)d
"""),

81: _("""
 &arret sur base modale ne comportant pas de modes propres
"""),

82: _("""

 nombre de modes propres demande superieur au nombre de modes dynamiques de la base
 nombre de modes demandes --> %(i1)d
 nombre de modes de la base --> %(i2)d
 nombre de frequences douteuses --> %(i3)d
"""),

83: _("""
 plusieurs champs  correspondant a l'acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
 , nombre : %(i1)d
"""),

84: _("""
 pas de champ  correspondant a un acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
"""),

87: _("""
 plusieurs champs  correspondant a l'acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
 , nombre : %(i1)d
"""),

88: _("""
 pas de champ  correspondant a un acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
"""),

89: _("""
 instant de reprise superieur a la liste des instants   instant de reprise:  %(r1)f
    instant max:  %(r2)f
"""),

90: _("""
 on n'a pas trouve l'instant   instant de reprise:  %(r1)f
    pas de temps:  %(r2)f
    borne min:  %(r3)f
    borne max:  %(r4)f
"""),

91: _("""
 instant final inferieur a la liste des instants   instant final:  %(r1)f
    instant min  :  %(r2)f
"""),

92: _("""
 on n'a pas trouve l'instant   instant final:  %(r1)f
    pas de temps:  %(r2)f
    borne min:  %(r3)f
    borne max:  %(r4)f
"""),

93: _("""
 plusieurs champs  correspondant a l'acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
 , nombre : %(i1)d
"""),

94: _("""
 pas de champ  correspondant a un acces demande.resultat  %(k1)s
 , acces "inst":  %(r1)f
"""),

96: _("""
 mauvaise valeur de typcum:  %(i1)d
"""),

97: _("""
 donnees erronees
 pas d'instant de calcul pour  l'instant d'archivage:  %(r1)f
"""),

98: _("""
 donnees erronees
 plusieurs instants de calcul pour  l'instant d'archivage:  %(r1)f
"""),

99: _("""
 erreur fatalele champ %(k1)s est incompatible avec  la commande  %(k2)s
"""),

}
