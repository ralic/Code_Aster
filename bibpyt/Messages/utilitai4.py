#@ MODIF utilitai4 Messages  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
 on a  %(k1)s  et  %(k2)s 
"""),

2: _("""
 erreur de donnees
"""),

3: _("""
 son type est :  %(k1)s 
"""),

4: _("""
 les sous-types de sensibilite pour l'influence de  %(k1)s  sont incoherents.
"""),

5: _("""
 impossible de trouver un sous-type de sensibilite pour l'influence de  %(k1)s 
"""),

6: _("""
 incompatibilite de nombre equations
"""),

7: _("""
 nombre de composantes de la grandeur superieure a 30.
"""),

8: _("""
 la composante  %(k1)s  n existe pas dans le champ de la grandeur.
"""),

9: _("""
 les numeros d'ordre des vitesses donnes sous le mot-cle "nume_ordre" ne sont pas valides.
"""),

10: _("""
 le mode demande n'est pas un mode couple.
"""),

11: _("""
 probleme(s) rencontre(s) lors de l'acces au resu_gene
"""),

12: _("""
 pb lors de l'extraction du champ
"""),

13: _("""
 on ne traite que le type complexe
"""),

14: _("""
 composante generalisee non trouvee
"""),

15: _("""
 probleme(s) rencontre(s) lors de la lecture des frequences.
"""),

16: _("""
  on ne traite pas le type de modes " %(k1)s ".
"""),

17: _("""
 on ne traite que le type reel
"""),

18: _("""
 fonctionnalite non developpee
"""),

19: _("""
 on ne traite que les champs par elements de                       type reel.
"""),

20: _("""
 on ne traite pas ce type                            de champ: %(k1)s 
"""),

21: _("""
 "interp_nume" interdit pour recuperer un parametre en fonction d'une variable                d'acces.
"""),

22: _("""
 aucun champ trouve pour l'acces  %(k1)s 
"""),

23: _("""
 le champ  %(k1)s  n'existe pas dans le resu_gene.
"""),

24: _("""
 probleme(s) rencontre(s) lors de la lecture des instants.
"""),

25: _("""
 probleme recup de ptem uniquement pour methode adapt
"""),

26: _("""
 acce_mono_appui est compatible uniquement avec un champ de type : acce 
"""),

27: _("""
 manque la definition d'un mot cle
"""),

28: _("""
 dvlp
"""),

29: _("""
 nouvelle longueur invalide, < 0 
"""),

30: _("""
 probleme dans le decodage de ( %(k1)s , %(k2)s )
"""),

31: _("""
 type_resultat inconnu : %(k1)s 
"""),

32: _("""
 pas de liste de frequences dans le resu_gene.
"""),

33: _("""
 type scalaire inconnu :  %(k1)s 
"""),

34: _("""
 sd  %(k1)s  inexistante
"""),

35: _("""
 pas de numero d'ordre pour  %(k1)s 
"""),

36: _("""
 longt trop grand
"""),

37: _("""
  le modele fourni par l'utilisateur est different de celui present dans la sd resultat.
"""),

38: _("""
  le cara_elem fourni par l'utilisateur est different de celui present dans la sd resultat, on poursuit les calculs avec le cara_elem fourni par l'utilisateur.
"""),

39: _("""
  -> Le matériau fourni par l'utilisateur est différent de celui présent dans
     la Structure de Données Résultat. On poursuit les calculs avec le matériau
     fourni par l'utilisateur.
  -> Risque & Conseil : Vérifiez si le matériau fourni dans la commande est
     bien celui que vous souhaitez. Si oui vous allez poursuivre les calculs
     (ex: calcul des contraintes: CALC_ELEM) avec un matériau différent de
     celui utilisé pour calculer les déplacements, températures,...

"""),

40: _("""
  le chargement (mot cle: charge) fourni par l'utilisateur est different de celui present dans la sd resultat,on poursuit les calculs avec le chargement fourni par l'utilisateur.
"""),

41: _("""
  les fonctions multiplicatrices du chargement (mot cle: fonc_mult) fourni par l'utilisateur  sont differentes de celles presentes dans la sd  resultat, on poursuit les calculs avec les fonctions multiplicatrices fournies par l'utilisateur.
"""),

42: _("""
 numero d'ordre trop grand.
"""),

43: _("""
 nom de champ interdit :  %(k1)s  pour le resultat :  %(k2)s 
"""),

44: _("""
  pas de variables d'acces
"""),

45: _("""
  pas de parametres
"""),

46: _("""
 cet acces est interdit pour un resultat de type "champ_gd".
"""),

47: _("""
 cet acces est interdit :  %(k1)s 
"""),

48: _("""
 pas de liste d'instants dans le resu_gene.
"""),

49: _("""
 probleme pour recuperer les numeros d'ordre dans la structure "resultat"  %(k1)s 
"""),

50: _("""
 probleme pour recuperer les parametres
"""),

51: _("""
 aucun numero d'ordre ne correspond au parametre demande  %(k1)s 
"""),

52: _("""
 aucun numero d'ordre ne correspond au champ demande  %(k1)s 
"""),

53: _("""
 aucun numero d'ordre trouve. stop.
"""),

54: _("""
 la chaine pref passe en argument est trop courte pour mettre la chaine prefix =  %(k1)s 
"""),

55: _("""
 la derivee de  %(k1)s  par rapport a  %(k2)s  est deja nommee.
"""),

56: _("""
 structure simple   :  %(k1)s 
"""),

57: _("""
 parametre sensible :  %(k1)s 
"""),

58: _("""
 la structure composee a supprimer est :  %(k1)s 
"""),

59: _("""
 mais la structure composee enregistree est :  %(k1)s 
"""),

60: _("""
 suppression impossible !
"""),

61: _("""
 choix=/prefixe/e/renc/remc/s svp.
"""),

62: _("""
 la structure nosimp est introuvable dans la memorisation inpsco
"""),

63: _("""
 acces inconnu  %(k1)s 
"""),

64: _("""
 la table n'existe pas
"""),

65: _("""
 pas de parametres definis
"""),

66: _("""
 pas de lignes definis
"""),

67: _("""
 mauvais numero de ligne
"""),

68: _("""
 nom de table incorrect
"""),

69: _("""
 nombre de valeur a ajoute superieur au nombre de ligne de la table
"""),

70: _("""
 numero de ligne negatif
"""),

71: _("""
 numero de ligne superieur aunombre de ligne de la table
"""),

72: _("""
 le parametre n existe pas
"""),

73: _("""
 les types du parametre ne correspondent pas entre eux.
"""),

74: _("""
 numero de ligne trop grand
"""),

75: _("""
 erreur pgmation le nom d'une table ne doit pas depasser 17 caracteres.
"""),

76: _("""
 pas de lignes definies
"""),

77: _("""
 types de parametres differents
"""),

78: _("""
 on n a pas trouve de ligne contenant les deux parametres.
"""),

79: _("""
 table  %(k1)s  : n'existe pas
"""),

80: _("""
 table  %(k1)s  : aucun parametre n'est defini
"""),

81: _("""
 pas de parametres de type i et r
"""),

82: _("""
 pas de lignes selectionnees
"""),

83: _("""
 table non diagonalisable
"""),

84: _("""
 impression de la table superieure a 2000 colonnes, selectionnez vos parametres.
"""),

85: _("""
 pagination supprimee, utiliser impr_table
"""),

86: _("""
 il faut 3 parametres pour une impression au format "tableau"
"""),

87: _("""
 on ne trie que 1 ou 2 parametres
"""),

88: _("""
 le tableau %(k1)s .vale" est de type  %(k2)s 
"""),

89: _("""
 seules les 50 premieres lignes du titre sont conservees.
"""),

90: _("""
  votre demon c'est n'importe quoi
"""),

91: _("""
  demon non actif desole.
"""),

92: _("""
 on ne sait pas ecrire les complexes. na !! 
"""),

93: _("""
 on ne sait pas ecrire n'importe quoi desole.
"""),

94: _("""
  concept  %(k1)s non existant 
"""),

95: _("""
 l'objet de nom " %(k1)s " est inconnu.
"""),

96: _("""
  expression incorrecte 
"""),

97: _("""
  nombre d'argument incorrect pour le demon 
"""),

98: _("""
  votre parametre  c'est n'importe quoi
"""),

99: _("""
 et alors typesd =  %(k1)s 
"""),
}
