#@ MODIF sensibilite Messages  DATE 16/05/2007   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE ABBAS M.ABBAS

def _(x) : return x

cata_msg={

1: _("""
 Type de dérivation voulu : %(i1)d 
 Ce type n'est pas implanté.
"""),

2: _("""
 On veut dériver %(k1)s par rapport à %(k2)s. 
 Cela n'est pas disponible.
"""),

3: _("""
 La dérivée de %(k1)s par rapport à %(k2)s est introuvable.
"""),

4: _("""
 Le champ de theta sensibilité est inexistant dans la sd %(k1)s
"""),

5: _("""
 On ne sait pas dériver ce type de structures : %(k1)s.
"""),

6: _("""
 Le paramètre de sensibilité doit etre un champ theta.
"""),

7: _("""
 Cette option est indisponible en sensibilité lagrangienne.
"""),

8: _("""
 Pour l'occurrence numéro %(i1)d ,
 la dérivée du champ %(k1)s de %(k2)s par rapport à %(k3)s est introuvable.
"""),

9: _("""
 On ne sait pas trouver le type de la dérivation par rapport à %(k1)s.
"""),

10: _("""
 Initialisation de la table associée à la table %(k1)s et au paramètre sensible %(k2)s
 connue sous le nom de concept %(k3)s 
"""),

11: _("""
 Le calcul de sensibilité n'est pas encore disponible pour les chargements de type epsi_init
"""),

12: _("""
 Il y a vraisemblablement %(i1)d modes propres multiples.
 Le calcul des sensibilités se limite actuellement aux modes propres simples
"""),

13: _("""
 On ne peut pas dériver avec une charge complexe en entrée de dyna_line_harm.
"""),

14: _("""
 La sensibilité en mécanique ne fonctionne pas encore avec un chargement thermique
"""),

15: _("""
 Le comportement %(k1)s n'est pas autorisé en sensibilité
"""),

16: _("""
 EXICHA différent de 0 et 1
"""),

22: _("""
 L'option sensibilité lagrangienne non opérationnelle en non lineaire
"""),

21: _("""
 Pour faire une reprise avec un calcul de sensibilité, il faut renseigner "evol_noli" dans "etat_init"
"""),

31: _("""
 L'option sensibilité n'est pas opérationnelle en séchage
"""),

32: _("""
 L'option sensibilité n'est pas opérationnelle en hydratation
"""),

35: _("""
 L'option sensibilité n'est pas opérationnelle pour le comportement %(k1)s
"""),

36: _("""
 L'option sensibilité n'est pas opérationnelle pour le type d'élément %(k1)s
"""),

37: _("""
 L'option sensibilité n'est pas opérationnelle pour la modélisation %(k1)s
"""),

38: _("""
 pb determination sensibilité de rayonnement
"""),

39: _("""
 pb determination sensibilité materiau ther_nl
"""),

41: _("""
 Déplacements initiaux imposés nuls pour les  calculs de sensibilité
"""),

42: _("""
 Vitesses initiales imposées nulles pour les  calculs de sensibilité
"""),

51: _("""
 Dérivation de g : un seul paramètre sensible par appel à CALC_G. 
"""),

52: _("""
 Actuellement, on ne sait dériver que les 'POU_D_E'.
"""),

53: _("""
 En thermoélasticité, le calcul des dérivées de g est pour le moment incorrect.
"""),

54: _("""
 Avec un chargement en déformations (ou contraintes) initiales, le calcul
 des dérivées de g est pour le moment incorrect.
"""),

55: _("""
 Le calcul de derivée n'a pas été étendu à la plasticité.
"""),

56: _("""
 Pas de calcul de sensibilité accessible.
"""),

57: _("""
 Valeur incorrecte pour le nom du concept ou du paramètre sensible.
 Ils doivent etre de type chaines de caractères de moins de 8 caractères.
                 Nom du concept : '%(k1)s'
      Nom du paramètre sensible : '%(k2)s'
"""),

58: _("""
                 Nom du concept : '%(k1)s'
      Nom du paramètre sensible : '%(k2)s'
                    
           Nom composé (dérivé) : '%(k3)s'

"""),

59: _("""
            Liste des mots-clés : %(k1)s
              Liste des valeurs : %(k2)s
   Liste des mots-clés facteurs : %(k3)s

"""),




71: _("""
 Dérivation par rapport au paramètre sensible : %(k1)s 
"""),

72: _("""
 Le résultat est insensible au paramètre %(k1)s.
"""),

73: _("""
 Le type de la dérivation est %(k1)s 
"""),

80: _("""
 mauvaise valeur pour %(k1)s : il doit etre positif ou nul mais on a donné %(i1)d 
"""),

81: _("""
 la structure nosimp est introuvable dans la memorisation inpsco
"""),

82: _("""
 choix=/prefixe/e/renc/remc/s svp et non pas %(k1)s.
"""),

83: _("""
 la chaine pref passe en argument est trop courte pour mettre la chaine prefix =  %(k1)s 
"""),

84: _("""
 mauvaise valeur pour %(k1)s : il faut entre %(i1)d et %(i2)d mais on a donné %(i3)d 
"""),

85: _("""
 probleme de declaration : la chaine %(k1)s est de longueur  %(i1)d 
 on veut y mettre %(k2)s de longueur  %(i2)d 
"""),

86: _("""
 choix=/e/l svp et non pas %(k1)s.
"""),

87: _("""
 La fonction numéro %(i1)d est deja memorisee sous le nom %(k1)s 
"""),

88: _("""
 La chaine %(k1)s est de longueur  %(i1)d 
 Pour un concept, pas plus de 8 caractères svp.
"""),

89: _("""
 Mauvaise valeur pour choix ; il faut 1,2 ou 3, mais pas  %(i1)d 
"""),

90: _("""
 La dérivée de %(k1)s par rapport à %(k2)s est déja nommée.
"""),

91: _("""
 Le pas de temps adaptatif n'est pas approprié pour le calcul de sensibilité
 par rapport au paramètre materiau 
"""),

92: _("""
 On ne peut pas dériver les concepts de type %(k1)s
"""),

93: _("""
 On ne peut pas dériver avec un vect_asse en entree de dyna_line_harm.
"""),

94: _("""
 La structure %(k1)s  apparait plusieurs fois en tant que derivée.
"""),

95: _("""
 Seuls sont possibles :
"""),

96: _("""
 Les sous-types de sensibilité pour l'influence de %(k1)s sont %(k2)s et %(k3)s
 C'est incohérent.
"""),

97: _("""
Mémorisation des données de la sensibilité :
   
   Les listes des mots-clés, des valeurs et mots-clés facteurs doivent etre de meme longueur.
"""),

98: _("""
 Dans le programme %(k1)s, le paramètre de sensibilité %(k2)s est introuvable.   
"""),

99: _("""
 Impossible de trouver un sous-type de sensibilité pour l'influence de %(k1)s 
"""),

}
