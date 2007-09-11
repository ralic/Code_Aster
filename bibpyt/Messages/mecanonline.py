#@ MODIF mecanonline Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
# ============================ ==========================================
def _(x) : return x

cata_msg={


1: _("""
 Le champ absolu n'est accessible pour OBSERVATION qu'en présence de modes statiques
"""),

2: _("""
 Melange de champs de nature différente dans le meme mot-clé facteur OBSERVATION
"""),

3: _("""
 Il y a trop de colonnes d'affichage (on est limité à quinze)
"""),

4: _("""
 Option non prevue pour la matrice de rigidité
"""),

5: _("""
 Le type de sélection du coefficient de pilotage est inconnu
"""),

6: _("""
 Les variables de commandes initiales induisent des contraintes incompatibles
"""),

7: _("""
 Le champ de déplacement Dirichlet differentiel n'est pas trouvé dans le concept EVOL_NOLI  %(k1)s
"""),

8: _("""
 Le criteère de convergence choisi est lache
 risque de résultats faux
"""),

9: _("""
 On surcharge un résultat sans définir d'état initial (pas d'ETAT_INIT)
 on suppose un état initial nul
"""),

10: _("""
 Le concept dans ETAT_INIT n'est pas du type EVOL_NOLI
"""),

11: _("""
 Pas de numéro d'ordre trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

12: _("""
 L'instant spécifié sous ETAT_INIT n'est pas trouvé
"""),

13: _("""
 Plusieurs instants correspondent à celui spécifié sous ETAT_INIT
"""),

14: _("""
 La derivee de  %(k1)s  par rapport a  %(k2)s  est introuvable
"""),

15: _("""
 Le champ de déplacement DEPL_R (ou sa dérivée) n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

16: _("""
 Pour faire une reprise avec un calcul de sensibilité,
 il faut obligatoirement renseigner EVOL_NOLI dans ETAT_INIT
"""),

17: _("""
 Le champ de contraintes SIEF_R (ou sa dérivée) n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

18: _("""
 Le champ de variables internes VARI_R (ou sa dérivée) n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

19: _("""
 Le champ de variables non locales VARI_NONL (ou sa dérivée) n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

20: _("""
 Le champ de Lagrangiens non locaux LANL_ELGA n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
"""),

21: _("""
 Le champ de vitesses VITE n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
 on crée un champ de vitesses nulles
"""),

22: _("""
 L'etat initial n'appartient pas à un EVOL_NOLI :
 on suppose qu'on part d'un état a vitesses nulles
"""),

23: _("""
 Le champ d'accélérations ACCE n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
 on calcule un champ d'accélérations, ce qui est possible puisque les vitesses sont nulles
"""),

24: _("""
 Le champ d'accélérations ACCE n'est pas trouvé dans le concept EVOL_NOLI nommé %(k1)s
 on ne peut pas, pour l'instant, calculer le champ des accélérations car les vitesses ne sont pas nulles
"""),

25: _("""
 Le paramètre REAC_INCR est négatif
"""),

26: _("""
 Le paramètre REAC_ITER est négatif
"""),

27: _("""
 Le parametre REAC_ITER_ELAS est negatif
"""),

28: _("""
 Il faut préciser un concept de type EVOL_NOLI dans le mot-clé facteur ETAT_INIT
 lorsque l'on fait une prédiction de type DEPL_CALCULE
"""),

29: _("""
 La définition des paramètres de recherche linéaire RHO_MIN et RHO_EXCL est contradictoire
 on prend l'intervalle [RHO_MIN,RHO_EXCL]
"""),

30: _("""
 La definition des paramètres de recherche linéaire RHO_MAX et RHO_EXCL est contradictoire
 on prend l'intervalle [-RHO_EXCL,RHO_MAX]
"""),

31: _("""
 Le paramètre de pilotage ETA_PILO_MAX doit etre inférieur à ETA_PILO_R_MAX
"""),

32: _("""
 Le paramètre de pilotage ETA_PILO_MIN doit etre supérieur à ETA_PILO_R_MIN
"""),

33: _("""
 Il faut au plus UN noeud pour le pilotage de type DDL_IMPO
"""),

34: _("""
 Il faut au plus UN groupe de noeuds pour le pilotage de type DDL_IMPO
"""),

35: _("""
 Il y a plus d'un noeud dans le groupe de noeuds définissant le pilotage de type DDL_IMPO
"""),

36: _("""
 Il faut préciser un groupe de noeuds dans la méthode de pilotage de type LONG_ARC
"""),

37: _("""
 Le groupe de noeud nommé %(k1)s que l'on utilise pour le pilotage de type LONG_ARC est vide
"""),

38: _("""
 La liste de composantes NOM_CMP que l'on utilise pour le pilotage de type LONG_ARC est vide
"""),

39: _("""
 On fait un calcul de flambement non linéaire avec l'hypothèse des petites perturbations (HPP)
"""),

40: _("""
 On fait un calcul de flambement non linéaire avec l'hypothèse des déformations de type GREEN
"""),

41: _("""
 Les déformations de type %(k1)s sont incompatibles avec le calcul du flambement
"""),

42: _("""
 Pour le traitement du contact avec X-FEM, le solveur MUMPS est vivement recommandé
"""),

43: _("""
 Contact et pilotage sont des fonctionnalités incompatibles
"""),

44: _("""
 Contact et recherche linéaire peuvent poser des problèmes de convergence
"""),

45: _("""
 Contact méthode continue et recherche linéaire sont incompatibles
"""),

46: _("""
 Le contact avec le solveur GCPC n'est pas disponible.
"""),

47: _("""
 Liaison unilatérale et pilotage sont des fonctionnalités incompatibles
"""),

48: _("""
 Liaison unilatérale et recherche linéaire peuvent poser des problèmes de convergence
"""),

49: _("""
 Le dénominateur est nul lors du calcul du paramètre de pilotage
"""),

50: _("""
 La lecture du champ DEPL_CALCULE est impossible
"""),

51: _("""
 La prediction par extrapolation est impossible : l'increment de temps initial est nul
"""),

52: _("""
 Le parametre de recherche linéaire ITER_LINE_MAXI doit etre inférieur à 1000
"""),

53: _("""
 Le champ de température est une grandeur inconnue (ni réelle, ni fonction)
"""),

54: _("""
 Erreur dans la découpe initiale des pas
"""),

55: _("""
 Attention, ARRET=NON donc poursuite du calcul sans avoir eu convergence
 risque de résultats faux
"""),

56: _("""
 Le nom du champ NOM_CHAMP de la variable de commande est obligatoire pour le découpage
"""),

57: _("""
 Le nom de la composante NOM_CMP du champ NOM_CHAMP de la variable de commande est obligatoire
"""),

58: _("""
 La valeur VALE du critère est obligatoire pour le découpage
"""),

59: _("""
 Cette loi de comportement n'est pas disponible pour le pilotage de type PRED_ELAS
"""),

60: _("""
 Le pilotage de type PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX pour la loi ENDO_ISOT_BETON
"""),

61: _("""
 Le pilotage de type PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX pour la loi ENDO_ORTH_BETON
"""),

62: _("""
 La valeur de NUME_SUIVI est incorrecte dans SUIVI_DDL
"""),

63: _("""
 Le ddl est inconnu sur le noeud ou la maille specifiee pour SUIVI_DDL
"""),

64: _("""
 Le parametre ITER_DUAL_MAXI est trop eleve (il doit etre inferieur a 10000)
"""),

65: _("""
 La fonction duale dans le lagrangien non local est non convexe
"""),

66: _("""
 Problème lors de la recherche linéaire dans le lagrangien non local
"""),

67: _("""
 Il y a plusieurs charges contenant un chargement de type FORCE_FLUIDE
"""),

68: _("""
 Impossible de faire du pilotage en contraintes planes
"""),

69: _("""
 Problème rencontré :
   la matrice de masse est non inversible.
   On ne peut donc pas s'en servir pour calculer l'accélération initiale.
   => on initialise l'accélération à zéro.

 Conseils :
   Avez-vous bien affecté une masse sur tous les éléments ?
"""),

70: _("""
 On detecte une divergence, on force la subdivision du pas de temps
"""),

71: _("""
 Nombre maximum d'iterations atteint sans convergence, on force la subdivision du pas de temps
"""),

72: _("""
 Le pas minimal de la subdivision est atteint. On ne peut diviser plus.
"""),

73: _("""
 Le contact avec le solveur MUMPS n'est pas disponible.
"""),

74: _("""
 Liaison unilaterale avec le solveur GCPC n'est pas disponible.
"""),

75: _("""
 Liaison unilaterale avec le solveur MUMPS n'est pas disponible.
"""),

76: _("""
 L'etat initial n'appartient pas a un EVOL_NOLI : on suppose qu'on part d'un etat a deplacements nuls
"""),

77: _("""
 Vous faites une reprise de calcul avec PILOTAGE en longueur d'arc et avec l'option ANGL_INCR_DEPL mais il n'y pas assez d'informations dans
 la structure de donnees resultats. Il vous faut en effet au moins les deux derniers champs deplacements solutions.
 Changer l'option de PILOTAGE (utilisez NORM_INCR_DEPL) ou refaites le premier calcul pour enrichir la SD resultat (modifiez vos options d'ARCHIVAGE).
"""),

78: _("""
 Problème rencontré :
   la matrice de masse est quasi-singulière.
   On se sert de cette matrice pour calculer l'accélération initiale.
   => l'accélération initiale calculée est peut etre excessive en quelques noeuds.

 Conseils :
   Ces éventuelles perturbations initiales sont en général sans influence sur
   la suite du calcul car elles sont localisées.
   Néanmoins, il peut etre bénéfique de laisser ces perturbations s'amortir au
   début du calcul en faisant plusieurs pas avec chargement transitoire nul,
   avec, eventuellement, un schéma d'integration choisi volontairement très
   dissipatif (par exemple HHT avec alpha=-0.3).
   On peut ensuite reprendre en poursuite avec un schéma moins dissipatif si besoin est.
"""), 

79: _("""
   Arret par manque de temps CPU au numéro d'instant : %(i1)d
                                 lors de l'itération : %(i2)d
      - Temps moyen par itération : %(r1)f
      - Temps cpu restant         : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

80: _("""
   Arret par manque de temps CPU au numéro d'instant : %(i1)d
      - Temps moyen par %(k1)s : %(r1)f
      - Temps cpu restant      : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

81: _("""
   Echec dans la recherche linéaire. Contactez les développeurs.
"""),

82: _("""
   Arret pour cause de matrice non inversible.
"""),

83: _("""
   Arret : absence de convergence avec le nombre d'itérations requis.
"""),

84: _("""
   Arret par échec dans le pilotage.
"""),

85: _("""
   Arret : absence de convergence au numéro d'instant : %(i1)d
                                  lors de l'itération : %(i2)d
"""),

86: _("""
    Erreur dans la gestion des erreurs. Contactez les développeurs.
"""),

87: _("""
    Recherche linéaire non favorable. Rho forcé à 1.
"""),

}
