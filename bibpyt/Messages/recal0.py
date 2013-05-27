# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {
 1 : _(u"""

 <INFO> Fichier d'informations de MACR_RECAL

"""),

 2 : _(u"""Impossible d'importer le module as_profil ! Vérifier la variable
d'environnement ASTER_ROOT ou mettez à jour ASTK.
"""),

 3 : _(u"""Le logiciel GNUPLOT ou le module python Gnuplot n'est pas disponible.
On désactive l'affichage des courbes par Gnuplot.
"""),

 4 : _(u"""Il n'y a pas de fichier .export dans le répertoire de travail !
"""),

 5 : _(u"""Il y a plus d'un fichier .export dans le répertoire de travail !
"""),

 6 : _(u"""Pour les calculs DISTRIBUES en mode INTERACTIF, il faut spécifier une valeur pour mem_aster
(menu Option de ASTK) pour limiter la mémoire allouée au calcul maître.
"""),

 7 : _(u"""Pour pouvoir lancer les calculs esclaves en MPI, le calcul maître doit être lancé 
en MPI sur un processeur."""),

 8 : _(u"""Vérifier les valeurs des paramètres mem_aster et memjeveux.
"""),

# 9 : _(u""" """),

10 : _(u"""Pour l'algorithme %(k1)s, on ne peut tracer qu'à la dernière itération.
"""),

11 : _(u"""Pour l'algorithme %(k1)s, on ne tient pas compte des bornes sur les paramètres.
"""),

12 : _(u"""Recalage :
   %(k1)s
"""),

13 : _(u"""Lancement de l'optimisation avec la méthode : %(k1)s.
"""),

14 : _(u"""Les dérivées sont calculées par Aster.
"""),

15 : _(u"""Les dérivées sont calculées par l'algorithme.
"""),

16 : _(u"""
--> Calcul du gradient par différences finies <--

"""),

17 : _(u"""Tracé des graphiques
"""),

18 : _(u"""Erreur dans l'algorithme de bornes de MACR_RECAL.
"""),

19 : _(u"""Erreur dans le test de convergence de MACR_RECAL.
"""),

23 : _(u"""Impossible d'importer le module de lecture des tables !
"""),

24 : _(u"""Impossible de récupérer les résultats de calcul esclave (lecture des tables) !
Message d'erreur :
   %(k1)s
"""),

25 : _(u"""
Calcul de F avec les paramètres :
     %(k1)s
"""),

26 : _(u"""
Calcul de F et G avec les paramètres :
     %(k1)s
"""),

27 : _(u"""
Calcul de G avec les paramètres :
   %(k1)s
"""),

28 : _(u"""
--> Mode de lancement BATCH impossible sur : %(k1)s, on bascule en INTERACTIF <--

"""),

29 : _(u"""
--> Mode de lancement des calculs esclaves : %(k1)s <--

"""),

30 : _(u"""
Informations de convergence :
======================================================================
"""),

31 : _(u"""Itération %(i1)d :

"""),

32 : _(u"""
=> Paramètres :
     %(k1)s

"""),

33 : _(u"""=> Fonctionnelle                        = %(r1)f
"""),

34 : _(u"""=> Résidu                               = %(r1)f
"""),

35 : _(u"""=> Norme de l'erreur                    = %(r1)f
"""),

36 : _(u"""=> Erreur                               = %(r1)f
"""),

37 : _(u"""=> Variation des paramètres (norme L2)  = %(r1)f
"""),

38 : _(u"""=> Variation de la fonctionnelle        = %(r1)f
"""),

39 : _(u"""=> Nombre d'évaluation de la fonction   = %(k1)s
"""),

#40 : _(u""" """),

41 : _(u"""Tracé des courbes dans le fichier : %(k1)s
"""),

42 : _(u"""Problème lors de l'affichage des courbes. On ignore et on continue.
Erreur :
   %(k1)s
"""),

43 : _(u"""Erreur :
   %(k1)s
"""),

44 : _(u"""Problème de division par zéro dans la normalisation de la fonctionnelle.
Une des valeurs de la fonctionnelle initiale est nulle ou inférieure à la précision machine : %(r1).2f
"""),

45 : _(u"""Problème de division par zéro dans le calcul de la matrice de sensibilité.
Le paramètre %(k1)s est nul ou plus petit que la précision machine.
"""),

46 : _(u"""Le paramètre %(k1)s est en butée sur un bord du domaine admissible.
"""),

47 : _(u"""Les paramètres %(k1)s sont en butée sur un bord du domaine admissible.
"""),

48 : _(u"""Problème lors de l'interpolation du calcul dérivé sur les données expérimentale !
Valeur à interpoler              :  %(k1)s
Domaine couvert par l'expérience : [%(k2)s : %(k3)s]
"""),

50 : _(u"""
--> Critère d'arrêt sur le résidu atteint, la valeur du résidu est : %(r1)f <--
"""),

51 : _(u"""
--> Critère d'arrêt TOLE_PARA atteint, la variation des paramètres est : %(r1)f <--
"""),

52 : _(u"""
--> Critère d'arrêt TOLE_FONC atteint, la variation de la fonctionnelle est : %(r1)f <--
"""),

53 : _(u"""
--> Arrêt par manque de temps CPU <--
"""),

54 : _(u"""
--> Le nombre maximum d'évaluations de la fonction (ITER_FONC_MAXI) a été atteint <--
"""),

55 : _(u"""
--> Le nombre maximum d'itérations de l'algorithme (ITER_MAXI) a été atteint <--
"""),

56 : _(u"""
======================================================================
                       CONVERGENCE ATTEINTE

"""),

57 : _(u"""
======================================================================
                      CONVERGENCE NON ATTEINTE

"""),

58 : _(u"""
                 ATTENTION : L'OPTIMUM EST ATTEINT AVEC
                 DES PARAMETRES EN BUTÉE SUR LE BORD
                     DU DOMAINE ADMISSIBLE
"""),

60 : _(u"""
Valeurs propres du Hessien:
%(k1)s
"""),

61 : _(u"""
Vecteurs propres associés:
%(k1)s
"""),

62 : _(u"""

              --------

"""),

63 : _(u"""
On peut en déduire que :

"""),

64 : _(u"""
Les combinaisons suivantes de paramètres sont prépondérantes pour votre calcul :

"""),

65 : _(u"""%(k1)s
      associée à la valeur propre %(k2)s

"""),

66 : _(u"""
Les combinaisons suivantes de paramètres sont insensibles pour votre calcul :

"""),

67 : _(u"""
Calcul avec les paramètres suivants (point courant) :
     %(k1)s
"""),

68 : _(u"""
Calcul avec les paramètres suivants (perturbation du paramètre %(k2)s pour le gradient) :
     %(k1)s
"""),


69 : _(u"""
Information : les calculs esclaves seront lancés en BATCH avec les paramètres suivants :
     Temps          : %(k1)s sec
     Mémoire totale : %(k2)s Mo
     dont Aster     : %(k3)s Mo
     Classe         : %(k4)s

"""),

72 : _(u"""
Fonctionnelle au point X0:
     %(k1)s
"""),

73 : _(u"""
Gradient au point X0:
"""),

74 : _(u"""
Calcul numéro:  %(k1)s - Diagnostic: %(k2)s
"""),

75 : _(u"""
                                    ----------------
                                      Informations

    Lors du calcul du gradient par différences finies, un paramètre perturbé sort de l'intervalle de validité :
        Paramètre                   : %(k1)s
        Paramètre perturbée         : %(k2)s
        Valeur minimale autorisée   : %(k3)s
        Valeur maximale autorisée   : %(k4)s

    --> On continue avec ce paramètre, mais l'étude esclave peut avoir des soucis.

    Pour information, voici le paramètre de perturbation (mot-clé PARA_DIFF_FINI), vérifier qu'il est suffisamment petit
    pour un calcul de gradient par différences finies :
        Paramètre de perturbation   : %(k5)s

                                    ----------------

"""),


76 : _(u"""
Le paramètre de perturbation (mot-clé PARA_DIFF_FINI) a pour valeur : %(k1)s

Vérifier qu'il est suffisamment petit pour un calcul de gradient par différences finies

--> On continue avec ce paramètre mais le calcul du gradient pourrait être faux.

"""),

#77 : _(u""" """),

#78 : _(u""" """),

79 : _(u"""

======================================================================
"""),

80 : _(u"""======================================================================


"""),

81 : _(u"""

Répertoire contenant les exécutions Aster :
   %(k1)s

"""),

82 : _(u"""Impossible de créer le répertoire temporaire : %(k1)s
"""),

83 : _(u"""
======================================================================

Erreur! Le calcul esclave '%(k1)s' ne s'est pas arrêté correctement!
Les fichiers output et error du job sont recopiés dans l'output du
maître juste au dessus de ce message.

L'output du job est également dans : %(k2)s

======================================================================
"""),

84 : _(u"""
Erreur! Au moins un calcul esclave ne s'est pas arrêté correctement! Vérifier le répertoire : %(k1)s
"""),

85 : _(u""" Erreur dans le calcul esclave:
   %(k1)s
"""),

86 : _(u"""
Erreur! Le calcul esclave '%(k1)s' n'a pas pu démarrer !
   Diagnostic : %(k2)s

Il s'agit vraisemblablement d'un problème de configuration du serveur de calcul ou de ressources disponibles.
Mettre UNITE_SUIVI et INFO=2 permettra d'avoir des messages supplémentaires dans l'output du maître.
"""),

#87 : _(u""" """),

#88 : _(u""" """),

#89 : _(u""" """),

#90 : _(u""" """),

#91 : _(u""" """),

#92 : _(u""" """),

#93 : _(u""" """),

#94 : _(u""" """),

#95 : _(u""" """),

#96 : _(u""" """),

#97 : _(u""" """),

#98 : _(u""" """),

99 : _(u"""Impossible de déterminer l'emplacement de Code_Aster !
Fixer le chemin avec la variable d'environnement ASTER_ROOT.
"""),

}
