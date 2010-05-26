#@ MODIF recal0 Messages  DATE 26/05/2010   AUTEUR ASSIRE A.ASSIRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {
 1 : _("""

 <INFO> Fichier d'informations de MACR_RECAL

"""),

 2 : _("""Impossible d'importer le module as_profil ! Vérifier la variable
d'environnement ASTER_ROOT ou mettez à jour ASTK.
"""),

 3 : _("""Le logiciel Gnuplot ou le module python Gnuplot.py n'est pas disponible.
On desactive l'affichage des courbes par Gnuplot.
"""),

 4 : _("""Il n'y a pas de fichier .export dans le repertoire de travail !
"""),

 5 : _("""Il y a plus d'un fichier .export dans le repertoire de travail !
"""),

 6 : _("""Pour les calculs DISTRIBUES en mode INTERACTIF, il faut spécifier une valeur pour 'mem_aster' 
(menu Option de ASTK) pour limiter la mémoire allouée au calcul maître.
"""),

 7 : _("""Aucune valeur pour le paramètre 'memjeveux'. Verifier le .export.
"""),

 8 : _("""Vérifier les valeurs des paramètres 'mem_aster' et 'memjeveux'.
"""),

 9 : _("""Information : les calculs esclaves utiliseront : %(r1).1f Mega Mots.
"""),

10 : _("""Pour l'algorithme %(k1)s, on ne peut tracer qu'à la derniere itération.
"""),

11 : _("""Pour l'algorithme %(k1)s, on ne tient pas compte des bornes sur les paramètres.
"""),

12 : _("""Recalage :
   %(k1)s
"""),

13 : _("""Lancement de l'optimisation avec la methode : %(k1)s.
"""),

14 : _("""Les dérivées sont calculées par Aster.
"""),

15 : _("""Les dérivées sont calculées par l'algorithme.
"""),

16 : _("""
--> Calcul du gradient par differences finies <--
"""),

17 : _("""Tracé des graphiques
"""),

18 : _("""Erreur dans l'algorithme de bornes de MACR_RECAL.
"""),

19 : _("""Erreur dans le test de convergence de MACR_RECAL.
"""),

#20 : _(""" """),

#21 : _(""" """),

#22 : _(""" """),

23 : _("""Impossible d'importer le module de lecture des tables !
"""),

24 : _("""Impossible de récupérer les résultats de calcul esclave (lecture des tables) ! 
Message d'erreur :
   %(k1)s
"""),

25 : _("""
Calcul de F avec les paramètres :
     %(k1)s
"""),

26 : _("""
Calcul de F et G avec les paramètres :
     %(k1)s
"""),

27 : _("""
Calcul de G avec les paramètres :
   %(k1)s
"""),

28 : _("""--> Mode de lancement BATCH impossible sur : %(k1)s, on bascule en INTERACTIF <--
"""),

29 : _("""--> Mode de lancement des calculs esclaves : %(k1)s <--
"""),

30 : _("""
Informations de convergence :
======================================================================
"""),

31 : _("""Iteration %(i1)d :

"""),

32 : _("""
=> Paramètres :
     %(k1)s

"""),

33 : _("""=> Fonctionnelle                        = %(r1)s
"""),

34 : _("""=> Residu                               = %(r1)s
"""),

35 : _("""=> Norme de l'erreur                    = %(r1)s
"""),

36 : _("""=> Erreur                               = %(r1)s
"""),

37 : _("""=> Variation des parametres (norme L2)  = %(r1)s
"""),

38 : _("""=> Variation de la fonctionnelle        = %(r1)s
"""),

39 : _("""=> Nombre d'evaluation de la fonction   = %(k1)s
"""),

#40 : _(""" """),

41 : _("""Tracé des courbes dans le fichier : %(k1)s
"""),

42 : _("""Problème lors de l'affichage des courbes. On ignore et on continue.
Erreur :
   %(k1)s
"""),

43 : _("""Erreur :
   %(k1)s
"""),

44 : _("""Problème de division par zéro dans la normalisation de la fonctionnelle.
Une des valeurs de la fonctionnelle initiale est nulle ou inférieure à la précision machine : %(r1).2f
"""),

45 : _("""Probleme de division par zéro dans le calcul de la matrice de sensiblité.
Le parametre %(k1)s est nul ou plus petit que la précision machine.
"""),

46 : _("""Le parametre %(k1)s est en butée sur un bord du domaine admissible.
"""),

47 : _("""Les parametres %(k1)s sont en butée sur un bord du domaine admissible.
"""),

48 : _("""Problème lors de l'interpolation du calcul dérivé sur les données expérimentale !
Valeur à interpoler              :  %(k1)s
Domaine couvert par l'experience : [%(k2)s : %(k3)s]
"""),

#49 : _(""" """),

50 : _("""
--> Critere d'arret sur le residu atteint, la valeur du residu est : %(r1)s <--
"""),

51 : _("""
--> Critere d'arret TOLE_PARA atteint, la variation des parametres est : %(r1)s <--
"""),

52 : _("""
--> Critere d'arret TOLE_FONC atteint, la variation de la fonctionnelle est : %(r1)s <--
"""),

53 : _("""
--> Arret par manque de temps CPU <--
"""),

54 : _("""
--> Le nombre maximum d'evaluations de la fonction (ITER_FONC_MAXI) a ete atteint <--
"""),

55 : _("""
--> Le nombre maximum d'iterations de l'algorithme (ITER_MAXI) a ete atteint <--
"""),

56 : _("""
======================================================================
                       CONVERGENCE ATTEINTE

"""),

57 : _("""
======================================================================
                      CONVERGENCE NON ATTEINTE

"""),

58 : _("""
                 ATTENTION : L'OPTIMUM EST ATTEINT AVEC
                 DES PARAMETRES EN BUTEE SUR LE BORD
                     DU DOMAINE ADMISSIBLE
"""),

#59 : _(""" """),

60 : _("""
Valeurs propres du Hessien:
%(k1)s
"""),

61 : _("""
Vecteurs propres associés:
%(k1)s
"""),

62 : _("""

              --------

"""),

63 : _("""
On peut en déduire que :

"""),

64 : _("""
Les combinaisons suivantes de paramètres sont prépondérantes pour votre calcul :

"""),

65 : _("""%(k1)s
      associée à la valeur propre %(k2)s

"""),

66 : _("""
Les combinaisons suivantes de paramètres sont insensibles pour votre calcul :

"""),

67 : _("""
Calcul avec les parametres suivants (point courant) :
     %(k1)s
"""),

68 : _("""
Calcul avec les parametres suivants (pertubation du parametre %(k2)s pour le gradient) :
     %(k1)s
"""),


69 : _("""
Information : les calculs esclaves seront lancés en BATCH avec les paramètres suivants :
     Temps   (sec) : %(k1)s
     Mémoire (Mo)  : %(k2)s
     Classe        : %(k3)s

"""),

70 : _("""
Erreur! Le calcul esclave '%(k1)s' ne s'est pas arrete correctement!
L'output du job est : %(k2)s
Il est recopié ci-dessous.
"""),

71 : _("""
Erreur! Au moins un calcul esclave ne s'est pas arreté correctement! Vérifier le repertoire : %(k1)s
"""),

72 : _("""
Fonctionnelle au point X0:
     %(k1)s
"""),

73 : _("""
Gradient au point X0:
"""),

74 : _("""
Calcul numero:  %(k1)s - Diagnostic: %(k2)s
"""),

#75 : _(""" """),

#76 : _(""" """),

#77 : _(""" """),

#78 : _(""" """),

79 : _("""

======================================================================
"""),

80 : _("""======================================================================


"""),

81 : _("""Repertoire contenant les executions Aster :
   %(k1)s
"""),

82 : _("""Impossible de creer le repertoire temporaire : %(k1)s
"""),

#83 : _(""" """),

#84 : _(""" """),

#85 : _(""" """),

#86 : _(""" """),

#87 : _(""" """),

#88 : _(""" """),

#89 : _(""" """),

#90 : _(""" """),

#91 : _(""" """),

#92 : _(""" """),

#93 : _(""" """),

#94 : _(""" """),

#95 : _(""" """),

#96 : _(""" """),

#97 : _(""" """),

#98 : _(""" """),

99 : _("""Impossible de déterminer l'emplacement d'Aster !
Fixer le chemin avec la variable d'environnement ASTER_ROOT.
"""),

}
