#@ MODIF recal0 Messages  DATE 13/10/2009   AUTEUR COURTOIS M.COURTOIS 
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
 1 : _("""Nombre d'evaluation de la fonction : %(k1)s
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

 6 : _("""Il faut spécifier une valeur pour 'mem_aster' (menu Option de ASTK)
pour limiter la mémoire allouée au calcul maître.
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
   %s
"""),

13 : _("""Lancement de l'optimisation avec la methode : %(k1)s.
"""),

14 : _("""Les dérivées sont calculées par Aster.
"""),

15 : _("""Les dérivées sont calculées par l'algorithme.
"""),

16 : _("""
Informations de convergence :
=======================================================
Fin de l'iteration %(i1)d
=> Fonctionnelle = %(r1)s
=> Residu        = %(r2)s
=======================================================
"""),

17 : _("""Tracé des graphiques
"""),

18 : _("""Erreur dans l'algorithme de bornes de MACR_RECAL.
"""),

19 : _("""Erreur dans le test de convergence de MACR_RECAL.
"""),

20 : _("""Impossible d'importer le module de lecture des tables !
"""),

21 : _("""Diagnostic du calcul esclave : %(k1)s
"""),

22 : _("""Le fichier esclave ne s'est pas terminé correctement.
"""),

23 : _("""Le nombre d'évaluation de la fonctionnelle dépasse le critère ITER_FONC_MAXI.
"""),

24 : _("""Impossible de relire le fichier esclave : %(k1)s
"""),

25 : _("""Calcul de F avec les paramètres :
%(k1)s
"""),

26 : _("""Impossible de récupérer les résultats de calcul esclave !
"""),

27 : _("""Impossible de récupérer les résultats de calcul esclave (lecture des tables) ! 
Le calcul esclave n'a pas dû se terminer correctement (ajouter un repertoire dans ASTK
en Résultat avec le type repe et voir l'output du fichier esclave dans ce répertoire.
Message :
   %(k1)s
"""),

28 : _("""Impossible de récupérer les résultats de calcul esclave (lecture des tables) ! 
Message :
   %(k1)s
"""),

29 : _("""Impossible de récupérer les résultats de calcul esclave (récupération des tables) !
Message :
   %(k1)s
"""),

30 : _("""Informations de convergence :
=======================================================
"""),

31 : _("""=> Fonctionnelle     = %(k1)s
"""),

32 : _("""=> Norme de l'erreur = %(k1)s
"""),

33 : _("""=> Erreur            = %(k1)s
"""),

34 : _("""Impossible de creer le repertoire temporaire : %(k1)s
"""),

35 : _("""Problème lors de l'interpolation du calcul dérivé sur les données expérimentale !
Valeur à interpoler              :  %(k1)s
Domaine couvert par l'experience : [%(k2)s : %(k3)s]
"""),

36 : _("""Problème de division par zéro dans la normalisation de la fonctionnelle.
Une des valeurs de la fonctionnelle initiale est nulle ou inférieure à la précision machine : %(r1).2f
"""),

37 : _("""On utilise les differences finies pour calculer la sensibilite de : %(k1)s
"""),

38 : _("""Probleme de division par zéro dans le calcul de la matrice de sensiblité.
Le parametre %(k1)s est nul ou plus petit que la précision machine.
"""),

39 : _("""On utilise le calcul de SENSIBILITE pour : %(k1)s
"""),

40 : _("""Arrêt de MACR_RECAL par manque de temps CPU.
"""),

41 : _("""Tracé des courbes dans le fichier : %(k1)s
"""),

42 : _("""Problème lors de l'affichage des courbes. On ignore et on continue.
Erreur :
   %(k1)s
"""),



83 : _("""Variable d'environnement ASTER_ROOT absente, on essaiera avec 'as_run' dans le $PATH.
"""),

84 : _("""Impossible d'écrire le fichier export : %(k1)s
"""),

85 : _("""Ecriture du fichier : %(k1)s
"""),

86 : _("""Dans la commande MACR_RECAL, il faut choisir METHODE='EXTERNE'.
"""),

87 : _("""La commande MACR_RECAL n'a pas été trouvée dans le .comm
"""),

88 : _("""Il faut mettre les paramètres sous la forme d'une ligne python :
   %(k1)s = [param1, param2, ...]
"""),

89 : _("""Lecture du fichier : %(k1)s
"""),

90 : _("""Le mode EXTERNE tourne en mode dégradé. Lire la documentation.
"""),

91 : _("""Impossible d'importer le module Utilitai !
"""),

92 : _("""Le fichier de commande :
   %(k1)s
ne semble pas comporter la commande MACR_RECAL.
"""),

93 : _("""Paramètres d'entrée : %(k1)s
"""),

94 : _("""Impossible de lire le fichier d'entrée : %(k1)s
"""),

95 : _("""Lecture du fichier : %(k1)s
"""),

96 : _("""Le fichier .comm suivant n'est pas défini : %(k1)s
"""),

97 : _("""Le fichier .comm n'est pas défini dans le .export.
"""),

98 : _("""Impossible de déterminer le fichier .export à utiliser.
Spécifier le sur la ligne de commande.
"""),

99 : _("""Impossible de déterminer l'emplacement d'Aster !
Fixer le chemin avec la variable d'environnement ASTER_ROOT.
"""),

}
