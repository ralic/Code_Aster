#@ MODIF supervis Messages  DATE 16/10/2007   AUTEUR REZETTE C.REZETTE 
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

2: _("""
 Arret sur erreur(s) utilisateur
"""),

3: _("""
 Erreur programmeur : JEMARQ/JEDEMA non appariés.
"""),

4: _("""
 Commande n  %(k1)s  :  "%(k2)s"  :   %(k3)s  erreur(s) détectée(s)
"""),

5: _("""
 Erreur(s) à l'exécution de "%(k1)s" : arret immédiat du programme.
"""),

6: _("""
 Fin à la suite de message(s) <E>
"""),

7: _("""
 Le concept " %(k1)s " est inconnu.
 Il n'est ni parmi les créés, ni parmi ceux à créer.
"""),

8: _("""
 Un nom de concept intermédiaire doit commencer par '.' ou '_' et non :  %(k1)s
"""),

9: _("""
 Longueur nulle
"""),

10: _("""
   - le concept  "%(k1)s" est détruit des bases de données.
"""),

11: _("""
 Impossible d'allouer la mémoire JEVEUX demandée
"""),

12: _("""
 Exécution de JEVEUX en mode DEBUG
"""),

13: _("""
  %(k1)s  nom de base déjà définie
"""),

14: _("""
  %(k1)s  statut impossible pour la base globale
"""),

15: _("""
 Problème d'allocation des bases de données
"""),

16: _("""
  Ecriture des catalogues dans ELEMBASE faite.
"""),

17: _("""
 Relecture des catalogues dans ELEMBASE faite.
"""),

18: _("""
  Trop de catalogues (maximum = 10)
"""),

19: _("""
 Debut de lecture
"""),

20: _("""
  "%(k1)s" argument invalide du mot clé "FICHIER" du mot clé facteur "CATALOGUE"
"""),

21: _("""
  Erreur(s) fatale(s) lors de la lecture des catalogues
"""),

22: _("""
L'argument du mot cle "NOM" sous le mot clé facteur "CODE" est tronqué à 8 caractères.
Le nom de code est donc "%(k1)s".
"""),

23: _("""
 Debug JXVERI demandé
"""),

24: _("""
 Debug SDVERI demandé
"""),

25: _("""
 Mémoire gestion : "COMPACTE"
"""),

26: _("""
 Type allocation memoire 2
"""),

27: _("""
 Type allocation memoire 3
"""),

28: _("""
 Type allocation memoire 4
"""),

29: _("""
 Trop de noms définis dans la liste argument de "FICHIER"
"""),

30: _("""
  %(k1)s est déjà (re-) défini
"""),

31: _("""
 Valeur invalide pour le mot clé RESERVE_CPU
"""),

32: _("""
 La procédure "%(k1)s" ne peut etre appelée en cours d'exécution des commandes
"""),

33: _("""
 Erreur fatale  **** appel à commande "superviseur".
"""),

34: _("""
 Arret de la lecture des commandes.
"""),

35: _("""
 La procédure "RETOUR" ne peut etre utilisée dans le fichier principal de commandes.
"""),

36: _("""
 Le concept de nom '%(k1)s' n'existe pas
"""),

38: _("""
 Il n'y a plus de temps pour continuer
"""),

39: _("""
 Arret de l'exécution et fermeture des bases jeveux
"""),

40: _("""
 Vous utilisez une version dont les routines suivantes ont été surchargées :
   %(ktout)s
"""),

41 : _("""
Le message d'alarme '%(k1)s' a été émis %(i1)d fois, il ne sera plus affiché.
"""),

43: _("""
 Debug SDVERI suspendu
"""),

44: _("""
 Debug JEVEUX demandé
"""),

45: _("""
 Debug JEVEUX suspendu
"""),

47: _("""
 Debug JXVERI suspendu
"""),

48: _("""
 Debug IMPR_MACRO demandé
"""),

49: _("""
 Debug IMPR_MACRO suspendu
"""),

50: _("""
 la commande a un numéro non appelable dans cette version.
 le numero erroné est  %(i1)d 
"""),

52: _("""
 fin de lecture (durée  %(r1)f  s.) %(k1)s 
"""),

53: _("""
 vous ne pouvez utiliser plus de  %(i1)d 
 niveaux de profondeur pour des appels par la procédure %(k1)s 
"""),

55: _("""
 Appels récursifs de messages d'erreur ou d'alarme.
"""),

56: _("""
 Incohérence entre le catalogue et le corps de la macro.
"""),

57: _("""
   Impossible d'importer '%(k1)s' dans Messages.
   Le fichier %(k1)s.py n'existe pas dans le répertoire 'Messages'
   ou bien la syntaxe du fichier est incorrecte.
   
   Merci de signaler cette anomalie.
   
   Traceback :
   %(k2)s
"""),

58: _("""
 valeur initiale du temps CPU maximum =   %(i1)d secondes 
"""),

59: _("""
 valeur du temps CPU maximum passé aux commandes =   %(i1)d secondes 
"""),

60: _("""
 la procédure a un numéro non appelable dans cette version.
 le numero errone est  %(i1)d 
"""),

61: _("""
  La commande a un numéro non appelable dans cette version
  Le numéro erroné est : %(i1)d 
"""),

62: _("""
  Les messages d'erreurs précédent concerne la commande :
"""),

63: _("""
     ARRET PAR MANQUE DE TEMPS CPU
     Les commandes suivantes sont ignorees, on passe directement dans FIN
     La base globale est sauvegardee
     Temps consomme de la reserve CPU        :  %(r1).2f s\n
"""),

64: _("""
  Réserve CPU prévue = %(i1)d secondes
"""),

65: _("""
   %(k1)s   %(k2)s   %(k3)s   %(k4)s
"""),
 
66: _("""
   %(k1)s   %(k2)s   %(k3)s   %(k4)s   %(k5)s
"""),
 
67: _("""
 Passage numéro %(i1)d
"""),
 
68: _("""
 information sur les concepts devant etre créés.
"""),
 
69: _("""
   %(k1)s   %(k2)s   %(k3)s   %(k4)s
"""),
 
70: _("""
   %(k1)s   %(k2)s   %(k3)s   %(k4)s   %(k5)s
"""),
 
71: _("""
 rappel sur les executions précédentes
   - il a ete executé %(i1)d procédures et opérateurs.
"""),
 
72: _("""
   - l'execution précédente s'est terminée correctement.
"""),
 
73: _("""
 
   - l'execution précédente s'est terminée en erreur dans la procédure %(k1)s.
"""),
 
74: _("""
 
   - l'execution précédente s'est terminée en erreur dans l'opérateur %(k1)s.
"""),
 
75: _("""
     le concept %(k1)s de type %(k2)s  est peut-etre errone.
"""),
 
76: _("""
   - l'execution precedente s'est terminee prematurement dans l'operateur %(k1)s.
"""),
 
77: _("""
     le concept %(k1)s de type %(k2)s  a ete néanmoims validé par l'opérateur
"""),
 
78: _("""
     Message attache au concept  %(k1)s
"""),
 
79: _("""
     Pas de message attache au concept %(k1)s
"""),
 
80: _("""
 
"""),
 
81: _("""
 %(k1)s nom symbolique inconnu
  - nombre de valeurs attendues %(i1)d
  - valeurs attendues : %(k1)s, %(k2)s,...
"""),
 
82: _("""
 L'argument du mot cle "CAS"  est errone.
 Valeur lue %(k1)s
 nombre de valeurs attendues %(i1)d
 valeurs attendues : %(k1)s,%(k2)s, ...
"""),
 
83: _("""
 
 le nombre d'enregistrements (nmax_enre) et leurs longueurs (long_enre) conduisent a un
fichier
 dont la taille maximale en octets (%(i1)d) est superieure a limite autorisee :  %(i2)d
 
"""),
 
84: _("""
 Nom symbolique errone pour un fichier de sortie
 Valeur lue %(k1)s
 - nombre de valeurs attendues %(i2)d
 - valeurs attendues           %(k2)s, %(k3)s
 
"""),

85: _("""
 information sur les concepts existants.
"""),

86: _("""
 Erreur a la relecture du fichier pick.1 : aucun objet sauvegardé ne sera récupéré.
"""),

87: _("""
Types incompatibles entre glob.1 et pick.1 pour le concept de nom %(k1)s.
"""),

88: _("""
Concept de nom %(k1)s et de type %(k2)s introuvable dans la base globale"
"""),

}
