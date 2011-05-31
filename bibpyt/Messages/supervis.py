#@ MODIF supervis Messages  DATE 30/05/2011   AUTEUR COURTOIS M.COURTOIS 
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

def _(x) : return x

cata_msg={

1 : _("""
 L'utilisation du mot-clé PAR_LOT='NON' permet d'accéder en lecture et en écriture
 au contenu des concepts Aster. De ce fait, votre étude est exclue du périmètre
 qualifié de Code_Aster puisque toutes ses étapes ne peuvent être certifiées.

 Conseils :
   - Il n'y a pas particulièrement de risque de résultat faux... sauf si votre
     programmation l'introduit.
   - Distinguez le calcul lui-même (qui doit sans doute passer en PAR_LOT='OUI')
     des post-traitements (qui nécessiteraient le mode PAR_LOT='NON') qui peuvent
     être réalisés en POURSUITE.
"""),

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
 Impossible d'allouer la mémoire JEVEUX demandée : %(i1)d Moctets.

 En général, cette erreur se produit car la mémoire utilisée hors du fortran
 (jeveux) est importante.

 Causes possibles :
   - le calcul produit de gros objets Python dans une macro-commande ou
     dans le jeu de commande lui-même,
   - le calcul appelle un solveur (MUMPS par exemple) ou un outil externe
     qui a besoin de mémoire hors jeveux,
   - utilisation de jeveux dynamique,
   - ...

 Solution :
   - distinguer la mémoire limite du calcul (case "Mémoire totale" de astk)
     de la mémoire réservée à jeveux (case "dont Aster"), le reste étant
     disponible pour les allocations dynamiques.
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

# on ne veut pas émettre d'alarme mais que le message se voit, donc on fait la mise en forme ici !
22 : _("""
   !---------------------------------------------------------------------------------------!
   !                                                                                       !
   ! Les mot-clés facteurs CODE et DEBUG dans DEBUT/POURSUITE sont réservés aux cas-tests. !
   ! Il ne faut pas les utiliser dans les études car ils modifient certaines valeurs par   !
   ! défaut des commandes DEBUT/POURSUITE qui ont des conséquences sur le comportement     !
   ! en cas d'erreur ou sur les performances.                                              !
   !                                                                                       !
   !---------------------------------------------------------------------------------------!
"""),

23: _("""
 Debug JXVERI demandé
"""),

24: _("""
 Debug SDVERI demandé
"""),

25: _("""
 Mémoire gestion : "COMPACTE"
 Ce mode de gestion peut augmenter sensiblement le temps système de certaines commandes,
 les lectures/écritures sur les bases Jeveux étant beaucoup plus fréquentes
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

36: _("""
 Le concept de nom '%(k1)s' n'existe pas
"""),

38: _("""
 Il n'y a plus de temps pour continuer
"""),

39: _("""
Arrêt de l'exécution suite à la réception du signal utilisateur USR1.
Fermeture des bases jeveux afin de permettre la POURSUITE ultérieure du calcul.
"""),

40: _("""
 Vous utilisez une version dont les routines suivantes ont été surchargées :
   %(ktout)s
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

56: _("""
 Incohérence entre le catalogue et le corps de la macro.
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
     Les commandes suivantes sont ignorées, on passe directement dans FIN
     La base globale est sauvegardée
     Temps consommé de la réserve CPU        :  %(r1).2f s\n
"""),

64: _("""
  Valeur initiale du temps CPU maximum =   %(i1)d secondes
  Valeur du temps CPU maximum passé aux commandes =   %(i2)d secondes
  Réserve CPU prévue = %(i3)d secondes
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

71: _("""
 rappel sur les exécutions précédentes
   - il a été executé %(i1)d procédures et opérateurs.
"""),

72: _("""
   - l'exécution précédente s'est terminée correctement.
"""),

73: _("""

   - l'exécution précédente s'est terminée en erreur dans la procédure %(k1)s.
"""),

74: _("""

   - l'exécution précédente s'est terminée en erreur dans l'opérateur %(k1)s.
"""),

75: _("""
     le concept %(k1)s de type %(k2)s  est peut-être erroné.
"""),

76: _("""
   - l'exécution précédente s'est terminée prématurément dans l'opérateur %(k1)s.
"""),

77: _("""
     le concept %(k1)s de type %(k2)s  a été néanmoims validé par l'opérateur
"""),

78: _("""
     Message attaché au concept  %(k1)s
"""),

79: _("""
     Pas de message attaché au concept %(k1)s
"""),

80: _("""

"""),

81: _("""
 %(k1)s nom symbolique inconnu
  - nombre de valeurs attendues %(i1)d
  - valeurs attendues : %(k1)s, %(k2)s,...
"""),

82: _("""
 L'argument du mot cle "CAS" est erroné.
 Valeur lue %(k1)s
 nombre de valeurs attendues %(i1)d
 valeurs attendues : %(k1)s,%(k2)s, ...
"""),

83: _("""

 Le nombre d'enregistrements (nmax_enre) et leurs longueurs (long_enre) conduisent à un
 fichier dont la taille maximale en Moctets (%(i1)d) est supérieure à limite autorisée :  %(i2)d

 Vous pouvez augmenter cette limite en utilisant l'argument "-max_base" sur la ligne
 de commande suivi d'une valeur en Moctets.

"""),

84: _("""
 Nom symbolique erroné pour un fichier de sortie
 Valeur lue %(k1)s
 - nombre de valeurs attendues %(i2)d
 - valeurs attendues           %(k2)s, %(k3)s

"""),

85: _("""
 information sur les concepts existants.
"""),

86: _("""
 Erreur à la relecture du fichier pick.1 : aucun objet sauvegardé ne sera récupéré.
"""),

87: _("""
Types incompatibles entre glob.1 et pick.1 pour le concept de nom %(k1)s.
"""),

88: _("""
Concept de nom %(k1)s et de type %(k2)s introuvable dans la base globale"
"""),

89: _("""
 Il n'y a pas de fichier glob.1 ou bhdf.1 dans le répertoire courant.

Conseils:
   - Vérifiez que vous avez une base (de type base ou bhdf) dans votre étude.
   - Vérifiez si elle doit être décompressée ou pas.
"""),

93 : _("""
La variable python "%(k1)s" fait référence au concept "%(k2)s".
Cela se produit avec ce type d'enchainement :
   %(k2)s = COMMANDE(...)
   %(k1)s = %(k2)s

On détruit cette variable ("%(k1)s" dans l'exemple ci-dessus).

-> Conseil :
   Pour éviter cette alarme, supprimer l'alias dans le jeu de commandes
   qui produit la base :
      del %(k1)s
"""),

94 : _("""
Le temps CPU system (%(r1).1f) atteint une valeur supérieure à %(i1)d%%
du temps CPU (%(r2).1f).
Ce comportement est peut-être anormal.
Le nombre d'appel au mécanisme de déchargement de la mémoire depuis le début du
calcul est de %(i2)d.

-> Conseil :
   Augmenter la mémoire JEVEUX peut permettre de diminuer le temps système.

"""),

95 : _("""
Le temps CPU system (%(r1)f) atteint une valeur supérieure à %(i1)d%% du temps CPU (%(r2)f).
Ce comportement est peut-être anormal.

"""),

# on ne veut pas émettre d'alarme mais que le message se voit, donc on fait la mise en forme ici !
96 : _("""

   !--------------------------------------------------------------!
   !                                                              !
   ! Réception du signal USR1. Interruption du calcul demandée... !
   !                                                              !
   !--------------------------------------------------------------!

"""),

97 : _("""
   !---------------------------------------------------------------!
   !                                                               !
   ! Interruption du calcul suite à la réception d'un <Control-C>. !
   !                                                               !
   !---------------------------------------------------------------!

"""),

}
