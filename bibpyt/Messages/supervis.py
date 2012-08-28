#@ MODIF supervis Messages  DATE 27/08/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={

1 : _(u"""
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

3: _(u"""
 Erreur programmeur : %(k1)s non appariés.
"""),

8: _(u"""
 Un nom de concept intermédiaire doit commencer par '.' ou '_' et non :  %(k1)s
"""),

12: _(u"""
 Exécution de JEVEUX en mode DEBUG
"""),

13: _(u"""
  %(k1)s  nom de base déjà définie
"""),

14: _(u"""
  %(k1)s  statut impossible pour la base globale
"""),

15: _(u"""
 Problème d'allocation des bases de données
"""),

16: _(u"""
  Écriture des catalogues des éléments faite.
"""),

17: _(u"""
 Relecture des catalogues des éléments faite.
"""),

18: _(u"""
  Trop de catalogues (maximum = 10)
"""),

19: _(u"""
 Début de lecture..."""),

20: _(u"""
  "%(k1)s" argument invalide du mot clé "FICHIER" du mot clé facteur "CATALOGUE"
"""),

21: _(u"""
  Erreur(s) fatale(s) lors de la lecture des catalogues
"""),

22 : { 'message' : _(u"""
   Les mots-clés facteurs CODE et DEBUG dans DEBUT/POURSUITE sont réservés aux cas-tests.
   Il ne faut pas les utiliser dans les études car ils modifient certaines valeurs par
   défaut des commandes DEBUT/POURSUITE qui ont des conséquences sur le comportement
   en cas d'erreur ou sur les performances.
"""), 'flags' : 'DECORATED',
},

23: _(u"""
 Débogage JXVERI demandé
"""),

24: _(u"""
 Débogage SDVERI demandé
"""),

31: _(u"""
 Valeur invalide pour le mot clé RESERVE_CPU
"""),

32: _(u"""
 La procédure "%(k1)s" ne peut être appelée en cours d'exécution des commandes
"""),

38: _(u"""
 Il n'y a plus de temps pour continuer
"""),

39: _(u"""
Arrêt de l'exécution suite à la réception du signal utilisateur %(k1)s.
Fermeture des bases jeveux afin de permettre la POURSUITE ultérieure du calcul.
"""),

40: _(u"""
 Vous utilisez une version dont les routines suivantes ont été surchargées :
   %(ktout)s
"""),

41: _(u"""La version %(k1)s a été modifiée par %(i1)d révisions.
"""),

42: _(u"""Les fichiers suivants ont été modifiés par rapport à la dernière révision %(k1)s :

%(k2)s
"""),

43: _(u"""
 Débogage %(k1)s suspendu
"""),

44: _(u"""
 Débogage %(k1)s demandé
"""),

50: _(u"""
 la commande a un numéro non appelable dans cette version.
 le numéro erroné est  %(i1)d
"""),

52: _(u"""
 Fin de lecture (durée  %(r1)f  s.) %(k1)s
"""),

56: _(u"""
 Incohérence entre le catalogue et le corps de la macro-commande.
"""),

60: _(u"""
 La procédure a un numéro non appelable dans cette version.
 le numéro erroné est %(i1)d.
"""),

61: _(u"""
  La commande a un numéro non appelable dans cette version
  Le numéro erroné est : %(i1)d
"""),

63: _(u"""
     ARRET PAR MANQUE DE TEMPS CPU
     Les commandes suivantes sont ignorées, on passe directement dans FIN
     La base globale est sauvegardée
     Temps consommé de la réserve CPU        :  %(r1).2f s\n
"""),

64: _(u"""
  Valeur initiale du temps CPU maximum =   %(i1)d secondes
  Valeur du temps CPU maximum passé aux commandes =   %(i2)d secondes
  Réserve CPU prévue = %(i3)d secondes
"""),

65 : _(u"""
 Liste des concepts issus de la base :
    Nom         Type du concept
"""),

66 : _(u"""    %(k1)-8s    %(k2)-20s    %(k3)s"""),

67 : _(u"""    pas de concept
"""),

68: _(u"""
 La signature de la base sauvegardée est (à l'adresse %(i1)d) :
    %(k1)s
"""),

69: _(u"""
Les fichiers glob.1 et pick.1 ne sont pas cohérents !

D'après le fichier pick.1, la signature de la base à l'adresse %(i1)d devrait être :
    %(k1)s
Or la signature de glob.1 est :
    %(k2)s
"""),

70: _(u"""
 La signature de la base relue est conforme à celle attendue (à l'adresse %(i1)d) :
    %(k1)s
"""),

71: _(u"""
 La signature de la base au format HDF ne peut pas être vérifiée.
"""),

72: _(u"""
 L'exécution précédente s'est terminée correctement.
"""),

76: _(u"""
 L'exécution précédente a été interrompue au cours d'une commande qui a produit
 le concept '%(k1)s' de type <%(k2)s> qui a été néanmoins validé par l'opérateur.
"""),

81: _(u"""
 %(k1)s nom symbolique inconnu
  - nombre de valeurs attendues %(i1)d
  - valeurs attendues : %(k1)s, %(k2)s,...
"""),

82: _(u"""
 L'argument du mot clé "CAS" est erroné.
 Valeur lue %(k1)s
 nombre de valeurs attendues %(i1)d
 valeurs attendues : %(k1)s,%(k2)s, ...
"""),

83: _(u"""

 Le nombre d'enregistrements (NMAX_ENRE) et leurs longueurs (LONG_ENRE) conduisent à un
 fichier dont la taille maximale en Mo (%(i1)d) est supérieure à limite autorisée :  %(i2)d

 Vous pouvez augmenter cette limite en utilisant l'argument "-max_base" sur la ligne
 de commande suivi d'une valeur en Mo.

"""),

86: _(u"""
 Erreur à la relecture du fichier pick.1 : aucun objet sauvegardé ne sera récupéré.
"""),

89: _(u"""
 Il n'y a pas de fichier glob.1 ou bhdf.1 dans le répertoire courant.

Conseils:
   - Vérifiez que vous avez une base (de type base ou bhdf) dans votre étude.
   - Vérifiez si elle doit être décompressée ou pas.
"""),

93 : _(u"""
La variable python "%(k1)s" fait référence au concept "%(k2)s".
Cela se produit avec ce type d'enchaînement :
   %(k2)s = COMMANDE(...)
   %(k1)s = %(k2)s

On détruit cette variable ("%(k1)s" dans l'exemple ci-dessus).

-> Conseil :
   Pour éviter cette alarme, supprimer la référence dans le jeu de commandes
   qui produit la base :
      %(k1)s
"""),

95 : _(u"""
Le temps CPU système (%(r1).1f) atteint une valeur supérieure à %(i1)d%% du temps CPU (%(r2).1f).
Ce comportement est peut-être anormal.

-> Conseil :
   Augmenter la quantité de mémoire peut permettre de diminuer le temps système.
"""),

96 : { 'message' : _(u"""

    Réception du signal USR1. Interruption du calcul demandée...

"""), 'flags' : 'DECORATED',
},

97 : { 'message' : _(u"""

    Interruption du calcul suite à la réception d'un <Control-C>.

"""), 'flags' : 'DECORATED',
},

}
