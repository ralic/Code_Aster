#@ MODIF supervis Messages  DATE 02/04/2007   AUTEUR COURTOIS M.COURTOIS 
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
 Impressions depuis python : nom de fichier incorrect
"""),

2: _("""
 Arret sur erreur(s) utilisateur
"""),

3: _("""
 Erreur programmeur : JEMARQ/JEDEMA non apparies.
"""),

4: _("""
 Commande n  %(k1)s  :  "%(k2)s"  :   %(k3)s  erreur(s) detectee(s)
"""),

5: _("""
 Erreur(s) à l'exécution de "%(k1)s" : arret immediat du programme.
"""),

6: _("""
 Fin à la suite de message(s) <E>
"""),

7: _("""
 Le concept " %(k1)s " est inconnu. Il n'est ni parmi les créés, ni parmi ceux à créer.
"""),

8: _("""
 Un nom de concept intermediaire doit commencer par '.' ou '_' et non :  %(k1)s
"""),

9: _("""
 Longueur nulle
"""),

10: _("""
   - le concept  "%(k1)s" est detruit des bases de donnees.
"""),

11: _("""
 Impossible d'allouer la mémoire jeveux demandée
"""),

12: _("""
 Exécution de jeveux en mode debug
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
L'argument du mot cle "NOM" sous le mot clé facteur "CODE" est tronqué à 8 caractères. Le nom de code est donc "%(k1)s".
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
 Trop de noms definis dans la liste argument de "FICHIER"
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
 Erreur fatale  **** appel a commande "superviseur".
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

37: _("""
 Le nom de matériau '%(k1)s' doit etre au plus de 6 caractères.
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

41 : _("Plus de 5 fois le meme message d'alarme."),

42: _("""
 DEBUG/SDVERI demandé par l'utilisateur mais indisponible sur cette machine
 car le compilateur fortran77 n'accepte pas les fonctions récursives.
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

46: _("""
 Debug JXVERI demandé
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
 la commande a un numero non appelable dans cette version.
 le numero errone est  %(i1)d 
"""),

51: _("""
 la commande a un numero non appelable dans cette version.
 le numero errone est  %(i1)d 
"""),

52: _("""
 fin de lecture (duree  %(r1)f  s.) %(k1)s 
"""),

53: _("""
 vous ne pouvez utiliser plus de  %(i1)d 
 niveaux de profondeur pour des appels par la  procedure %(k1)s 
"""),

54: _("""
 la procedure a un numero non appelable dans cette version.
 le numero errone est  %(i1)d 
"""),

55 : _("""
 Appels récursifs de messages d'erreur ou d'alarme.
"""),

56 : _("""
 Incohérence entre le catalogue et le corps de la macro.
"""),

57 : _("""
   Impossible d'importer %(k1)s dans Messages.
   Le fichier %(k1)s.py n'existe pas dans le répertoire 'Messages'
   ou bien la syntaxe du fichier est incorrecte.
   
   Merci de signaler cette anomalie.
   
   Traceback :
   %(k2)s
"""),

}
