#@ MODIF supervis Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 commande n  %(k1)s  :  " %(k2)s "  :   %(k3)s  erreur(s) detectee(s)
"""),

5: _("""
 erreur(s) a l'execution de " %(k1)s " : arret immediat du programme.
"""),

6: _("""
 fin a la suite de message(s) <e>
"""),

7: _("""
 le concept " %(k1)s " est inconnu. il n'est ni parmi les crees, ni parmi ceux a creer.
"""),

8: _("""
  un nom de concept intermediaire    doit commencer par '.' ou '_' et non :  %(k1)s
"""),

9: _("""
  longueur nulle
"""),

10: _("""
   - le concept  " %(k1)s " est detruit des bases de donnees.
"""),

11: _("""
 impossible d'allouer la memoire jeveux demandee
"""),

12: _("""
 execution de jeveux en mode debug
"""),

13: _("""
  %(k1)s  nom de base deja definie
"""),

14: _("""
  %(k1)s  statut impossible pour la base globale
"""),

15: _("""
 probleme d'allocation des bases de donnees
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
 " %(k1)s " argument invalide du mot cle "fichier" du mot cle facteur "catalogue"
"""),

21: _("""
 erreur(s) fatale(s) lors de la lecture des catalogues
"""),

22: _("""
 l'argument du mot cle "nom" sous le mot cle facteur "code" est tronque a 8 caracteres. le nom de code est donc " %(k1)s ".
"""),

23: _("""
 debug jxveri demande
"""),

24: _("""
 debug sdveri demande
"""),

25: _("""
 memoire gestion : "compacte"
"""),

26: _("""
 type allocation memoire 2
"""),

27: _("""
 type allocation memoire 3
"""),

28: _("""
 type allocation memoire 4
"""),

29: _("""
 trop de noms definis dans la liste argument de "fichier"
"""),

30: _("""
  %(k1)s  est deja (re-) defini
"""),

31: _("""
 valeur invalide pour le mot clereserve_cpu
"""),

32: _("""
 la procedure " %(k1)s " ne peut etre appelee en cours d'execution des commandes
"""),

33: _("""
 erreur fatale  **** appel a commande "superviseur".
"""),

34: _("""
 arret de la lecture des commandes.
"""),

35: _("""
 la procedure "retour" ne peut etre utilisee dans le fichier principal de commandes.
"""),

36: _("""
 le concept de nom '  %(k1)s  ' n'existe pas
"""),

37: _("""
 le nom de materiau ( %(k1)s ) doit etre au plus de 6 caracteres.
"""),

38: _("""
 il n'y a plus de temps pour continuer
"""),

39: _("""
 arret de l'execution et fermeture des bases jeveux
"""),
}
