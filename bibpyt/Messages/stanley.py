#@ MODIF stanley Messages  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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

cata_msg = {

1 : _(u"""
Redéfinition du DISPLAY vers %(k1)s.
"""),

2 : _(u"""
STANLEY fonctionne en mode validation de non régression.
"""),

3 : _(u"""
Aucune variable d'environnement DISPLAY définie !
%(k1)s ne pourra pas fonctionner. On l'ignore.

Si vous êtes en Interactif, cochez le bouton Suivi Interactif
dans ASTK.

Vous pouvez également préciser votre DISPLAY dans les arguments
de la commande STANLEY :

STANLEY(DISPLAY='adresse_IP:0.0');
"""),

4 : _(u"""
Une erreur est intervenue. Raison : %(k1)s
"""),

5 : _(u"""
Cette action n'est pas réalisable: %(k1)s
"""),

6 : _(u"""
En mode DISTANT, la variable %(k1)s est obligatoire. On abandonne.
"""),

7 : _(u"""
Le paramètre 'machine_GMSH_exe' ou 'machine_VISU' n'est pas renseigné, 
il faut ouvrir le fichier manuellement.
"""),

8 : _(u"""
Lancement terminé.
"""),

9 : _(u"""
Exécution de %(k1)s
"""),

10 : _(u"""
Erreur de lancement de la commande!
"""),

11 : _(u"""
Dans le mode WINDOWS, la variable %(k1)s est obligatoire. On abandonne.
"""),

12 : _(u"""
Les fichiers de post-traitement sont copiés.
Veuillez maintenant ouvrir manuellement skin.pos avec GMSH.
"""),

13 : _(u"""
Le fichier de post-traitement est copie.
Veuillez maintenant ouvrir manuellement fort.33.pos avec GMSH.
"""),

14 : _(u"""
Impossible de contacter le serveur SALOME! Vérifier qu'il est bien lancé.
"""),

15 : _(u"""
Impossible de récupérer le nom de la machine locale! 
Solution alternative : utiliser le mode DISTANT en indiquant l'adresse IP
ou le nom de la machine dans la case 'machine de Salomé'.
"""),

16 : _(u"""
Pour visualisation dans Salomé, la variable %(k1)s est obligatoire. On abandonne.
"""),

17 : _(u"""
Pour visualisation dans Salomé, la variable machine_SALOME_port est obligatoire. 
On abandonne.
"""),

18 : _(u"""
Erreur : mode WINDOWS non implémenté
"""),

19 : _(u"""
Erreur: il est possible que STANLEY ne puisse pas contacter Salomé :

 - machine Salomé définie   : %(k1)s
 - port de Salomé           : %(k2)s
 - lanceur runSalomeScript  : %(k3)s

Vous pouvez modifier ces valeurs dans les paramètres dans STANLEY. 

Si STANLEY est bien lancé, vous pouvez essayer d'activer le module VISU.

"""),

20 : _(u"""
Exécution terminée.
"""),

#21 : _(u""" """),

22 : _(u"""
Impossible d'affecter la variable [%(k1)s / %(k2)s].
"""),

23 : _(u"""
Lecture du fichier d'environnement : %(k1)s
"""),

24 : _(u"""
Il n'y a pas de fichier d'environnement. 
On démarre avec une configuration par défaut.
"""),

25 : _(u"""
Le fichier d'environnement n'a pas la version attendue. 
On continue mais en cas de problème, effacer le répertoire ~/%(k1)s et relancer.
"""),

26 : _(u"""
Le fichier d'environnement n'est pas exploitable (par exemple c'est une ancienne version).
On démarre avec une configuration par défaut.
"""),

27 : _(u"""
On initialise une configuration par défaut.
"""),

28 : _(u"""
Nouveaux paramètres sauvegardés dans : %(k1)s
"""),

29 : _(u"""
Impossible de sauvegarder les paramètres dans : %(k1)s
"""),

31 : _(u"""
Problème : %(k1)s
"""),

32 : _(u"""
Impossible d'ouvrir en écriture le fichier %(k1)s
"""),

33 : _(u"""
Attention : on ne peut pas tracer un champ aux points de Gauss sur la déformée...
"""),

34 : _(u"""
Le champ est tracé avec la déformée.
"""),

36 : _(u"""
On ne peut pas tracer une courbe avec une seule abscisse.
"""),

37 : _(u"""
Tous les concepts Aster nécessaires à STANLEY n'ont pas été calculés. 
Il manque :
%(k1)s
"""),

38 : _(u"""
STANLEY - Erreur lors de l'appel à la commande Aster:

%(k1)s
Raison:
%(k2)s
"""),

40 : _(u"""
STANLEY - Projection aux points de Gauss: type de résultat non développé
%(k1)s
"""),

}
