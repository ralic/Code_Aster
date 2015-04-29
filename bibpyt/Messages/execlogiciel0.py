# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
Format Salomé, l'argument 1 doit être
le nom du fichier MED produit par le script python.
"""),

    2 : _(u"""
On ne sait pas traiter le format %(k1)s
"""),

    3 : _(u"""
Code retour incorrect (MAXI %(i1)d) : %(i2)d

"""),

    4 : _(u"""
Le mot-clé logiciel n'est pas utilise avec ce format.
"""),

    5 : _(u"""
Erreurs lors de l'exécution du fichier ci-dessous :
<<<<<<<<<<<<<<< DEBUT DU FICHIER >>>>>>>>>>>>>>>
%(k1)s
<<<<<<<<<<<<<<<  FIN  DU FICHIER >>>>>>>>>>>>>>>
"""),

    6 : _(u"""
Le fichier %(k1)s n'existe pas.
"""),

    7 : _(u"""
Mode de lancement inconnu : %(k1)s
"""),

    8 : _(u"""
 Commande :
   %(k1)s
"""),

    9 : _(u"""
----- Sortie standard (stdout) ---------------------------------------------------
%(k1)s
----- fin stdout -----------------------------------------------------------------
"""),

    10 : _(u"""
----- Sortie erreur standard (stderr) --------------------------------------------
%(k1)s
----- fin stderr -----------------------------------------------------------------
"""),

    11 : _(u"""
 Code retour = %(i2)d      (maximum toléré : %(i1)d)
"""),

    # 12 : _(u""" """),

    13 : _(u"""
 -> Le maillage n'a pas été produit par le logiciel externe (format "%(k1)s")

 -> Conseil :
    Vous devriez trouver ci-dessus des messages du logiciel en question
    expliquant les raisons de cet échec.
"""),




    14 : _(u"""
 -> Il y a eu une erreur lors de la connexion à la machine distante via SSH :
    . Il est probable que les clés SSH ne soient pas configurées correctement.

 -> Conseil :
    Vérifier que les clés SSH sont correctement configurées sur les différentes machines.
    Vous pouvez essayer de relancer manuellement la commande suivante depuis le serveur Aster :
%(k1)s
"""),


    15 : _(u"""
 -> Il y a eu une erreur lors de la connexion à la machine distante via SSH :
    . La machine distante n'a pas pus être contactée.

 -> Conseil :
    Vérifier l'adresse de la machine.
    Vous pouvez essayer de relancer manuellement la commande suivante depuis le serveur Aster :
%(k1)s
"""),


    16 : _(u"""
 -> Il y a eu une erreur lors de la connexion à la machine distante via SSH :
    . Le serveur SSH de la machine distante n'a pas pus être contacté.

 -> Conseil :
    Vérifier le port SSH de la machine.
    Vous pouvez essayer de relancer manuellement la commande suivante depuis le serveur Aster :
%(k1)s
"""),

    17 : _(u"""
 -> Il y a eu une erreur lors de la connexion à la machine distante via SSH :
    . Il est probable que le logiciel défini par le mot clé LOGICIEL ne soit pas présent
      sur la machine distante.

 -> Conseil :
    Vérifier le mot-clé LOGICIEL
    Vous pouvez essayer de relancer manuellement la commande suivante depuis le serveur Aster :
%(k1)s
"""),

    18 : _(u"""
 -> Il y a eu une erreur lors de la connexion à la machine distante via SSH :
    . Soit l'adresse (ou le port SSH) de la machine n'est pas correcte.
    . Soit les clés SSH ne sont pas configurées correctement.
    . Soit le logiciel défini par le mot clé LOGICIEL n'est pas présent sur la machine distante.
    . Soit une autre raison est à l'origine de cet échec.

 -> Conseil :
    Vous devriez trouver ci-dessus des messages expliquant les raisons de cet échec.
    Vous pouvez essayer de relancer manuellement la commande suivante depuis le serveur Aster :
%(k1)s
"""),

    19 : _(u"""Information : le champs "%(k1)s" n'a pas été trouvé dans le script Salomé, mais il n'est peut être pas nécessaire."""),

    20 : _(u"""


----------------------------------------------------------------------------------
----- Script Salomé --------------------------------------------------------------

%(k1)s

----- fin Script Salomé ----------------------------------------------------------
----------------------------------------------------------------------------------


"""),

    21 : _(u"""

----------------------------------------------------------------------------------
----- Commandes à exécuter -------------------------------------------------------

%(k1)s

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

"""),

    22 : _(u"""
Les mots-clés SALOME_HOST et SSH_ADRESSE ont été définis simultanément et sont différents.
C'est SALOME_HOST qui définira l'adresse de la machine distante.
"""),

    23 : _(u"""
Les listes de paramètres (mot-clé NOM_PARA) et de valeurs (mot-clé VALE) doivent être de même longueur !
"""),

    24 : _(u"""
Le nom du répertoire contenant les outils externes est trop long pour être stocké
dans la variable prévue (de longueur %(i1)d).

Conseil :
    - Vous pouvez déplacer/copier les outils dans un autre répertoire de nom plus court
      et utiliser l'argument optionnel "-rep_outils /nouveau/chemin" pour contourner
      le problème.
"""),

}
