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
Format SALOME, l'argument 1 doit être le nom du fichier MED produit
par le script SALOME. Les autres arguments ne sont pas utilisés.
"""),

    2 : _(u"""
On ne sait pas traiter le format %(k1)s
"""),

    3 : _(u"""
Code retour incorrect (MAXI %(i1)d) : %(i2)d

"""),

    6 : _(u"""
Le fichier %(k1)s n'existe pas.
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

    24 : _(u"""
Le nom du répertoire contenant les outils externes est trop long pour être stocké
dans la variable prévue (de longueur %(i1)d).

Conseil :
    - Vous pouvez déplacer/copier les outils dans un autre répertoire de nom plus court
      et utiliser l'argument optionnel "-rep_outils /nouveau/chemin" pour contourner
      le problème.
"""),

}
