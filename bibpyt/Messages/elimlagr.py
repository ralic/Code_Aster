# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais la version du programme ne dispose pas du solveur PETSC
   qui est nécessaire à cette fonctionnalité.

 Risques & conseils :
   Il faut utiliser la version MPI avec un seul processeur.
"""),

2 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Il y a plusieurs processeurs actifs.
 Risques & conseils :
   Il faut utiliser la version MPI avec un seul processeur.
"""),

3 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   La matrice n'est pas réelle (mais sans doute complexe).
   C'est interdit pour l'instant.
 Risques & conseils :
   Il faut ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
"""),

4 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Certaines conditions aux limites sont réalisées par AFFE_CHAR_CINE.
   C'est interdit pour l'instant.
 Risques & conseils :
   Il faut remplacer AFFE_CHAR_CINE par AFFE_CHAR_MECA..
"""),

5 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais la matrice n'est pas symétrique.
   C'est interdit pour l'instant.
 Risques & conseils :
   Il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
"""),

6 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais la matrice est une matrice généralisée.
   C'est interdit.
 Risques & conseils :
   Il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
"""),

7 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais la matrice "réduite" est de taille nulle.
   (tous les ddls sont imposés)
   C'est interdit pour l'instant.
 Risques & conseils :
   Il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
"""),

8 : _(u"""
 Erreur de programmation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Un incohérence interne est détectée.
 Risques & conseils :
   Il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
   Il faut émettre une fiche d'anomalie.
"""),

9 : _(u"""
 Erreur d'utilisation :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais il n'y a aucune condition limite à éliminer.
   Il n'y a donc pas lieu d'utiliser ELIM_LAGR='OUI'
 Risques & conseils :
   Il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI'.
"""),

10 : _(u"""
 Erreur :
   On veut utiliser la fonctionnalité SOLVEUR / ELIM_LAGR='OUI'
   (ou la commande ELIM_LAGR).
   Mais les coefficients des relations linéaires sont tous nuls
   dans la matrice %(k1)s .
 Risques & conseils :
   Par exemple, il ne faut pas utiliser SOLVEUR / ELIM_LAGR='OUI' avec
   une matrice de masse.
"""),

11 : _(u"""
 Erreur utilisateur :
   On veut utiliser la commande ELIM_LAGR pour éliminer les équations
   de Lagrange dans une matrice qui n'est pas une matrice de rigidité.
   Il faut d'abord utiliser la commande ELIM_LAGR sur la matrice
   de rigidité.

 Risques & conseils :
   La séquence d'appel doit ressembler à :
      K2=ELIM_LAGR(MATR_RIGI=K1, )
      M2=ELIM_LAGR(MATR_RIGI=K1, MATR_ASSE=M1)
"""),

}
