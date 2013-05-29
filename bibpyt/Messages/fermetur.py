# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={

1: _(u"""
Le solveur "MUMPS" n'est pas installé dans cette version de Code_Aster.

Conseil : Vérifiez que vous avez sélectionné la bonne version de Code_Aster.
          Attention, certains solveurs ne sont disponibles que dans les versions parallèles de Code_Aster.
"""),

2: _(u"""
La bibliothèque "MED" n'est pas installée dans cette version de Code_Aster.
"""),

3: _(u"""
La bibliothèque "HDF5" n'est pas installée dans cette version de Code_Aster.
"""),

4: _(u"""
La bibliothèque "ZMAT" n'est pas installée dans cette version de Code_Aster ou bien elle
n'a pas été trouvée.

Conseil : Vérifiez que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

5: _(u"""
Erreur de programmation :
    On essaie d'utiliser un opérateur qui n'est pas encore programmé.
"""),

# identique au précédent mais il faudrait modifier tous les appelants dans fermetur
6: _(u"""
Erreur de programmation :
    On essaie d'utiliser un opérateur qui n'est pas encore programmé.
"""),

7: _(u"""
Le renuméroteur "SCOTCH" n'est pas installé dans cette version de Code_Aster.
"""),

8: _(u"""
Erreur de programmation :
    On essaie d'utiliser une routine de calcul élémentaire
    qui n'est pas encore programmée.
"""),

9: _(u"""
Erreur de programmation :
    On essaie d'utiliser une routine d'initialisation élémentaire
    qui n'est pas encore programmée.
"""),

10: _(u"""
Le solveur "PETSc" n'est pas installé dans cette version de Code_Aster.

Conseil : Vérifiez que vous avez sélectionné la bonne version de Code_Aster.
          Attention, certains solveurs ne sont disponibles que dans les versions parallèles de Code_Aster.
"""),

11: _(u"""
Erreur de programmation :
    On essaie d'utiliser une routine de comportement
    qui n'est pas encore programmée.
"""),

12: _(u"""
La bibliothèque "YACS" n'est pas installée dans cette version de Code_Aster.
"""),

13 : _(u"""
La bibliothèque %(k1)s n'a pas pu être chargée.

Nom de la bibliothèque : %(k2)s

Conseil : Vérifiez que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

14 : _(u"""
Le symbole demandé n'a pas été trouvé dans la bibliothèque %(k1)s.

Nom de la bibliothèque : %(k2)s
        Nom du symbole : %(k3)s

Conseil : Vérifiez que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

15 : _(u"""
La bibliothèque "METIS" n'est pas installée dans cette version de Code_Aster.
"""),

}
