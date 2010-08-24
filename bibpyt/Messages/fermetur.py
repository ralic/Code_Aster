#@ MODIF fermetur Messages  DATE 24/08/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

1: _("""
Le solveur "MUMPS" n'est pas installé sur cette machine.
"""),

2: _("""
La bibliothèque "MED" n'est pas installée sur cette machine.
"""),

3: _("""
La bibliothèque "HDF5" n'est pas installée sur cette machine.
"""),

4: _("""
La bibliothèque "ZMAT" n'est pas installée sur cette machine ou bien elle
n'a pas été trouvée.

Conseil : Vérifier que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

5: _("""
Erreur de programmation :
    On essaie d'utiliser un opérateur (op0xxx) qui n'est pas encore programmé.
"""),

6: _("""
Erreur de programmation :
    On essaie d'utiliser un opérateur (ops0xx) qui n'est pas encore programmé.
"""),

7: _("""
Le logiciel "SCOTCH" n'est pas installé sur cette machine.
"""),

8: _("""
Erreur de programmation :
    On essaie d'utiliser une routine de calcul élémentaire (te0xxx)
    qui n'est pas encore programmée.
"""),

9: _("""
Erreur de programmation :
    On essaie d'utiliser une routine d'initialisation élémentaire (ini0xx)
    qui n'est pas encore programmée.
"""),

10: _("""
Le solveur "PETSc" n'est pas installé sur cette machine.
"""),

11: _("""
Erreur de programmation :
    On essaie d'utiliser une routine de comportement (lc00xx)
    qui n'est pas encore programmée.
"""),

12: _("""
La bibliothèque "YACS" n'est pas installée sur cette machine.
"""),

13 : _("""
La bibliothèque UMAT n'a pas pu être chargée.

Nom de la bibliothèque : %(k1)s

Conseil : Vérifier que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

14 : _("""
Le symbole demandé n'a pas été trouvé dans la bibliothèque UMAT.

Nom de la bibliothèque : %(k1)s
        Nom du symbole : %(k2)s

Conseil : Vérifier que l'environnement est correctement défini,
          notamment la variable LD_LIBRARY_PATH.
"""),

}
