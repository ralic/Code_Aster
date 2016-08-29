# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
Solveur PETSc :
 Erreur à l'initialisation de PETSc. Il y a certainement un problème dans l'installation.
"""),

2 : _(u"""
Solveur PETSc :
 On ne traite que des matrices réelles avec PETSc.
"""),

3 : _(u"""
Solveur PETSc :
  Limite atteinte : le solveur PETSc est utilisé par plus de 5 matrices simultanément.
Solution :
  Il faut corriger le programme
  Contactez l'assistance.
"""),

4 : _(u"""
Solveur PETSc :
  Le préconditionneur a déjà été calculé, on ne le recalcule pas.
"""),

5 : _(u"""
Solveur PETSc :
 La résolution du système linéaire a échoué car le critère de convergence n'a pu être satisfait avec le nombre d'itérations autorisées (%(i1)d).

 Conseils :
  * Augmentez le nombre d'itérations autorisées (SOLVEUR/NMAX_ITER).
  * Vérifiez la nature du problème résolu (symétrique ou non, mixte ou non, etc) et, le cas échéant, changez d'algorithme ou de préconditionneur (SOLVEUR/ALGORITHME ou SOLVEUR/PRE_COND).
  * Si vous utilisez une commande non-linéaire (STAT_NON_LINE par exemple), diminuez la précision demandée pour la convergence (SOLVEUR/RESI_RELA).
    Prenez garde cependant car cela peut empêcher la convergence de l'algorithme non-linéaire.
"""),

6 : _(u"""
Solveur PETSc :
  Le résidu a été multiplié par plus de %(i1)d par rapport au résidu initial : on diverge
  Vous pouvez utiliser un préconditionneur plus précis, voire changer d'algorithme.
"""),

7 : _(u"""
Solveur PETSc :
  On a rencontré un 'breakdown', on ne peut plus agrandir l'espace de Krylov, or on n'a pas encore
  convergé.
  Il faut changer de préconditionneur ou d'algorithme.
"""),

8 : _(u"""
Solveur PETSc :
  Le solveur a échoué : la norme du résidu est une valeur invalide. 
"""),

9 : _(u"""
Solveur PETSc :
  La matrice du système est non symétrique, or l'algorithme que vous utilisez requiert
  la symétrie.
  Changez d'algorithme ou bien utilisez le mot-clé MATR_RIGI_SYME  ='OUI' sous le mot-clé 
  facteur NEWTON  pour symétriser la matrice.
"""),

10 : _(u"""
Solveur PETSc :
  Le préconditionneur construit à partir de la matrice du système n'est défini positif, or l'algorithme
  que vous utilisez requiert la définie positivité.
  Changez d'algorithme.
"""),

11 : _(u"""
Solveur PETSc :
  La matrice du système n'est pas définie positive, or l'algorithme que vous utilisez requiert
  la définie positivité.
  Changez d'algorithme.
"""),

12 : _(u"""
Solveur PETSc :
  L'algorithme itératif a rencontré un erreur dont le code retourné par PETSC est : %(i1)d.
  Consulter le manuel de PETSc pour plus d'informations et prévenez l'assistance.
"""),

13 : _(u"""
Solveur PETSc :
  La résolution a échoué ; consultez le message ci-dessus.
  Cela peut être dû à une propriété particulière de la matrice du système non supportée par l'algorithme choisi.
  Par exemple, une matrice avec des zéros sur la diagonale et l'algorithme SOR, qui utilise ces entrées pour réaliser des divisions.
"""),

14 : _(u"""
Solveur PETSc :
  La création du préconditionneur a échoué ; consultez le message ci-dessus.
  Cela peut être dû à une propriété particulière de la matrice du système non supportée par le préconditionneur choisi.
  Par exemple, une matrice nécessitant des pivotages pour la factoriser ne peut pas utiliser le préconditionneur 'LDLT_INC'.

  Conseil : changez de préconditionneur.
"""),

15 : _(u"""
Solveur PETSc :
  La création du préconditionneur 'LDLT_SP' a échoué car on manque de mémoire.

  Conseil : augmentez la valeur du mot clé SOLVEUR/PCENT_PIVOT.
"""),

16 : _(u"""
Solveur PETSc :
  La résolution du système linéaire a abouti mais la solution obtenue ne vérifie pas le critère de convergence.
  Cela peut arriver: 
     - si la matrice du système linéaire est mal conditionnée
     - si vous avez utilisé le préconditionneur 'LDLT_SP' avec ALGORITHME='GMRES'

  Conseils : utilisez le préconditionneur 'LDLT_SP' avec ALGORITHME='FGMRES' ou un solveur direct ('MULT_FRONT' ou 'MUMPS')
"""),


18 : _(u"""
Solveur PETSc :
  Les préconditionneurs 'ML', 'BOOMER' et 'GAMG' ne doivent pas être utilisés lorsque:
  - soit le modèle comporte des charges dualisées issues de AFFE_CHAR_MECA,
  - soit les noeuds du modèle ne portent pas tous les mêmes degrés de liberté. 
  
  Conseils :
  - ne mélangez pas des modélisations dans votre calcul
  - remplacez AFFE_CHAR_MECA par AFFE_CHAR_CINE si cela est possible 
  - utilisez le préconditionneur 'LDLT_SP' ou un solveur direct ('MULT_FRONT' ou 'MUMPS').
"""),

19 : _(u"""
Solveur PETSc :
  La sélection du préconditionneur '%(k1)s' a échoué.
  L'installation de PETSc dont vous disposez n'a vraisemblablement pas été compilée avec le support de ce préconditionneur.

  Conseils :
  - reconstruisez une version de PETSc avec le support des préconditionneurs BOOMER, ML et GAMG
  - utilisez un autre préconditionneur (comme 'LDLT_SP' par exemple)
"""),

20 : _(u"""
 Erreur d'utilisation :
   On veut utiliser le préconditionneur SOLVEUR / PRE_COND='BLOC_LAGR'
   Il y a plusieurs processeurs actifs.
 Risques & conseils :
   Il faut utiliser la version MPI avec un seul processeur.
"""),




21 : _(u"""
Solveur PETSc :
  Les préconditionneurs 'ML', 'BOOMER' et 'GAMG' exigent que tous les noeuds
  du modèle portent les mêmes degrés de liberté.
  Comme ce n'est pas le cas ici, on ajoute des degrés de liberté
  fictifs au système linéaire pour répondre à cette contrainte.
  On a ajouté %(i1)d degrés de liberté fictifs (%(i2)d %%)
"""),


22 : _(u"""
Solveur PETSc :
  Les préconditionneurs 'ML', 'BOOMER' et 'GAMG' exigent que tous les noeuds
  du modèle portent les mêmes degrés de liberté.
  Ce n'est pas le cas ici. On arrête l'exécution.
"""),


}
