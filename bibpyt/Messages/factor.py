#@ MODIF factor Messages  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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


#-----------------------------------------------------------------------------------------------
10: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
"""),

#-----------------------------------------------------------------------------------------------
11: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
"""),

#-----------------------------------------------------------------------------------------------
20: _("""
  -> Matrice non factorisable :
     Le pivot est presque nul à la ligne %(i1)d pour le noeud %(k1)s et
     la composante %(k2)s.
     Pour information, le nombre de décimales perdues est de %(i2)d.

  -> Conseil & Risque :
     Il s'agit peut etre d'un mouvement de corps rigide mal bloqué.
     Vérifiez les conditions aux limites.
     Si vous faites du contact, il ne faut pas que la
     structure ne "tienne" que par le contact.
"""),

#-----------------------------------------------------------------------------------------------
21: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  pour le noeud %(k1)s et la composante %(k2)s
"""),


#-----------------------------------------------------------------------------------------------
30: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
  Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
  La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
"""),

#-----------------------------------------------------------------------------------------------
31: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  Il s'agit sans doute d'une relation linéaire entre ddls surabondante.
  La liste des noeuds concernés par cette relation est imprimée ci-dessus dans le fichier MESSAGE.
"""),


#-----------------------------------------------------------------------------------------------
40: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
  Il s'agit sans doute d'une relation de blocage surabondante.
  blocage concerné : %(k4)s
"""),

#-----------------------------------------------------------------------------------------------
41: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  Il s'agit sans doute d'une relation de blocage surabondante.
  blocage concerné : %(k4)s
"""),

#-----------------------------------------------------------------------------------------------
42: _("""
Matrice non factorisable :
  Le solveur MUMPS considère la matrice comme numériquement singulière.
  (Mais il n'en dit pas plus)

Conseil :
  Il s'agit peut-etre d'un manque de conditions aux limites,
  ou au contraire, de redondances entre de trop nombreuses conditions.
"""),

#-----------------------------------------------------------------------------------------------
51: _("""
Solveur MUMPS interdit ici.
Causes possibles :
  - contact/frottement discret
  - STAT_NON_LINE / FLAMBEMENT
"""),

#-----------------------------------------------------------------------------------------------
52: _("""
Solveurs LDLT et MULT_FRONT seuls permis ici.
Causes possibles :
  - contact/frottement discret
  - STAT_NON_LINE / FLAMBEMENT
"""),

#-----------------------------------------------------------------------------------------------
53: _("""
Solveur MUMPS :
  Mumps manque de mémoire lors de la factorisation de la matrice.

Solution :
  Il faut augmenter la valeur du mot clé  SOLVEUR/PCENT_PIVOT.

Remarque : on a le droit de dépasser la valeur 100.
"""),

#-----------------------------------------------------------------------------------------------
54: _("""
Solveur MUMPS :
  Mumps manque de mémoire lors de la factorisation de la matrice.

Solution :
  Il faut augmenter la mémoire donnée à Mumps.
  Pour cela, il faut diminuer le pourcentage de mémoire donné à JEVEUX.
  C'est à dire diminuer la valeur du paramètre "mem_aster" du menu "Options" d'ASTK.

Remarque :
  On peut par exemple choisir mem_aster=50 ce qui correspond à un partage
  équitable (50/50) de la mémoire entre JEVEUX et Mumps.
"""),

#-----------------------------------------------------------------------------------------------
55: _("""
Solveur MUMPS :
  Problème dans le solveur MUMPS.
  Le code retour de mumps (INFOG(1)) est : %(i1)d

Solution :
  Consulter le manuel d'utilisation de Mumps.
  Prévenir l'équipe de développement de Code_Aster.
"""),

#-----------------------------------------------------------------------------------------------
56: _("""
Solveur MUMPS :
  Il ne faut pas utiliser TYPE_RESOL = '%(k1)s'
  Pour une matrice non-symétrique.

Solution :
  Il faut utiliser TYPE_RESOL = 'NONSYM' (ou 'AUTO').
"""),

#-----------------------------------------------------------------------------------------------
57: _("""
Solveur MUMPS :
  La solution du système linéaire est trop imprécise :
  Erreur calculée   : %(r1)g
  Erreur acceptable : %(r2)g   (RESI_RELA)

Solution :
  On peut augmenter la valeur du mot clé SOLVEUR/RESI_RELA.
"""),

#-----------------------------------------------------------------------------------------------
58: _("""
Solveur MUMPS :
  La matrice est singulière.

Solution :
  On peut essayer d'aller plus loin en précisant : STOP_SINGULIER='NON'
"""),

#-----------------------------------------------------------------------------------------------
59: _("""
Solveur MUMPS :
  La matrice est déjà factorisée. On ne fait rien.

Solution :
  Il y a sans doute une erreur de programmation.
  Contactez l'assistance.
"""),

#-----------------------------------------------------------------------------------------------
60: _("""
Solveur MUMPS :
  Limite atteinte : le solveur Mumps est utilisé par plus de 5 matrices simultanément.

Solution :
  Il faut corriger le programme (PARAMETER (NMXINS=5) dans amumps.f)
  Contactez l'assistance.
"""),

}
