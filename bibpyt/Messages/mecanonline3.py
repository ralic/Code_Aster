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


    2 : _(u"""
 Votre modèle contient des variables de commandes (température, irradiation, etc.)
 or on utilise une matrice élastique constante au cours du temps.
 Si vous faites de l'amortissement de Rayleigh, il y a un risque de résultats faux
 si l'amortissement dépend de cette variable de commande (via les coefficients élastiques).

 """),





    88 : _(u"""
  -> Vous utilisez l'algorithme de contact 'GCP' avec un préconditionneur qui n'est pas adapté.

  -> Conseil :
     Utilisez le préconditionneur 'LDLT_SP' en spécifiant PRE_COND='LDLT_SP' sous le mot-clé SOLVEUR.
"""),

    89 : _(u"""
 contact et recherche linéaire peuvent poser des problèmes de convergence
"""),

    90 : _(u"""
  -> Vous utilisez une formulation 'DISCRETE' de contact conjointement avec le solveur linéaire '%(k1)s'.
     Le solveur '%(k1)s' n'est actuellement autorisé qu'avec les algorithmes de contact 'GCP','VERIF' et 'PENALISATION'.

  -> Conseil :
     Changez d'algorithme de contact en utilisant le mot-clé ALGO_CONT de DEFI_CONTACT ou bien changez de solveur linéaire
     en utilisant le mot-clé METHODE de SOLVEUR.
"""),

    91 : _(u"""
Contact méthode continue et recherche linéaire sont incompatibles
"""),

    92 : _(u"""
Contact méthode continue et pilotage sont incompatibles
"""),

    93 : _(u"""
 Le contact de type CONTINUE et l'amortissement modal AMOR_MODAL sont des fonctionnalités incompatibles
"""),

    94 : _(u"""
 Le contact de type liaison unilatérale (sans appariement) et le pilotage sont des fonctionnalités incompatibles
"""),

    95 : _(u"""
 Le contact de type liaison unilatérale (sans appariement) et la recherche linéaire peuvent poser des problèmes de convergence
"""),

    96 : _(u"""
  -> Vous utilisez la formulation 'LIAISON_UNIL' conjointement avec le solveur linéaire '%(k1)s'.
     Ce dernier n'est pas compatible avec le traitement de conditions unilatérales.

  -> Conseil :
     Changez de solveur linéaire en utilisant le mot-clé METHODE de SOLVEUR.
"""),

    97 : _(u"""
  -> Vous utilisez la formulation 'CONTINUE' de contact conjointement avec un solveur itératif et le préconditionneur '%(k1)s'.
     Le préconditionneur '%(k1)s' ne supporte pas les matrices issues de cette formulation du contact.

  -> Conseil :
     Changez de préconditionneur.
"""),

    98 : _(u"""
  -> Vous utilisez la formulation 'CONTINUE' de contact avec le solveur linéaire 'PETSC' et vous demandez la distribution de la matrice (MATR_DISTRIBUEE='OUI').
     La distribution de la matrice n'est pas possible dans ce cas d'utilisation.

  -> Conseil :
     Désactivez la distribution de la matrice en parallèle.
"""),


}
