#@ MODIF mecanonline3 Messages  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
 Il y a trop de colonnes � afficher dans le tableau de convergence.
 La largeur maximale affichable est de 256 caract�res, donc 14 colonnes au maximum.
 Or vous avez <%(i1)d> colonnes !
 Si vous avez des colonnes SUIVI_DDL, supprimez en.
 Vous pouvez �ventuellement d�sactiver INFO_RESIDU ou INFO_TEMPS.
"""),

2 : _(u"""
 Votre mod�le contient des variables de commandes (temp�rature, irradiation, etc.)
 or on utilise une matrice �lastique constante au cours du temps.
 Si vous faites de l'amortissement de Rayleigh, il y a un risque de r�sultats faux
 si l'amortissement d�pend de cette variable de commande (via les coefficients �lastiques).

 """),

70 : _(u"""
 macro-�l�ment statique et FETI incompatibles
"""),

71 : _(u"""
 chargement onde plane et FETI incompatibles
"""),

73 : _(u"""
 forces d'inertie et FETI incompatibles
"""),

75 : _(u"""
 forces d'inertie d�riv�es et FETI incompatibles
"""),

78 : _(u"""
 FETI et contact discret incompatibles !
"""),

79 : _(u"""
 FETI et contact continu incompatibles !
"""),

89 : _(u"""
 contact et recherche lin�aire peuvent poser des probl�mes de convergence
"""),

90 : _(u"""
  -> Vous utilisez une formulation 'DISCRETE' de contact conjointement avec le solveur lin�aire '%(k1)s'.
     Le solveur '%(k1)s' n'est actuellement autoris� qu'avec les algorithmes de contact 'VERIF' et 'PENALISATION'.

  -> Conseil :
     Changez d'algorithme de contact en utilisant le mot-cl� ALGO_CONT de DEFI_CONTACT ou bien changez de solveur lin�aire
     en utilisant le mot-cl� METHODE de SOLVEUR.
"""),

91 : _(u"""
Contact m�thode continue et recherche lin�aire sont incompatibles
"""),

92 : _(u"""
Contact m�thode continue et pilotage sont incompatibles
"""),

93 : _(u"""
  -> Vous utilisez la formulation 'CONTINUE' de contact conjointement avec le solveur lin�aire '%(k1)s' et le renum�roteur 'RCMK'.
     Le renum�roteur 'RCMK' n'est actuellement pas autoris� avec la formulation 'CONTINUE'.

  -> Conseil :
     Il ne faut pas utiliser de renum�roteur (renseignez RENUM='SANS' sous le mot-cl� facteur SOLVEUR).
"""),

94 : _(u"""
 Le contact de type liaison unilat�rale (sans appariement) et le pilotage sont des fonctionnalit�s incompatibles
"""),

95 : _(u"""
 Le contact de type liaison unilat�rale (sans appariement) et la recherche lin�aire peuvent poser des probl�mes de convergence
"""),

96 : _(u"""
  -> Vous utilisez la formulation 'LIAISON_UNIL' conjointement avec le solveur lin�aire '%(k1)s'.
     Ce dernier n'est pas compatible avec le traitement de conditions unilat�rales.

  -> Conseil :
     Changez de solveur lin�aire en utilisant le mot-cl� METHODE de SOLVEUR.
"""),



}
