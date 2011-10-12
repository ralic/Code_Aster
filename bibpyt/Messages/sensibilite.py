#@ MODIF sensibilite Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
 Type de dérivation voulu : %(i1)d
 Ce type n'est pas implanté.
"""),

2 : _(u"""
 On veut dériver %(k1)s par rapport à %(k2)s.
 Cela n'est pas disponible.
"""),

3 : _(u"""
 La dérivée de %(k1)s par rapport à %(k2)s est introuvable.
"""),

4 : _(u"""
 Le champ de theta sensibilité est inexistant dans la sd %(k1)s
"""),

5 : _(u"""
 On ne sait pas dériver ce type de structures : %(k1)s.
"""),

6 : _(u"""
 Le paramètre de sensibilité doit etre un champ theta.
"""),

7 : _(u"""
 Cette option est indisponible en sensibilité lagrangienne.
"""),

8 : _(u"""
 Pour l'occurrence numéro %(i1)d ,
 la dérivée du champ %(k1)s de %(k2)s par rapport à %(k3)s est introuvable.
"""),

9 : _(u"""
 On ne sait pas trouver le type de la dérivation par rapport à %(k1)s.
"""),

10 : _(u"""
 Initialisation de la table associée à la table %(k1)s et au paramètre sensible %(k2)s
 connue sous le nom de concept %(k3)s
"""),

11 : _(u"""
 Le calcul de sensibilité n'est pas encore disponible pour les chargements de type epsi_init
"""),

12 : _(u"""
 Il y a vraisemblablement %(i1)d modes propres multiples.
 Le calcul des sensibilités se limite actuellement aux modes propres simples
"""),

13 : _(u"""
 On ne peut pas dériver avec une charge complexe en entrée de dyna_line_harm.
"""),

15 : _(u"""
 Le comportement %(k1)s n'est pas autorisé en sensibilité
"""),

16 : _(u"""
 EXICHA différent de 0 et 1
"""),

22 : _(u"""
 L'option sensibilité lagrangienne non opérationnelle en non-linéaire
"""),

31 : _(u"""
 L'option sensibilité n'est pas opérationnelle en séchage
"""),

32 : _(u"""
 L'option sensibilité n'est pas opérationnelle en hydratation
"""),

35 : _(u"""
 L'option sensibilité n'est pas opérationnelle pour le comportement %(k1)s
"""),

37 : _(u"""
 L'option sensibilité n'est pas opérationnelle pour la modélisation %(k1)s
"""),

38 : _(u"""
 pb determination sensibilité de rayonnement
"""),

39 : _(u"""
 pb determination sensibilité materiau ther_nl
"""),

41 : _(u"""
 Déplacements initiaux imposés nuls pour les  calculs de sensibilité
"""),

42 : _(u"""
 Vitesses initiales imposées nulles pour les  calculs de sensibilité
"""),

51 : _(u"""
 Dérivation de g : un seul paramètre sensible par appel à CALC_G.
"""),

52 : _(u"""
 Actuellement, on ne sait dériver que les 'POU_D_E'.
"""),

53 : _(u"""
 En thermoélasticité, le calcul des dérivées de g est pour le moment incorrect.
"""),

54 : _(u"""
 Avec un chargement en déformations (ou contraintes) initiales, le calcul
 des dérivées de g est pour le moment incorrect.
"""),

55 : _(u"""
 Le calcul de derivée n'a pas été étendu à la plasticité.
"""),

56 : _(u"""
Problème :
  Le calcul de sensibilité dans MODE_ITER_SIMULT n'est pas conseillé ici.
  Il aura besoin de beaucoup de mémoire (>3 Go).

Conseil :
  Pour les nombres de ddls importants, il faut préférer le calcul de sensibilité
  par "différences finies".
"""),

57 : _(u"""
 Le calcul de derivée n'a pas été prévu pour les variables de commande de séchage ou d'hydratation.
"""),

71 : _("""
 Dérivation par rapport au paramètre sensible : %(k1)s
"""),

72 : _(u"""
 Le résultat est insensible au paramètre %(k1)s.
"""),

73 : _(u"""
 Le type de la dérivation est %(k1)s
"""),

81 : _(u"""
 la structure nosimp est introuvable dans la memorisation inpsco
"""),

91 : _(u"""
 Le pas de temps adaptatif n'est pas approprié pour le calcul de sensibilité
 par rapport au paramètre materiau
"""),

92 : _(u"""
 On ne peut pas dériver les concepts de type %(k1)s
"""),

93 : _(u"""
 On ne peut pas dériver avec un vect_asse en entree de dyna_line_harm.
"""),

95 : _(u"""
 Seuls sont possibles :
"""),

96 : _(u"""
 Les sous-types de sensibilité pour l'influence de %(k1)s sont %(k2)s et %(k3)s
 C'est incohérent.
"""),

}
