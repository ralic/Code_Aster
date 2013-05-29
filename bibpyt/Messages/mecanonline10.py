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
# Attention a ne pas faire de retour à la ligne !

cata_msg = {

1  : _(u"""
 <Erreur> Échec dans l'intégration de la loi de comportement
"""),

2 : _(u"""
 <Erreur> Échec dans le pilotage
"""),

3 : _(u"""
 <Erreur> Le nombre maximum d'itérations de Newton est atteint
"""),

4 : _(u"""
 <Erreur> Échec dans le traitement du contact discret
"""),

5 : _(u"""
 <Erreur> Il n'y a pas assez de temps CPU pour continuer les pas de temps
"""),

6 : _(u"""
 <Erreur> La matrice du système est singulière
"""),

7 : _(u"""
 <Erreur> Il n'y a pas assez de temps CPU pour continuer les itérations de Newton
"""),

8 : _(u"""
 <Erreur> Arrêt demandé par l'utilisateur.
"""),

9 : _(u"""
 <Erreur> On dépasse le nombre de boucles de point fixe de géométrie.
 """),

10 : _(u"""
 <Erreur> On dépasse le nombre de boucles de point fixe de frottement.
 """),

11 : _(u"""
 <Erreur> On dépasse le nombre de boucles de point fixe de contact.
 """),

12 : _(u"""
 <Erreur> Nombre maximum d'itérations atteint dans le solveur linéaire itératif.
 """),

20 : _(u"""
 <Évènement> Instabilité détectée.
 """),

21 : _(u"""
 <Évènement> Collision détectée.
 """),

22 : _(u"""
 <Évènement> Interpénétration détectée.
 """),

23 : _(u"""
 <Évènement> Divergence du résidu (DIVE_RESI).
 """),

24 : _(u"""
 <Évènement> Valeur atteinte (DELTA_GRANDEUR).
 """),

25 : _(u"""
 <Évènement> La loi de comportement est utilisée en dehors de son domaine de validité (VERI_BORNE).
 """),

30 : _(u"""
 <Action> On arrête le calcul.
 """),

31 : _(u"""
 <Action> On essaie de réactualiser le préconditionneur.
 """),

32: _(u"""
 <Action> On essaie d'autoriser des itérations de Newton supplémentaires.
"""),

33: _(u"""
 <Action> On essaie de découper le pas de temps.
"""),

34 : _(u"""
 <Action> On essaie d'utiliser la solution de pilotage rejetée initialement.
 """),

35 : _(u"""
 <Action> On essaie d'adapter le coefficient de pénalisation.
 """),

40 : _(u""" <Action><Échec> On a déjà réactualisé le préconditionneur LDLT_SP."""),

41 : _(u""" <Action> On réactualise le préconditionneur LDLT_SP."""),

42 : _(u""" <Action><Échec> On a déjà choisi l'autre solution de pilotage."""),

43 : _(u""" <Action> On choisit l'autre solution de pilotage."""),

44 : _(u""" <Action><Échec> On ne peut plus adapter le coefficient de pénalisation (on atteint COEF_MAXI)."""),

45 : _(u""" <Action> On a adapté le coefficient de pénalisation."""),

46 : _(u"""          Sur la zone <%(i1)d>, le coefficient de pénalisation adapté vaut <%(r1)13.6G>.
 """),

}
