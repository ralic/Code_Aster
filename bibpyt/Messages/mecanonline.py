#@ MODIF mecanonline Messages  DATE 12/02/2013   AUTEUR SELLENET N.SELLENET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""
 Échec dans le calcul des matrices élastiques pour l'amortissement.
"""),

2 : _(u"""
 Lors de la recopie du champ %(k1)s donné dans ETAT_INIT de la commande
 STAT_NON_LINE vers le champ %(k2)s, certaines composantes de %(k2)s
 ont du être mises à zéro.
 
 Ce problème survient lorsque le champ donné dans ETAT_INIT ne comporte
 pas assez de composantes, on complète donc pas des zéros.
"""),

23 : _(u"""
 Le calcul de l'accélération initiale a ignoré les chargements de type:
 - ONDE_PLANE
 - LAPLACE
 - GRAPPE_FLUIDE
"""),

24 : _(u"""
 L'état initial n'a pas d'accélération donnée.
 On la calcule.
 """),

43 : _(u"""
 Contact et pilotage sont des fonctionnalités incompatibles
"""),

59 : _(u"""
 Cette loi de comportement n'est pas disponible pour le pilotage de type PRED_ELAS
"""),

60 : _(u"""
 Le pilotage de type PRED_ELAS nécessite ETA_PILO_R_MIN et ETA_PILO_R_MAX pour la loi %(k1)s
"""),

61 : _(u"""
 Le pilotage de type DEFORMATION n'est pas disponible pour la modélisation %(k1)s
"""),

62 : _(u"""
 Pilotage: SELECTION='MIXTE' incompatible avec ACTION='AUTRE_PILOTAGE' dans DEFI_LIST_INST
"""),

69 : _(u"""
 Problème rencontré :
   la matrice de masse est non inversible.
   On ne peut donc pas s'en servir pour calculer l'accélération initiale.
   => on initialise l'accélération à zéro.

 Conseils :
   Avez-vous bien affecté une masse sur tous les éléments ?

 Certains éléments ne peuvent évaluer de matrice masse.
 Dans ce cas, vous pouvez donner un champ d'accélération explicitement nul dans ETAT_INIT pour supprimer l'alarme.
"""),

70 : _(u"""
 Problème rencontré :
   Le calcul de l'accélération initiale a échoué lors de la phase de résolution.
   => on initialise l'accélération à zéro.

 Conseils :
   Avez-vous bien affecté une masse sur tous les éléments ?

 Certains éléments ne peuvent évaluer de matrice masse.
 Dans ce cas, vous pouvez donner un champ d'accélération explicitement nul dans ETAT_INIT pour supprimer l'alarme.
"""),




78 : _(u"""
 Problème rencontré :
   la matrice de masse est quasi singulière.
   On se sert de cette matrice pour calculer l'accélération initiale.
   => l'accélération initiale calculée est peut être excessive en quelques noeuds.

 Conseils :
   Ces éventuelles perturbations initiales sont en général sans influence sur
   la suite du calcul car elles sont localisées.
   Néanmoins, il peut être bénéfique de laisser ces perturbations s'amortir au
   début du calcul en faisant plusieurs pas avec chargement transitoire nul,
   avec, éventuellement, un schéma d'intégration choisi volontairement très
   dissipatif (par exemple HHT avec alpha=-0.3).
   On peut ensuite reprendre en poursuite avec un schéma moins dissipatif si besoin est.
"""),





}
