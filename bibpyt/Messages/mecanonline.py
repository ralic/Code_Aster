#@ MODIF mecanonline Messages  DATE 27/02/2012   AUTEUR GREFFET N.GREFFET 
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
 Échec dans le calcul des matrices élastiques pour l'amortissement.
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
 Le pilotage de type PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX pour la loi ENDO_ISOT_BETON
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




82 : _(u"""
   Arrêt pour cause de matrice non inversible.
"""),

83 : _(u"""
   Arrêt : absence de convergence avec le nombre d'itérations requis.
"""),

84 : _(u"""
   Arrêt par échec dans le pilotage.
"""),

85 : _(u"""
   Arrêt : absence de convergence au numéro d'instant : %(i1)d
                                  lors de l'itération : %(i2)d
"""),

86 : _(u"""
   Arrêt par échec du traitement de la collision.
"""),

87 : _(u"""
   Arrêt par détection d'instabilité (mot-clé CRIT_STAB dans STAT_NON_LINE / DYNA_NON_LINE).
   La charge critique correspondante est accessible de deux manières :
     - dans le fichier de message,
     - dans la SD résultat EVOL_NOLI, elle correspond au paramètre CHAR_CRIT.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),
}
