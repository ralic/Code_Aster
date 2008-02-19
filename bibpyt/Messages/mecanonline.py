#@ MODIF mecanonline Messages  DATE 19/02/2008   AUTEUR COURTOIS M.COURTOIS 
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

def _(x) : return x

cata_msg = {

10 : _("""
 Le concept dans ETAT_INIT n'est pas du type EVOL_NOLI
"""),

12 : _("""
 L'instant spécifié sous ETAT_INIT n'est pas trouvé
"""),

13 : _("""
 Plusieurs instants correspondent à celui spécifié sous ETAT_INIT
"""),

22 : _("""
 L'etat initial n'appartient pas à un EVOL_NOLI :
 on suppose qu'on part d'un état a vitesses nulles
"""),

43 : _("""
 Contact et pilotage sont des fonctionnalités incompatibles
"""),

59 : _("""
 Cette loi de comportement n'est pas disponible pour le pilotage de type PRED_ELAS
"""),

60 : _("""
 Le pilotage de type PRED_ELAS nécessite ETA_PILO_MIN et ETA_PILO_MAX pour la loi ENDO_ISOT_BETON
"""),

69 : _("""
 Problème rencontré :
   la matrice de masse est non inversible.
   On ne peut donc pas s'en servir pour calculer l'accélération initiale.
   => on initialise l'accélération à zéro.

 Conseils :
   Avez-vous bien affecté une masse sur tous les éléments ?
"""),

77 : _("""
 Vous faites une reprise de calcul avec PILOTAGE en longueur d'arc et avec l'option ANGL_INCR_DEPL mais il n'y pas assez d'informations dans
 la structure de donnees resultats. Il vous faut en effet au moins les deux derniers champs déplacements solutions.
 Changer l'option de PILOTAGE (utilisez NORM_INCR_DEPL) ou refaites le premier calcul pour enrichir la SD resultat (modifiez vos options d'ARCHIVAGE).
"""),

78 : _("""
 Problème rencontré :
   la matrice de masse est quasi-singulière.
   On se sert de cette matrice pour calculer l'accélération initiale.
   => l'accélération initiale calculée est peut etre excessive en quelques noeuds.

 Conseils :
   Ces éventuelles perturbations initiales sont en général sans influence sur
   la suite du calcul car elles sont localisées.
   Néanmoins, il peut etre bénéfique de laisser ces perturbations s'amortir au
   début du calcul en faisant plusieurs pas avec chargement transitoire nul,
   avec, eventuellement, un schéma d'integration choisi volontairement très
   dissipatif (par exemple HHT avec alpha=-0.3).
   On peut ensuite reprendre en poursuite avec un schéma moins dissipatif si besoin est.
"""),

79 : _("""
   Arret par manque de temps CPU au numéro d'instant : %(i1)d
                                 lors de l'itération : %(i2)d
      - Temps moyen par itération : %(r1)f
      - Temps cpu restant         : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

80 : _("""
   Arret par manque de temps CPU au numéro d'instant : %(i1)d
      - Temps moyen par %(k1)s : %(r1)f
      - Temps cpu restant      : %(r2)f
   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arret.
"""),

81 : _("""
   Echec dans la recherche linéaire. Contactez les développeurs.
"""),

82 : _("""
   Arret pour cause de matrice non inversible.
"""),

83 : _("""
   Arret : absence de convergence avec le nombre d'itérations requis.
"""),

84 : _("""
   Arret par échec dans le pilotage.
"""),

85 : _("""
   Arret : absence de convergence au numéro d'instant : %(i1)d
                                  lors de l'itération : %(i2)d
"""),

86 : _("""
    Erreur dans la gestion des erreurs. Contactez les développeurs.
"""),

87 : _("""
    Recherche linéaire non favorable. Rho forcé à 1.
"""),

}
