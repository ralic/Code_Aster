#@ MODIF mecanonline9 Messages  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
   Arrêt par manque de temps CPU pendant les itérations de Newton, au numéro d'instant < %(i1)d >
      - Temps moyen par itération de Newton : %(r1)f
      - Temps CPU restant                   : %(r2)f
   Conseil:
      - Augmenter le temps CPU.   
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

2 : _(u"""
   Arrêt par manque de temps CPU au numéro d'instant < %(i1)d >
      - Temps moyen par pas de temps        : %(r1)f
      - Temps CPU restant                   : %(r2)f
   Conseil:
      - Augmenter le temps CPU.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

3 : _(u"""
   Arrêt suite à l'échec de l'intégration de la loi de comportement.
   Conseils:
      - Vérifier vos paramètres, la cohérence des unités.
      - Essayer d'augmenter ITER_INTE_MAXI, ou de subdiviser le pas de temps localement via ITER_INTE_PAS.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

4 : _(u"""
   Arrêt pour cause de matrice non inversible.
   Conseils:
      - Vérifier vos conditions limites.
      - Vérifier votre modèle, la cohérence des unités.
      - Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

5 : _(u"""
   Arrêt pour cause d'échec lors du traitement du contact discret.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

6 : _(u"""
   Arrêt pour cause de matrice de contact singulière.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

7 : _(u"""
   Arrêt pour cause d'absence de convergence avec le nombre d'itérations requis.
   Conseils:
   - Augmenter ITER_GLOB_MAXI.
   - Réactualiser plus souvent la matrice tangente.
   - Raffiner votre discrétisation temporelle.
   - Essayer d'activer la gestion des événements (découpe du pas de temps par exemple) dans la commande DEFI_LIST_INST.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

8  : _(u"""
   Arrêt par échec dans le pilotage.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

9  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur la géométrie.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

10  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur le seuil de frottement.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

11  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur le statut de contact.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

12 : _(u"""
   Arrêt par échec du traitement de la collision.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

13  : _(u"""
   Arrêt par détection d'instabilité (mot-clé CRIT_STAB dans STAT_NON_LINE / DYNA_NON_LINE).
   La charge critique correspondante est accessible de deux manières :
     - dans le fichier de message,
     - dans la SD résultat EVOL_NOLI, elle correspond au paramètre CHAR_CRIT.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

14 : _(u"""
   Arrêt par échec de l'adaptation du coefficient de pénalisation pour limiter l'interpénétration.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

}
