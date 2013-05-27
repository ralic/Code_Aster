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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

3 : _(u"""
 Calcul des valeurs propres en grandes déformations
"""),

14 : _(u"""
 Vous utilisez la méthode CONTINUE pour le traitement du contact et faites une reprise de calcul (mot-clé reuse). De plus, vous n'avez pas activé
 l'initialisation automatique des statuts de contact. L'état initial de contact sera donc non contactant.
 Cela peut entraîner des difficultés de convergence en présence de fortes non-linéarités. En présence de frottement, la solution peut bifurquer
 différemment.
 
 Conseils :
   - si vous le pouvez, faites votre calcul en une seule fois.
   - activez la détection automatique du contact initial sur toutes les zones (CONTACT_INIT='INTERPENETRE' dans DEFI_CONTACT).
"""),

15 : _(u"""
 Vous utilisez la méthode CONTINUE pour le traitement du contact et définissez un état initial via le mot-clé ETAT_INIT.  De plus, vous n'avez pas activé
 l'initialisation automatique des statuts de contact. L'état initial de contact sera donc non contactant.
 
 Il est conseillé d'activer la détection automatique du contact initial sur toutes les zones (CONTACT_INIT='INTERPENETRE' dans DEFI_CONTACT).
"""),

22 : _(u"""
 On suppose qu'on part d'un état a vitesses nulles
"""),

23 : _(u"""
 On estime une accélération initiale.
"""),

47 : _(u"""
 Vous faites une reprise de calcul avec PILOTAGE en longueur d'arc et avec l'option ANGL_INCR_DEPL mais il n'y pas assez d'informations dans
 la structure de données résultat. Il vous faut en effet au moins les deux derniers champs déplacements solutions.
 Changer l'option de PILOTAGE (utilisez NORM_INCR_DEPL) ou refaites le premier calcul pour enrichir la structure de données résultat (modifiez vos options du mot-clé ARCHIVAGE).
"""),

}
