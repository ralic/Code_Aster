#@ MODIF mecanonline2 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
 L'erreur précédente est fatale.
 Conseils:
   - S'il s'agit d'un manque de temps CPU, augmentez-le.
   - Vérifiez votre modèle 
   - Essayez d'activer la gestion des événements (découpe du pas de temps par exemple) dans la commande DEFI_LIST_INST.
 
 La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

3 : _(u"""

 Le résidu global converge plus vite que la condition des contraintes planes. 
 La convergence de la condition des contraintes planes peut être améliorée en 
 augmentant ITER_MAXI_DEBORST (=1 par défaut), sous le mot clef facteur COMP_INCR. 

"""),

4 : _(u"""
 La charge definie dans STAT_NON_LINE en tant que une charge de type suiveuse, 
 sous le mot-clé TYPE_CHARGE = 'SUIV' n'est pas une Charge SUIVEUSE.
"""),

27 : _(u"""
 La prédiction par DEPL_CALCULE à l'instant de calcul %(r1)f à partir du concept %(k1)s n'a pas pu être construite.
 Explications possibles :
  - le concept ne contient pas de champs de déplacement
  - l'instant de calcul demandé est en dehors de l'intervalle des instants calculés dans le concept fourni (il n'y a pas de prolongement à gauche ni à droite)
  
 Conseil :
 - vérifiez que le concept fourni sous le mot-clé EVOL_NOLI contient suffisamment d'instants pour interpoler le champ souhaité
"""),

36 : _(u"""
 Erreur dans la découpe initiale des pas.
"""),

37 : _(u"""
 Attention, ARRET=NON donc poursuite du calcul sans avoir eu convergence.
"""),

67 : _(u"""
 Le code %(i1)d retourné lors de l'intégration de la loi de comportement n'est pas traité.  
"""),

93 : _(u"""
  -> Risque & Conseil :  dans le cas d'une résolution incrémentale, 
     on ne considère que la variation des variables de commande entre
     l'instant précédent et l'instant actuel.
     On  ne prend donc pas en compte d'éventuelles contraintes incompatibles
     dues à ces variables de commande initiales. 
     Pour tenir compte de ces contraintes vous pouvez :
     - partir d'un instant fictif antérieur où toutes les variables de 
       commande sont nulles ou égales aux valeurs de référence
     - choisir des valeurs de référence adaptées
     Pour plus d'informations, voir la documentation de STAT_NON_LINE 
     (U4.51.03) mot-clé EXCIT, et le test FORMA09 (V7.20.101).
"""),

94 : _(u"""
  -> Indications supplémentaires : pour la variable de commande :  %(k1)s
     et la composante :  %(k2)s
     Valeur maximum : %(r1)f sur la maille : %(k3)s
     Valeur minimum : %(r2)f sur la maille : %(k4)s
"""),

95 : _(u"""
  -> Indications supplémentaires : pour la variable de commande :  %(k1)s 
     et la composante :  %(k2)s
     Valeur maximum de abs( %(k2)s - %(k5)s_REF) : %(r1)f sur la maille : %(k3)s
     Valeur minimum de abs( %(k2)s - %(k5)s_REF) : %(r2)f sur la maille : %(k4)s
"""),



97 : _(u"""
  -> Les variables de commandes initiales induisent des contraintes 
     incompatibles : 
     l'état initial (avant le premier instant de calcul) est tel que 
     les variables de commande (température, hydratation, séchage...)
     conduisent à des contraintes non équilibrées. 
"""),

98 : _(u"""
  -> Les forces extérieures (chargement imposé et réactions d'appui) sont détectées comme quasiment nulles (%(r1)g).
     Or vous avez demandé une convergence avec le critère relatif (RESI_GLOB_RELA). 
     Pour éviter une division par zéro, le code est passé automatiquement en mode de convergence
     de type absolu (RESI_GLOB_MAXI).
     On a choisi un RESI_GLOB_MAXI de manière automatique et de valeur %(r2)g.
  -> Risque & Conseil : Vérifier bien que votre chargement doit etre nul (ainsi que les réactions d'appui) à cet instant 
     Dans le cas des problèmes de type THM, penser à utiliser éventuellement un 
     critère de type référence (RESI_REFE_RELA).
"""),

}
