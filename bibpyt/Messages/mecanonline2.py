#@ MODIF mecanonline2 Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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

cata_msg={

67: _("""
 Le code %(i1)d retourné lors de l'intégration de la loi de comportement n'est pas traité.  
"""),

93: _("""
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

94: _("""
  -> Indications supplémentaires : pour la variable de commande :  %(k1)s
     et la composante :  %(k2)s
     Valeur maximum : %(r1)f sur la maille : %(k3)s
     Valeur minimum : %(r2)f sur la maille : %(k4)s
"""),
95: _("""
  -> Indications supplémentaires : pour la variable de commande :  %(k1)s 
     et la composante :  %(k2)s
     Valeur maximum de abs( %(k2)s - %(k5)s_REF) : %(r1)f sur la maille : %(k3)s
     Valeur minimum de abs( %(k2)s - %(k5)s_REF) : %(r2)f sur la maille : %(k4)s
"""),

96: _("""
    -> Les surfaces en contact relatif ont bougé de plus de 5%%.
       Or vous n'avez pas activé la réactualisation géométrique (REAC_GEOM) automatique ou
       vous utiliser le mode "CONTROLE"
    -> Risque & Conseil : Vos résultats risquent d'etre faux, les mailles ne
       seront peut-etre pas correctement appariées et donc la condition de contact sera peut
       etre fausse.
       Si vous avez volontairement négligé la non-linéarité géoémtrique de contact (pour des raisons
       de performance), nous vous invitons à vérifier visuellement qu'il n'y a effectivement
       pas interpénétration.
"""),

97: _("""
  -> Les variables de commandes initiales induisent des contraintes 
     incompatibles : 
     l'état initial (avant le premier instant de calcul) est tel que 
     les variables de commande (température, hydratation, séchage...)
     conduisent à des contraintes non équilibrées. 
"""),

98: _("""
  -> Le chargement extérieur est nul (à la précision près).
     Or vous avez demandé une convergence avec le critère relatif (RESI_GLOB_RELA). 
     Pour éviter une division par zéro, le code est passé automatiquement en mode de convergence
     de type absolu (RESI_GLOB_MAXI)
  -> Risque & Conseil : Vérifier bien que votre chargement doit etre nul à cet instant 
     Dans le cas des problèmes de type THM, penser à utiliser éventuellement un 
     critère de type référence (RESI_REFE_RELA).
     La valeur automatique prise pour RESI_GLOB_MAXI est égale à 1E-6 fois la dernière valeur
     de résidu maximum à l'instant précédent. 
"""),

99: _("""
  -> Le chargement extérieur est nul (à la précision près).
     Or vous avez demandé une convergence avec le critère relatif (RESI_GLOB_RELA). 
  -> Risque & Conseil : Vérifier bien que votre chargement doit etre nul à cet instant 
     Le chargement est "nul" dans le cas de l'utilisation d'AFFE_CHAR_CINE en particulier.
     Il vous faut changer votre critère de convergence: RESI_GLOB_MAXI ou RESI_REFE_RELA
"""),

}
