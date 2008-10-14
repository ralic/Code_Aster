#@ MODIF xfem Messages  DATE 14/10/2008   AUTEUR GENIAUT S.GENIAUT 
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

1: _("""
  -> Les fissures X-FEM sont surement trop proches.
     Il faut au minimum 2 mailles entre les fissures.
  -> Risque & Conseil:
     Veuillez raffiner le maillage entre les fissures
     (ou écarter les fissures). 
"""),

2: _("""
  -> Le calcul de la distance d'un noued à l'ellipse n'a pas convergé 
     avec le nombre d'itérations maximal fixé (10). Cela est dû à une
     ellipse très allongée.
  -> Conseil:
     Contacter les développeur.
     Dans la mesure du possible, définissez une ellipse moins allongée.
"""),


3: _("""
  -> Le modèle %(k1)s est incompatible avec la méthode X-FEM.
  -> Risque & Conseil:
     Vérifier qu'il a bien été créé par l'opérateur MODI_MODELE_XFEM. 
"""),

4: _("""
  -> Il est interdit de mélanger dans un modèle les fissures X-FEM 
     avec et sans contact. 
  -> Risque & Conseil:
     Veuillez rajouter les mots clés CONTACT manquants 
     dans DEFI_FISS_XFEM.
"""),

5: _("""
  -> Attention, vous avez défini un enrichissement géométrique sur %(i1)d
     couches d'éléments autour du fond de fissure. 
  -> Risque :
     Au delà de 7 couches, il y a des risques de pivots nuls lors de la 
     résolution dans STAT_NON_LINE.
  -> Conseils :
     Pour éviter ces risques de pivots nuls, il est conseillé de ne pas 
     dépasser NB_COUCHES = 7.
     Vous pouvez aussi laisser NB_COUCHES = %(i1)d, mais il pourra s'avérer
     nécessaire d'augmenter le nombre maximales de décimales perdues dans 
     STAT_NON_LINE (mot-clé NPREC de SOLVEUR pour les méthodes LDLT, MULT_FRONT
     ou FETI.
"""),

6: _("""
  -> Le rayon d'enrichissement RAYON_ENRI doit être un réel strictement 
     supérieur à 0.
"""),

7: _("""
     Il y a %(i1)s mailles %(k1)s 
"""),

8: _("""
     Le nombre de %(k1)s X-FEM est limité à 10E6. 
     Risque & Conseil:
     Veuillez réduire la taille du maillage.
"""),

9: _("""
     L'option K_G_MODA n'est pas autorisée avec une fissure définie 
     par la commande DEFI_FISS_XFEM (méthode X-FEM).
"""),

10: _("""
     La direction du champ theta n'a pas été donnée. La direction automatique
     est une direction variable, basée sur le grandient de la level set tangente.
"""),

11: _("""
  -> On a trouvé plus de 2 points de fond de fissure, ce qui est impossible en 2d.
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

12: _("""
  Le gradient de la level set tangente est nul au noeud %(k1)s.
  Ceci est certainement du à un point singulier dans la définition de la levet set.
  Il vaut veuiller à ce que ce point singulier ne soit pas inclus dans la couronne
  d'intégration du champ theta. 
  Conseil : réduisez la taille de la couronne du champ theta : (mot-clés RSUP et RINF).
"""),

13: _("""
     Dans le modèle, des mailles SEG2 ou SEG3 possèdent des noeuds enrichis par X-FEM.
     Ceci n'est pas encore possible en 3D.
     Conseils : si ces mailles sont importantes pour le calcul (charge linéique...), il faut
     les mettre loin de de la fissure.
     Si ces mailles ne servent pas pour le calcul, il vaut mieux ne pas les affecter dans le modèle,
     ou bien les supprimer du maillage.
"""),

14: _("""
     On ne peut pas appliquer un cisaillement 2d sur les lèvres d'une fissure X-FEM.
"""),

15: _("""
  -> Cette option n'a pas encore été programmée.
  -> Risque & Conseil:
     Veuillez utiliser un autre chargement (en pression) ou contacter votre
     correspondant.
"""),

16: _("""
  -> Il n'y a aucun élément enrichi.
  -> Risque & Conseil:
     - Si le contact est défini sur les lèvres de la fissure, la modélisation
       doit etre 3D_XFEM_CONT ou C_PLAN_XFEM_CONT ou D_PLAN_XFEM_CONT.
     - Si le contact n'est pas défini sur les lèvres de la fissure,
       la modélisation doit etre 3D ou C_PLAN ou D_PLAN'.
"""),

17: _("""
     il ne faut qu'un mot-clé parmi RAYON_ENRI et NB_COUCHES.
"""),

18: _("""
     Dimension de l'espace incorrecte. 
     Le modèle doit etre 2D ou 3D et ne pas comporter de sous-structures.
"""),

21: _("""
     Le mot-clef ORIE_FOND n'est pas nécessaire en 2D.
"""),

22: _("""
     Plus d'une occurrence du mot-clef ORIE_FOND.
"""),

23: _("""
  -> Erreur dans le choix de la méthode de calcul des level-sets
  -> Risque & Conseil :
     Veuillez renseignez FONC_LT/LN ou GROUP_MA_FISS/FOND.
"""),

26: _("""
     L'approche <<Grands glissements avec XFEM>> fonctionne seulement pour le cas 2D.
"""),

27: _("""
     Seulement les mailles QUAD4 sont prises en compte par l'approche
     <<Grands glissements avec XFEM>>.
"""),

57: _("""
  -> Aucune maille de fissure n'a été trouvée. 
  -> Risque & Conseil :
     Suite des calculs risquée.
"""),

58: _("""
  -> Aucun point du fond de fissure n'a été trouvé !
  -> Risque & Conseil :
     Ce message est normal si vous souhaitiez définir une interface (et non une fissure).
     Si vous souhaitiez définir une fissure, la définition des level sets (Méthode XFEM)
     ne permet pas de trouver de points du fond de fissure à l'intèrieur de la structure.
     Il doit y avoir une erreur lors de la définition de la level set tangente.
     Vérifier la définition des level sets.
"""),

59: _("""
     Ne pas utiliser le mot-clef RAYON_ENRI lorsque le fond de fissure
     est en dehors de la structure.
"""),

60: _("""
  -> Le point initial de fissure n'est pas un point de bord de fissure,
     bien que la fissure soit débouchante
  -> Risque & Conseil:
     Assurez-vous de la bonne définition de PFON_INI.
"""),


}
