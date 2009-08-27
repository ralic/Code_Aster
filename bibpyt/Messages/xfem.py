#@ MODIF xfem Messages  DATE 24/08/2009   AUTEUR GENIAUT S.GENIAUT 
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
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une %(k1)s.
     Or la forme que vous avez sélectionnée < %(k2)s >
     correspond à une %(k3)s.
     Conseil :
     Sélectionnez une forme de %(k1)s.
"""),

24: _("""
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une fissure.
     Pour cela il est nécessaire de définir 2 level sets : LT et LN.
     Conseil :
     Veuillez renseignez %(k1)s.
"""),

25: _("""
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une interface.
     Pour cela il ne faut est pas définir la level set normale LT.
     %(k1)s ne sera pas considéré.
     Conseil :
     Pour ne plus obtenir ce message, ne renseignez pas %(k1)s.
"""),

27: _("""
     Si vous êtes en 3D pour l'approche de contact <<Grands glissements avec XFEM>>,
     seul la formulation aux noeuds sommets est possible, les mailles doivent être de type
     HEXA8, PENTA6 ou TETRA4
"""),

28: _("""
     Pour un modèle XFEM avec contact utilisant l'approche Lagranges aux noeuds,
     il est indispensable d'utiliser ALGO_LAG='VERSION1' ou 'VERSION2'. On
     passe outre ALGO_LAG='NON' dans ce cas, et on utilise la version 2.
"""),

40: _("""
     Trop d'aretes traversées par la fissure sont connectées au noeud %(i1)d.
     Nombre maximum d'aretes toléré: %(i2)d. 
"""),

57: _("""
  -> Aucune maille de fissure n'a été trouvée. 
  -> Risque & Conseil :
     Suite des calculs risquée.
"""),

58: _("""
  -> Aucun point du fond de fissure n'a été trouvé !
     Cela signifie que le fond de fissure se trouve en dehors de la structure.

  -> Risque & Conseil :
     - Si vous souhaitiez définir une interface, il faut choisir TYPE_DISCONTINUITE = 'INTERFACE'
        pour ne plus avoir ce message.
     -  Si vous souhaitiez définir une fissure, il doit y avoir une erreur lors de la définition 
        de la level set tangente Vérifier la définition des level sets.
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

61: _("""
  -> Une face contient a priori au moins 3 points d'intersection avec l'iso-zéro du champ
     de level-set car la valeur des level-sets aux noeuds de la maille a probablement été
     mal reactualisée lors de la phase de réinitialisation.
  -> Risque & Conseil:
     Tentez de réduire le rayon d'estimation du résidu pour accélerer la convergence de la 
     réinitialisation et limiter ce risque d'anomalie.
"""),

}
