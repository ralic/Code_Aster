#@ MODIF xfem Messages  DATE 16/06/2010   AUTEUR CARON A.CARON 
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
  -> Le calcul de la distance d'un noeud à l'ellipse n'a pas convergé 
     avec le nombre d'itérations maximal fixé (10). Cela est dû à une
     ellipse très allongée.
  -> Conseil:
     Contacter les développeurs.
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
  -> On a trouvé plus de 2 points de fond de fissure, ce qui est impossible en 2D.
  -> Risque & Conseil:
     Cela est normalement causé par une mauvaise définition des level sets.

     Si les level set ont été définies par DEFI_FISS_XFEM, veuillez revoir leur
     définition.

     Si les level sets ont été calculées par PROPA_FISS, vous pouvez essayer
     d'utiliser un maillage plus raffiné dans toute la zone de propagation ou bien
     une grille auxiliaire.
     Si vous avez utilisé la méthode simplexe avec restriction de la zone de mise à
     jour des level sets (ZONA_MAJ='TORE'), vous pouvez spécifier un rayon plus élevé
     de celui qui a été utilisé (écrit dans le fichier .mess) en utilisant l'opérande
     RAYON_TORE ou vous pouvez déactiver la restriction de la zone de mise à jour
     (ZONE_MAJ='TOUT').
     Sinon vous pouvez changer la méthode utilisée par PROPA_FISS (opérande
     METHODE_PROPA).
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
     - Si vous souhaitez définir du contact sur les lèvres de la fissure, il est préférable
       d'utiliser les modélisations 3D, C_PLAN ou D_PLAN afin de bénéficier des dernières 
       avancées concernant le contact avec X-FEM.
     - Les modélisations 3D_XFEM_CONT, C_PLAN_XFEM_CONT ou D_PLAN_XFEM_CONT sont réservés à
       l'ancienne formulation qui stocke des inconnues de contact aux arêtes.
       Si vous souhaitez tout de même utiliser l'ancienne formulation, le maillage doit être
       quadratique afin de stocker les inconnues de contact aux noeuds milieux. Le problème
       sera alors traité avec une interpolation linéaire.
     - Vous pouvez utilisez l'opérateur LINE_QUAD pour rendre le maillage quadratique.
"""),

17: _("""
     il ne faut qu'un mot-clé parmi RAYON_ENRI et NB_COUCHES.
"""),

18: _("""
     Dimension de l'espace incorrecte. 
     Le modèle doit etre 2D ou 3D et ne pas comporter de sous-structures.
"""),

19: _("""
     Il y a %(i1)s mailles dans la zone fissure. 
"""),

20: _("""
   Le mot-clef ORIE_FOND est indispensable en 3D si vous n'utilisez pas 
   le catalogue des formes de fissure prédéfinies : FORM_FISS pour définir
   les level-sets.
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

26: _("""
     Numeros des mailles de la zone fissure. 
"""),

27: _("""
     Si vous êtes en 3D pour l'approche de contact <<Grands glissements avec XFEM>>,
     seule la formulation aux noeuds sommets est possible.
     Vous pouvez activer cette formulation en commentant LINE_QUAD afin que les mailles
     soient de type HEXA8, PENTA6, PYRAM5 ou TETRA4.
"""),

28: _("""
     Pour un modèle XFEM avec contact utilisant l'approche Lagranges aux noeuds,
     il est indispensable d'utiliser ALGO_LAG='VERSION1' ou 'VERSION2'. On
     passe outre ALGO_LAG='NON' dans ce cas, et on utilise la version 2.
"""),

29: _("""
     Nombre de mailles contenant le fond de fissure : %(i1)s  
"""),

30: _("""
     Nombre de mailles de type Heaviside : %(i1)s  
"""),

31: _("""
     Nombre de mailles de type Crack-tip : %(i1)s  
"""),

32: _("""
     Nombre de mailles de type Heaviside Crack-tip : %(i1)s  
"""),

33: _("""
     Nombre de points du fond de fissure : %(i1)s  
"""),

34: _("""
     Nombre de fonds de fissure : %(i1)s  
"""),

35: _("""
     Coordonnées des points des fonds de fissure
"""),

36: _("""
     fond de fissure : %(i1)s  
"""),

37: _("""
     Nombre de level-sets réajustées : %(i1)s  
"""),

38: _("""
     Si vous êtes en 2D pour l'approche de contact <<Grands glissements avec XFEM>>,
     seule la formulation aux noeuds sommets est possible si la fissure possède un fond. 
     Vous pouvez activer cette formulation en commentant LINE_QUAD afin que les mailles
     soient de type QUAD4 ou TRIA3.
     
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
  -> Une face contient au moins 3 points d'intersection avec l'iso-zéro du champ
     de level-set car la valeur des level-sets aux noeuds de la maille a probablement été
     mal reactualisée lors de la phase de réinitialisation ou à la propagation précédente.
  -> Risque & Conseil:
     Vous pouvez utiliser un maillage plus raffiné ou bien une grille auxiliaire plus
     raffiné du maillage actuel.
     Vous pouvez vérifier que la zone de mise à jour des level sets est localisé autour du
     fond de la fissure (il ne faut pas utiliser ZONE_MAJ='TOUT' dans PROPA_FISS). Dans ce
     cas, si vous utilisez la méthode simplexe (METHODE='SIMPLEXE'), vous pouvez essayer
     d'utiliser un rayon de localisation plus élevé (opérande RAYON_TORE).
     Si vous utilisez la méthode simplexe, vous pouvez essayer d'utiliser la méthode upwind
     qui est plus robuste, stable et performante (METHODE='UPWIND').

     Dans tout le cas, il faut vérifier que l'angle de propagation de la fissure calculée
     par CALC_G a sens physique pour le problème à résoudre.
"""),

63: _("""
  -> ---Eléments XFEM quadratiques 2D---
       Un sous élément est intersecté par l'iso-zéro de la level set normale en deux endroits
       sur une arête.
       Cette configuration est proscrite.
"""),

64: _("""
  -> ---Eléments XFEM quadratiques 2D---
     Le calcul ne peut aboutir pour l'une des raisons suivante :
     - les calculs des coordonnées des points d'intersection entre un élément et la fissure
       se sont mal déroulés
     - l'élément ne peut être découpé selon la configuration de fissure qui le traverse
"""),

65: _("""
  -> ---Eléments XFEM quadratiques 2D---
     On recherche un point de l'iso-zéro de la level set normale intersectant l'arête
     d'un sous élément qui n'existe pas.
"""),

66: _("""
  -> ---Eléments XFEM quadratiques 2D---
     Le calcul d'abscisse curviligne sur une arête quadratique ne peut aboutir pour l'une des
     raisons suivante :
     - les trois points qui définissent l'arête quadratique sont identiques
     - l'arête est "trop" arrondie
"""),

67: _("""
  -> ---Eléments XFEM quadratiques 2D---
     Newton : nombre d'itérations maximal atteint
"""),

}
