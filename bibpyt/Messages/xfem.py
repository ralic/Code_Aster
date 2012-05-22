#@ MODIF xfem Messages  DATE 22/05/2012   AUTEUR GENIAUT S.GENIAUT 
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


cata_msg={

1: _(u"""
  -> Problème lors de l'orientation du fond de fissure : le nombre 
     de points de bords du fond de fissure devrait être pair. Or on 
     en trouve %(i1)d. 
  -> Conseil:
     Vérifiez la mise en données de la fissure.
     Notamment si GROUP_MA_FOND est utilisé, il ne doit pas coïncider
     avec un bord de la structure.
     Si vous êtes sûr de votre mise en données, contactez l'assistance.
"""),

2: _(u"""
  -> Le calcul de la distance d'un noeud à l'ellipse n'a pas convergé
     avec le nombre d'itérations maximal fixé (10). Cela est dû à une
     ellipse très allongée.
  -> Conseil:
     Contacter les développeurs.
     Dans la mesure du possible, définissez une ellipse moins allongée.
"""),


3: _(u"""
  -> Le modèle %(k1)s est incompatible avec la méthode X-FEM.
  -> Risque & Conseil:
     Vérifiez qu'il a bien été créé par l'opérateur MODI_MODELE_XFEM.
"""),

4: _(u"""
  -> Il est interdit de mélanger dans un modèle les fissures X-FEM
     avec et sans contact.
  -> Risque & Conseil:
     Veuillez rajouter les mots clés CONTACT manquants
     dans DEFI_FISS_XFEM.
"""),

5: _(u"""
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

6: _(u"""
  -> Le rayon d'enrichissement RAYON_ENRI doit être un réel strictement
     supérieur à 0.
"""),

7: _(u"""
     Il y a %(i1)d mailles %(k1)s
"""),

8: _(u"""
     Le nombre de %(k1)s X-FEM est limité à 10E6.
     Risque & Conseil:
     Veuillez réduire la taille du maillage.
"""),

9: _(u"""
     Le groupe de mailles donné pour définir la fissure contient des
     mailles qui ne sont pas connectées aux autres. Cela empêche
     d'orienter correctement la normale à la surface de la fissure.

     Risque & Conseil:
       Veuillez vérifier que les mailles données en entrée sont toutes
       connectées entre elles, c'est-à-dire qu'elle forme un groupe de
       mailles contiguës.
       Sinon, il faut définir une fissure pour chacun des groupes
       non connexes.
"""),

10: _(u"""
     La direction du champ thêta n'a pas été donnée. La direction automatique
     est une direction variable, basée sur le gradient de la level-set tangente.
"""),

11: _(u"""
  -> On a trouvé plus de 2 points de fond de fissure, ce qui est impossible en 2D.
  -> Risque & Conseil:
     Cela est normalement causé par une mauvaise définition des level-sets.

     Si les level-sets ont été définies par DEFI_FISS_XFEM, veuillez revoir leur
     définition.

     Si les level-sets ont été calculées par PROPA_FISS, vous pouvez essayer
     d'utiliser un maillage plus raffiné dans toute la zone de propagation ou bien
     une grille auxiliaire.
     Si vous avez utilisé la méthode simplexe avec restriction de la zone de mise à
     jour des level-sets (ZONA_MAJ='TORE'), vous pouvez spécifier un rayon plus élevé
     de celui qui a été utilisé (écrit dans le fichier .mess) en utilisant l'opérande
     RAYON_TORE ou vous pouvez déactiver la restriction de la zone de mise à jour
     (ZONE_MAJ='TOUT').
     Sinon vous pouvez changer la méthode utilisée par PROPA_FISS (opérande
     METHODE_PROPA).
"""),

12: _(u"""
  Le gradient de la level-set tangente est nul au noeud %(k1)s.
  Ceci est certainement du à un point singulier dans la définition de la level-set.
  Il vaut veiller à ce que ce point singulier ne soit pas inclus dans la couronne
  d'intégration du champ thêta.
  Conseil : réduisez la taille de la couronne du champ thêta : (mots-clés RSUP et RINF).
"""),

13: _(u"""
     Dans le modèle, des mailles SEG2 ou SEG3 possèdent des noeuds enrichis par X-FEM.
     Ceci n'est pas encore possible en 3D.
     Conseils : si ces mailles sont importantes pour le calcul (charge linéique...), il faut
     les mettre loin de de la fissure.
     Si ces mailles ne servent pas pour le calcul, il vaut mieux ne pas les affecter dans le modèle,
     ou bien les supprimer du maillage.
"""),

14: _(u"""
     On ne peut pas appliquer un cisaillement 2d sur les lèvres d'une fissure X-FEM.
"""),

15: _(u"""
  -> Cette option n'a pas encore été programmée.
  -> Risque & Conseil:
     Veuillez utiliser un autre chargement (en pression) ou contacter votre
     correspondant.
"""),

16: _(u"""
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

17: _(u"""
     il ne faut qu'un mot-clé parmi RAYON_ENRI et NB_COUCHES.
"""),

18: _(u"""
     Dimension de l'espace incorrecte.
     Le modèle doit être 2D ou 3D et ne pas comporter de sous-structures.
"""),

19: _(u"""
     Il y a %(i1)d mailles dans la zone fissure.
"""),

20: _(u"""
   Le mot-clef ORIE_FOND est indispensable en 3D si vous n'utilisez pas
   le catalogue des formes de fissure prédéfinies : FORM_FISS pour définir
   les level-sets.
"""),

21: _(u"""
     Le mot-clef ORIE_FOND n'est pas nécessaire en 2D.
"""),

22: _(u"""
     Plus d'une occurrence du mot-clef ORIE_FOND.
"""),

23: _(u"""
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une %(k1)s.
     Or la forme que vous avez sélectionnée < %(k2)s >
     correspond à une %(k3)s.
     Conseil :
     Sélectionnez une forme de %(k1)s.
"""),

24: _(u"""
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une fissure.
     Pour cela il est nécessaire de définir 2 level-sets : LT et LN.
     Conseil :
     Veuillez renseignez %(k1)s.
"""),

25: _(u"""
     Erreur dans le choix de la méthode de calcul des level-sets.
     Vous souhaitez définir une interface.
     Pour cela il ne faut est pas définir la level-set normale LT.
     %(k1)s ne sera pas considéré.
     Conseil :
     Pour ne plus obtenir ce message, ne renseignez pas %(k1)s.
"""),

26: _(u"""
     Numéros des mailles de la zone fissure.
"""),

27: _(u"""
     Si vous êtes en 3D pour l'approche de contact <<Grands glissements avec XFEM>>,
     seule la formulation aux noeuds sommets est possible.
     Vous pouvez activer cette formulation en commentant LINE_QUAD afin que les mailles
     soient de type HEXA8, PENTA6, PYRAM5 ou TETRA4.
"""),

28: _(u"""
     Pour un modèle XFEM avec contact utilisant l'approche Lagrange aux noeuds,
     il est indispensable d'utiliser ALGO_LAG='VERSION1' ou 'VERSION2'. On
     passe outre ALGO_LAG='NON' dans ce cas, et on utilise la version 2.
"""),

29: _(u"""
     Nombre de mailles contenant le fond de fissure : %(i1)d
"""),

30: _(u"""
     Nombre de mailles de type Heaviside : %(i1)d
"""),

31: _(u"""
     Nombre de mailles de type Crack-tip : %(i1)d
"""),

32: _(u"""
     Nombre de mailles de type Heaviside Crack-tip : %(i1)d
"""),

33: _(u"""
     Nombre de points du fond de fissure : %(i1)d
"""),

34: _(u"""
     Nombre de fonds de fissure : %(i1)d
"""),

35: _(u"""
     Coordonnées des points des fonds de fissure
"""),

36: _(u"""
     fond de fissure : %(i1)d
"""),

37: _(u"""
     Nombre de level-sets réajustées : %(i1)d
"""),

38: _(u"""
     Si vous êtes en 2D pour l'approche de contact <<Grands glissements avec XFEM>>,
     seule la formulation aux noeuds sommets est possible si la fissure possède un fond.
     Vous pouvez activer cette formulation en commentant LINE_QUAD afin que les mailles
     soient de type QUAD4 ou TRIA3.

"""),

39: _(u"""
     Erreur utilisateur : incohérence entre les mots-clés FISSURE et MODELE_IN.
     Il faut que les (ou la) fissure sous le mot-clé FISSURE soient toutes définies à
     partir du même modèle.
     Or :
     - la fissure %(k1)s est définie à partir du modèle %(k2)s
     - le modèle renseigné sous MODELE_IN est %(k3)s.
     Conseil :
     Veuillez revoir la définition de la fissure %(k1)s ou bien changer MODELE_IN.
"""),

40: _(u"""
      La maille %(k1)s doit être enrichie avec plus de 4 fonctions Heaviside,
      le multi-Heaviside est limité à 4 fonctions Heaviside, un noeud du maillage
      ne doit pas être connecté à plus de 4 fissures.
      Pour ne pas activer le multi-Heaviside, les fissures doivent être séparées de 2 mailles
      minimum. Veuillez raffiner le maillage entre les fissures (ou écarter les fissures).
"""),

41: _(u"""
      La maille %(k1)s est quadratique et elle est connectée à 2 fissures,
      le multi-Heaviside n'a été généralisé en quadratique.
      Pour ne pas activer le multi-Heaviside, les fissures doivent être séparées de 2 mailles
      minimum. Veuillez raffiner le maillage entre les fissures (ou écarter les fissures).
"""),

42: _(u"""
      La table de facteurs d'intensité des contraintes donnée en entré pour la fissure
      %(k1)s ne contient pas le bon numéro de fonds de fissure et/ou de points.

      Veuillez faire les vérifications suivantes:
      - si la fissure %(k1)s  est formée par un seul fond, veuillez vérifier d'avoir donné
        la bonne table
      - si la fissure %(k1)s est formée par plusieurs fonds, veuillez vérifier que la
        table donnée contient les valeurs des facteurs d'intensité des contraintes de
        chaque fond (voir colonne NUME_FOND de la table)
"""),


43: _(u"""
      Le contact autre que P1P1 est actif et la maille %(k1)s est connectée à 2 fissures,
      le multi-Heaviside ne peut pas être pris en compte si le contact autre que P1P1 est utilisé.
      Pour ne pas activer le multi-Heaviside, les fissures doivent être séparées de 2 mailles
      minimum. Veuillez raffiner le maillage entre les fissures (ou écarter les fissures).
"""),

44: _(u"""
      La maille %(k1)s est connectée à 2 fissures, or il s'agit d'une maille possédant des
      enrichissements de fond de fissure.
      Le multi-Heaviside ne peut pas être pris en compte en fond de fissure.
      Pour ne pas activer le multi-Heaviside, les fissures doivent être séparées de 2 mailles
      minimum. Veuillez raffiner le maillage entre les fissures (ou écarter les fissures).
"""),

45: _(u"""
      Jonction X-FEM et contact

      Une facette de contact XFEM doit être redécoupée. Ceci n'est pas implémenté pour l'instant.
      Les efforts de contact ne seront pas prise en compte sur cette facette.
"""),

46: _(u"""
      Les fissures sont mal ordonnées dans le mot clé FISSURE de MODI_MODELE_XFEM

      L'ordre dans lequel sont définis les fissures avec l'utilisation du mot clé JONCTION impose que %(k1)s doit
      être donné après %(k2)s. Veiller permuter %(k1)s et %(k2)s dans le mot clé FISSURE de MODI_MODELE_XFEM.
"""),

47: _(u"""
      La fissure %(k1)s est déjà attaché à la fissure %(k2)s, on ne peut pas l'attacher à %(k3)s.

      Il est possible d'attacher globalement %(k1)s à la fois à %(k2)s et %(k3)s,
      mais il ne faut pas qu'un élément soit connecté à la fois à %(k2)s, %(k3)s et %(k1)s.

      Pour résoudre ce problème, soit il faut écarter (ou raffiner le maillage entre) les fissures %(k2)s et %(k3)s.
      Soit il faut lier la fissure %(k3)s à la fissure %(k2)s en ajoutant une ligne du type
      JONCTION=_F(FISSURE=%(k2)s,POINT=...) lorsqu'on appelle DEFI_FISS_XFEM pour définir %(k3)s.
"""),

48: _(u"""
     Le calcul de G avec X-FEM est impossible avec COMP_INCR.
"""),

49: _(u"""
     Le calcul de G avec X-FEM est impossible en grandes déformations.
"""),

50: _(u"""
     La méthode X-FEM n'est pas disponible avec 'PETIT_REAC'.
"""),

57: _(u"""
  -> Aucune maille de fissure n'a été trouvée.
  -> Risque & Conseil :
     Suite des calculs risquée.
"""),

58: _(u"""
  -> Aucun point du fond de fissure n'a été trouvé !
     Cela signifie que le fond de fissure se trouve en dehors de la structure.

  -> Risque & Conseil :
     - Si vous souhaitiez définir une interface, il faut choisir TYPE_DISCONTINUITE = 'INTERFACE'
        pour ne plus avoir ce message.
     -  Si vous souhaitiez définir une fissure, il doit y avoir une erreur lors de la définition
        de la level-set tangente Vérifier la définition des level-sets.
"""),

59: _(u"""
     Ne pas utiliser le mot-clef RAYON_ENRI lorsque le fond de fissure
     est en dehors de la structure.
"""),

60: _(u"""
  -> Le point initial de fissure n'est pas un point de bord de fissure,
     bien que la fissure soit débouchante
  -> Risque & Conseil:
     Assurez-vous de la bonne définition de PFON_INI.
"""),

61: _(u"""
  -> Une face contient au moins 3 points d'intersection avec la courbe d'isovaleur zéro du champ
     de level-set car la valeur des level-sets aux noeuds de la maille a probablement été
     mal réactualisée lors de la phase de réinitialisation ou à la propagation précédente.
  -> Risque & Conseil:
     Vous pouvez utiliser un maillage plus raffiné ou bien une grille auxiliaire plus
     raffiné du maillage actuel.
     Vous pouvez vérifier que la zone de mise à jour des level-sets est localisé autour du
     fond de la fissure (il ne faut pas utiliser ZONE_MAJ='TOUT' dans PROPA_FISS). Dans ce
     cas, si vous utilisez la méthode simplexe (METHODE='SIMPLEXE'), vous pouvez essayer
     d'utiliser un rayon de localisation plus élevé (opérande RAYON_TORE).
     Si vous utilisez la méthode simplexe, vous pouvez essayer d'utiliser la méthode UPWIND
     qui est plus robuste, stable et performante (METHODE='UPWIND').

     Dans tout le cas, il faut vérifier que l'angle de propagation de la fissure calculée
     par CALC_G a sens physique pour le problème à résoudre.
"""),

63: _(u"""
  -> ---Éléments XFEM quadratiques 2D---
       Un sous élément est découpé par la courbe d'isovaleur zéro de la level-set normale en deux endroits
       sur une arête.
       Cette configuration est proscrite.
"""),

64: _(u"""
  -> ---Éléments XFEM quadratiques 2D---
     Le calcul ne peut aboutir pour l'une des raisons suivante :
     - les calculs des coordonnées des points d'intersection entre un élément et la fissure
       se sont mal déroulés
     - l'élément ne peut être découpé selon la configuration de fissure qui le traverse
"""),

65: _(u"""
  -> ---Éléments XFEM quadratiques 2D---
     On recherche un point de la courbe d'isovaleur zéro de la level-set normale découpant l'arête
     d'un sous élément qui n'existe pas.
"""),

66: _(u"""
  -> ---Éléments XFEM quadratiques 2D---
     Le calcul d'abscisse curviligne sur une arête quadratique ne peut aboutir pour l'une des
     raisons suivante :
     - les trois points qui définissent l'arête quadratique sont identiques
     - l'arête est "trop" arrondie
"""),

67: _(u"""
  -> ---Éléments XFEM quadratiques 2D---
     Newton : nombre d'itérations maximal atteint
"""),

68: _(u"""
  -> Aucune grille n'est associée à la fissure donnée par FISS_GRILLE.

  -> Risque & Conseil:
     Veuillez donner une fissure avec une grille associée.

"""),

69: _(u"""
  -> La fissure à propager a été définie par DEFI_FISS_XFEM en donnant directement les deux
     champs level-sets (mots-clés CHAMP_NO_LSN et CHAMP_NO_LST).
     Aucune grille auxiliaire n'a été associée à cette fissure.

  -> Risque & Conseil:
     Dans le cas où les deux champs level-sets ont été obtenus d'une fissure propagée par
     PROPA_FISS, les informations sur la localisation du domaine (mot-clé ZONE_MAJ) et sur
     l'utilisation d'une grille auxiliaire ont été perdues, ce qui fait que le calcul de la
     propagation de la fissure pourrait donner des résultats faux.

     Vous pouvez ignorer cette alarme seulement si les deux champs level-sets donnés dans
     DEFI_FISS_XFEM:
     - n'ont pas été calculés par PROPA_FISS, c'est-à-dire qu'ils n'ont pas été extraits
       d'une fissure propagée par PROPA_FISS
     - ont été extraits d'une fissure propagée par PROPA_FISS sans grille auxiliaire associée
       et la mise à jours des level-sets a été faite sur tous les noeuds du maillage
       (mot-clé ZONE_MAJ='TOUT' dans PROPA_FISS)

     Dans tous les autres cas, pour éviter des résultats faux, il faut absolument associer
     une grille auxiliaire à la fissure à propager, grille héritée de la fissure de laquelle
     les deux level-sets ont été extraites (mot-clé FISS_GRILLE de DEFI_FISS_XFEM).

"""),

70: _(u"""
  -> Un élément convexe a été détecté. Cela peut-être dû à l'imposition de DTAN sur les bords.
     Ceci pourrait être une source d'erreur lors du calcul des level-sets avec DEFI_FISS_XFEM.

  -> Pour éviter ce problème, DAMAX a été diminué de %(r1)f à %(r2)f. Le nombre de cycles a donc
     été modifié : NBCYCL = %(r3)f

"""),

}
