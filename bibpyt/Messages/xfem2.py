#@ MODIF xfem2 Messages  DATE 07/11/2011   AUTEUR COURTOIS M.COURTOIS 
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
  -> On ne peut pas faire propager une interface.
     Seule les fissures (possédant un fond de fissure) peuvent être propagées.
"""),


2 : _(u"""
  -> Seules les modélisations C_PLAN/D_PLAN sont disponibles pour XFEM.
  -> Risques et conseils:
     Veuillez considérer l'une des deux modélisations dans AFFE_MODELE.
"""),

3 : _(u"""
  -> Erreur utilisateur : Pour le post-traitement de visualisation en présence
     de contact aux arêtes ('P1P1A' dans la commande MODI_MODELE_XFEM), il faut
     obligatoirement renseigner le mot-clé MAILLAGE_SAIN de la commande %(k1)s.
  -> Risques et conseils:
     Veuillez renseigner le mot-clé MAILLAGE_SAIN avec le maillage linéaire.
"""),

4 : _(u"""
  -> Pour le post-traitement de visualisation standard (sans présence
     de contact aux arêtes dans la commande MODI_MODELE_XFEM), le mot-clé
     MAILLAGE_SAIN de la commande %(k1)s ne sert à rien. Il ne sera pas
     utilisé.
  -> Risques et conseils:
     Pour ne plus avoir cette alarme, veuillez supprimer le mot-clé MAILLAGE_SAIN
     de la commande %(k1)s.
"""),

7 : _(u"""
  -> Le contact a été activé dans XFEM (CONTACT_XFEM='OUI' dans MODI_MODELE_XFEM)
  -> Risque & Conseil:
     Vous devez également l'activer dans AFFE_CHAR_MECA/CONTACT_XFEM
"""),

8 : _(u"""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM.
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à
     AFFE_CHAR_MECA/CONTACT un modèle XFEM.
"""),

9 : _(u"""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM avec contact.
  -> Risque & Conseil:
     Veuillez activer CONTACT='OUI' dans MODI_MODELE_XFEM.
"""),

11 : _(u"""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas
     le modèle XFEM utilisé dans le AFFE_CHAR_MECA/CONTACT nommé %(k2)s.
  -> Risque & Conseil:
     Risques de résultats faux.
"""),

12 : _(u"""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas un modèle
     XFEM.
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à
     AFFE_CHAR_MECA/CONTACT_XFEM un modèle XFEM.
"""),

15 : _(u"""
  -> Point de FOND_FISS sans maille de surface rattachée.
  -> Risque & Conseil:
     Veuillez revoir la définition des level-sets.
"""),

17 : _(u"""
  -> Segment de FOND_FISS sans maille de surface rattachée
  -> Risque & Conseil:
     Veuillez revoir la définition des level-sets.
"""),

20 : _(u"""
  -> PFON_INI = POINT_ORIG
  -> Risque & Conseil :
     Veuillez définir deux points différents pour PFON_INI et POINT_ORIG.
"""),

21 : _(u"""
  -> Problème dans l'orientation du fond de fissure : POINT_ORIG mal choisi.
  -> Risque & Conseil :
     Veuillez redéfinir POINT_ORIG.
"""),

22 : _(u"""
  -> Tous les points du fond de fissure sont des points de bord.
  -> Risque & Conseil :
     Assurez-vous du bon choix des paramètres d'orientation de fissure.
"""),

23 : _(u"""
  -> PFON_INI semble être un point mal choisi, on le modifie automatiquement.
"""),

25 : _(u"""
  -> La norme du vecteur VECT_ORIE est nulle.
  -> Risque & Conseil :
     Veuillez redéfinir VECT_ORIE.
"""),


50 : _(u"""
  -> Le maillage utilisé pour la représentation des level-sets est 2D
     mais il contient des éléments 1D aussi.
  -> La méthode UPWIND sélectionnée dans PROPA_FISS peut gérer des
     grilles 2D définies seulement par des éléments QUAD4.
  -> Risque & Conseil:
     Veuillez donner un maillage défini seulement par des éléments
     QUAD4.
  """),

51 : _(u"""
  -> Il n'y a aucune maille enrichie.
  -> Risque & Conseil:
     Veuillez vérifier les définitions des level-sets.
  """),

52 : _(u"""
  -> Le maillage utilisé pour la représentation des level-sets est 3D
     mais il contient des éléments 2D et/ou 1D aussi.
  -> La méthode UPWIND sélectionnée dans PROPA_FISS peut gérer des
     grilles 3D définies seulement par des éléments HEXA8.
  -> Risque & Conseil:
     Veuillez donner un maillage défini seulement par des éléments
     HEXA8.
  """),

53 : _(u"""
  -> Dans le maillage utilisé pour la représentation des level-sets,
     il y a des éléments qui ne sont pas disponibles pour la méthode
     UPWIND (PROPA_FISS).
  -> Risque & Conseil:
     Veuillez vérifier le maillage et utiliser uniquement des éléments
     QUAD4 en 2D et HEXA8 en 3D.
  """),

54 : _(u"""
  -> Il n'y a pas d'éléments disponibles pour la méthode UPWIND
     (PROPA_FISS) dans le maillage utilisé pour la représentation
     des level-sets.
  -> Risque & Conseil:
     Veuillez vérifier le maillage et utiliser uniquement des éléments
     QUAD4 en 2D et HEXA8 en 3D.
  """),

55 : _(u"""
  -> Dans le maillage utilisé pour la représentation des level-sets
     (PROPA_FISS), il y a des arêtes qui ne sont pas orthogonales aux
     autres arêtes.
  -> Risque & Conseil:
     Risques de résultats faux.
     Veuillez vérifier que toutes les arêtes des éléments du maillage
     soient orthogonales entre elles.
  """),

56 : _(u"""
  -> Aucun noeud n'a été trouvé pour le calcul du résidu local.
  -> Le calcul du résidu local n'est pas possible.
  -> Risque & Conseil:
     Veuillez vérifier que la fissure n'est pas à l'extérieur du
     maillage après la propagation actuelle.
  """),

57 : _(u"""
  -> La définition de un ou plusieurs éléments du maillage utilisé pour
     la représentation des level-sets (PROPA_FISS) n'est pas correcte.
  -> Risque & Conseil:
     Il y a une arête avec une longueur nulle dans le maillage.
     Veuillez vérifier la définition des éléments du maillage (par
     exemple: un noeud est utilisé seulement une fois dans la définition
     d'un élément; il n'y a pas de noeuds doubles...)
  """),

58 : _(u"""
  -> La dimension (2D ou 3D) du modèle physique et la dimension (2D ou
     3D) du modèle utilisé pour la grille auxiliaire ne sont pas égales.
  -> Risque & Conseil:
     Veuillez utiliser deux modèles avec la même dimension (les deux 2D
     ou les deux 3D).
  """),

60 : _(u"""
  -> L'opérande TEST_MAIL a été utilisée dans l'opérateur PROPA_FISS.
     La même vitesse d'avancée est utilisée pour tous les points du
     fond de fissure et l'angle de propagation est fixé égal à zéro.
  -> Risque & Conseil:
     L'avancée de la fissure n'est pas liée aux contraintes affectant
     la structure et donc les résultats de la propagation n'ont pas
     une signification physique.
     L'opérande TEST_MAIL doit être utilisé uniquement pour vérifier
     si le maillage est suffisamment raffiné pour la représentation
     des level-sets.
  """),

63 : _(u"""
  -> La valeur de l'avancée DA_MAX utilisée est petite par rapport à la
     longueur de la plus petite arrête du maillage utilisé pour
     la représentation des level-sets:
     DA_MAX = %(r1)f
     Longueur minimale arrêt = %(r2)f
  -> Risque & Conseil:
     Risques de résultats faux. Veuillez vérifier les résultats en
     utilisant un maillage plus raffiné pour la représentation des
     level-sets.
  """),

64 : _(u"""
  -> La valeur du RAYON est plus petite que la longueur de la plus petite
     arrête du maillage utilisé pour la représentation des level-sets:
     RAYON = %(r1)f
     LONGUEUR minimale arrêt = %(r2)f
  -> Le calcul du résidu local n'est pas possible.
  -> Risque & Conseil:
     Veuillez utiliser une valeur du RAYON plus grande.
  """),

65 : _(u"""
  -> Le nombre maximal d'itérations a été atteint.
  -> Risque & Conseil:
     Essayer d'utiliser un maillage plus raffiné, ou bien une grille auxiliaire.
  """),

66 : _(u"""
  -> Le taux de restitution d'énergie G est négatif sur certains des
     noeuds du fond de fissure : le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...).
  """),

68 : _(u"""
  -> Le nombre des résultats dans un des tableaux des facteurs
     d'intensité de contraintes (SIF) donné à PROPA_FISS est supérieur
     à deux.
  -> Risque & Conseil:
     Veuillez donner des tableaux avec seulement les SIF correspondant
     aux conditions de chargement maximal et minimal du cycle de
     fatigue.
     Dans le cas de tableau contenant un seul résultat, on se place dans
     l'hypothèse de rapport de charge égal à zéro (R=0).
  """),

71 : _(u"""
     Un tableau doit être donné pour chaque fissure du modèle.

     Attention! Si une fissure est formée par plusieurs morceaux, un
     tableau n'est pas suffisant et on doit donner un tableau pour
     chaque morceau.
  """),

72 : _(u"""
  -> L'angle de propagation de la fissure n'est pas constant dans le
     cycle de chargement.
  -> La valeur de la première configuration donnée dans les tableaux des
     facteurs d'intensité de contraintes a été retenue.
  -> Risque & Conseil:
     Risques des résultats faux.
     Veuillez vérifier les conditions de chargement du modèle.
  """),

73 : _(u"""
  -> L'option NB_POINT_FOND a été utilisé dans PROPA_FISS mais le
     modèle est 2D.
  -> Risque & Conseil:
     Ne pas utiliser cette option avec un modèle 2D.
  """),

74 : _(u"""
  -> Aucune fissure du modèle ne propage.
  -> Risque & Conseil:
     Veuillez vérifier les conditions du chargement du modèle et les
     constantes de la loi de propagation données à PROPA_FISS.
  """),

76 : _(u"""
  -> Une seule valeur des facteurs d'intensité de contraintes (SIF) a
     été donné pour chaque point du fond de la fissure. Cela ne permit
     pas de bien définir le cycle de fatigue.
  -> En défaut, le rapport de charge du cycle de fatigue a été fixé égal
     à zéro et les  SIF donnés ont été affectés à le chargement maximal
     du cycle.
  -> Risque & Conseil:
     Veuillez vérifier si les hypothèses faits ci-dessus sont correctes.
     Dans ce cas, c'est mieux de les bien expliciter en utilisant dans
     PROPA_FISS l'opérateur COMP_LINE comme ceci:

     COMP_LINE=_F(COEF_MULT_MINI=0.,
                  COEF_MULT_MAXI=1.),

     Cela permit d'éviter aussi l'émission de cette alarme.

     Sinon, si le rapport de charge du cycle de fatigue n'est pas égal à
     zéro, veuillez donner un tableau avec les SIF correspondants à les
     conditions de chargement maximal et minimal du cycle de fatigue.
  """),

77 : _(u"""
  -> La valeur des facteurs d'intensité de contraintes (SIF) n'a pas
     été trouvée pour un point du fond de fissure:
     Nom de la fissure = %(k1)s
     Nombre des morceaux = %(i1)d
     Morceau élaboré = %(i2)d
     Point inexistant = %(i3)d
  -> Risque & Conseil:
     Veuillez vérifier que les tableaux de SIF donnés dans l'opérateur
     PROPA_FISS sont corrects. Si NB_POINT_FOND a été utilisé, veuillez
     vérifier aussi que les valeurs données sont correctes.
  """),

78 : _(u"""
  -> L'option NB_POINT_FOND a été utilisée dans PROPA_FISS
     mais le nombre de valeurs données n'est pas égale au nombre total
     des morceaux des fissures dans le modèle.
  -> Nombre total de valeurs données %(k2)s au nombre total
     des morceaux des fissures du modèle.

  -> Conseil:
     Veuillez vérifier que l'option NB_POINT_FOND a été utilisée
     correctement dans PROPA_FISS et que les valeurs données pour
     chaque fissure sont correctes.
  """),

79 : _(u"""
  -> Une des valeurs donnée pour NB_POINT_FOND n'est pas valide.
  -> Risque & Conseil:
     Veuillez vérifier que toutes les valeurs sont égales à 0 ou
     supérieures à 1.
  """),

80 : _(u"""
  -> Le nombre des valeurs dans un des tableaux des facteurs
     d'intensité de contraintes (SIF) est supérieur au nombre des
     points du fond de la fissure correspondante.
  -> Risque & Conseil:
     Veuillez vérifier que les tableaux de SIF donnés par l'opérateur
     PROPA_FISS sont corrects. Si NB_POINT_FOND a été utilisé, veuillez
     vérifier aussi que la liste donnée pour chaque fissure est correcte.
  """),

81 : _(u"""
  -> Les valeurs de COEF_MULT_MAXI et COEF_MULT_MINI de COMP_LINE données
     dans l'opérateur PROPA_FISS sont égales à zéro.
  -> Risque & Conseil:
     Au moins une des deux valeurs doit être différente de zéro pour
     avoir une cycle de fatigue. Veuillez vérifier les valeurs données.
  """),

82 : _(u"""
  -> L'opérande COMP_LINE a été utilisée dans PROPA_FISS et il y a
     plusieurs résultats dans un des tableaux des facteurs d'intensité
     de contraintes (SIF).
  -> Risque & Conseil:
     Veuillez donner des tableaux avec seulement les SIF correspondant à
     la conditions de chargement de référence.
  """),

83 : _(u"""
  -> Le taux de restitution maximal d'énergie G dans le cycle de fatigue
     est zéro sur certains des noeuds du fond de fissure:
     le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...) et les chargements affectant le
     modèle.
"""),

84 : _(u"""
  -> Le taux de restitution d'énergie G ne change pas dans le cycle de
     fatigue sur certains des noeuds du fond de fissure:
     le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...) et les chargements affectant le
     modèle.
"""),

85 : _(u"""
   Les propriétés matériaux dépendent de la température. La température en fond
   de fissure n'étant pas connue, le calcul se poursuit en prenant la température
   de référence du matériau (TEMP = %(r1)f).
"""),

86 : _(u"""
 -> Le maillage/la grille sur lequel/laquelle vous voulez créer le group
    n'est pas associé/associée à la fissure donnée.

 -> Risque & Conseil:
    Veuillez vérifier d'avoir spécifié le bon maillage/grille et/ou
    la bonne fissure.
"""),

87 : _(u"""
  -> L'opérande TEST_MAIL a été utilisé dans l'opérateur PROPA_FISS.
  -> Cet opérande n'a pas des sens que pour un modèle 3D.
  -> Risque & Conseil:
     Ne pas utiliser TEST_MAIL pour un modèle 2D.
  """),

88 : _(u"""
  -> La valeur du rayon du tore de localisation de la zone de mise à
     jour est supérieure à la valeur limite. Cette dernière est
     déterminée par la valeur du rayon du tore utilisée à la propagation
     précédente et la valeur de l'avancée de la fissure (DA_MAX) imposé
     à la propagation courante.

     Rayon actuel = %(r1)f
     Rayon limite = %(r2)f

  -> Risque & Conseil:
     Risques de résultats faux si la fissure ne propage pas en mode I.

     Pour éviter ce risque, vous pouvez utiliser la même avancée de la
     fissure (DA_MAX) et le même rayon (RAYON) que ceux qui ont été
     utilisés à la propagation précédente.

     Si vous ne pouvez pas utiliser les même valeurs, vous pouvez
     choisir une des solutions suivantes:
     - donner une valeur de RAYON_TORE inférieure à la valeur limite
       pour la propagation courante
     - utiliser une valeur de RAYON_TORE plus grande pour les
       propagations précédentes
     - augmenter l'avancée de la fissure (DA_MAX) à la propagation
       courante

     Sinon, même si fortement déconseillé, vous pouvez choisir de ne pas
     utiliser la localisation de la zone de mise à jour
     (ZONE_MAJ='TOUT').
  """),

89 : _(u"""
  -> La fissure à propager n'existe pas dans le modèle:
     FISS_ACTUELLE = %(k1)s
     MODELE        = %(k2)s
  -> Conseil:
     Veuillez vérifier que la fissure et le modèle sont correctement
     définis.
  """),

90 : _(u"""
  -> Un ou plusieurs tableaux des facteurs d'intensité de contraintes
     ne contient pas la colonne NUME_FOND.
  -> Conseil:
     Veuillez ajouter cette colonne aux tableaux (voir documentation
     utilisateur de PROPA_FISS).
  """),

91 : _(u"""
  -> Le nouveau fond de fissure n'est pas très régulier. Cela signifie
     que le maillage ou la grille auxiliaire utilisés pour la
     représentation de la fissure par level-sets ne sont pas
     suffisamment raffinés pour bien décrire la forme du fond de la
     fissure utilisée.
  -> Risque & Conseil:
     Risques de résultats faux en utilisant le maillage ou la grille
     auxiliaire testés. Veuillez utiliser un maillage ou une grille
     auxiliaire plus raffinés.
  """),

92 : _(u"""
  -> Vous avez demandé la création d'un group de noeuds dans un tore
     construit autour du fond de la fissure suivante:

     FISSURE = %(k1)s

     Toutefois cette fissure a été calculée par PROPA_FISS en utilisant
     la localisation du domaine (ZONE_MAJ='TORE', par défaut).
     Dans ce cas le group de noeuds doit être forcement défini en
     utilisant le tore déjà utilisé par PROPA_FISS.

  -> Le group de noeuds sera crée en utilisant le domaine de
     localisation de la fissure (option TYPE_GROUP='ZONE_MAJ').

  """),


93 : _(u"""
  -> Aucune fissure n'est définie sur le modèle spécifié:
     MODELE = %(k1)s
  -> Risque & Conseil:
     Veuillez définir une fissure sur le modèle ci-dessus en utilisant
     les opérateurs DEFI_FISS_XFEM et MODI_MODELE_XFEM avant
     l'utilisation de PROPA_FISS.
  """),

94 : _(u"""
  -> L'avancée donnée (DA_MAX) pour la propagation courante est
     inférieure à la valeur minimale conseillée.

     DA_MAX donnée                     = %(r1)f
     Avancée maximale fissure courante = %(r2)f
     DA_MAX minimal conseillé          = %(r3)f

  -> Risque & Conseil:
     Risque de résultats faux. Dans le cas de propagation 3D en mode
     mixte, on conseille en général d'utiliser une avancée de fissure
     supérieure à celle minimale écrite ci-dessus, même si des bonnes
     résultats peuvent être obtenus en utilisant une avancée inférieure.

     La valeur minimale de DA_MAX dépende de la valeur de l'opérande
     RAYON et de l'angle de propagation de la fissure. Dans le cas où la
     valeur DA_MAX donnée ne peut pas être changée, sa valeur minimale
     conseillée peut être diminuée en agissant sur la valeur de RAYON,
     c'est-à-dire en utilisant une valeur de RAYON plus petite. Cela
     influence l'opérateur CALC_G aussi et normalement est faisable en
     utilisant un maillage plus raffiné.
  """),

95 : _(u"""
  -> Le modèle grille donné est défini sur un maillage (%(k1)s)
     et pas sur une grille.

  -> Risque & Conseil:
     Veuillez donner un modèle grille défini sur une grille. Cette
     grille doit être définie par DEFI_GRILLE à partir d'un maillage.
  """),

96 : _(u"""
 -> Le maillage sur lequel vous voulez créer le group n'est pas associé à
    la fissure donnée.

 -> Les maillages suivants sont associés à cette fissure:
      maillage physique = %(k1)s
      maillage grille   = %(k2)s

 -> Risque & Conseil:
    Veuillez vérifier d'avoir spécifié le bon maillage et/ou la bonne fissure.
"""),

97 : _(u"""
  -> La localisation de la zone de mise à jour a été utilisé pour la
     détermination de la configuration actuelle des fissures du modèle.
     Par contre, pour la propagation courante, la localisation n'a pas
     été activée.
  -> Risque & Conseil:
     Veuillez utiliser la localisation de la zone de mise à jour
     (ZONE_MAJ='TORE') pour la propagation courante aussi.
  """),

98 : _(u"""
  -> Aucune grille auxiliaire n'est utilisée pour la représentation de
     la fissure donnée.
  -> Risque & Conseil:
     Veuillez vérifier que vous avez demandé les level-sets de la bonne
     fissure.
  """),

99 : _(u"""
  -> La valeur du rayon du tore de localisation de la zone de mise à
     jour est plus petite que celle qui est nécessaire pour la bonne
     mise à jour des level-sets.
     Rayon à utiliser = %(r1)f
     Rayon minimal    = %(r2)f
  -> Risque & Conseil:

  -> Si vous avez utilisé l'opérande RAYON_TORE, veuillez augmenter la
     valeur donné ou diminuer la valeur de DA_MAX ou RAYON.

  -> Si vous n'avez pas utilisé l'opérande RAYON_TORE, cette erreur
     signifie que l'estimation automatique faite par l'opérateur
     PROPA_FISS ne marche pas bien pour la propagation courante. Elle
     peut être utilisée dans les cas où les valeurs de RAYON et DA_MAX
     ne changent pas entre deux propagations successives et la taille
     des éléments dans la zone de propagation est presque constante.
     Veuillez donc donner explicitement une valeur du rayon en utilisant
     l'opérande RAYON_TORE.
     Vous pouvez calculer une estimation de cette valeur en utilisant la
     formule suivante:

     RAYON_TORE=RAYON_max+DA_MAX_max+2*h_max

     où RAYON_max et DA_MAX_max sont les valeurs maximales des opérandes
     RAYON et DA_MAX qu'on va utiliser et h_max est la valeur de la plus
     grande arête des éléments du maillage ou de la grille auxiliaire
     dans la zone de propagation.
  """),
}
