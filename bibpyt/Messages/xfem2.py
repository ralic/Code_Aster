#@ MODIF xfem2 Messages  DATE 27/10/2009   AUTEUR GENIAUT S.GENIAUT 
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

1 : _("""
  -> On ne peut pas faire propager une interface.
     Seule les fissures (possédant un fond de fissure) peuvent etre propagées.
"""),


2 : _("""
  -> Seules les modélisations C_PLAN/D_PLAN sont disponibles pour XFEM.
  -> Risques et conseils:
     Veuillez considérer l'une des deux modélisations dans AFFE_MODELE.
"""),

4 : _("""
  -> Le type de formulation du contact (DISCRET/CONTINUE/XFEM) doit etre le meme pour
     toutes les zones de contact.
  -> Risque & Conseil:
     Veuillez revoir la mise en données de AFFE_CHAR_MECA/CONTACT.
"""),

7 : _("""
  -> Le contact a été activé dans XFEM (CONTACT_XFEM='OUI' dans MODI_MODELE_XFEM)
  -> Risque & Conseil:
     Vous devez également l'activer dans AFFE_CHAR_MECA/CONTACT_XFEM
"""),

8 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM. 
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à 
     AFFE_CHAR_MECA/CONTACT un modèle XFEM.
"""),

9 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT n'est pas un modèle
     XFEM avec contact.
  -> Risque & Conseil:
     Veuillez activer CONTACT='OUI' dans MODI_MODELE_XFEM.
"""),

11 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas 
     le modèle XFEM utilisé dans le AFFE_CHAR_MECA/CONTACT nommé %(k2)s.
  -> Risque & Conseil:
     Risques de résultats faux.
"""),

12 : _("""
  -> Le modèle %(k1)s transmis dans AFFE_CHAR_MECA/CONTACT_XFEM n'est pas un modèle
     XFEM. 
  -> Risque & Conseil:
     Veuillez utiliser la commande MODI_MODELE_XFEM pour fournir à 
     AFFE_CHAR_MECA/CONTACT_XFEM un modèle XFEM.
"""),

15 : _("""
  -> Point de FOND_FISS sans maille de surface rattachée.
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

17 : _("""
  -> Segment de fond_fiss sans maille de surface rattachée
  -> Risque & Conseil:
     Veuillez revoir la définition des level sets.
"""),

20 : _("""
  -> PFON_INI = POINT_ORIG
  -> Risque & Conseil :
     Veuillez définir deux points différents pour PFON_INI et POINT_ORIG.
"""),

21 : _("""
  -> Problème dans l'orientation du fond de fissure : POINT_ORIG mal choisi.
  -> Risque & Conseil : 
     Veuillez redéfinir POINT_ORIG.
"""),

22 : _("""
  -> Tous les points du fond de fissure sont des points de bord.
  -> Risque & Conseil : 
     Assurez-vous du bon choix des paramètres d'orientation de fissure.
"""),

23 : _("""
  -> PFON_INI semble etre un point mal choisi, on le modifie automatiquement.
"""),

24 : _("""
  -> La méthode "UPWIND" est en cours d'implémentation.
  -> Risque & Conseil :
     Veuillez choisir une autre méthode.
"""),

25 : _("""
  -> La norme du vecteur VECT_ORIE est nulle.
  -> Risque & Conseil :
     Veuillez redéfinir VECT_ORIE.
"""),


39 : _("""
  -> Deux points du fond de fissure sont très proches ou coincident.
  -> Risque & Conseil :
     Vérifier les définitions des level sets et la liste des points du fond
     de fissure trouvés. Si c'est normal, contactez votre correspondant.
"""),

50 : _("""
  -> Le maillage utilisé pour la représentation des level sets est 2D
     mais il contient des éléments 1D aussi.
  -> La méthode upwind sélectionnée dans PROPA_FISS peut gérer des
     grilles 2D définies seulement par des éléments QUAD4.
  -> Risque & Conseil:
     Veuillez donner un maillage défini seulement par des éléments
     QUAD4.
  """),

51 : _("""
  -> Il n'y a aucune maille enrichie.
  -> Risque & Conseil:
     Veuillez vérifier les définitions des level sets.
  """),

52 : _("""
  -> Le maillage utilisé pour la représentation des level sets est 3D
     mais il contient des éléments 2D et/ou 1D aussi.
  -> La méthode upwind sélectionnée dans PROPA_FISS peut gérer des
     grilles 3D définies seulement par des éléments HEXA8.
  -> Risque & Conseil:
     Veuillez donner un maillage défini seulement par des éléments
     HEXA8.
  """),
  
53 : _("""
  -> Dans le maillage utilisé pour la représentation des level sets,
     il y a des éléments qui ne sont pas disponibles pour la méthode 
     upwind (PROPA_FISS).
  -> Risque & Conseil:
     Veuillez vérifier le maillage et utiliser uniquement des éléments
     QUAD4 en 2D et HEXA8 en 3D.
  """),

54 : _("""
  -> Il n'y a pas d'éléments disponibles pour la méthode upwind 
     (PROPA_FISS) dans le maillage utilisé pour la représentation 
     des level sets.
  -> Risque & Conseil:
     Veuillez vérifier le maillage et utiliser uniquement des éléments
     QUAD4 en 2D et HEXA8 en 3D.
  """),

55 : _("""
  -> Dans le maillage utilisé pour la représentation des level sets
     (PROPA_FISS), il y a des arêtes qui ne sont pas orthogonales aux
     autres arêtes.
  -> Risque & Conseil:
     Risques de résultats faux.
     Veuillez vérifier que toutes les arêtes des éléments du maillage
     soient orthogonales entre elles.
  """),

56 : _("""
  -> Aucun noeud n'a été trouvé pour le calcul du résidu local.
  -> Le calcul du résidu local n'est pas possible.
  -> Risque & Conseil:
     Veuillez vérifier que la fissure n'est pas à l'extérieur du 
     maillage après la propagation actuelle.
  """),

57 : _("""
  -> La définition de un ou plusieurs éléments du maillage utilisé pour
     la représentation des level sets (PROPA_FISS) n'est pas correcte.
  -> Risque & Conseil:
     Il y a une arête avec une longueur nulle dans le maillage.
     Veuillez vérifier la définition des éléments du maillage (par
     exemple: un noeud est utilisé seulement une fois dans la définition
     d'un élément; il n'y a pas de noeuds doubles...)
  """),
  
58 : _("""
  -> La dimension (2D ou 3D) du modèle physique et la dimension (2D ou 
     3D) du modèle utilisé pour la représentation des level sets ne sont
     pas égales.
  -> Risque & Conseil:
     Veuillez utiliser deux modèles avec la même dimension (tous deux 2D
     ou tous deux 3D).
  """),
  
60 : _("""
  -> L'opérande TEST_MAIL a été utilisée dans l'opérateur PROPA_FISS.
  -> Type de test: VITESSE CONSTANTE
     La même vitesse d'avancée est utilisée pour tous les points du
     fond de fissure et l'angle de propagation est fixé égal à zéro.
  -> Risque & Conseil:
     L'avancée de la fissure n'est pas liée aux contraintes affectant
     la structure et donc les résultats de la propagation n'ont pas
     une signification physique.
     L'opérande TEST_MAIL doit être utilisé uniquement pour vérifier
     si le maillage est suffisamment raffiné pour la représentation
     des level sets.
  """),
  
63 : _("""
  -> La valeur de l'avancée DA_MAX utilisée est petite par rapport à la
     longueur de la plus petite arrête du maillage utilisé pour
     la représentation des level sets:
     DA_MAX = %(r1)f
     Longeur minimale arrêt = %(r2)f
  -> Risque & Conseil:
     Risques de résultats faux. Veuillez vérifier les résultats en
     utilisant un maillage plus raffiné pour la représentation des
     level sets.
  """),
  
64 : _("""
  -> La valeur du RAYON est plus petite que la longueur de la plus petite
     arrête du maillage utilisé pour la représentation des level sets:
     RAYON = %(r1)f
     Longeur minimale arrêt = %(r2)f
  -> Le calcul du résidu local n'est pas possible.
  -> Risque & Conseil:
     Veuillez utiliser une valeur du RAYON plus grande.
  """),
  
65 : _("""
  -> Le nombre maximal d'itérations a été atteint.
  -> Risque & Conseil:
     Essayer d'utiliser un maillage plus raffiné, ou bien une grille auxiliaire.
  """),
  
66 : _("""
  -> Le taux de restitution d'énergie G est négatif sur certains des 
     noeuds du fond de fissure : le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...). 
  """),
  
68 : _("""
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
  
71 : _("""
     Un tableau doit être donné pour chaque fissure du modèle.
     
     Attention! Si une fissure est formée par plusieurs morceaux, un
     tableau n'est pas suffisant et on doit donner un tableau pour
     chaque morceau.
  """),
  
72 : _("""
  -> L'angle de propagation de la fissure n'est pas constant dans le
     cycle de chargement.
  -> La valeur de la première configuration donnée dans les tableaux des
     facteurs d'intensité de contraintes a été retenue.
  -> Risque & Conseil:
     Risques des résultats faux.
     Veuillez vérifier les conditions de chargement du modèle.
  """),
  
73 : _("""
  -> L'option NB_POINT_FOND a été utilisé dans PROPA_FISS mais le
     modèle est 2D.
  -> Risque & Conseil:
     Ne pas utiliser cette option avec un modèle 2D.
  """),
  
74 : _("""
  -> Aucune fissure du modèle ne propage.
  -> Risque & Conseil:
     Veuillez vérifier les conditions du chargement du modèle et les
     constantes de la loi de propagation données à PROPA_FISS.
  """),
  
75 : _("""
  -> Le nombre des fissures définies dans le modèle donné pour la grille
     des level sets n'est pas correct.
  -> Modèle: %(k1)s
     Nombre de fissures: %(i1)d
  -> Risque & Conseil:
     Veuillez donner un modèle contenant une seule fissure.
  """),
  
76 : _("""
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
  
77 : _("""
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
  
78 : _("""
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
  
79 : _("""
  -> Une des valeurs donnée pour NB_POINT_FOND n'est pas valide.
  -> Risque & Conseil:
     Veuillez vérifier que toutes les valeurs sont égales à 0 ou
     supérieures à 1.
  """),
  
80 : _("""
  -> Le nombre des valeurs dans un des tableaux des facteurs
     d'intensité de contraintes (SIF) est supérieur au nombre des 
     points du fond de la fissure correspondante.
  -> Risque & Conseil:
     Veuillez vérifier que les tableaux de SIF donnés par l'opérateur
     PROPA_FISS sont corrects. Si NB_POINT_FOND a été utilisé, veuillez 
     vérifier aussi que la liste donnée pour chaque fissure est correcte.
  """),
  
81 : _("""
  -> Les valeurs de COEF_MULT_MAXI et COEF_MULT_MINI de COMP_LINE données
     dans l'opérateur PROPA_FISS sont égales à zéro.
  -> Risque & Conseil:
     Au moins une des deux valeurs doit être différente de zéro pour
     avoir une cycle de fatigue. Veuillez vérifier les valeurs données. 
  """),
  
82 : _("""
  -> L'opérande COMP_LINE a été utilisée dans PROPA_FISS et il y a
     plusieurs résultats dans un des tableaux des facteurs d'intensité
     de contraintes (SIF).
  -> Risque & Conseil:
     Veuillez donner des tableaux avec seulement les SIF correspondant à
     la conditions de chargement de référence.
  """),
  
83 : _("""
  -> Le taux de restitution maximal d'énergie G dans le cycle de fatigue
     est zéro sur certains des noeuds du fond de fissure:
     le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...) et les chargements affectant le
     modèle.
"""),
  
84 : _("""
  -> Le taux de restitution d'énergie G ne change pas dans le cycle de
     fatigue sur certains des noeuds du fond de fissure:
     le calcul de propagation est impossible.
  -> Risque & Conseil:
     Veuillez vérifier les paramètres du calcul de G (rayons des
     couronnes, type de lissage...) et les chargements affectant le
     modèle.
"""),
  
85 : _("""
   Les propriétés matériaux dépendent de la température. La température en fond
   de fissure n'étant pas connue, le calcul se poursuit en prenant la température
   de référence du matériau (TEMP = %(r1)f).
"""),

86 : _("""
  -> L'opérande TEST_MAIL a été utilisée dans l'opérateur PROPA_FISS.
  -> Type de test: VITESSE LINEAIRE
     La même composante tangentielle de la vitesse d'avancée de la
     fissure est utilisée pour tous les noeuds du fond de fissure.
     L'angle de propagation change en façon linéaire. Il est fixé égal
     à zéro pour le point moyenne du fond.  Pour les deux points
     d'extrémité il est fixé égal à +/-5 degrees.
  -> Risque & Conseil:
     L'avancement de la fissure n'est pas lié à les contraintes
     affectant la structure et donc les résultats de la propagation
     n'ont pas de sens physique.
     L'opérande TEST_MAIL doit être utilisé uniquement pour vérifier
     si le maillage est suffisamment raffiné pour la représentation
     des level sets.
  """),
  
87 : _("""
  -> L'opérande TEST_MAIL a été utilisé dans l'opérateur PROPA_FISS.
  -> Cet opérande n'a pas des sens que pour un modèle 3D.
  -> Risque & Conseil:
     Ne pas utiliser TEST_MAIL pour un modèle 2D.
  """),
  
89 : _("""
  -> La fissure à propager n'existe pas dans le modèle.
     Fissure donnée: %(k1)s
  -> Conseil:
     Veuillez vérifier la liste des fissures donnée dans l'opérande
     FISS_PROP.
  """),
  
90 : _("""
  -> Un ou plusieurs tableaux des facteurs d'intensité de contraintes
     ne contient pas la colonne NUME_FOND.
  -> Conseil:
     Veuillez ajouter cette colonne aux tableaux (voir documentation
     utilisateur de PROPA_FISS).
  """),
  
91 : _("""
  -> Le nouveau fond de fissure n'est pas très régulier. Cela signifie
     que le maillage ou la grille auxiliaire utilisés pour la
     représentation de la fissure par level sets ne sont pas
     suffisamment raffinés pour bien décrire la forme du fond de la
     fissure utilisée.
  -> Risque & Conseil:
     Risques de résultats faux en utilisant le maillage ou la grille
     auxiliaire testés. Veuillez utiliser un maillage ou une grille
     auxiliaire plus raffinés.
  """),

}
