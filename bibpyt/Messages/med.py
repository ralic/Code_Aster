#@ MODIF med Messages  DATE 24/09/2012   AUTEUR SELLENET N.SELLENET 
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
  -> Absence de localisation de points de Gauss dans le fichier MED
     pour l'élément de référence %(k1)s.
     On suppose que l'ordre des points de Gauss est celui de Code_Aster.
  -> Risque & Conseil :
     Risque de résultats faux.
"""),

2 : _(u"""
  -> Le nombre de points de Gauss est différent entre le fichier MED et Aster:
      - nombre de points de Gauss contenu dans le fichier MED : %(i2)d
      - nombre de points de Gauss défini dans Aster           : %(i1)d

     Visiblement les éléments finis décrits dans le fichier MED ne sont pas les
     mêmes que dans Code_Aster.
     Si vous avez choisi PROL_ZERO='OUI', le champ sera initialisé à zéro sur
     ces éléments.
     Sinon, le champ ne sera pas initialisé (NaN, not a number). C'est le
     comportement par défaut.

  -> Risque & Conseil :
      - Choisissez des éléments finis compatibles entre Aster et le code tiers
"""),

3 : _(u"""
  -> Les point de Gauss MED/Aster ne correspondent pas géométriquement.
  -> Risque & Conseil:
     Risque de résultats faux à cause cette incompatibilité.
"""),

4 : _(u"""

     Point De Gauss : %(i1)d              MED               ASTER
"""),

5 : _(u"""
        %(k1)s                          %(r1)f          %(r2)f
"""),

6 : _(u"""
  -> Une ou plusieurs permutations ont été effectuées sur l'ordre des points
     de Gauss pour que la localisation MED corresponde à celle de Code_Aster.
"""),

7 : _(u"""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est trop long. Il sera tronqué à 8 caractères.
     Le groupe "%(k2)s" est renommé en "%(k3)s".
"""),

8 : _(u"""
  -> Famille %(k1)s :
       Incohérence sur les nombres de %(k2)s, il y en a %(i1)d alors
       que la fonction MED en annonce %(i2)d.
  -> Risque & Conseil:
       Impossible de lire ce fichier.
       On peut utiliser mdump (utilitaire MED) pour voir si le problème
       vient du fichier MED ou de la lecture dans Code_Aster.
"""),

9 : _(u"""
  -> Vous ne pouvez pas renommer le groupe "%(k1)s" en "%(k2)s"
     car "%(k2)s" existe déjà dans le fichier MED.
"""),

10 : _(u"""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est contient des caractères interdits.
     Le groupe "%(k2)s" est renommé en "%(k3)s".
"""),

11 : _(u"""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est vide.
"""),

12 : _(u"""
  -> Erreur lors de l'appel à EFNEMA, code retour = %(k1)s
  -> Risque & Conseil :
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.
     Si le maillage a été produit par un code externe, vérifier que les
     noms de maillage, de groupes, de familles ne contiennent pas de
     blancs à la fin.
     Dans Salomé, on peut renommer ces entités et supprimer les espaces
     invalides.
"""),

13 : _(u"""
  -> La famille %(k1)s n'a ni groupe, ni attribut.
"""),

14 : _(u"""
  -> Lecture de la famille numéro %(i1)4d de nom %(k1)s.
"""),

15 : _(u"""
      Groupe numéro %(i1)6d : %(k1)s
"""),

16 : _(u"""
      Groupe numéro %(i1)6d : %(k1)s
                renommé en : %(k2)s
"""),

17 : _(u"""
  -> Aucune famille n'est présente dans ce fichier MED.
  -> Risque & Conseil :
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.
"""),

18 : _(u"""
  -> Arrêt en raison des conflits sur les noms de groupe.
"""),

19 : _(u"""
  -> Les mailles  %(k1)s ne sont pas nommées dans le fichier MED.
"""),

20 : _(u"""
  -> Impossible de retrouver l'adresse associée au groupe  %(k1)s
"""),

21 : _(u"""
  -> Il manque les coordonnées !
"""),

22 : _(u"""
  Le nom de groupe numéro  %(i1)d  est en double. %(k1)s
  - premier nom MED  :  %(k2)s
  - second nom MED   :  %(k3)s
  - nom aster retenu :  %(k4)s
"""),

23 : _(u"""
  -> Mailles  %(k1)s
"""),

24 : _(u"""
  -> Le fichier n'a pas été construit avec la même version de MED.
  -> Risque & Conseil :
     La lecture du fichier peut échouer !

"""),

25 : _(u"""
   Version de la bibliothèque MED utilisée par Code_Aster:  %(i1)d %(i2)d %(i3)d
"""),

26 : _(u"""
   Version de la bibliothèque MED qui a créé le fichier  : < 2.1.5
"""),

27 : _(u"""
   Version de la bibliothèque MED pour créer le fichier  :  %(i1)d %(i2)d %(i3)d
"""),

28 : _(u"""

   Un utilitaire vous permet peut-être de convertir votre fichier (medimport)
"""),

29 : _(u"""
  -> Il manque les mailles !
"""),

30: _(u"""
  -> Votre modèle semble être composé de plusieurs modélisations, les composantes
      de %(k1)s qui n'existent pas pour une partie du modèle ont été
      mises à zéro.

     Dans certains cas, le fichier MED produit peut devenir volumineux. Dans ce
     cas, l'utilisation du mot-clé NOM_CMP est conseillée.
"""),

31 : _(u"""
  -> Ce champ existe déjà dans le fichier MED avec un nombre de composantes
     différent à un instant précédent. On ne peut pas le créer de nouveau.

     Nom MED du champ : "%(k1)s"

  -> Risque & Conseil :
     On ne peut pas imprimer un champ dont le nombre de composantes varie en
     fonction du temps. Plusieurs possibilités s'offrent à vous:
     - si vous souhaitez disposer d'un champ disposant des mêmes composantes
     à chaque instant, il faut renseigner derrière le mot-clé NOM_CMP le nom
     des composantes commun aux différents instants.
     - si vous souhaitez imprimer un champ avec l'ensemble des composantes
     Aster qu'il contient, il suffit de faire plusieurs IMPR_RESU et de
     renseigner pour chaque impression une liste d'instants ad hoc.

     Pour la visualisation dans Salomé (Scalar Map par exemple),
     sélectionner la composante dans Scalar Range/Scalar Mode.
"""),

32 : _(u"""
     Le champ %(k1)s est inconnu dans le fichier MED.
"""),

33 : _(u"""
     Il manque des composantes.
"""),

34 : _(u"""
     Aucune valeur n'est présente à cet instant.
"""),

35 : _(u"""
     Aucune valeur n'est présente à ce numéro d'ordre.
"""),

36 : _(u"""
     Le nombre de valeurs n'est pas correct.
"""),

37 : _(u"""
  -> La lecture est donc impossible.
  -> Risque & Conseil :
     Veuillez vérifier l'intégrité du fichier MED avec medconforme/mdump.
"""),

38 : _(u"""
  -> Incohérence catalogue - fortran (nbtyp fortran différent de nbtyp catalogue)
"""),

39 : _(u"""
  -> Incohérence catalogue - fortran (nomtyp fortran différent de nomtyp catalogue)
"""),

40 : _(u"""
  -> Ouverture du fichier MED en mode  %(k1)s  %(k2)s
"""),

41 : _(u"""
  -> Incohérence de version détectée.
"""),

42 : _(u"""
  -> Le type d'entité  %(k1)s  est inconnu.
"""),

43 : _(u"""
  -> Le maillage est introuvable !
"""),

44 : _(u"""
  -> Pas d'écriture pour  %(k1)s
"""),

45 : _(u"""
     Issu de  %(k1)s
"""),

46 : _(u"""
  -> Le type de champ est inconnu :  %(k1)s
"""),

47 : _(u"""
  -> Création des tableaux de valeurs à écrire avec :
"""),

48 : _(u"""
  -> Renumérotation impossible avec plus d'un sous-point.
"""),

49 : _(u"""
  -> Véritable écriture des tableaux de valeurs
"""),

50 : _(u"""
  -> Pas de maillage dans  %(k1)s
"""),

51 : _(u"""
  -> Maillage  %(k1)s  inconnu dans  %(k2)s
"""),

52 : _(u"""
  ->  Instant inconnu pour ce champ et ces supports dans le fichier.
"""),

53 : _(u"""
  ->  La version de la lib MED utilisée par Code_Aster est plus récente que
      celle qui a produit votre fichier MED.
  ->  Conséquence:  On considère les champs aux noeuds par élément
      comme des pseudo champs aux points de Gauss.
      (On utilise pour la lecture du champ %(k1)s
       contenu dans votre fichier MED, le type d'entité MED_MAILLE au lieu
       de MED_NOEUD_MAILLE).
"""),

54 : _(u"""
  -> Le modèle fourni à LIRE_RESU n'est pas cohérent avec le type de structure
     de données résultat que vous souhaitez produire.
"""),


55 : _(u"""
  -> Lecture impossible pour  %(k1)s  au format MED
"""),

56 : _(u"""
     En effet, le phénomène %(k1)s de votre modèle n'est pas compatible avec une
     SD Résultat de type %(k2)s.
  -> Risque & Conseil :
     Veuillez fournir à LIRE_RESU un autre modèle ou changer de TYPE_RESU.
"""),

57 : _(u"""
  -> Le champ  %(k1)s n'existe pas dans le fichier MED.
  -> Conseils :
     Vérifier la présence du champ demandé dans le fichier.
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.

  Remarque : Les champs disponibles dans ce fichier sont listés ci-dessous :
"""),

58 : _(u"""
  -> Le nombre de type de maille présent dans le fichier MED est
      différent du nombre de type de maille présent dans le maillage fourni.
  -> Risque & Conseil :
     Le modèle sur lequel le résultat a été créé n'est pas le même
      que le modèle fourni.
     Vérifiez le maillage de votre modèle !
"""),
59 : _(u"""
     Les éléments du modèle fourni ont pour support géométrique des
     mailles ne figurant pas dans le fichier MED.
     Par exemple, il y %(i1)d mailles de types %(k1)s dans le fichier MED,
     alors que le modèle en contient %(i2)d.
  -> Risque & Conseil :
     Veuillez fournir un modèle dont le maillage correspond à celui présent
     dans le fichier MED.
"""),

60 : _(u"""
  -> On ne traite pas les maillages distants.
"""),

61 : _(u"""
     Le maillage contenu dans le fichier MED contient plus de mailles
     que celui associé au maillage fourni par le modèle.
     Par exemple, on dénombre %(i1)d mailles de types %(k1)s dans le maillage
     MED, alors que le modèle n'en contient que %(i2)d !
  -> Risque & Conseil :
     Veuillez vérifier que le modèle fourni ne résulte pas d'une restriction,
     ou que l'un des maillages est quadratique et l'autre linéaire.
"""),

62 : _(u"""
  -> Impossible de déterminer un nom de maillage MED.
"""),

63 : _(u"""
  -> Le mot clé "INFO_MAILLAGE" est réservé au format MED.
"""),

65 : _(u"""
  -> Grandeur inconnue.
"""),

66 : _(u"""
  -> Composante inconnue pour la grandeur.
"""),

67 : _(u"""
  -> Le maillage %(k2)s est déjà présent dans le fichier MED %(k1)s.
"""),

68 : _(u"""
  -> Instant voulu :  %(r1)f
"""),

69 : _(u"""
  -> Numéro d'ordre :  %(i1)d numéro de pas de temps :  %(i2)d

"""),

70 : _(u"""
  -> Trop de composantes pour la grandeur.
"""),

71 : _(u"""
  -> le mot-clé MODELE est obligatoire pour lire un CHAM_ELEM
"""),

72 : _(u"""
  -> Nom de composante tronqué à 8 caractères ( %(k1)s  >>>  %(k2)s )
"""),

73 : _(u"""
  -> Impossible de trouver la composante ASTER associée a  %(k1)s
"""),

74 : _(u"""
  -> Écriture des localisations des points de gauss.
"""),

75 : _(u"""
  -> Problème dans la lecture du nom du champ et de ses composantes.
"""),

76 : _(u"""
  -> Problème dans le diagnostic.
"""),

77: _(u"""
  -> On ne peut lire aucune valeur du champ %(k1)s dans le fichier d'unité %(i1)d.
  -> Risques et conseils:
     Ce problème est peut-être lié à une incohérence entre le champ à lire dans
     le fichier MED (NOEU/ELGA/ELNO/...) et le type du champ que vous avez demandé
     (mot clé TYPE_CHAM).
"""),

78: _(u"""
  Problème à l'ouverture du fichier MED sur l'unité %(k1)s
  -> Conseil :
     Vérifier la présence de ce fichier dans le répertoire de lancement de l'étude.
"""),

79 : _(u"""
  -> Attention le maillage n'est pas de type non structuré
"""),

80 : _(u"""
  -> Le maillage ' %(k1)s ' est inconnu dans le fichier.
"""),

81 : _(u"""
  -> Attention, il s'agit d'un maillage structuré
"""),

82 : _(u"""
  -> Le champ %(k1)s n'est associé à aucun modèle.
  -> Conseil :
     Veuillez renseigner le modèle.
"""),

83 : _(u"""
Le nombre de valeurs lues dans le fichier MED est différent du nombre de valeurs réellement
 affectées dans le champ :
  - valeurs lues dans le fichier        : %(i1)d
  - valeurs non affectées dans le champ : %(i2)d

Risques :
  Soit le modèle n'est pas adapté au champ que vous souhaitez lire, auquel cas vous risquez
   d'obtenir des résultats faux. Soit le modèle est constitué d'un mélange de modélisations
   qui ne portent pas les mêmes composantes sur les différents éléments du maillage auquel
   cas, cette alarme n'est pas légitime.

Conseil :
  Vérifiez la cohérence du modèle et du fichier MED.
"""),

84 : _(u"""
  -> Type incorrect  %(i1)d
"""),

85 : _(u"""
  -> Maillage présent :  %(k1)s
"""),

86 : _(u"""
  -> champ à lire :  %(k1)s typent :  %(i1)d typgeo :  %(i2)d
     instant voulu :  %(r1)f
     --> numéro d'ordre :  %(i3)d
     --> numéro de pas de temps :  %(i4)d

"""),

87 : _(u"""
  Le numéro d'ordre %(i1)d que vous avez renseigné ne figure pas
  dans la liste des numéros d'ordre du résultat MED.
  Conséquence: le champ correspondant ne figurera pas dans la
  SD Résultat %(k1)s
"""),


88 : _(u"""
  -> Fichier MED :  %(k1)s, nombre de maillages présents : %(i1)d
"""),

89 : _(u"""
  -> Écriture impossible pour  %(k1)s  au format MED.
"""),

90 : _(u"""
     Début de l'écriture MED de  %(k1)s
"""),

91 : _(u"""
  -> Impossible de déterminer un nom de champ MED.
  -> Risque & Conseil:
"""),

92 : _(u"""
  -> Le type de champ  %(k1)s  est inconnu pour MED.
  -> Risque & Conseil:
     Veuillez vérifier la mise en données du mot-clé NOM_CHAM_MED
     (LIRE_RESU) ou NOM_MED (LIRE_CHAMP).
"""),

93 : _(u"""
     Fin de l'écriture MED de  %(k1)s
"""),

94 : _(u"""
  Vous imprimez un champ dont le maillage change au cours du temps.
  Ce type de champ n'est pas autorisé.
  -> Conseil :
     Si la structure de données résultat a été produite par
     CREA_RESU, vérifiez que les champs fournis à cette commande
     reposent sur un seul et même maillage.
"""),

95 : _(u"""
  -> Le champ MED %(k1)s est introuvable.
  -> Risque & Conseil:
     Veuillez vérifier la mise en données du mot-clé NOM_CHAM_MED
     ainsi que le fichier MED fourni à l'opérateur.
"""),

96 : _(u"""
  -> NOM_MED absent !
  -> Risque & Conseil:
     Veuillez renseigner le mot-clé NOM_MED de l'opérateur LIRE_CHAMP.
"""),

97 : _(u"""
  -> Fichier MED :  %(k1)s, Champ :  %(k2)s, Instant voulu :  %(r1)f
     - typent :  %(i1)d
     - typgeo :  %(i2)d

"""),

98 : _(u"""
  -> Fichier MED :  %(k1)s champ :  %(k2)s
"""),

99 : _(u"""
  Le nombre de composantes à imprimer est trop grand pour le champ %(k1)s.
  Le format MED accepte au maximum 80 composantes dans un champ.
  
  Le champ ne sera donc pas imprimé au format MED.
"""),

}
