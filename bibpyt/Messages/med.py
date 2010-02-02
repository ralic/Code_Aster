#@ MODIF med Messages  DATE 02/02/2010   AUTEUR SELLENET N.SELLENET 
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
  -> Absence de localisation de points de Gauss dans le fichier MED
     pour l'élément de référence %(k1)s.
     On suppose que l'ordre des points de Gauss est celui d'Aster.
  -> Risque & Conseil :     
     Risque de résultats faux.
"""),

2 : _("""
  -> Le nombre de points de Gauss est différent entre le fichier med et Aster:
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

3 : _("""
  -> Les point de Gauss Med/Aster ne correspondent pas géométriquement.
  -> Risque & Conseil:
     Risque de résultats faux à cause cette incompatibilité.
"""),

4 : _("""

     Point De Gauss : %(i1)d              MED               ASTER
"""),

5 : _("""
        %(k1)s                          %(r1)f          %(r2)f
"""),

6 : _("""
  -> Une ou plusieurs permutations ont été effectuées sur l'ordre des points
     de Gauss pour que la localisation Med corresponde à celle d'Aster.
"""),

7 : _("""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est trop long. Il sera tronqué à 8 caractères.
     Le groupe "%(k2)s" est renommé en "%(k3)s".
"""),

8 : _("""
  -> Famille %(k1)s :
       Incohérence sur les nombres de %(k2)s, il y en a %(i1)d alors
       que la fonction MED en annonce %(i2)d.
  -> Risque & Conseil: 
       Impossible de lire ce fichier. 
       On peut utiliser mdump (utilitaire med) pour voir si le problème
       vient du fichier MED ou de la lecture dans Code_Aster.
"""),

9 : _("""
  -> Vous ne pouvez pas renommer le groupe "%(k1)s" en "%(k2)s"
     car "%(k2)s" existe déjà dans le fichier MED.
"""),

10 : _("""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est contient des caractères interdits.
     Le groupe "%(k2)s" est renommé en "%(k3)s".
"""),

11 : _("""
  -> Le nom de groupe numéro %(i1)d de la famille %(k1)s
     est vide.
"""),

12 : _("""
  -> Erreur lors de l'appel à EFNEMA, code retour = %(k1)s
  -> Risque & Conseil :
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.
     Si le maillage a été produit par un code externe, vérifier que les
     noms de maillage, de groupes, de familles ne contiennent pas de
     blancs à la fin.
     Dans Salomé, on peut renommer ces entités et supprimer les espaces
     invalides.
"""),

13 : _("""
  -> La famille %(k1)s n'a ni groupe, ni attribut.
"""),

14 : _("""
  -> Lecture de la famille numéro %(i1)4d de nom %(k1)s.
"""),

15 : _("""
      Groupe numéro %(i1)6d : %(k1)s
"""),

16 : _("""
      Groupe numéro %(i1)6d : %(k1)s
                renommé en : %(k2)s
"""),

17 : _("""
  -> Aucune famille n'est présente dans ce fichier med.
  -> Risque & Conseil :
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.
"""),

18 : _("""
  -> Arret en raison des conflits sur les noms de groupe.
"""),

19 : _("""
  -> Les mailles  %(k1)s ne sont pas nommées dans le fichier med.
"""),

20 : _("""
  -> Impossible de retrouver l'adresse associée au groupe  %(k1)s 
"""),

21 : _("""
  -> Il manque les coordonnées !
"""),

22 : _("""
  Le nom de groupe numéro  %(i1)d  est en double. %(k1)s
  - premier nom med  :  %(k2)s
  - second nom med   :  %(k3)s
  - nom aster retenu :  %(k4)s
"""),

23 : _("""
  -> Mailles  %(k1)s 
"""),

24 : _("""
  -> Le fichier n'a pas été construit avec la meme version de med.
  -> Risque & Conseil :
     La lecture du fichier peut échouer !

"""),

25 : _("""
   Version de la bibliothèque med utilisee par Code_Aster:  %(i1)d %(i2)d %(i3)d
"""),

26 : _("""
   Version de la bibliothèque med qui a créé le fichier  : < 2.1.5
"""),

27 : _("""
   Version de la bibliothèque med pour créer le fichier  :  %(i1)d %(i2)d %(i3)d 
"""),

28 : _("""

   Un utilitaire vous permet peut-etre de convertir votre fichier (medimport)
"""),

29 : _("""
  -> Il manque les mailles !
"""),

30: _("""
  -> Votre modèle semble etre composé de plusieurs modélisations, les composantes
     qui n'existent pas pour une partie du modèle ont été mises à zéro.
"""),

31 : _("""
  -> Ce champ existe déjà dans le fichier MED avec un nombre de composantes
     différent à un instant précédent. On ne peut pas le créer de nouveau.

     Nom MED du champ : "%(k1)s"

  -> Risque & Conseil :
     On ne peut pas imprimer un champ dont le nombre de composantes varie en
     fonction du temps. Plusieurs possibilités s'offrent à vous:
     - si vous souhaitez disposer d'un champ disposant des memes composantes
     à chaque instant, il faut renseigner derrière le mot-clé NOM_CMP le nom 
     des composantes commun aux différents instants.
     - si vous souhaitez imprimer un champ avec l'ensemble des composantes
     Aster qu'il contient, il suffit de faire plusieurs IMPR_RESU et de 
     renseigner pour chaque impression une liste d'instants adoc.
     
     Pour la visualisation dans Salomé (Scalar Map par exemple),
     sélectionner la composante dans Scalar Range/Scalar Mode.
"""),

32 : _("""
     Le champ est inconnu.
"""),

33 : _("""
     Il manque des composantes.
"""),

34 : _("""
     Aucune valeur n'est présente à cet instant.
"""),

35 : _("""
     Aucune valeur n'est présente à ce numéro d'ordre.
"""),

36 : _("""
     Le nombre de valeurs n'est pas correct.
"""),

37 : _("""
  -> La lecture est donc impossible.
  -> Risque & Conseil :
     Veuillez vérifier l'intégrité du fichier MED avec medconforme/mdump.
"""),

38 : _("""
  -> Incohérence catalogue - fortran (nbtyp fortran différent de nbtyp catalogue)
"""),

39 : _("""
  -> Incohérence catalogue - fortran (nomtyp fortran différent de nomtyp catalogue)
"""),

40 : _("""
  -> Ouverture du fichier med en mode  %(k1)s  %(k2)s 
"""),

41 : _("""
  -> Incohérence de version détectée.
"""),

42 : _("""
  -> Le type d'entité  %(k1)s  est inconnu.
"""),

43 : _("""
  -> Le maillage est introuvable !
"""),

44 : _("""
  -> Pas d'écriture pour  %(k1)s 
"""),

45 : _("""
     Issu de  %(k1)s 
"""),

46 : _("""
  -> Le type de champ est inconnu :  %(k1)s 
"""),

47 : _("""
  -> Création des tableaux de valeurs à écrire avec :
"""),

48 : _("""
  -> Renumérotation impossible avec plus d'un sous-point.
"""),

49 : _("""
  -> Veritable écriture des tableaux de valeurs
"""),

50 : _("""
  -> Pas de maillage dans  %(k1)s 
"""),

51 : _("""
  -> Maillage  %(k1)s  inconnu dans  %(k2)s 
"""),

52 : _("""
  ->  Instant inconnu pour ce champ et ces supports dans le fichier.
"""),

53 : _("""
  ->  La version de la lib med utilisée par Code-Aster est plus récente que 
      celle qui a produit votre fichier med.
  ->  Conséquence:  On considère les champs aux noeuds par élément 
      comme des pseudo champs aux points de Gauss. 
      (On utilise pour la lecture du champ %(k1)s 
       contenu dans votre fichier med, le type d'entité MED_MAILLE au lieu
       de MED_NOEUD_MAILLE).
"""),

54 : _("""
  -> Le modèle fourni à LIRE_RESU n'est pas cohérent avec le type de structure
     de données résultat que vous souaitez produire.
"""),


55 : _("""
  -> Lecture impossible pour  %(k1)s  au format MED
"""),

56 : _("""
     En effet, le phénomène %(k1)s de votre modèle n'est pas compatible avec une 
     SD Résultat de type %(k2)s.
  -> Risque & Conseil :
     Veuillez fournir à LIRE_RESU un autre modèle ou changer de TYPE_RESU.
"""),

57 : _("""
  -> Le champ  %(k1)s n'existe pas dans le fichier med.
  -> Conseils :
     Vérifier la présence du champ demandé dans le fichier.
     Vérifier l'intégrité du fichier MED avec medconforme/mdump.

  Remarque : Les champs disponibles dans ce fichier sont listés ci-dessous :
"""),

58 : _("""
  -> Le nombre de type de maille présent dans le fichier MED est 
      différent du nombre de type de maille présent dans le maillage fourni.
  -> Risque & Conseil :
     Le modèle sur lequel le résultat a été créé n'est pas le même
      que le modèle fourni.
     Vérifiez le maillage de votre modèle !
"""),
59 : _("""
     Les éléments du modèle fourni ont pour support géométrique des 
     mailles ne figurant pas dans le fichier med.
     Par exemple, il y %(i1)s mailles de types %(k1)s dans le fichier med,
     alors que le modèle en contient %(i2)s. 
  -> Risque & Conseil :
     Veuillez fournir un modèle dont le maillage correspond à celui présent
     dans le fichier med.
"""),

60 : _("""
  -> On ne traite pas les maillages distants.
"""),

61 : _("""
     Le maillage contenu dans le fichier med contient beaucoup plus de mailles
     que celui associé au modèle fourni. 
     Par exemple, on dénombre %(i1)s mailles de types %(k1)s dans le maillage
     med, alors que le modèle n'en contient que %(i2)s ! 
  -> Risque & Conseil :
     Veuillez vérifier que le modèle fourni ne résulte pas d'une restriction,
     ou que l'un des maillages est quadratique et l'autre linéaire.
"""),

62 : _("""
  -> Impossible de déterminer un nom de maillage MED.
"""),

63 : _("""
  -> Le mot clé "INFO_MAILLAGE" est réservé au format med.
"""),

64 : _("""
  -> Le modèle fourni à LIRE_CHAMP n'est pas cohérent avec le type du champ
     que vous souaitez produire:
     - phenomène du modèle: %(k1)s
     - type du champ : %(k2)s 
"""),

65 : _("""
  -> Grandeur inconnue.
"""),

66 : _("""
  -> Composante inconnue pour la grandeur.
"""),

67 : _("""
  -> Le maillage %(k2)s est déjà présent dans le fichier med %(k1)s.
"""),

68 : _("""
  -> Instant voulu :  %(r1)f
"""),

69 : _("""
  -> Numéro d'ordre :  %(i1)d numéro de pas de temps :  %(i2)d 

"""),

70 : _("""
  -> Trop de composantes pour la grandeur.
"""),

71 : _("""
  -> le mot-clé MODELE est obligatoire pour lire un CHAM_ELEM
"""),

72 : _("""
  -> Nom de composante tronqué à 8 caractères ( %(k1)s  >>>  %(k2)s )
"""),

73 : _("""
  -> Impossible de trouver la composante ASTER associée a  %(k1)s 
"""),

74 : _("""
  -> Ecriture des localisations des points de gauss.
"""),

75 : _("""
  -> Problème dans la lecture du nom du champ et de ses composantes.
"""),

76 : _("""
  -> Problème dans le diagnostic.
"""),

77: _("""
  -> On ne peut lire aucune valeur du champ %(k1)s dans le fichier d'unité %(i1)s.
  -> Risques et conseils:
     Ce problème est peut-être lié à une incohérence entre le champ à lire dans 
     le fichier MED (NOEU/ELGA/ELNO/...) et le type du champ que vous avez demandé 
     (mot clé TYPE_CHAM).
"""),

78: _("""
  Problème à l'ouverture du fichier MED sur l'unité %(k1)s
  -> Conseil :
     Vérifier la présence de ce fichier dans le répertoire de lancement de l'étude.
"""),

79 : _("""
  -> Attention le maillage n'est pas de type non structuré
"""),

80 : _("""
  -> Le maillage ' %(k1)s ' est inconnu dans le fichier.
"""),

81 : _("""
  -> Attention, il s'agit d'un maillage structuré
"""),

82 : _("""
  -> L'objet  %(k1)s  n'existe pas.
  -> Risque & Conseil:
     Veuillez renseigner le modèle.
"""),

83 : _("""
  -  valeurs lues dans le fichier        : %(i1)d 
  -  valeurs non affectees dans le champ : %(i2)d 
"""),

84 : _("""
  -> Type incorrect  %(i1)d 
"""),

85 : _("""
  -> Maillage présent :  %(k1)s 
"""),

86 : _("""
  -> champ à lire :  %(k1)s typent :  %(i1)d typgeo :  %(i2)d 
     instant voulu :  %(r1)f 
     --> numéro d'ordre :  %(i3)d 
     --> numéro de pas de temps :  %(i4)d 
 
"""),

87 : _("""
  Le numéro d'ordre %(i1)d que vous avez renseigné ne figure pas
  dans la liste des numéros d'ordre du résultat med. 
  Conséquence: le champ correspondant ne figurera pas dans la 
  SD Résultat %(k1)s
"""),


88 : _("""
  -> Fichier med :  %(k1)s, nombre de maillages présents : %(i1)d 
"""),

89 : _("""
  -> Ecriture impossible pour  %(k1)s  au format MED.
"""),

90 : _("""
     Début de l'écriture MED de  %(k1)s 
"""),

91 : _("""
  -> Impossible de déterminer un nom de champ MED.
  -> Risque & Conseil:  
"""),

92 : _("""
  -> Le type de champ  %(k1)s  est inconnu pour med.
  -> Risque & Conseil:
     Veuillez vérifier la mise en données du mot-clé NOM_CHAM_MED
     (LIRE_RESU) ou NOM_MED (LIRE_CHAMP).
"""),

93 : _("""
     Fin de l'écriture MED de  %(k1)s 
"""),

95 : _("""
  -> Le champ med %(k1)s est introuvable.
  -> Risque & Conseil:
     Veuillez vérifier la mise en données du mot-clé NOM_CHAM_MED
     ainsi que le fichier med fourni à l'opérateur.
"""),

96 : _("""
  -> NOM_MED absent !
  -> Risque & Conseil:
     Veuillez renseigner le mot-cle NOM_MED de l'opérateur LIRE_CHAMP.
"""),

97 : _("""
  -> Fichier med :  %(k1)s, Champ :  %(k2)s, Instant voulu :  %(r1)f 
     - typent :  %(i1)d 
     - typgeo :  %(i2)d 
 
"""),

98 : _("""
  -> Fichier med :  %(k1)s champ :  %(k2)s 
"""),

99 : _("""
  -> Des éléments finis différents s'appuient sur un meme type de maille(%(k1)s).
     Le nombre de valeurs à écrire est différent entre ces deux types
     d'éléments, on ne peut pas écrire le champ complet au format med.
  -> Risque & Conseil:
     Veuillez utiliser la restriction géométrique GROUP_MA de l'opérateur
     IMPR_RESU pour spécifier les mailles à considérer.
"""),

}
