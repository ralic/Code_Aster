# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1 : _(u"""
Erreur utilisateur dans MODI_MAILLAGE / DEFORME :
  Le fichier de déplacement fourni est associé au maillage : %(k2)s
  Alors que le maillage à déformer est : %(k1)s

  Il faut que ces 2 maillages soient les mêmes.

Conseils :
  Pour créer un champ de déplacement adapté au maillage %(k1)s, on peut utiliser
  la commande PROJ_CHAMP.
"""),

3 : _(u"""
 Erreur d'utilisation de POST_CHAMP :
   Dans la structure de données %(k2)s,
   vous avez demandé l'extraction du champ %(k1)s pour le numéro d'ordre %(i1)d.
   Mais ce champ n'existe pas.
"""),

4 : _(u"""
 !! problème création CHAM_ELEM nul dans alchml !!
"""),

5 : _(u"""
Erreur utilisateur :
  Vous essayez de faire un calcul non-linéaire mécanique sur un modèle dont les éléments
  ne sont pas programmés pour cela.
  On arrête le calcul.

Risques & conseils :
  Vous devriez changer de MODELISATION.
  Par exemple, la modélisation 'DST' ne peut pas faire du non-linéaire alors que la
  modélisation 'DKT' le peut.
"""),

6 : _(u"""
Erreur utilisateur :
 Vous utilisez le mot clé NOM_CMP, mais l'une (au moins) des composantes indiquées
 n'appartient pas à la grandeur : %(k1)s
"""),

7 : _(u"""
Alarme utilisateur :
  Vous utilisez la commande PROJ_CHAMP ou un mot clé nécessitant de "projeter"
  des noeuds sur des mailles (par exemple LIAISON_MAIL).
  L'un des noeuds (%(k1)s) du maillage (%(k2)s) a été projeté à une distance D non nulle significative.
  (D>10%% de la taille de la maille la plus proche (%(k3)s) du maillage (%(k4)s)).
"""),

8 : _(u"""
 il faut renseigner le mot clé MAILLE
"""),

9 : _(u"""
Erreur utilisateur :
  Vous ne pouvez pas utiliser la méthode ECLA_PG avec le mot-clé RESULTAT.
Conseil :
   Extrayez le champ aux ELGA que contient votre résultat puis utilisez la méthode ECLA_PG avec le mot-clé CHAM_GD.
"""),

10: _(u"""
Erreur d'utilisation dans la commande CREA_MAILLAGE :
  Le mot clé MAILLAGE est ici obligatoire.
"""),

11 : _(u"""
 le paramètre est a valeurs de type  " %(k1)s "  et la valeur de référence de type  " %(k2)s ".
"""),

12 : _(u"""
 TYPE_TEST inconnu
"""),

13 : _(u"""
 le champ  %(k1)s  est a valeurs de type  " %(k2)s "  et la valeur de référence de type  " %(k3)s ".
"""),

14 : _(u"""
 le champ  %(k1)s  est de type inconnu.
"""),

15 : _(u"""
***********************************
 Erreur dans un calcul élémentaire.
 Contexte de l'erreur :
   Maille concernée : %(k1)s
"""),

16 : _(u"""   Option de calcul : %(k1)s
   Commentaire:"""),
17 : _(u"""%(k1)s"""),
18 : _(u"""     Paramètre d'entrée : %(k1)s
      Commentaire:"""),
19 : _(u"""     Paramètre de sortie : %(k1)s
     Commentaire:"""),

20 : _(u"""
 le GROUP_NO  %(k1)s  contient  %(k2)s  noeuds
"""),

21 : _(u"""
 le GROUP_MA  %(k1)s  contient  %(k2)s  mailles
"""),

22 : _(u"""     Grandeur associée au champ : %(k1)s
     Commentaire:"""),
23 : _(u"""***********************************"""),

30 : _(u"""
PROJ_CHAMP :
  La méthode SOUS_POINT accepte uniquement les résultats de type
  EVOL_THER."""),

31 : _(u"""
PROJ_CHAMP :
  Le mot-clé facteur VIS_A_VIS est interdit avec la méthode SOUS_POINT."""),

32 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  La méthode %(k1)s est incompatible avec les champs aux noeuds."""),

33 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  La méthode %(k1)s est incompatible avec les champs par élément de type %(k2)s."""),

34 : _(u"""
   Maillage quadratique obligatoire avec terme source non nul.
"""),

35 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Vous cherchez à projeter un champ par élément (ELGA).
  Pour cela, il vous faut renseigner le mot-clé MODELE_1."""),

36 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le mot-clé TYPE_CHAM est incompatible avec le mot-clé CHAM_GD.
  Il n'est utilisable qu'avec le mot-clé RESULTAT."""),

37 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Vous cherchez à projeter un champ par élément (ELNO, ELEM ou ELGA).
  Pour cela, il vous faut renseigner le mot-clé MODELE_2."""),

38 : _(u"""
  il faut définir un champ de vitesse
"""),

39 : _(u"""
 la grandeur pour la variable:  %(k1)s  doit être:  %(k2)s  mais elle est:  %(k3)s
"""),

40 : _(u"""
PROJ_CHAMP  :
  Vous utilisez la méthode SOUS_POINT.
  Pour cela, il vous faut renseigner le mot-clé MODELE_2."""),

41 : _(u"""
 pas de variables internes initiales pour la maille  %(k1)s
"""),

42 : _(u"""
 comportements incompatibles :  %(k1)s  et  %(k2)s  pour la maille  %(k3)s
"""),

43 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le noeud %(k1)s de coordonnées (%(r1)f,%(r2)f,%(r3)f) est projeté à la distance %(r4)f"""),

44 : _(u"""
 ! le champ doit être un CHAM_ELEM !
"""),

45 : _(u"""
 ! longueurs des modes locaux incompatibles entre eux !
"""),

46 : _(u"""
 ! terme normalisation global nul !
"""),

48 : _(u"""
 PROJ_CHAMP (ou LIAISON_MAIL) :
 Nombre de noeuds projetés sur des mailles un peu distantes : %(i1)d.
 (la distance à la maille est supérieure à 1/10ème du diamètre de la maille)

 Le noeud %(k1)s est projeté le plus loin à la distance %(r1)f"""),


49 : _(u"""
 LIAISON_MAIL :
 La relation linéaire destinée à éliminer le noeud esclave %(k1)s est une tautologie
 car la maille maître en vis à vis de ce noeud possède ce même noeud dans sa connectivité.
 On ne l'écrit donc pas.
"""),

52 : _(u"""
 Le calcul du champ SIGM_ELNO n'a pas été fait sur la maille volumique %(k1)s qui borde
 la maille surfacique %(k2)s.

 Conseils :
  Il faut faire le calcul du champ SIGM_ELNO sur les éléments volumiques de l'autre "coté"
  de la face en choisissant le bon groupe de mailles soit en faisant le calcul sur tout
  le volume.
  Il est aussi possible de supprimer le calcul préalable de SIGM_ELNO, le calcul sera fait
  automatiquement sur les bonnes mailles volumiques.
"""),


53 : _(u"""
 La SUPER_MAILLE %(k1)s n'existe pas dans le maillage %(k2)s.
"""),

54 : _(u"""
 Aucune maille de peau n'a été fournie.

 Vous devez renseigner le mot-clé MAILLE/GROUP_MA en donnant une liste de mailles ou
 un groupe de maille contenant des mailles de peau.
 Si vous avez renseigné le mot-clé TOUT='OUI', cela signifie qu'il n'y a pas de mailles
 de peau dans votre modèle ; il faut revoir le maillage.
"""),

56 : _(u"""
 La combinaison 'fonction multiplicatrice' et 'chargement de type fonction' n'est pas autorisée car
 votre chargement %(k1)s contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP).
 On poursuit sans tenir compte de la fonction multiplicatrice.
"""),

57 : _(u"""
 La combinaison de chargements de même type n'est pas autorisée car l'un des chargements
 contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP)
"""),

58 : _(u"""
 La combinaison de chargements de type 'déformation initiale' n'a aucun sens physique.'
"""),

59 : _(u"""
 La combinaison de chargements de type 'pesanteur' n'a aucun sens physique.'
"""),

60 : _(u"""
 La combinaison de chargements de type 'rotation' est déconseillée.
 Veuillez plutôt utiliser un chargement de type 'force interne'.
"""),

65 : _(u"""
 composante non définie dans  la grandeur.  composante:  %(k1)s
"""),

66 : _(u"""

 le nombre de composantes affectées n'est pas égal  au nombre de composantes a affecter
 occurrence de AFFE numéro %(i1)d
 nombre de composante affectées :  %(i2)d
 nombre de composante a affecter :  %(i3)d
"""),

67 : _(u"""
 erreurs données le GROUP_MA  %(k1)s
  n'a pas le même nombre de mailles  que le GROUP_MA  %(k2)s
"""),

68 : _(u"""
 erreurs données le GROUP_MA  %(k1)s
  n'a pas les mêmes types de maille  que le GROUP_MA  %(k2)s
"""),

69 : _(u"""
 erreurs données : la maille  %(k1)s  du maillage  %(k2)s
  n'est pas la translation de la  maille  %(k3)s
  du maillage  %(k4)s
    vecteur translation :  %(r1)f %(r2)f %(r3)f
"""),

70 : _(u"""
 l'instant  de calcul  %(r1)f  n'existe pas dans  %(k1)s
"""),

71 : _(u"""
 plusieurs numéros d'ordre trouves pour l'instant  %(r1)f
"""),

72 : _(u"""
 cette commande est réentrante :   sd resultat en sortie     %(k1)s
    sd resultat "RESU_FINAL"  %(k2)s
"""),

73 : _(u"""
 la sd resultat en sortie  %(k1)s
  doit contenir qu'un seul NUME_ORDRE %(k2)s
"""),

76 : _(u"""
 Il n'est pas encore possible de découper le type_élément :  %(k1)s  en sous-éléments
    elrefa  :  %(k2)s ;
    famille :  %(k3)s.
 Faites une demande d'évolution.
"""),

78 : _(u"""
 Il n'est pas encore possible de découper le type_élément :  %(k1)s  en sous-éléments
    elrefa :  %(k2)s.
 Faites une demande d'évolution.
"""),






85 : _(u"""
 Problème liste de mailles carte : %(k1)s  numéro entité : %(i1)d
  position dans liste : %(i2)d
  numéro de maille  : %(i3)d
"""),

}
