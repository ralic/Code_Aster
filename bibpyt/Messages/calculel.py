#@ MODIF calculel Messages  DATE 09/01/2012   AUTEUR PELLET J.PELLET 
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
 Le champ à tester comporte %(i1)d sous-points.
 Or vous n'avez pas donné de numéro de sous-point à tester.
 Il faut renseigner POINT et SOUS_POINT.
"""),

2 : _(u"""
Erreur Utilisateur :
 Quand on utilise AFFE_CHAR_CINE/EVOL_IMPO, c'est le champ de l'EVOL_XXX correspondant
 au 1er instant qui impose sa "loi" : tous les ddls de ce champ seront imposés pour tous
 les instants du calcul.

 Malheureusement, on ne trouve pas un ddl dans l'EVOL_XXX %(k1)s :
   instant : %(r1)f  noeud : %(i1)d  composante : %(k2)s

Risques & conseils :
 Assurez-vous que l'évolution imposée %(k1)s concerne les mêmes ddls pour tous les instants.
"""),

3 : _(u"""
 la grandeur :  %(k1)s  n existe pas dans le catalogue des grandeurs.
"""),

4 : _(u"""
 incohérence des maillages : %(k1)s  et  %(k2)s
"""),

5 : _(u"""
 Erreur de programmation (ou d'utilisation ?) :
   Le changement de discrétisation : %(k1)s n'est pas encore programmé.
 Risques et conseils :
   Il y a peut-être une demande d'évolution à émettre ...
"""),

6 : _(u"""
 Erreur d'utilisation :
   On n'arrive pas à construire correctement le champ contenant le nombre de sous-points
   des éléments finis (coques multi-couches, tuyaux, poutre multi-fibres, ...)  du modèle %(k1)s.

 Risques & conseils :
   Cette erreur intervient lorsque l'on ne définit pas TOUTES les caractéristiques élémentaires
   dans le même AFFE_CARA_ELEM.
   Pour les commandes de calcul, il ne faut qu'un seul MODELE et qu'un seul CARA_ELEM.
"""),

7 : _(u"""
 Erreur de maillage :
   La maille %(k1)s de type %(k2)s est trop distordue.
   Le jacobien de la transformation géométrique n'a pas le même signe sur tous les
   points de Gauss.

 Risques & conseils :
   Le maillage a-t-il été produit par un mailleur ?
   La connectivité respecte-t-elle bien la convention Aster ?
"""),

8 : _(u"""
 sur la maille %(k1)s le calcul est thermo mécanique. Or il manque la température de référence.
 On ne peut donc pas calculer de déformation thermique.
"""),

9 : _(u"""
 Erreur d'utilisation dans AFFE_CHAR_CINE :
   Aucun des ddls que l'on souhaite bloquer n'appartient au modèle.
   La charge cinématique produite est donc vide.

 Risques & Conseils :
   Vérifier le nom des ddls portés par les noeuds des éléments de votre modèle.
"""),

10 : _(u"""
Erreur de programmation lors de l'assemblage :
   Les quantités que l'on cherche à assembler (MATR_ELEM ou VECT_ELEM) ont été calculées avec au
   moins 2 partitions différentes :  %(k1)s et %(k2)s
"""),

11 : _(u"""
 le mode_local:  %(k1)s  ne doit pas être vecteur ou matrice.
"""),

12 : _(u"""
 le mode_local:  %(k1)s  ne doit pas être "DIFF__".
"""),

13 : _(u"""
Erreur utilisateur concernant le parallélisme des calculs élémentaires :
  La partition des éléments du modèle a été faite sur %(i1)d processeurs.
  Mais maintenant, le nombre de processeurs disponibles est de %(i2)d.

Conseil :
  Il faut utiliser la commande MODI_MODELE pour modifier la partition du modèle
  afin qu'elle soit cohérente avec le nombre de processeurs disponibles pour les calculs.
"""),

14 : _(u"""
  incompatibilité des type_champ ("ELGA"/"ELNO")  pour l option :  %(k1)s  entre les 2 TYPE_ELEM :  %(k2)s  et  %(k3)s
"""),

15 : _(u"""
 Erreur Utilisateur :
 On cherche à calculer une déformation thermique mais on ne trouve pas toutes les
 quantités nécessaires :
    - température
    - température de référence
    - coefficient de dilatation
"""),

16 : _(u"""
Erreur Utilisateur :
  Le modèle n'a pas pu être trouvé.

Conseils :
  Il faut soit l'indiquer par le mot-clé MODELE soit qu'il soit présent dans la
  structure de données %(k1)s.
"""),

17 : _(u"""
 type de champ inconnu
"""),

18 : _(u"""
 Vous utilisez CALC_CHAMP en reuse mais la structure de données en entrée est
 différente de celle en sortie. Ce n'est pas autorisé.
"""),

19 : _(u"""
Erreur :
 Le CHAM_ELEM %(k1)s est incohérent :
   Il possède %(i1)d GREL.
   Il a été calculé avec le LIGREL %(k2)s qui possède %(i2)d GREL.

Risques & Conseils :
 Il peut s'agir d'une erreur de programmation.
 Mais ce problème peut aussi se produire si le LIGREL (ou le MODELE)
 a été entre temps détruit et recréé sous le même nom.
"""),

20 : _(u"""
 le champ de grandeur  %(k1)s  ne respecte pas le format XXXX_r
"""),

21 : _(u"""
 les champs réel et imaginaire à assembler ne contiennent pas la même grandeur
"""),

22 : _(u"""
 problème dans le catalogue des grandeurs simples
 la grandeur %(k1)s  ne possède pas le même nombre de champs que son homologue complexe %(k2)s
"""),

23 : _(u"""
 problème dans le catalogue des grandeurs simples
 la grandeur  %(k1)s  ne possède pas les mêmes champs que son homologue complexe  %(k2)s
"""),

24 : _(u"""
 Le modèle donné dans le mot-clé MODELE n'est pas le même que celui présent dans la
 structure de données résultat. Ce n'est pas autorisé.
 En effet, le mot-clé MODELE de CALC_CHAMP n'est utilisable que dans le cas où le
 modèle est manquant dans la structure de données résultat.
"""),

25 : _(u"""
Erreur utilisateur dans PROJ_SPEC_BASE :
 La commande n'accepte que le parallélisme de type PARTITION='CENTRALISE'.
 Modèle impliqué : %(k1)s

Conseil :
 Dans la commande AFFE_MODELE (ou MODI_MODELE), il faut utiliser PARTITION='CENTRALISE'
"""),

26 : _(u"""
 Le modèle est peut-être trop grossier :
   Sur la maille %(k1)s et pour la composante %(k2)s de la grandeur %(k3)s,
   il y a une variation entre les points de la maille de %(r1)f
   alors que, globalement, les valeurs du champ ne dépassent pas %(r2)f (en valeur absolue).
   Cela fait une variation sur la maille supérieure à %(r3)f%%.
"""),

27 : _(u"""
 CHAM_ELEM à combiner incompatible
"""),

28 : _(u"""
 Problème lors de l'utilisation de la structure de données %(k1)s.
 Cette structure de données est de type "évolution temporelle" et l'on n'a pas le droit
 de l'utiliser en dehors de l'intervalle [tmin, tmax].
 Mais ici, il n'y a qu'un seul instant dans la structure de donnée (tmin=tmax).
 Dans ce cas, on suppose alors que ce transitoire est "permanent" et que l'on peut l'utiliser
 pour toute valeur du temps.
"""),

29 : _(u"""
Erreur de programmation :
 Option de calcul élémentaire inconnue au catalogue :  %(k1)s
"""),

30 : _(u"""
Erreur utilisateur :
  -> Le TYPE_ELEMENT %(k1)s  ne sait pas encore calculer l'option:  %(k2)s.

  -> Risques & Conseils :
   * Si vous utilisez une commande de "calcul" (THER_LINEAIRE, STAT_NON_LINE, ...), il n'y a pas
     moyen de contourner ce problème. Il faut changer de modélisation ou  émettre une demande d'évolution.

   * Si c'est un calcul de post-traitement, vous pouvez sans doute "éviter" le problème
     en ne faisant le post-traitement que sur les mailles qui savent le faire.
     Pour cela, il faut sans doute utiliser un mot clé de type "GROUP_MA".
     S'il n'y en a pas, il faut faire une demande d'évolution.
"""),

31 : _(u"""
  La température n'est pas correctement renseignée
"""),

32 : _(u"""
Erreur utilisateur :
  Sur la maille %(k1)s le calcul est thermo mécanique. Mais il manque le paramètre matériau
  %(k2)s . On ne peut donc pas calculer la déformation thermique.

Conseils :
  Si le problème concerne TEMP_REF, vérifiez que vous avez bien affecté une température
  de référence (AFFE_MATERIAU/AFFE_VARC/NOM_VARC='TEMP', VALE_REF=...)
"""),

33 : _(u"""
Vous utilisez CALC_ELEM, CALC_NO ou CALC_CHAMP en reuse en surchargeant le mot-clé
%(k1)s. Or ce paramètre déjà présent dans structure de données résultat sur laquelle
vous travaillez est différent de celui donné (%(k2)s et %(k3)s).

Dans ce cas, le reuse est interdit.

Conseil :
  Relancez le calcul en créant une nouvelle structure de données résultat.
"""),

34 : _(u"""
 le calcul de l'option :  %(k1)s
 n'est possible pour aucun des types d'éléments du LIGREL.
"""),

35 : _(u"""
 Erreur utilisateur :
  On essaye de fusionner 2 CHAM_ELEM mais ils n'ont pas le même nombre
  "points" (noeuds ou points de Gauss) pour la maille numéro : %(i1)d.
  Nombres de points :  %(i2)d et %(i3)d
"""),

36 : _(u"""
 Erreur utilisateur :
  On essaye de fusionner 2 CHAM_ELEM mais ils n'ont pas le même nombre
  de "sous-points" (fibres, couches, ...) pour la maille numéro : %(i1)d.
  Nombres de sous-points :  %(i2)d et %(i3)d
"""),

37 : _(u"""
 Erreur dans la lecture des CHAR_CINE ou dans les CHAR_CINE
"""),

38 : _(u"""
 la carte concerne aussi des mailles tardives qui sont oubliées
"""),

39 : _(u"""
Le chargement (mot clé: EXCIT) fourni par l'utilisateur est différent de celui présent
dans la structure de sonnées Résultat. Dans ce cas, le reuse est interdit.

Conseil :
  Relancez le calcul en créant une nouvelle structure de données résultat.
"""),




42 : _(u"""
 Erreur Programmeur:
 Incohérence fortran/catalogue
 TYPE_ELEMENT :  %(k1)s
 OPTION       :  %(k2)s
 La routine texxxx.f correspondant au calcul élémentaire ci-dessus est erronée
 Elle écrit en dehors de la zone allouée au paramètre (OUT) %(k3)s.

"""),

47 : _(u"""
  le CHAM_ELEM:  %(k1)s  n'existe pas.
"""),

48 : _(u"""
 le CHAM_ELEM: %(k1)s  n'a pas le même nombre de composantes dynamiques sur tous ses éléments.
"""),

49 : _(u"""
 le CHAM_ELEM : %(k1)s a des sous-points.
"""),


50 : _(u"""
 Vous cherchez à projeter un champ inhabituel sur le modèle final.
 Vérifiez que les modélisations que vous utilisez sont compatibles.

 Message destiné aux développeurs :
 Le paramètre:  %(k1)s  de l'option:  %(k2)s  n'est pas connu des TYPE_ELEM du LIGREL:  %(k3)s
 Champ : %(k4)s
"""),

52 : _(u"""
 La composante: %(k1)s  n'appartient pas à la grandeur: %(k2)s
 Champ : %(k4)s
"""),

53 : _(u"""
 Option : %(k1)s  inexistante dans les catalogues.
 Champ : %(k4)s
"""),

54 : _(u"""
 Le paramètre:  %(k1)s  de l'option:  %(k2)s  n'est pas connu des TYPE_ELEM du LIGREL:  %(k3)s
 Champ : %(k4)s
"""),

55 : _(u"""
 Erreur utilisateur :
   On cherche à créer un CHAM_ELEM mais sur certains points, on ne trouve pas la composante : %(k1)s
   Champ : %(k4)s
 Risques & conseils :
   Si la commande que vous exécutez comporte le mot clé PROL_ZERO='OUI', vous devriez peut-être l'utiliser.
"""),

56 : _(u"""
 Le LIGREL contient des mailles tardives
 Champ : %(k4)s
"""),

57 : _(u"""
 Erreur Utilisateur :
   On cherche à transformer un champ simple en CHAM_ELEM.
   Le nombre de "points" (points de Gauss ou noeuds) du champ simple (%(i2)d) est
   différent du nombre de points attendu pour le CHAM_ELEM (%(i1)d) :
     - maille              :  %(k1)s
     - nom du CHAM_ELEM    :  %(k4)s
     - nom du champ simple :  %(k5)s

"""),

58 : _(u"""
Erreur lors de la fabrication d'un champ par éléments :
 Il manque la composante : %(k1)s  sur la maille : %(k2)s
 Champ : %(k4)s

Risques et conseils :
 Si cette erreur se produit lors de l'exécution de la commande PROJ_CHAMP,
 il est possible de poursuivre le calcul en choisissant PROL_ZERO='OUI'
"""),

67 : _(u"""
 grandeur:  %(k1)s  inconnue au catalogue.
"""),

68 : _(u"""
 numéro de maille invalide     :  %(k1)s  (<1 ou > nombre de mailles)
"""),

69 : _(u"""
 numéro de point invalide      :  %(k1)s  (<1 ou > nombre de points)
 pour la maille                :  %(k2)s
"""),

70 : _(u"""
 numéro de sous-point invalide :  %(k1)s  (<1 ou > nombre de sous-points)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
"""),

71 : _(u"""
 numéro de composante invalide :  %(k1)s  (<1 ou > nombre de composantes)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
 pour le sous-point            :  %(k4)s
"""),

72 : _(u"""
 Erreur commande CALC_FERRAILLAGE :
   On n'a pas réussi à calculer la carte de ferraillage sur un élément.
   Code_retour de la routine clcplq.f : %(i1)d

 Signification du code d'erreur :
   1000 : Levier négatif ou nul (l'utilisateur a fourni des valeurs d'enrobage incompatibles avec l'épaisseur de l'élément)
   1010 : Dépassement déformation béton
   1020 : Erreur calcul ELU
   1050 : Dépassement contrainte béton;
"""),

73 : _(u"""
 Erreur utilisateur commande CALC_FERRAILLAGE :
   Certains mots clés de CALC_FERRAILLAGE / AFFE sont obligatoires :
     pour TYPE_COMB='ELU' :
        PIVA et PIVB
     pour TYPE_COMB='ELS' :
        CEQUI
"""),





91 : _(u"""
 incohérence des familles de points de Gauss pour la maille  %(k1)s
 ( %(k2)s / %(k3)s )
"""),

92 : _(u"""
 type scalaire du CHAM_NO :  %(k1)s  non réel.
"""),

93 : _(u"""
 type scalaire du NUME_DDL :  %(k1)s  non réel.
"""),

99 : _(u"""
 mélange de CHAM_ELEM_S et CHAM_NO_S
"""),

}
