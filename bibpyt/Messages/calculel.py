#@ MODIF calculel Messages  DATE 30/08/2011   AUTEUR BERARD A.BERARD 
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

def _(x) : return x

cata_msg = {




2 : _("""
Erreur Utilisateur :
 Quand on utilise AFFE_CHAR_CINE/EVOL_IMPO, c'est le champ de l'evol_xxx correspondant
 au 1er instant qui impose sa "loi" : tous les ddls de ce champ seront imposés pour tous
 les instants du calcul.

 Malheureusement, on ne trouve pas un ddl dans l'evol_xxx %(k1)s :
   instant : %(r1)f  noeud : %(i1)d  cmp : %(k2)s

Risques & conseils :
 Assurez-vous que l'évolution imposée %(k1)s concerne les memes ddls pour tous les instants.
"""),

3 : _("""
 la grandeur :  %(k1)s  n existe pas dans le catalogue des grandeurs.
"""),

4 : _("""
 incoherence des maillages : %(k1)s  et  %(k2)s
"""),

5 : _("""
 Erreur de programmation (ou d'utilisation ?) :
   Le changement de discrétisation : %(k1)s n'est pas encore programmé.
 Risques et conseils :
   Il y a peut-etre une demande d'évolution à émettre ...
"""),

6 : _("""
 Erreur d'utilisation :
   On n'arrive pas à construire correctement le champ contenant le nombre de sous-points
   des éléments finis (coques multi-couches, tuyaux, poutre multi-fibres, ...)  du modèle %(k1)s.

 Risques & conseils :
   Cette erreur intervient lorsque l'on ne définit pas TOUTES les caractéristiques élémentaires
   dans le meme AFFE_CARA_ELEM.
   Pour les commandes de calcul, il ne faut qu'un seul MODELE et qu'un seul CARA_ELEM.
"""),

7 : _("""
 Erreur de maillage :
   La maille %(k1)s de type %(k2)s est trop distordue.
   Le jacobien de la transformation géométrique n'a pas le meme signe sur tous les
   points de Gauss.

 Risques & conseils :
   Le maillage a-t-il été produit par un mailleur ?
   La connectivité respecte-elle bien la convention Aster ?
"""),

8 : _("""
 sur la maille %(k1)s le calcul est thermo mécanique. Or il manque la température de référence.
 On ne peut donc pas calculer de déformation thermique.
"""),

9 : _("""
 Erreur d'utilisation dans AFFE_CHAR_CINE :
   Aucun des ddls que l'on souhaite bloquer n'appartient au modèle.
   La charge cinématique produite est donc vide.

 Risques & Conseils :
   Vérifier le nom des ddls portés par les noeuds des éléments de votre modèle.
"""),

10 : _("""
Erreur de programmation lors de l'assemblage :
   Les quantités que l'on cherche à assembler (matr_elem ou vect_elem) ont été calculées avec au
   moins 2 partitions différentes :  %(k1)s et %(k2)s
"""),

11 : _("""
 le mode_local:  %(k1)s  ne doit pas etre vecteur ou matrice.
"""),

12 : _("""
 le mode_local:  %(k1)s  ne doit pas etre "DIFF__".
"""),

13 : _("""
Erreur utilisateur concernant le parallélisme des calculs élémentaires :
  La partition des éléments du modèle a été faite sur %(i1)d processeurs.
  Mais maintenant, le nombre de processeurs disponibles est de %(i2)d.

Conseil :
  Il faut utiliser la commande MODI_MODELE pour modifier la partition du modèle
  afin qu'elle soit cohérente avec le nombre de processeurs disponibles pour les calculs.
"""),

14 : _("""
  incompatibilite des type_champ ("elga"/"elno")  pour l option :  %(k1)s  entre les 2 type_elem :  %(k2)s  et  %(k3)s
"""),

15 : _("""
 Erreur Utilisateur :
 On cherche à calculer une déformation thermique mais on ne trouve pas toutes les
 quantités nécessaires :
    - température
    - température de référence
    - coefficient de dilatation
"""),

17 : _("""
 type de champ inconnu
"""),

18 : _("""
 les parties réelle et imaginaire du champ à assembler ne sont pas du même type
 l'un est un CHAM_NO et l'autre un CHAM_ELEM
"""),

19 : _("""
Erreur :
 Le cham_elem %(k1)s est incohérent :
   Il possède %(i1)d GREL.
   Il a été calculé avec le LIGREL %(k2)s qui possède %(i2)d GREL.

Risques & Conseils :
 Il peut s'agir d'une erreur de programmation.
 Mais ce problème peut aussi se produire si le LIGREL (ou le MODELE)
 a été entre temps détruit et recréé sous le meme nom.
"""),

20 : _("""
 le champ de grandeur  %(k1)s  ne respecte pas le format xxxx_r
"""),

21 : _("""
 les champs réel et imaginaire à assembler ne contiennent pas la même grandeur
"""),

22 : _("""
 problème dans le catalogue des grandeurs simples
 la grandeur %(k1)s  ne possède pas le même nombre de champs que son homologue complexe %(k2)s
"""),

23 : _("""
 problème dans le catalogue des grandeurs simples
 la grandeur  %(k1)s  ne possède pas les mêmes champs que son homologue complexe  %(k2)s
"""),

24 : _("""
 les champs à assembler n'ont pas la même longueur
"""),


25 : _("""
Erreur utilisateur dans PROJ_SPEC_BASE :
 La commande n'accepte que le parallélisme de type PARTITION='CENTRALISE'.
 Modèle impliqué : %(k1)s

Conseil :
 Dans la commande AFFE_MODELE (ou MODI_MODELE), il faut utiliser PARTITION='CENTRALISE'
"""),

26 : _("""
Les commandes CALC_ELEM et CALC_NO ne doivent pas etre utilisées en "reuse" lorsque
l'on utilise l'un des mots clés suivants :
  * MODELE
  * CHAM_MATER
  * CARA_ELEM
  * EXCIT
  * GROUP_MA / MAILLE

Risques & conseils :
  Si on utilise de tels mots clés, la structure de données enrichie manque de cohérence.
  Par exemple, le champ de déplacement DEPL a pu etre calculé avec un champ de matériau,
  alors que le champ d'énergie cinétique ECIN_ELEM_DEPL est calculé avec un autre champ de matériau.
"""),

27 : _("""
 CHAM_ELEM à combiner incompatible
"""),

28 : _("""
 Problème lors de l'utilisation de la structure de données %(k1)s.
 Cette structure de donnéees est de type "évolution temporelle" et l'on n'a pas le droit
 de l'utiliser en dehors de l'intervalle [tmin, tmax].
 Mais ici, il n'y a qu'un seul instant dans la structure de donnée (tmin=tmax).
 Dans ce cas, on suppose alors que ce transitoire est "permanent" et que l'on peut l'utiliser
 pour toute valeur du temps.
"""),

29 : _("""
Erreur de programmation :
 Option de calcul élémentaire inconnue au catalogue :  %(k1)s
"""),

30 : _("""
Erreur utilisateur :
  -> Le TYPE_ELEMENT %(k1)s  ne sait pas encore calculer l'OPTION:  %(k2)s.

  -> Risques & Conseils :
   * Si vous utilisez une commande de "calcul" (THER_LINEAIRE, STAT_NON_LINE, ...), il n'y a pas
     moyen de contourner ce problème.Il faut changer de modélisation ou  émettre une demande d'évolution.

   * Si c'est un calcul de post-traitement, vous pouvez sans doute "éviter" le problème
     en ne faisant le post-traitement que sur les mailles qui savent le faire.
     Pour cela, il faut sans doute utiliser un mot clé de type "GROUP_MA".
     S'il n'y en a pas, il faut faire une demande d'évolution.
"""),

31 : _("""
  La température n'est pas correctement renseignée
"""),

32 : _("""
Erreur utilisateur :
  Sur la maille %(k1)s le calcul est thermo mécanique. Mais il manque le paramètre matériau
  %(k2)s . On ne peut donc pas calculer la déformation thermique.

Conseils :
  Si le problème concerne TEMP_REF, vérifiez que vous avez bien affecté une température
  de référence (AFFE_MATERIAU/AFFE_VARC/NOM_VARC='TEMP', VALE_REF=...)
"""),





34 : _("""
 le calcul de l'option :  %(k1)s
 n'est possible pour aucun des types d'éléments du LIGREL.
"""),

35 : _("""
 Erreur utilisateur :
  On essaye de fusionner 2 cham_elem mais ils n'ont pas le meme nombre
  "points" (noeuds ou points de Gauss) pour la maille numéro : %(i1)d.
  Nombres de points :  %(i2)d et %(i3)d
"""),

36 : _("""
 Erreur utilisateur :
  On essaye de fusionner 2 cham_elem mais ils n'ont pas le meme nombre
  de "sous-points" (fibres, couches, ...) pour la maille numéro : %(i1)d.
  Nombres de sous-points :  %(i2)d et %(i3)d
"""),

37 : _("""
 Erreur dans la lecture des CHAR_CINE ou dans les CHAR_CINE
"""),

38 : _("""
 la carte concerne aussi des mailles tardives qui sont oubliées
"""),

42 : _("""
 Erreur Programmeur:
 Incohérence fortran/catalogue
 TYPE_ELEMENT :  %(k1)s
 OPTION       :  %(k2)s
 La routine texxxx.f correspondant au calcul élémentaire ci-dessus est bugguée
 Elle écrit en dehors de la zone allouée au paramètre (OUT) %(k3)s.

"""),

47 : _("""
  le CHAM_ELEM:  %(k1)s  n'existe pas.
"""),

48 : _("""
 le CHAM_ELEM: %(k1)s  n'a pas le même nombre de composantes dynamiques sur tous ses éléments.
"""),

49 : _("""
 le CHAM_ELEM : %(k1)s a des sous-points.
"""),


50 : _("""
 Vous cherchez à projeter un champ inhabituel sur le modèle final.
 Vérifiez que les modélisations que vous utilisez sont compatibles.

 Message destiné aux développeurs :
 Le paramètre:  %(k1)s  de l'option:  %(k2)s  n'est pas connu des TYPE_ELEM du LIGREL:  %(k3)s
 Champ : %(k4)s
"""),

52 : _("""
 La composante: %(k1)s  n'appartient pas à la grandeur: %(k2)s
 Champ : %(k4)s
"""),

53 : _("""
 Option : %(k1)s  inexistante dans les catalogues.
 Champ : %(k4)s
"""),

54 : _("""
 Le paramètre:  %(k1)s  de l'option:  %(k2)s  n'est pas connu des TYPE_ELEM du LIGREL:  %(k3)s
 Champ : %(k4)s
"""),

55 : _("""
 Erreur utilisateur :
   On cherche à créer un CHAM_ELEM mais sur certains points, on ne trouve pas la composante : %(k1)s
   Champ : %(k4)s
 Risques & conseils :
   Si la commande que vous exécutez comporte le mot clé PROL_ZERO='OUI', vous devriez peut-etre l'utiliser.
"""),

56 : _("""
 Le LIGREL contient des mailles tardives
 Champ : %(k4)s
"""),

57 : _("""
 Erreur Utilisateur :
   On cherche à transformer un champ simple en cham_elem.
   Le nombre de "points" (points de Gauss ou noeuds) du champ simple (%(i2)d) est
   différent du nombre de points attendu pour le cham_elem (%(i1)d) :
     - maille              :  %(k1)s
     - nom du cham_elem    :  %(k4)s
     - nom du champ simple :  %(k5)s

"""),

58 : _("""
 Il manque la composante : %(k1)s  sur la maille : %(k2)s
 Champ : %(k4)s
"""),

67 : _("""
 grandeur:  %(k1)s  inconnue au catalogue.
"""),

68 : _("""
 numéro de maille invalide     :  %(k1)s  (<1 ou >nbma)
"""),

69 : _("""
 numéro de point invalide      :  %(k1)s  (<1 ou >nbpt)
 pour la maille                :  %(k2)s
"""),

70 : _("""
 numéro de sous_point invalide :  %(k1)s  (<1 ou >nbspt)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
"""),

71 : _("""
 numéro de composante invalide :  %(k1)s  (<1 ou >nbcmp)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
 pour le sous-point            :  %(k4)s
"""),

72 : _("""
 Erreur commande CALC_FERRAILLAGE :
   On n'a pas réussi à calculer la carte de ferraillage sur un élément.
   Code_retour de la routine clcplq.f : %(i1)d

 Signification du code d'erreur :
   1000 : Levier negatif ou nul (l'utilisateur a fourni des valeurs d'enrobage incompatibles avec l'épaisseur de l'élément)
   1010 : Dépassement déformation béton
   1020 : Erreur calcul ELU
   1050 : Dépassement contrainte béton;
"""),

73 : _("""
 Erreur utilisateur commande CALC_FERRAILLAGE :
   Certains mots clés de CALC_FERRAILLAGE / AFFE sont obligatoires :
     pour TYPE_COMB='ELU' :
        PIVA et PIVB
     pour TYPE_COMB='ELS' :
        CEQUI
"""),





91 : _("""
 incohérence des familles de points de Gauss pour la maille  %(k1)s
 ( %(k2)s / %(k3)s )
"""),

92 : _("""
 type scalaire du CHAM_NO :  %(k1)s  non réel.
"""),

93 : _("""
 type scalaire du NUME_DDL :  %(k1)s  non réel.
"""),

99 : _("""
 melange de CHAM_ELEM_S et CHAM_NO_S
"""),

}
