# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

    6 : _(u"""
Erreur utilisateur :
 Vous utilisez le mot clé NOM_CMP, mais l'une (au moins) des composantes indiquées
 n'appartient pas à la grandeur : %(k1)s
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

    20 : _(u"""
 le GROUP_NO  %(k1)s  contient  %(k2)s  noeuds
"""),

    21 : _(u"""
 le GROUP_MA  %(k1)s  contient  %(k2)s  mailles
"""),



    28 : _(u"""
PROJ_CHAMP :
  La méthode SOUS_POINT accepte uniquement des évolutions de type TEMP HYDR NEUT
  pour définir des évolutions de variables de commandes EVOL_VARC"""),

    29 : _(u"""
PROJ_CHAMP :
  La méthode SOUS_POINT accepte uniquement des champs de type TEMP HYDR NEUT SIEF
  Un champ de type %(k1)s n'est actuellement pas géré."""),

    30 : _(u"""
PROJ_CHAMP :
  La méthode SOUS_POINT accepte uniquement les résultats de type
  EVOL_THER."""),

    31 : _(u"""
PROJ_CHAMP :
  Le mot-clé %(k1)s est interdit avec la méthode SOUS_POINT."""),

    32 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  La méthode %(k1)s est incompatible avec les champs aux noeuds."""),

    33 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  La méthode %(k1)s est incompatible avec les champs par élément de type %(k2)s."""),

    34 : _(u"""
   Maillage quadratique obligatoire avec terme source non nul."""),

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
  Pour cela, il vous faut renseigner le mot-clé  %(k1)s."""),

    43 : _(u"""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le noeud %(k1)s de coordonnées (%(r1)e,%(r2)e,%(r3)e) est projeté à la distance %(r4)e"""),

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
 Vous utilisez la commande PROJ_CHAMP ou un mot clé nécessitant de "projeter"
 des noeuds sur des mailles (par exemple LIAISON_MAIL).
 Il y a %(i1)d noeuds qui ont été projetés sur des mailles distantes.
 Pour ces noeuds, la distance à la maille la plus proche est supérieure à 1/10ème
 de la taille de cette maille.
 Les %(i2)d noeuds les plus éloignés ont été imprimés ci-dessus.

Risques et conseils :
  Le mot clé DISTANCE_MAX permet d'éviter que les noeuds trop distants ne soient
  projetés (ou "liés" quand on utilise le mot clé LIAISON_MAIL).
"""),

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

    55 : _(u"""
Alarme utilisateur :
  Vous avez utilisé le mot clé LIAISON_SOLIDE pour solidifier un ensemble
  de noeuds.
  Le nuage formé par ces noeuds est volumique mais il est très aplati.
  Le rapport entre les dimensions 3 et 1 est faible : %(r1)f
  Les relations cinématiques engendrées peuvent être proches de la 
  redondance et provoquer des problèmes de type "pivot nul".
 
Risques et Conseils :
  En utilisant le mot clé DIST_MIN, vous pouvez faire en sorte que le
  programme considère le nuage de points comme surfacique.
  Pour cela, vous devez choisir un DIST_MIN > %(r2)f
"""),

    56 : _(u"""
Alarme utilisateur :
  Vous avez utilisé le mot clé LIAISON_SOLIDE pour solidifier un ensemble
  de noeuds.
  Le nuage formé par ces noeuds est surfacique mais il est très allongé.
  Le rapport entre les dimensions 2 et 1 est faible : %(r1)f
  Les relations cinématiques engendrées peuvent être proches de la 
  redondance et provoquer des problèmes de type "pivot nul".

Risques et Conseils :
  En utilisant le mot clé DIST_MIN, vous pouvez faire en sorte que le
  programme considère le nuage de points comme linéique.
  Pour cela, vous devez choisir un DIST_MIN > %(r2)f
"""),

    57 : _(u"""
Erreur utilisateur dans la commande PROJ_CHAMP :
  La structure de données résultat à projeter ne contient que des champs 'ELGA'.
  La méthode de projection adaptée à ces champs est la méthode 'ECLA_PG' mais
  elle ne fonctionne qu'avec un champ isolé (mot clé CHAM_GD).
"""),

    58 : _(u"""
Erreur utilisateur dans la commande PROJ_CHAMP :
  On cherche à projeter un champ par éléments 'ELGA' isolé (mot clé CHAM_GD).
  La méthode de projection doit être 'ECLA_PG' et non pas %(k1)s
"""),





    65 : _(u"""
 composante non définie dans  la grandeur.  composante:  %(k1)s
"""),

    66 : _(u"""

 le nombre de composantes affectées n'est pas égal  au nombre de composantes à affecter
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
 Problème lors de la vérification de correspondance entre le
 GROUP_MA_INIT '%(k1)s' et le GROUP_MA_FINAL '%(k2)s' :

 - Aucune maille du GROUP_MA_FINAL '%(k2)s' ne semble être en correspondance
 avec la maille '%(k3)s' du GROUP_MA_INIT '%(k1)s'.

 - Vérifiez que le groupe '%(k2)s' (GROUP_MA_FINAL) du maillage %(k5)s
 (MAILLAGE_FINAL) est bien la translation du groupe '%(k1)s' (GROUP_MA_INIT)
 du maillage  %(k4)s (MAILLAGE_INIT), suivant le vecteur de translation
   ( %(r1)f , %(r2)f , %(r3)f ), avec la précision donnée.

 - Une erreur fréquente lors de l'utilisation de la commande PERM_MAC3COEUR
 est que les 2 assemblages à permuter ne sont pas du même type de conception.
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
