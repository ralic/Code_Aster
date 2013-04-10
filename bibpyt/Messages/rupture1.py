#@ MODIF rupture1 Messages  DATE 08/04/2013   AUTEUR LADIER A.LADIER 
# -*- coding: iso-8859-1 -*-
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg={
1: _(u"""
L'option de lissage 'LAGRANGE_REGU' n'a pas été développée lorsque
le nombre de noeuds d'un fond de fissure fermé est pair.
-> Risque et Conseil :
Veuillez utiliser une autre option de lissage
(par exemple, le lissage 'LAGRANGE' pour le champ thêta)
"""),

4: _(u"""
La commande CALC_G ne traite pas le cas des fonds doubles.
"""),

5: _(u"""
Le paramètre R_INF automatiquement choisi vaut %(r1)f.
Le paramètre R_SUP automatiquement choisi vaut %(r2)f.
"""),

6: _(u"""
Le rayon R_SUP (ou R_SUP_FO) doit obligatoirement être supérieur au rayon
R_INF (respectivement R_INF_FO).
-> Risque et Conseil :
Veuillez revoir votre mise en données.
"""),

7: _(u"""
Il n'est pas possible de calculer automatiquement R_INF et R_SUP en cas
de modélisation FEM avec une fissure en configuration décollée.
-> Risque et Conseil :
Veuillez indiquer les mots-clés R_INF et R_SUP (ou R_INF_FO et R_SUP_FO).
"""),

8: _(u"""
Le groupe %(k1)s n'appartient pas au maillage : %(k2)s
"""),

9: _(u"""
Le fond de fissure n'est pas complet.
-> Risque et Conseil :
Veuillez revoir la mise en données de l'opérateur DEFI_FOND_FISS.
"""),

10: _(u"""
Le fond de fissure ne doit être défini que par un noeud.
-> Risque et Conseil :
Veuillez revoir le contenu du mot-clé GROUP_NO ou NOEUD ou FOND_FISS.
"""),

11: _(u"""
Il faut un mot clé parmi FOND_FISS ou FISSURE.
Veuillez le renseigner.
"""),

12: _(u"""
Le champ de contrainte initiale n'est ni de type ELNO, ni NOEU, ni ELGA.
"""),

13: _(u"""
%(k1)s : prise en compte d'un état initial impossible avec cette option.
"""),

14: _(u"""
Nombre de bornes erroné.
-> Risque et Conseil :
On doit en avoir autant que de numéros d'ordre.
"""),

15: _(u"""
Le résultat n'est pas un EVOL_NOLI.
"""),

16: _(u"""
La différence entre la taille maximale et la taille minimale des mailles connectées aux
noeuds du fond de fissure est importante.
La taille minimale vaut : %(r1)f
La taille maximale vaut : %(r2)f
-> Risque et Conseil :
Il a été choisi de multiplier par quatre la taille maximale des mailles connectées aux
noeuds du fond de fissure pour calculer le paramètre R_SUP et par deux, pour calculer
le paramètre R_INF. Or, si ces tailles sont importantes, vous risquez de post-traiter
vos résultats sur une zone trop grande autour du fond de fissure et d'obtenir des
valeurs de taux de restitution d'énergie et de facteurs d'intensité moins précises.
Vérifiez que les valeurs de R_INF et de R_SUP calculées sont licites.
Sinon, veuillez spécifier directement les valeurs de R_INF et de R_SUP ou bien revoir
le maillage de manière à rendre les mailles proches du fond de fissure de taille homogène.
"""),

17: _(u"""
L'association: lissage du champ THETA par les polynômes de Lagrange
               lissage de G autre que par des polynômes de Lagrange
n'est pas possible.
-> Risque et Conseil :
Veuillez consulter la documentation U4.82.03 pour déterminer une
association satisfaisante.
"""),

18: _(u"""
Les mots-clés R_INF_FO et R_SUP_FO ne peuvent être employés dans le cas 2D.
-> Risque et Conseil :
Veuillez les remplacer par R_INF ou R_SUP, ou bien ne rien indiquer afin que les rayons
de la couronne soient calculés avec les données du maillage.
"""),

19: _(u"""
L'utilisation du mot-clé facteur %(k1)s n'est compatible qu'avec une modélisation %(k2)s.
Veuillez vérifiez vos données.
"""),

20: _(u"""
Votre étude comporte une charge de type PRE_EPSI. Ceci est incompatible
avec la présence d'une contrainte initiale dans le calcul de G(mot clé SIGM_INIT
de l'opérateur CALC_G).
-> Risque et Conseil :
On ne peut pas faire de calcul de G en introduisant simultanément une contrainte
initiale ET une déformation initiale. Veuillez revoir les données.
"""),

21: _(u"""
La liste de taille n'a pas la taille de la liste des groupes mailles.
Vérifiez vos données.
"""),

22: _(u"""
Les mailles volumiques caractérisant les zones de calcul doivent absolument être des
hexaèdres.
Vérifiez votre maillage ou vos données.
"""),

23: _(u"""
CALC_G - option CALC_G : détection de chargements non nuls sur l'axe,
le calcul est impossible.
-> Risque et Conseil :
En 2D axisymétrique, le calcul de G n'est pas possible pour les éléments de l'axe de
symétrie si un chargement est imposé sur ceux-ci.
Modifier les couronnes R_INF et R_SUP pour qu'elles soient toutes les deux
plus petites que le rayon du fond de fissure.
"""),

24: _(u"""
L'option CALC_K_G est incompatible avec les comportements incrémentaux,
avec les comportements non linéaires et avec la déformation GROT_GDEP.
"""),

25: _(u"""
Il faut affecter les éléments de bord (E et NU) pour le calcul des fic.
-> Risque et Conseil :
Veuillez revoir la mise en données des opérateurs DEFI_MATERIAU
et AFFE_MATERIAU.
"""),

26: _(u"""
La masse volumique RHO n'a pas été définie.

-> Risque et Conseil :
Pour le calcul de l'option CALC_K_G avec un résultat de type MODE_MECA,
il est indispensable de fournir la masse volumique du matériau considéré.
La masse volumique doit être définie dans l'opérateur DEFI_MATERIAU.
"""),

27: _(u"""
La connexité entre les mailles caractérisant les zones de calcul n'est pas valide.
-> Risque et Conseil :
Veuillez vous assurer que les copeaux de chaque tranche définies dans
GROUP_MA du mot-clé facteur TRANCHE_3D, suivent les règles précisées dans
la documentation U de CALC_GP.
"""),

28: _(u"""
Le champ de nom symbolique %(k1)s existe déjà dans la SD RESULTAT  %(k1)s.
"""),

29: _(u"""
Au moins une des mailles caractérisant les zones de calcul a une forme trop
trapézoïdale.
-> Risque et Conseil :
Le calcul de la surface de sa face appartenant au plan de symétrie de
l'entaille risque d'être altéré et par conséquent celui de GP également.
Veuillez vérifier votre maillage.
"""),

30: _(u"""
Il faut donner 3 composantes de la direction.
-> Risque et Conseil :
Si vous utilisez CALC_THETA/THETA_2D ou CALC_G/THETA en 2d, veuillez fournir
une valeur nulle pour la 3ème composante.
"""),



35: _(u"""
Les directions normales au plan de la fissure entre les points %(i1)d et %(i2)d successifs du fond forment un angle
supérieur à 10 degrés.
-> Risque et Conseils
L'interpolation des sauts de déplacements est basée sur les champs singuliers
correspondants à une fissure plane. La fissure utilisée ici est trop irrégulière et
il y a donc un risque d'obtenir des résultats imprécis.
"""),

38: _(u"""
La fissure contient %(i1)d fond(s) et le calcul est demandé pour le fond numéro %(i2)d.
-> Risque et Conseil :
Vérifier le paramètre défini sous le mot clé NUME_FOND de POST_K1_K2_K3.
"""),

39: _(u"""
La récupération des contraintes à partir de la SD Résultat n'est permise que si les fissures sont maillées.
-> Risque et Conseil :
Veillez à ne pas vous servir de FISSURE avec le mot-clé CALCUL_CONTRAINTE.
"""),

42: _(u"""
 Lois de comportement différentes pour la maille %(k3)s :
 - loi de comportement extraite de la SD Résultat   : %(k1)s
 - loi de comportement fournie à l'opérateur CALC_G : %(k2)s

--> Risques & conseils :
On doit généralement utiliser la même loi de comportement entre le calcul et le
post-traitement. On peut utiliser deux comportements différents, mais alors
l'utilisateur doit être vigilant sur l'interprétation des résultats(cf.U2.05.01).
Si plusieurs comportements sont définis sur la structure, le comportement à
indiquer dans CALC_G est celui du matériau dans lequel la fissure se développe.
Dans ce cas, ce message d'alarme est quand même émis mais le résultat est bien cohérent.
Un post-traitement élastique non-linéaire d'un calcul élastoplastique est
admissible, si le chargement est radial et monotone. La comparaison du G calculé
à partir des contraintes issues de STAT_NON_LINE (option CALC_CONTRAINTE='NON')
ou à partir des contraintes recalculées avec la loi de comportement
(CALC_CONTRAINTES='OUI') peut fournir une indication sur le respect de ces
hypothèses.
"""),

43: _(u"""
Le calcul de G avec une modélisation X-FEM n'est pas possible avec une loi de comportement
élastoplastique.
--> Risques & conseils :
Remplacer si possible le comportement élastoplastique (COMP_INCR) par un comportement
élastique non linéaire (COMP_ELAS).
"""),

44: _(u"""
Les paramètres K1 et/ou K2 et/ou G sont absents du tableau des facteurs d'intensité des
contraintes (SIF) fourni.
-> Risque et Conseil :
Le tableau des facteurs d'intensité des contraintes doit absolument contenir ces trois
paramètres ainsi que K3 en 3D. Veuillez vérifier le contenu de votre tableau de SIF.
"""),

45: _(u"""
 Lois de comportement différentes pour la maille %(k3)s :
 - type de déformation de la loi de comportement extraite de la SD Résultat   : %(k1)s
 - type de déformation de la loi de comportement fournie à l'opérateur CALC_G : %(k2)s

--> Risques & conseils :
On doit généralement utiliser la même type de déformation de loi de comportement entre le calcul et le
post-traitement. On peut utiliser deux type de déformation différents, mais alors
l'utilisateur doit être vigilant sur l'interprétation des résultats(cf.U2.05.01).
Si plusieurs types de déformation de comportements sont définis sur la structure, le type de déformation comportement à
indiquer dans CALC_G est celui du matériau dans lequel la fissure se développe.
Dans ce cas, ce message d'alarme est quand même émis mais le résultat est bien cohérent.
"""),

46: _(u"""
Le taux de restitution d'énergie G est négatif ou nul sur certains des noeuds du fond de fissure :
le calcul de propagation est impossible.
-> Risque et Conseil :
Vérifier les paramètres du calcul de G (rayons des couronnes ou abscisse curviligne
maximale, type de lissage, ...).
"""),

47: _(u"""
Vous demandez un calcul de G en post-traitement d'un calcul élastoplastique. Ceci n'est valable que 
si votre CHARGEMENT est MONOTONE PROPORTIONNEL.
Si tel est le cas, renseignez, dans CALC_G, l'option COMP_ELAS, RELATION = ELAS_VMIS_XXX pour un calcul de G.
Si votre chargement n'est pas monotone proportionnel, il faut renseigner, dans CALC_G, 
l'option COMP_INCR, RELATION=VMIS_XXX, et dans ce cas vous calculerez GTP (modèle en cours de validation).
"""),


51: _(u"""
PROPA_FISS / METHODE = 'MAILLAGE' : les noeuds définissant la fissure initiale ne sont
pas ordonnés. Vérifiez le maillage donné en entrée (MAIL_ACTUEL).
"""),

52: _(u"""
PROPA_FISS / METHODE = 'INITIALISATION' : les deux vecteurs VECT_X et VECT_Y
définissant la fissure initiale, de forme elliptique, ne sont pas orthogonaux. Vérifiez
les données d'entrée.
"""),

53: _(u"""
L'instant %(r1)f n'appartient pas au résultat %(k1)s.
"""),

54:_(u"""
Les champs de contraintes et de déformations ne sont pas de même taille. Vérifiez que votre
calcul mécanique s'est bien passé.
"""),

55:_(u"""
Problème dans la liste d'instants du résultats: 2 instants consécutifs sont égaux.
"""),

56:_(u"""
La contrainte de référence est nulle à l'instant %(r1)f.
"""),

57:_(u"""
Problème dans la dimension du modèle. POST_BORDET ne supporte pas les raccords 2D-3D
"""),

58:_(u"""
L'utilisation de POST_BORDET n'est possible qu'avec 1 seul MODELE et 1 seul
CHAM_MATERIAU
"""),

59:_(u"""
La table %(k1)s ne contient pas le paramètre %(k2)s.
"""),

60:_(u"""
Le critère 'K2_NUL' donne des mauvais résultats pour des angles supérieurs à 60 degrés.
Il se peut que le signe de l'angle soit faux.
Conseil : utiliser le critère par défaut.
"""),

61:_(u"""
Impossible de réaliser le comptage sur les quantités demandées car
le nombre de cycles pour chacune d'elles est différent.
Conseil : limiter le comptage des cycles à une seule quantité (K_EQ par exemple).
"""),

62:_(u"""
Pour l'opération %(k1)s, la table doit être réentrante (reuse obligatoire).
"""),

63:_(u"""La récupération des contraintes à partir de la SD Résultat
en présence d'un état initial n'est pas permise.
Pour l'opération %(k1)s, la table ne doit pas être réentrante (reuse interdit).
"""),

64:_(u"""
Pour le comptage %(k1)s, la table doit comporter uniquement 1 instant/NUME_ORDRE (ou aucun).
Or la table %(k2)s contient %(i1)d instants/NUME_ORDRE.
Conseil : Vérifier la table en entrée ou utiliser un autre type de comptage des cycles.
"""),

65:_(u"""
La table %(k1)s ne doit pas contenir le paramètre %(k2)s.
"""),

66:_(u"""
L'opération %(k1)s nécessite une seule table sous le mot-clé TABLE, or il y en a %(i1)d.
"""),

67:_(u"""
Les caractéristiques du matériau ne peuvent dépendre que de la température.
-> Conseil :
Veuillez revoir les données du matériau.
"""),

68:_(u"""
La macro-commande POST_RUPTURE ne fonctionne pas quand les paramètres matériau ne sont pas constants.
"""),

}
