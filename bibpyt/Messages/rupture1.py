#@ MODIF rupture1 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg={
1: _(u"""
L'option de lissage 'LAGRANGE_REGU' n'a pas été développée lorsque
le nombre de noeuds d'un fond de fissure fermé est pair.
-> Risque et Conseil :
Veuillez utiliser une autre option de lissage
(par exemple, le lissage 'LAGRANGE' pour le champ theta)
"""),


5: _(u"""
La commande POST_RUPTURE / OPERATION = '%(k1)s' traite uniquement
un seul fond de fissure. Or la table %(k2)s contient %(i1)s fonds de fissure
(colonne 'NUME_FOND').
"""),

6: _(u"""
Le rayon R_SUP (ou R_SUP_FO) doit obligatoirement etre supérieur au rayon
R_INF (resp. R_INF_FO).
-> Risque et Conseil :
Veuillez revoir votre mise en données.
"""),

7: _(u"""
Problème dans RINF et RSUP.
-> Risque et Conseil :
Veuillez revoir les valeurs fournies aux mots-clés R_INF ou R_INF_FO 
ou R_SUP ou R_SUP_FO.
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
Le fond de fissure ne doit etre défini que par un noeud.
-> Risque et Conseil :
Veuillez revoir le contenu du mot-clé GROUP_NO ou NOEUD ou FOND_FISS.
"""),

11: _(u"""
Il faut un mot clé parmis FOND_FISS ou FISSURE pour l'option %(k1)s
Veuillez le renseigner.
"""),

12: _(u"""
Aucun champ initial trouvé.
"""),

13: _(u"""
%(k1)s : champ initial impossible avec cette option.
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
Le champ %(k1)s n'a pas été trouvé.
"""),

17: _(u"""
L'association: lissage du champ THETA par les polynomes de Lagrange
               lissage de G autre que par des polynomes de Lagrange
n'est pas possible.
-> Risque et Conseil :
Veuillez consulter la documentation U4.82.03 pour déterminer une
association satisfaisante.
"""),


20: _(u"""
Une déformation initiale est présente dans la charge. Ceci est incompatible 
avec la contrainte initiale sigma_init.
-> Risque et Conseil :
On ne peut pas faire de calcul en introduisant simultanément une contrainte
initiale ET une déformation initiale. Veuillez revoir les données.
"""),

21: _(u"""
Seule une loi de comportement élastique isotrope est valide pour
le calcul de DG.
"""),

22: _(u"""
Le calcul de DG n'a pas été étendu à la plasticité !
"""),

23: _(u"""
CALC_G - option CALC_G : détection de chargements non nuls sur l'axe, 
le calcul est impossible.
-> Risque et Conseil :
En 2D-axi, le calcul de G n'est pas possible pour les éléments de l'axe de
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
Pour l'option K_G_MODA, il est indispensable de fournir la masse volumique
du matériau considéré. Veuillez revoir la mise en données de l'opérateur
DEFI_MATERIAU.
"""),

27: _(u"""
L'option est incompatible avec les comportements incrémentaux ainsi
qu'avec la déformation GROT_GDEP.
"""),

28: _(u"""
Le champ de nom symbolique %(k1)s existe déjà dans la SD RESULTAT  %(k1)s.
"""),



30: _(u"""
Il faut donner 3 composantes de la direction.
-> Risque et Conseil :
Si vous utilisez CALC_THETA/THETA_2D ou CALG_G/THETA en 2d, veuillez fournir
une valeur nulle pour la 3eme composante.
"""),

31: _(u"""
Option non opérationnelle:
Seule l'option COURONNE est à utiliser dans le cas ou
on emploie le mot clé THETA_3D ou THETA_2D.
"""),

32: _(u"""
Option inexistante:
Seule l'option BANDE est à utiliser dans le cas ou on emploie
le mot clé THETA_BANDE .
"""),

33: _(u"""
La tangente à l'origine n'est pas orthogonale à la normale au plan de la fissure.
Normale au plan :  (%(r1)f,%(r2)f,%(r3)f)
-> Risque et Conseil :
La tangente à l'origine DTAN_ORIG est nécessairement dans le plan de la fissure, 
donc orthogonale à la normale au plan, calculée à partir des fonctions de niveaux
(level set) qui définissent la fissure. Vérifier les données.
"""),

34: _(u"""
La tangente à l'extrémité n'est pas orthogonale à la normale au plan de la fissure.
Normale au plan :  (%(r1)f,%(r2)f,%(r3)f)
-> Risque et Conseil :
La tangente à l'extrémité DTAN_EXTR est nécessairement dans le plan de la fissure, 
donc orthogonale à la normale au plan, calculée à partir des fonctions de niveaux
(level set) qui définissent la fissure. Vérifier les données.
"""),

35: _(u"""
Les directions normales au plan de la fissure entre les points %(i1)d et %(i2)d successifs du fond forment un angle 
supérieur à 10 degrés.
-> Risque et Conseils
L'interpolation des sauts de déplacements est basée sur les champs singuliers 
correspondants à une fissure plane. La fissure utilisée ici est trop irrégulière et 
il y a donc un risque d'obtenir des résultats imprécis.
"""),

36: _(u"""
La tangente à l'origine n'est pas orthogonale à la normale au plan de la fissure 
défini par VECT_K1.
-> Risque et Conseil :
La tangente à l'origine DTAN_ORIG est nécessairement dans le plan de la fissure, 
donc orthogonale au VECT_K1 fourni. Vérifier les données.
"""),

37: _(u"""
La tangente à l'extrémité n'est pas orthogonale à la normale au plan de la fissure 
défini par VECT_K1.
-> Risque et Conseil :
La tangente à l'extrémité DTAN_EXTR est nécessairement dans le plan de la fissure, 
donc orthogonale au VECT_K1 fourni. Vérifier les données.
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

41: _(u"""
On recalcule donc les champs de contraintes et d'énergie
comme si vous aviez renseigné CALCUL_CONTRAINTE='OUI'.
"""),

42: _(u"""
 Lois de comportement différentes pour la maille %(k3)s :
 - loi de comportement extraite de la SD Résultat   : %(k1)s
 - loi de comportement fournie à l'opérateur CALC_G : %(k2)s

--> Risques & conseils :
On doit généralement utiliser la meme loi de comportement entre le calcul et le
post-traitement. On peut utiliser deux comportements différents, mais alors
l'utilisateur doit etre vigilant sur l'interprétation des résultats(cf.U2.05.01).
Si plusieurs comportements sont définis sur la structure, le comportement à
indiquer dans CALC_G est celui du matériau dans lequel la fissure se développe.
Dans ce cas, ce message d'alarme est quand meme émis mais le résultat est bien cohérent.
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
Les paramètres K1 et/ou G sont absents du tableau des facteurs d'intensité des
contraintes fourni.
-> Risque et Conseil :
Le mot clé METHODE_POSTK doit être fourni si et seulement si la table TABLE a été 
calculée avec l'opérateur POST_K1_K2_K3. 
"""),


46: _(u"""
Le taux de restitution d'énergie G est négatif ou nul sur certains des noeuds du fond de fissure :
le calcul de propagation est impossible.
-> Risque et Conseil :
Vérifier les paramètres du calcul de G (rayons des couronnes ou abscisse curviligne 
maximale, type de lissage, ...). 
"""),

48: _(u"""
Le calcul de la propagation d'une fissure avec la méthode 'MAILLAGE' n'est possible que
si la fissure ne comporte qu'un seul fond. La fissure fournie en entrée de PROPA_FISS
en comporte ici %(i1)s.
-> Risque et Conseil :
Vérifier la fissure produite en sortie de DEFI_FISS_XFEM.
"""),

49: _(u"""
Le facteur d'intensité des contraintes K1 est négatif sur certains des noeuds du fond 
de fissure : le calcul de propagation est impossible.
-> Risque et Conseil :
Vérifier les paramètres du calcul de K1 (rayons des couronnes ou abscisse curviligne 
maximale, type de lissage, ...). 
"""),

50: _(u"""
La définition d'une loi de propagation (mot clé facteur LOI_PROPA) est obligatoire pour
le calcul de la propagation de la fissure.
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
L'instant %(r1)d n'appartient pas au résultat %(k1)s.
"""),

54:_(u"""
Les champs de contraintes et de déformations ne sont pas de même taille. Vérifiez que votre
calcul mécanique s'est bien passé.
"""),

55:_(u"""
Problème dans la liste d'instants du résultats: 2 instants consécutifs sont égaux.
"""),

56:_(u"""
La contrainte de référence est nulle à l'instant %(r1)d.
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
le nombre de cycles pour chaqune d'elles est différent.
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
Pour le comptage %(k1)s, la table doit comporter uniquement 1 instant/nume_ordre (ou aucun).
Or la table %(k2)s contient %(i1)s instants/nume_ordre.
Conseil : Vérifier la table en entrée ou utiliser un autre type de comptage des cycles.
"""),

65:_(u"""
La table %(k1)s ne doit pas contenir le paramètre %(k2)s.
"""),

66:_(u"""
L'opération %(k1)s nécessite une seule table sous le mot-clé TABLE, or il y en a %(i1)s.
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
