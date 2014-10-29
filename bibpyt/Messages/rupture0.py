# coding=utf-8
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

cata_msg = {


    2: _(u"""
Erreur dans la récupération de la dimension (du maillage ou du modèle).
L'opérateur CALC_G ne fonctionne que pour des maillages et des modèles purement 2d ou 3d.
Notamment, un mélange de dimensions n'est pas autorisé.
Si le besoin est réel, faites une demande d'évolution auprès de l'équipe de développement.
"""),

    3: _(u"""
Erreur utilisateur.
Incohérence entre le mot-clé OPTION et la dimension du problème.
L'option %(k1)s ne s'utilise qu'en 3d. Or le problème est 2d.
"""),

    4: _(u"""
Erreur utilisateur :
Incohérence entre le mot-clé FISSURE et le modèle associé au mot-clé RESULTAT.
- Pour utiliser une fissure maillée, renseignez sous le mot-clé FOND_FISS
une fissure provenant de la commande DEFI_FOND_FISS.
- Pour utiliser une fissure non maillée (calcul X-FEM), renseignez sous le mot-clé FISSURE
une fissure provenant de la commande DEFI_FISS_XFEM. Le modèle associé au mot-clé RESULTAT
doit être un modèle X-FEM provenant de la commande MODI_MODELE_XFEM.
"""),


    5: _(u"""
Il faut définir ELAS dans DEFI_MATERIAU.
"""),

    6: _(u"""
La température en fond de fissure, nécessaire pour le calcul des propriétés
matériaux et donc des facteurs d'intensité des contraintes, n'est pas connue.
Le calcul se poursuite en prenant la température de référence du matériau
(TEMP = %(r1)f).
-> Risque et Conseil :
Quand les propriétés matériau dépendent de la température, il faut fournir
en entrée de POST_K1_K2_K3 le champ de température utilisé pour le calcul
mécanique, sous le mot clé EVOL_THER.
"""),

    7: _(u"""
L'entité %(k1)s renseignée au mot-clé %(k2)s n'est pas dans le maillage.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé %(k2)s.
"""),

    8: _(u"""
Problème dans la création de la base locale au fond de fissure.
Il est impossible de déterminer le sens de la direction de propagation (vecteur tangent aux lèvres).
Dans le cas symétrique (SYME='OUI') il faut :
- soit donner les lèvres de la fissure (LEVRE_SUP),
- soit indiquer le vecteur tangent au point origine du fond de fissure (DTAN_ORIG).
"""),

    9: _(u"""
Dans le cas d'une SD RESULTAT de type DYNA_TRANS,
le mot-clé EXCIT est obligatoire.
Veuillez le renseigner.
"""),

    10: _(u"""
Modélisation non implantée.
"""),

    11: _(u"""
Problème à la récupération des noeuds du fond de fissure.
-> Risque et Conseil :
Vérifier que le concept %(k1)s indiqué sous le mot clé FOND_FISS a été
correctement crée par l'opérateur DEFI_FOND_FISS.
"""),

    12: _(u"""
Type de mailles du fond de fissure non défini.
-> Risque et Conseil :
Pour une modélisation 3D, les mailles de votre fond de fissure
doivent être de type SEG2 ou SEG3.
Veuillez revoir la création de votre fond de fissure
(opérateur DEFI_FOND_FISS).
"""),

    13: _(u"""
Le GROUP_NO %(k1)s n'est pas dans le maillage.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé GROUP_NO.
"""),

    14: _(u"""
Le noeud  %(k1)s  n'appartient pas au maillage :  %(k2)s
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé SANS_GROUP_NO.
"""),

    15: _(u"""
Le noeud %(k1)s n'appartient pas au fond de fissure.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé GROUP_NO ou NOEUD.
"""),

    16: _(u"""
Les lèvres de la fissure ne sont pas initialement collées.
POST_K1_K2_K3 ne fonctionne que sur des lèvres initialement collées.
-> Risque et Conseil :
   Veuillez vérifier la définition du fond de fissure (DEFI_FOND_FISS/CONFIG_INIT).
   Si les lèvres sont vraiment décollées alors il faut utiliser CALC_G.
"""),

    17: _(u"""
La différence entre la taille maximale et la taille minimale des mailles connectées aux
noeuds du fond de fissure est importante.
La taille minimale vaut : %(r1)f
La taille maximale vaut : %(r2)f
-> Risque et Conseil :
Il a été choisi de multiplier par %(i1)d la taille maximale des mailles connectées aux
noeuds du fond de fissure pour calculer le paramètre ABSC_CURV_MAXI. Or, si cette taille
est importante, vous risquez de post-traiter vos résultats sur une zone trop éloignée
du fond de fissure et d'obtenir des valeurs de facteurs d'intensité moins précises.
Vérifiez que la valeur de ABSC_CURV_MAXI calculée est licite.
Sinon, veuillez spécifier directement la valeur de ABSC_CURV_MAXI ou bien revoir
le maillage de manière à rendre les mailles proches du fond de fissure de taille homogène.
"""),

    18: _(u"""
Problème à la récupération du modèle dans la structure de données résultat fournie.
-> Risque et Conseil :
Veuillez vérifier que le concept fourni au mot-clé RESULTAT correspond
au résultat à considérer.
"""),

    19: _(u"""
Problème à la récupération des noeuds de la lèvre supérieure :
-> Risque et Conseil :
Pour un calcul avec POST_K1_K2_K3, la lèvre supérieure de la fissure doit
être obligatoirement définie dans DEFI_FOND_FISS à l'aide du mot-clé
LEVRE_SUP. Vérifier la définition du fond de fissure.
"""),


    21: _(u"""
Les noeuds ne sont pas en vis-à-vis dans le plan perpendiculaire
au noeud %(k1)s.
-> Risque et Conseil :
Pour interpoler les sauts de déplacement, les noeuds doivent être par défaut
en vis-à-vis deux à deux sur les lèvres. Si ce n'est pas le cas, utilisez
l'option TYPE_MAILLE='LIBRE' dans POST_K1_K2_K3.
"""),

    22: _(u"""
Il manque des points dans le plan défini par la lèvre
supérieure et perpendiculaire au fond %(k1)s.
-> Risque et Conseil :
"""),

    23: _(u"""
Vérifier les tangentes extrémités ou
"""),

    24: _(u"""
Augmenter PREC_NORM dans DEFI_FOND_FISS.
"""),

    25: _(u"""
Augmenter ABSC_CURV_MAXI.
"""),

    26: _(u"""
Il manque des points dans le plan défini par la lèvre
inférieure et perpendiculaire au fond  %(k1)s.
-> Risque et Conseil :
"""),

    27: _(u"""
Pour un résultat de type MODE_MECA, seule l'option CALC_K_G est disponible.
"""),

    28: _(u"""
Le cas de charge %(k1)s n'a pas été trouvé dans la SD Résultat %(k2)s.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé NOM_CAS.
"""),

    29: _(u"""
Lorsque une modélisation 3D est de type FEM l'option %(k1)s nécessite une
fissure en configuration collée.
-> Risque et Conseil :
Veuillez mettre CONFIG_INIT='COLLEE' dans DEFI_FOND_FISS.
"""),

    30: _(u"""
Calcul possible pour aucun noeud du fond.
-> Risque et Conseil :
Veuillez vérifier les données, notamment celles du mot-clé DIRECTION.
"""),

    31: _(u"""
Il n'y a pas de mailles de bord connectées au noeud %(k1)s.
"""),

    32: _(u"""
Le paramètre ABSC_CURV_MAXI automatiquement choisi vaut : %(r1)f.
"""),

    34: _(u"""
L'hypothèse de lèvres collées n'est pas valide.
Cela peut être dû au fait :
 - que seule la lèvre supérieure est modélisée. Dans ce cas, il faut mettre SYME='OUI'.
 - que les lèvres sont initialement décollées. Dans ce cas, il faut mettre CONFIG_INIT='DECOLLEE'.
"""),

    35: _(u"""
Attention, le vecteur tangent au premier noeud du fond de fissure (DTAN_ORIG) est dans le sens
opposé à celui calculé automatiquement (%(r1)f %(r2)f %(r3)f).
Cela est probablement une erreur, qui peut conduire à des résultats faux.
-> Risque et Conseil :
  - vérifiez DTAN_ORIG,
  - ou bien ne le renseignez pas.
"""),

    36: _(u"""
Attention, le vecteur tangent au dernier noeud du fond de fissure (DTAN_EXTR) est dans le sens
opposé à celui calculé automatiquement (%(r1)f %(r2)f %(r3)f).
Cela est probablement une erreur, qui peut conduire à des résultats faux.
-> Risque et Conseil :
  - vérifiez DTAN_EXTR,
  - ou bien ne le renseignez pas.
"""),

    37: _(u"""
Le numéro d'ordre %(i1)d n'a pas été trouvé dans la table.
"""),

    38: _(u"""
Pas d'instant trouvé dans la table pour l'instant %(r1)f.
"""),

    39: _(u"""
Plusieurs instants trouvés dans la table pour l'instant %(r1)f.
"""),

    41 : _(u"""
Le groupe de mailles %(k1)s défini sous le mot-clé GROUP_MA n'existe pas.
"""),

    42 : _(u"""
Dans le cas où le fond est une courbe fermée, les mots-clés MAILLE_ORIG ou GROUP_MA_ORIG doivent accompagner le mot-clé NOEUD_ORIG ou GROUP_NO_ORIG.
"""),

    43 : _(u"""
Le noeud défini le mot-clé NOEUD_ORIG ou GROUP_NO_ORIG n'appartient pas à la maille définie
sous le mot-clé MAILLE_ORIG ou GROUP_MA_ORIG.
"""),

    44 : _(u"""
La maille %(k1)s définie sous le mot-clé MAILLE_ORIG ou GROUP_MA_ORIG n'appartient pas au fond de fissure.
"""),

    45 : _(u"""
Une seule maille doit constitué le groupe de mailles GROUP_MA_ORIG. La maille utilisée est %(k1)s.
"""),

    46: _(u"""
Il faut au moins trois noeuds dans le plan défini par les lèvres et perpendiculaire
au fond de fissure. Le calcul est impossible. On extrapole les valeurs du point le plus proche.
"""),

    47: _(u"""
Noeud %(k1)s
"""),

    48: _(u"""
Le mot-clé 'FOND_FISS' est obligatoire avec l'option  %(k1)s.
Veuillez le renseigner.
"""),

    49: _(u"""
Le maillage ne permet pas de déterminer la taille des mailles en fond de fissure, et donc
R_INF/R_SUP ou ABSC_CURV_MAXI seront obligatoires en post-traitement.
-> Conseil :
Pour ne plus avoir cette alarme, il faut revoir le maillage et faire en sorte que chaque noeud du fond
de fissure soit connecté à au moins une arête faisant un angle inférieur à 60 degrés avec le vecteur de
direction de propagation.
"""),

    50: _(u"""
Nombre de modes différent entre la base modale
et %(k1)s : on prend le minimum des deux %(i1)d.
"""),

    51: _(u"""
Le numéro d'ordre %(i1)d n'appartient pas au résultat %(k1)s.
"""),

    52: _(u"""
Vous avez utilisé des paramètres matériaux dépendant de la température.
Cependant, 'TEMP_DEF_ALPHA' dans DEFI_MATERIAU n'est pas renseigné.
-> Conseil :
Renseignez une température pour 'TEMP_DEF_ALPHA',
ou utilisez l'option 'EVOL_THER' de POST_K1_K2_K3.
"""),

    53: _(u"""
Vous avez utilisé un module de Young nul. Le post-traitement ne peut pas se poursuivre."""),

    54: _(u"""
Aucun instant ou numéro d'ordre trouvé.
"""),

    55: _(u"""
-> Attention: Le mot-clé EXCIT est facultatif, il n'est pas nécessaire dans tous les cas.
Il n'est utile que si le résultat à post-traiter a été créé avec la commande CREA_RESU.
-> Risque et Conseil :
Si vous utilisez CALC_G en dehors de ce cas spécifique,
vérifiez la présence de tous les chargements ayant servi à créer le résultat.
"""),

    56 : _(u"""
CALC_G - option CALC_K_G : le calcul est impossible sur un point de rayon nul
(point sur l'axe de rotation).
-> Risque et Conseil :
Modifier les couronnes R_INF et R_SUP pour qu'elles soient toutes les deux plus
petites que le rayon du fond de fissure. De manière générale en axisymétrie, le
calcul de K est d'autant plus précis que le rayon des couronnes est petit devant
le rayon du fond de fissure.
"""),

    57 : _(u"""
Pour cette option en 3D, le champ THETA doit être calculé directement
dans l'opérateur CALC_G.
-> Risque et Conseil :
Dans le mot-clé facteur THETA, supprimez le mot-clé THETA et renseignez les
mots-clés FOND_FISS, R_SUP, R_INF, MODULE, et DIRECTION pour la détermination
automatique du champ thêta.
"""),








    60 : _(u"""
Mélange de mailles de type SEG2 et SEG3 dans la définition du fond de fissure.
-> Risque et Conseil :
Les mailles du fond de fissure doivent toutes être du même type.
Modifiez le maillage ou définissez plusieurs fonds de fissure consécutifs.
"""),

    61 : _(u"""
L'angle entre 2 vecteurs normaux consécutifs est supérieur à 10 degrés.
Cela signifie que la fissure est fortement non plane.
-> Risque et Conseil :
 - Le calcul des facteurs d'intensité des contraintes sera potentiellement imprécis,
 - Un raffinement du fond de fissure est probablement nécessaire.
"""),

    63 : _(u"""
Les mailles du fond de fissure doivent être du type segment (SEG2 ou SEG3).
"""),

    65 : _(u"""
Détection d'une maille de type %(k1)s dans la définition des lèvres de la
fissure (%(k2)s).
-> Risque et Conseil :
Les mailles des lèvres doivent être du type quadrangle ou triangle.
Vérifiez que les mailles définies correspondent bien aux faces des éléments
3D qui s'appuient sur la lèvre.
"""),

    66 : _(u"""
Le groupe de noeuds ou la liste de noeuds définissant le fond de fissure n'est pas ordonné.
-> Risque et Conseil :
Il faut ordonner les noeuds du fond de fissure.
Les options SEGM_DROI_ORDO et NOEUD_ORDO de l'opérateur
DEFI_GROUP/CREA_GROUP_NO peuvent être utilisées.
."""),

    67 : _(u"""
Les mots-clés LISSAGE_G et LISSAGE_THETA n'ont de sens que pour un calcul 3d local.
Dans le cas présent, ils sont ignorés.
-> Conseil pour ne plus avoir cette alarme :
Supprimer les mots-clés LISSAGE_G et/ou LISSAGE_THETA.
."""),

    68 : _(u"""
Les mailles de FOND_INF et de FOND_SUP sont de type différent.
  Type de mailles pour FOND_SUP : %(k1)s
  Type de mailles pour FOND_INF : %(k2)s
"""),

    69: _(u"""
Les noeuds %(k1)s de FOND_INF et %(k2)s de FOND_SUP ne sont pas en vis à vis.
-> Risque et Conseil :
Vérifiez que les deux groupes correspondent bien à des noeuds coïncidents
géométriquement et que les groupes de noeuds sont ordonnés dans le même sens.
"""),

    70 : _(u"""
Erreur utilisateur : la lèvre définie sous %(k1)s possède une maille répétée 2 fois :
maille %(k2)s.
-> Risque et Conseil :
Veuillez revoir les données.
"""),


    72 : _(u"""
Le noeud %(k1)s du fond de fissure n'est rattaché à aucune maille surfacique
de la lèvre définie sous %(k2)s.
-> Risque et Conseil :
Veuillez vérifier les groupes de mailles.
"""),

    73 : _(u"""
Erreur utilisateur : la lèvre inférieure et la lèvre supérieure ont une maille
surfacique en commun. Maille en commun : %(k1)s
-> Risque et Conseil :
Revoir les données.
"""),

    74: _(u"""
Le mode %(i1)d n'a pas été trouvé dans la table.
"""),

    75 : _(u"""
Détection d'une maille de type %(k1)s dans la définition des lèvres de la
fissure (%(k2)s).
-> Risque et Conseil :
Les mailles des lèvres doivent être linéiques. Vérifiez que les mailles
définies correspondent bien aux faces des éléments 2D qui s'appuient
sur la lèvre.
"""),

    76: _(u"""
Erreur utilisateur.
Cette combinaison de lissage est interdite avec X-FEM.
"""),

    78: _(u"""
La tangente à l'origine n'est pas orthogonale à la normale :
   Normale aux lèvres de la fissure : %(r1)f %(r2)f %(r3)f
   Tangente à l'origine (= direction de propagation) :  %(r4)f %(r5)f %(r6)f
-> Risque et Conseil :
La tangente à l'origine est nécessairement dans le plan de la fissure,
donc orthogonale à la normale fournie. Vérifier les données.
"""),

    79: _(u"""
La tangente à l'extrémité n'est pas orthogonale à la normale :
   Normale aux lèvres de la fissure : %(r1)f %(r2)f %(r3)f
   Tangente à l'origine (= direction de propagation) :  %(r4)f %(r5)f %(r6)f
-> Risque et Conseil :
La tangente à l'extrémité est nécessairement dans le plan de la fissure,
donc orthogonale à la normale fournie. Vérifier les données.
"""),

    81: _(u"""
Il faut donner la direction de propagation en 2D
La direction par défaut n'existe plus.
-> Risque et Conseil :
Veuillez renseigner le mot-clé DIRECTION.
"""),

    83: _(u"""
Cette combinaison de lissage n'est pas programmée pour l'option : %(k1)s.
-> Risque et Conseil :
Veuillez consulter la documentation U4.82.03 pour déterminer une combinaison de lissage
licite avec l'option désirée.
"""),

    84: _(u"""
Le degré des polynômes de Legendre doit être inférieur au nombre de noeuds
du fond de fissure (ici égal à %(i1)i) lorsque le lissage de G est de type
LEGENDRE et le lissage de THETA de type LAGRANGE.
"""),

    86: _(u"""
Erreur utilisateur.
Cette combinaison de lissage est interdite.
"""),

    88: _(u"""
Aucune fréquence trouvée dans la table pour la fréquence %(r1)f.
"""),

    89: _(u"""
Plusieurs fréquences trouvées dans la table pour la fréquence %(r1)f.
"""),



    90: _(u"""
L'usage des polynômes de Legendre dans le cas d'un fond de fissure clos
est interdit.
-> Risque et Conseil :
Veuillez redéfinir le mot-clé LISSAGE_THETA.
"""),

    91: _(u"""
Aucune direction de propagation n'est fournie par l'utilisateur, la direction est
calculée à partir de la normale au fond de fissure (donnée dans DEFI_FOND_FISS).

-> Risque :
  - La direction calculée est correcte, au signe près. En effet, comme il n'y a
    aucun moyen de vérifier que la direction de propagation est dans le bon sens,
    cela peut inverser le signe du G calculé.

-> Conseils pour ne plus avoir cette alarme :
  - On peut préciser la direction de propagation sous le mot clé DIRECTION.
    Cette solution n'est applicable que si le fond de fissure est rectiligne.
  - La solution la plus générale (donc préférable) est de définir le fond de
    fissure à partir des mailles de lèvres (DEFI_FOND_FISS/LEVRE_SUP et LEVRE_INF).
"""),

    92: _(u"""
Le mot-clef BORNES est obligatoire avec l'option  %(k1)s  !
"""),

    93: _(u"""
Accès impossible au champ SIEF_ELGA pour le numéro d'ordre : %(i1)d.
Or il est nécessaire de connaître SIEF_ELGA car vous avez activé le mot-clé
CALCUL_CONTRAINTE='NON'. Le calcul s'arrête.
-> Conseils :
- Vérifiez vote mise en données pour archiver SIEF_ELGA à tous les instants
demandés dans la commande CALC_G.
- Ou bien supprimer CALCUL_CONTRAINTE='NON' de la commande CALC_G.
"""),

    94: _(u"""
La direction de propagation de la fissure et la normale au fond de fissure
ne sont pas orthogonales.
-> Risque et Conseil :
Si la fissure est plane, la normale et la direction de propagation sont
nécessairement orthogonales, sinon les résultats sont faux: vérifier la
mise en données dans DEFI_FOND_FISS et CALC_G.
Si la fissure n'est pas plane, on ne peut pas utiliser le mot-clé NORMALE
dans DEFI_FOND_FISS: définissez la fissure à partir des mailles de ses lèvres.
"""),

    95: _(u"""
Erreur utilisateur.
Il y a incompatibilité entre le mot-clé FOND_FISS et le modèle %(k1)s associé
à la structure de donnée RESULTAT. En effet, le modèle %(k1)s est un modèle X-FEM.
-> Conseil :
Lorsque le modèle est de type X-FEM il faut obligatoirement utiliser
le mot-clé FISSURE de la commande CALC_G.
"""),

    96: _(u"""
Erreur utilisateur.
Il y a incompatibilité entre le mot-clé FISSURE et le modèle %(k1)s associé
à la structure de donnée RESULTAT. En effet, le modèle %(k1)s est un modèle non X-FEM.
-> Conseil :
Veuillez utiliser les mots-clés FOND_FISS ou THETA ou revoir votre modèle.
"""),

    97: _(u"""
Erreur utilisateur.
Il y a incompatibilité entre le mot-clé FISSURE et le modèle %(k2)s associé
à la structure de donnée RESULTAT. En effet, la fissure %(k1)s n'est pas associée au
modèle %(k2)s.
-> Conseils :
  - Vérifier le mot-clé FISSURE,
  - Vérifier le mot-clé RESULTAT,
  - Vérifier la commande MODI_MODELE_XFEM qui a créé le modèle %(k2)s.
"""),

    99: _(u"""
Point du fond numéro : %(i1)d.
Augmenter NB_NOEUD_COUPE. S'il s'agit d'un noeud extrémité, vérifier les tangentes
(DTAN_ORIG et DTAN_EXTR).
"""),

}
