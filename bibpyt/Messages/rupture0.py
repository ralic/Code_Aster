#@ MODIF rupture0 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg={
1: _(u"""
La valeur de Kj critique demandée n'est pas atteinte dans l'étude demandée;
il n'est donc pas possible d'identifier le Gp critique dans cette étude.
-> Risque et Conseil :
Augmentez le chargement.
"""),

2: _(u"""
Le label %(k1)s doit etre présent dans la table %(k2)s.
"""),

3: _(u"""
Création de la table  %(k1)s.
"""),

4: _(u"""
Erreur utilisateur :
Incohérence entre le mot-clé FISSURE et le modèle associé au mot-clé RESULTAT.
- Pour utiliser une fissure maillée, renseignez sous le mot-clé FOND_FISS
une fissure provenant de la commande DEFI_FOND_FISS.
- Pour utiliser une fissure non-maillée (calcul X-FEM), renseignez sous le mot-clé FISSURE
une fissure provenant de la commande DEFI_FISS_XFEM. Le modèle associé au mot-clé RESULTAT 
doit être un modèle X-FEM provenant de la commande MODI_MODELE_XFEM.
"""),


5: _(u"""
Il faut définir ELAS dans DEFI_MATERIAU.
"""),

6: _(u"""
La température en fond de fissure, nécessaire pour le calcul des proprietés
matériaux et donc des facteurs d'intensité des contraintes, n'est pas connue.
Le calcul se poursuite en prenant la température de réference du materiau
(TEMP = %(r1)f).
-> Risque et Conseil :
Quand les propriétés matériau dépendent de la température, il faut fournir 
en entrée de POST_K1_K2_K3 le champ de température utilisé pour le calcul 
mécanique, sous le mot clé EVOL_THER.
"""),

7: _(u"""
L'entité %(k1)s renseignée au mot-clé %(k2)s sous %(k3)s n'est pas dans le maillage.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé %(k2)s.
"""),

8: _(u"""
Problème dans la création de la base locale au fond de fissure.
Il est impossible de déterminer le sens de la direction de propagation (vecteur tangent aux lèvres).
Dans le cas symétrique (SYME='OUI') il faut : 
- soit donner les lèvres de la fissure (LEVRE_SUP),
- soit indiquer le vecteut tangent au point origine du fond de fissure (DTAN_ORIG).
"""),

9: _(u"""
Dans le cas d'une SD RESULTAT de type DYNA_TRANS,
le mot-cle EXCIT est obligatoire.
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
doivent etre de type SEG2 ou SEG3.
Veuillez revoir la création de votre fond de fissure 
(opérateur DEFI_FOND_FISS). 
"""),

13: _(u"""
Le group_no %(k1)s n'est pas dans le maillage.
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
Le mot clé RESULTAT est obligatoire pour TYPE_MAILLAGE = LIBRE.
"""),

18: _(u"""
Problème à la récupération du modèle dans la sd résultat fournie.
-> Risque et Conseil :
Veuillez vérifier que le concept fourni au mot-clé RESULTAT correspond
au résultat à considérer.
"""),

19: _(u"""
Problème à la récupération des noeuds de la lèvre sup : 
-> Risque et Conseil :
Pour un calcul avec POST_K1_K2_K3, la lèvre supérieure de la fissure doit 
être obligatoirement définie dans DEFI_FOND_FISS à l'aide du mot-clé
LEVRE_SUP. Vérifier la définition du fond de fissure.
"""),

20: _(u"""
Problème à la récupération des noeuds de la lèvre inf : 
-> Risque et Conseil :
Pour un calcul avec POST_K1_K2_K3, la lèvre inférieure de la fissure doit
être obligatoirement définie dans DEFI_FOND_FISS à l'aide du mot-clé
LEVRE_INF. 
Si seule la lèvre supérieure est maillée, ne pas oublier de spécifier
SYME_CHAR = 'SYME' dans POST_K1_K2_K3. 
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
Vérifier les tangentes extremités ou
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
Pour un résultat de type MODE_MECA,
l'option de calcul doit etre K_G_MODA.
-> Risque et Conseil :
Veuillez fournir au mot-clé OPTION l'option K_G_MODA
et vérifier que le concept fourni au mot-clé RESULTAT
est de type MODE_MECA.
"""),

28: _(u"""
Le cas de charge %(k1)s n'a pas été trouvé dans la SD Résultat %(k2)s.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé NOM_CAS.
"""),

29: _(u"""
Le mot-clé 'FISSURE' est obligatoire avec l'option  %(k1)s.
Veuillez le renseigner.
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
Différence entre la normale au plan déduite de VECT_K1 et la normale 
au plan de la fissure calculée pour le noeud %(i1)d :
  VECT_K1 : (%(r4)f,%(r5)f,%(r6)f)
  Vecteur normal calculé à partir de la définition de la fissure : (%(r1)f,%(r2)f,%(r3)f)
-> Risque et Conseil :
On poursuit le calcul mais si l'écart entre les deux vecteurs est trop important, 
le calcul risque d'échouer ou de conduire à des résultats peu précis.
Vérifier absolument le VECT_K1 fourni ou supprimer ce mot clé pour que la normale
au plan soit calculée automatiquement.
"""),

33: _(u"""
Problème dans la récupération du saut de déplacement sur les lèvres.
-> Risque et Conseil :
Il y a plusieurs causes possibles :
- vérifiez que le résultat correspond bien à un calcul sur des éléments x-fem;
- si le calcul correspond à un calcul X-FEM avec contact sur les lèvres de la
  fissure,vérifiez que le maillage fourni est bien le maillage linéaire initial;
- vérifiez que le paramètre ABSC_CURV_MAXI est cohérent avec la taille de la
  fissure : les segments pour l'interpolation du déplacement des lèvres, 
  perpendiculaires au fond de fissure et de longueur ABSC_CURV_MAXI, ne doivent
  pas "sortir" de la matière.
"""),

34: _(u"""
L'hypothèse de lèvres collées n'est pas valide.
Conseil : il faut utiliser CONFIG_INIT='DECOLLEE'.
Ou bien vérifier l'utilisation du mot-clé SYME.
"""),

35: _(u"""
Attention, le vecteur tangent au 1er noeud du fond de fissure (DTAN_ORIG) est dans le sens
opposé à celui calculé automatiquement (%(r1)f %(r2)f %(r3)f).
Cela est probablement une erreur, qui peut conduire à des résultats faux.
-> Risque et Conseil :
  - vérifiez DTAN_ORIG,
  - ou bien ne le renseignez pas.
"""),

36: _(u"""
Attention, le vecteur tangent au denier noeud du fond de fissure (DTAN_EXTR) est dans le sens
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

40: _(u"""
ABSC_CURV non croissants pour %(k1)s.
"""),

41 : _(u"""
Le groupe de mailles %(k1)s défini sous le mot-clé GROUP_MA n'existe pas.
"""),

42 : _(u"""
Dans le cas où le fond est une courbe fermée, les mot-clés MAILLE_ORIG ou GROUP_MA_ORIG doivent accompagner le mot-clé NOEUD_ORIG ou GROUP_NO_ORIG.
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
au fond de fissure. Le calcul est impossible : on met la ligne correspondant au noeud
considéré à zéro et on poursuit le calcul pour le noeud du fond suivant.
-> Risque et Conseil :
"""),

47: _(u"""
Noeud %(k1)s 
"""),

48: _(u"""
Le mot-clé 'FOND_FISS' est obligatoire avec l'option  %(k1)s.
Veuillez le renseigner.
"""),

49: _(u"""
Déplacement normal du noeud %(k1)s non nul
et SYME_CHAR = %(k2)s.
-> Risque et Conseil :
Vérifier les conditions aux limites et VECT_K1.
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
ou utilisez l'otion 'EVOL_THER' de POST_K1_K2_K3.
"""),

53: _(u"""
Vous avez utilisé un module d'Young nul. Le post-traitement ne peut pas se poursuivre."""),

54: _(u"""
Aucun instant ou numéro d'ordre trouvé.
"""),

55: _(u"""
-> Attention: En présence d'une SD Résultat de type mult_elas, les mots-clés
EXCIT et NOM_CAS sont obligatoires.
-> Risque et Conseil :
Risque de résultats faux si un des chargements impacte le calcul de G et de K
(par exemple force de pression sur les lèvres de la fissure,force volumique...)
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
automatique du champ theta.
"""),

59 : _(u"""
Le champ de THETA est inexistant dans la structure de données  %(k1)s
de type THETA_GEOM.
-> Risque et Conseil :
Veuillez revoir la création du champ theta (opérateur CALC_THETA).
"""),

60 : _(u"""
Mélange de mailles de type SEG2 et SEG3 dans la définition du fond de fissure.
-> Risque et Conseil :
Les mailles du fond de fissure doivent toutes être du meme type. 
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
DEFI_GROUP/CREA_GROUP_NO peuvent etre utilisées.
."""),


68 : _(u"""
Les mailles de FOND_INF et de FOND_SUP sont de type différent.
  Type de mailles pour FOND_SUP : %(k1)s
  Type de mailles pour FOND_INF : %(k2)s
"""),

69: _(u"""
Les noeuds %(k1)s de FOND_INF et %(k2)s de FOND_SUP ne sont pas en vis à vis. 
-> Risque et Conseil :
Vérifiez que les deux groupes correspondent bien à des noeuds coincidents
géométriquement et que les groupes de noeuds sont ordonnés dans le meme sens. 
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

 

75 : _(u"""
Détection d'une maille de type %(k1)s dans la définition des lèvres de la
fissure (%(k2)s).
-> Risque et Conseil :
Les mailles des lèvres doivent etre linéiques. Vérifiez que les mailles 
définies correspondent bien aux faces des éléments 2D qui s'appuient
sur la lèvre.
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

80: _(u"""
Il ne faut donner la direction de propagation si le champ thêta est donné.

-> Conseil :
Veuillez supprimer le mot-clé DIRECTION sous CALC_G/THETA.
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
Veuillez consulter la doc U4.82.03 pour déterminer une combinaison de lissage
licite avec l'option désirée.
"""), 

84: _(u"""
Le degré des polynomes de Legendre doit etre inférieur au nombre de noeuds
du fond de fissure (ici égal à %(i1)i) lorsque le lissage de G est de type
LEGENDRE et le lissage de THETA de type LAGRANGE.
"""), 

85: _(u"""
Le lissage de G doit etre de type LEGENDRE si le lissage de THETA
est de type LEGENDRE.
-> Risque et Conseil :
Veuillez redéfinir le mot-clé LISSAGE_G.
"""), 

87: _(u"""
Si la méthode LAGRANGE_REGU est utilisée pour le lissage, 
alors le lissage de G et de theta doivent etre de type LAGRANGE_REGU.
"""),


90: _(u"""
L'usage des polynomes de Legendre dans le cas d'un fond de fissure clos
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
Accès impossible au champ : %(k1)s pour le numéro d'ordre : %(i1)d
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
Accès impossible au mode propre champ : %(k1)s pour le numéro d'ordre : %(i1)d.
"""), 

96: _(u"""
Option non disponible actuellement.
"""), 

99: _(u"""
Point du fond numéro : %(i1)s.
Augmenter NB_NOEUD_COUPE. S'il s'agit d'un noeud extrémité, vérifier les tangentes 
(DTAN_ORIG et DTAN_EXTR).
"""), 

}
