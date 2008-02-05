#@ MODIF rupture0 Messages  DATE 04/02/2008   AUTEUR GALENNE E.GALENNE 
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

cata_msg={
1: _("""
Interpolation hors du domaine (prolongement constant utilisé).
"""),

2: _("""
Le label %(k1)s doit etre présent dans la table %(k2)s.
"""),

3: _("""
Création de la table  %(k1)s.
"""),

5: _("""
Il faut définir ELAS dans DEFI_MATERIAU.
"""),

6: _("""
La température en fond de fissure, nécessaire pour le calcul des proprietés
matériaux et donc des facteurs d'intensité des contraintes, n'est pas connue.
Le calcul se poursuite en prenant la température de réference du materiau
(TEMP = %(r1)f).
-> Risque et Conseil :
Quand les propriétés matériau dépendent de la température, il faut fournir 
en entrée de POST_K1_K2_K3 le champ de température utilisé pour le calcul 
mécanique, sous le mot clé EVOL_THER.
"""),

9: _("""
Dans le cas d'une SD RESULTAT de type DYNA_TRANS,
le mot-cle EXCIT est obligatoire.
Veuillez le renseigner.
"""),

10: _("""
Modélisation non implantée.
"""),

11: _("""
Problème à la récupération des noeuds du fond de fissure.
-> Risque et Conseil :
Vérifier que le concept %(k1)s indiqué sous le mot clé FOND_FISS a été 
correctement crée par l'opérateur DEFI_FOND_FISS.
"""),

12: _("""
Type de mailles du fond de fissure non défini.
-> Risque et Conseil :
Pour une modélisation 3D, les mailles de votre fond de fissure
doivent etre de type SEG2 ou SEG3.
Veuillez revoir la création de votre fond de fissure 
(opérateur DEFI_FOND_FISS). 
"""),

13: _("""
Le group_no %(k1)s n'est pas dans le maillage.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé GROUP_NO.
"""),

14: _("""
Le noeud  %(k1)s  n'appartient pas au maillage :  %(k2)s
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé SANS_GROUP_NO.
"""),

15: _("""
Le noeud %(k1)s n'appartient pas au fond de fissure.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé GROUP_NO ou NOEUD.
"""),

16: _("""
Le mot clé RESULTAT est obligatoire pour TYPE_MAILLAGE = LIBRE.
"""),

17: _("""
Le nombre de noeuds NB_NOEUD_COUPE doit etre supérieur à 3.
-> Risque et Conseil :
Le calcul s'est poursuivi avec la valeur par défaut (=5, cf doc U4.82.05)

"""),

18: _("""
Problème à la récupération du modèle dans la sd résultat fournie.
-> Risque et Conseil :
Veuillez vérifier que le concept fourni au mot-clé RESULTAT correspond
au résultat à considérer.
"""),

19: _("""
Problème à la récupération des noeuds de la lèvre sup : 
-> Risque et Conseil :
Pour un calcul avec POST_K1_K2_K3, la lèvre supérieure de la fissure doit 
être obligatoirement définie dans DEFI_FOND_FISS à l'aide du mot-clé
LEVRE_SUP. Vérifier la définition du fond de fissure.
"""),

20: _("""
Problème à la récupération des noeuds de la lèvre inf : 
-> Risque et Conseil :
Pour un calcul avec POST_K1_K2_K3, la lèvre inférieure de la fissure doit
être obligatoirement définie dans DEFI_FOND_FISS à l'aide du mot-clé
LEVRE_INF. Vérifier la définition du fond de fissure.
"""),

21: _("""
Les noeuds ne sont pas en vis-à-vis dans le plan perpendiculaire
au noeud %(k1)s.
-> Risque et Conseil :
Pour interpoler les sauts de déplacement, les noeuds doivent être par défaut
en vis-à-vis deux à deux sur les lèvres. Si ce n'est pas le cas, utilisez
l'option TYPE_MAILLE='LIBRE' dans POST_K1_K2_K3.
"""),

22: _("""
Il manque des points dans le plan défini par la lèvre
supérieure et perpendiculaire au fond %(k1)s.
-> Risque et Conseil :
"""),

23: _("""
Vérifier les tangentes extremités ou
"""),

24: _("""
Augmenter PREC_NORM dans DEFI_FOND_FISS.
"""),

25: _("""
Augmenter ABSC_CURV_MAXI.
"""),

26: _("""
Il manque des points dans le plan défini par la lèvre
inférieure et perpendiculaire au fond  %(k1)s.
-> Risque et Conseil :
"""),

27: _("""
Pour un résultat de type MODE_MECA,
l'option de calcul doit etre K_G_MODA.
-> Risque et Conseil :
Veuillez fournir au mot-clé OPTION l'option K_G_MODA
et vérifier que le concept fourni au mot-clé RESULTAT
est de type MODE_MECA.
"""),

28: _("""
Le cas de charge %(k1)s n'a pas été trouvé dans la SD Résultat %(k2)s.
-> Risque et Conseil :
Veuillez vérifier les données fournies au mot-clé NOM_CAS.
"""),

29: _("""
Le mot-clé 'FISSURE' est obligatoire avec l'option  %(k1)s.
Veuillez le renseigner.
"""),

30: _("""
Calcul possible pour aucun noeud du fond.
-> Risque et Conseil :
Veuillez vérifier les données, notamment celles du mot-clé DIRECTION.
"""),

32: _("""
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

33: _("""
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


35: _("""
Les mots-clés TABL_DEPL_SUP et TABL_DEPL_INF sont obligatoires
si SYME_CHAR=SANS.
"""),

37: _("""
Le numéro d'ordre %(i1)d n'a pas été trouvé dans la table.
"""),

38: _("""
Pas d'instant trouvé dans la table pour l'instant %(r1)f.
"""),

39: _("""
Plusieurs instants trouvés dans la table pour l'instant %(r1)f.
"""),

40: _("""
ABSC_CURV non croissants pour %(k1)s.
"""),

42: _("""
Différence de points entre la lèvre supérieure et la lèvre inférieure.
"""),

43: _("""
Pour traiter le noeud %(k1)s:
 Nombre de points - lèvre supérieure : %(i1)d
 Nombre de points - lèvre inférieure : %(i2)d
"""),

44: _("""
Les noeuds ne sont pas en vis-à-vis.
-> Risque et Conseil :
Pour interpoler les sauts de déplacement, les noeuds doivent être par défaut en 
vis-à-vis deux à deux sur les lèvres. Si ce n'est pas le cas, utilisez l'option 
TYPE_MAILLE='LIBRE' dans POST_K1_K2_K3.

"""),

46: _("""
Il faut au moins trois noeuds dans le plan défini par les lèvres
et perpendiculaire au fond de fissure.
-> Risque et Conseil :
"""),

47: _("""
Noeud %(k1)s 
"""),

48: _("""
Le mot-clé 'FOND_FISS' est obligatoire avec l'option  %(k1)s.
Veuillez le renseigner.
"""),

49: _("""
Déplacement normal du noeud %(k1)s non nul
et SYME_CHAR = %(k2)s.
-> Risque et Conseil :
Vérifier les conditions aux limites et VECT_K1.
"""),

50: _("""
Nombre de modes différent entre la base modale
et %(k1)s : on prend le minimum des deux %(i1)d.
"""),

51: _("""
Le numéro d'ordre %(i1)d n'appartient pas au résultat %(k1)s.
"""),

54: _("""
Aucun instant ou numéro d'ordre trouvé.
"""),

55: _("""
-> Attention: En présence d'une SD Résultat de type mult_elas, les mots-clés
EXCIT et NOM_CAS sont obligatoires.
-> Risque et Conseil :
Risque de résultats faux si un des chargements impacte le calcul de G et de K
(par exemple force de pression sur les lèvres de la fissure,force volumique...)
"""),

56 : _("""
CALC_G - option CALC_K_G : le calcul est impossible sur un point de rayon nul
(point sur l'axe de rotation).
-> Risque et Conseil :
Modifier les couronnes R_INF et R_SUP pour qu'elles soient toutes les deux plus
petites que le rayon du fond de fissure. De manière générale en axisymétrie, le
calcul de K est d'autant plus précis que le rayon des couronnes est petit devant
le rayon du fond de fissure.
"""),

57 : _("""
Pour l'option CALC_G en 3D, le champ THETA doit être calculé directement
dans l'opérateur CALC_G.
-> Risque et Conseil :
Dans le mot-clé facteur THETA, supprimez le mot-clé THETA et renseignez les 
mots-clés FOND_FISS, R_SUP, R_INF, MODULE, et DIRECTION pour la détermination
automatique du champ theta.
"""),

58 : _("""
Pour l'option  %(k1)s (3d local), utiliser le mot-clé THETA_LAGR.
"""),

59 : _("""
Le champ de THETA est inexistant dans la structure de données  %(k1)s
de type THETA_GEOM.
-> Risque et Conseil :
Veuillez revoir la création du champ theta (opérateur CALC_THETA).
"""),

60 : _("""
Mélange de mailles de type SEG2 et SEG3 dans la définition du fond de fissure.
-> Risque et Conseil :
Les mailles du fond de fissure doivent toutes être du meme type. 
Modifiez le maillage ou définissez plusieurs fonds de fissure consécutifs.
"""),

61 : _("""
Le groupe de noeuds  %(k1)s définissant le fond de fissure n'est pas ordonné.
-> Risque et Conseil :
Il faut ordonner les noeuds du fond de fissure. 
Les options SEGM_DROI_ORDO et NOEUD_ORDO de l'opérateur 
DEFI_GROUP/CREA_GROUP_NO peuvent etre utilisées.
."""),

62 : _("""
Arret sur erreur utilisateur : deux GROUP_NO consécutifs incohérents dans la 
définition du fond de fissure.
-> Risque et Conseil :
Les noeuds de chaque groupe doivent etre ordonnés et le dernier noeud d'un
groupe identique au premier noeud du groupe suivant dans la liste.
"""),

63 : _("""
Les mailles du fond de fissure doivent etre du type segment (SEG2 ou SEG3).
"""),

64 : _("""
Arret sur erreur utilisateur : deux mailles ou groupes de mailles du fond de
fissure sont non consécutives dans la numérotation des noeuds.
"""),

65 : _("""
Détection d'une maille de type %(k1)s dans la définition des lèvres de la
fissure (%(k2)s).
-> Risque et Conseil :
Les mailles des lèvres doivent etre du type quadrangle ou triangle. 
Vérifiez que les mailles définies correspondent bien aux faces des éléments
3D qui s'appuient sur la lèvre.
"""),

66 : _("""
La liste de noeuds définissant le fond de fissure n'est pas ordonnée. 
-> Risque et Conseil :
Veuillez vérifier l'ordre des noeuds.
"""),

67 : _("""
Arret sur erreur utilisateur : le fond de fissure possède un noeud
répété deux fois (noeud  %(k1)s). 
-> Risque et Conseil :
Veuillez revoir la définition du fond dans FOND_FISS ou FOND_FERME.
"""),

68 : _("""
Les mailles de FOND_INF et de FOND_SUP sont de type différent.
  Type de mailles pour FOND_SUP : %(k1)s
  Type de mailles pour FOND_INF : %(k2)s
"""),

69: _("""
Les noeuds %(k1)s de FOND_INF et %(k2)s de FOND_SUP ne sont pas en vis à vis. 
-> Risque et Conseil :
Vérifiez que les deux groupes correspondent bien à des noeuds coincidents
géométriquement et que les groupes de noeuds sont ordonnés dans le meme sens. 
"""), 

70 : _("""
Erreur utilisateur : la lèvre supérieure possède une maille répétée 2 fois : 
maille  %(k1)s. 
-> Risque et Conseil :
Veuillez revoir les données.
"""),

71 : _("""
Erreur utilisateur : la lèvre inférieure possède une maille répétée 2 fois : 
maille  %(k1)s. 
-> Risque et Conseil :
Veuillez revoir les données.
"""),

72 : _("""
Le noeud %(k1)s du fond de fissure n'est rattaché à aucune maille surfacique
de la lèvre supérieure.
-> Risque et Conseil :
Veuillez vérifier les groupes de mailles.
"""), 

73 : _("""
Erreur utilisateur : la lèvre inférieure et la lèvre supérieure ont une maille
surfacique en commun. Maille en commun : %(k1)s
-> Risque et Conseil :
Revoir les données.
"""),

74: _("""
Le noeud %(k1)s du fond de fissure n'est rattaché à aucune maille
surfacique de la lèvre inférieure.
-> Risque et Conseil :
Veuillez vérifier les groupes de mailles.
"""), 

75 : _("""
Détection d'une maille de type %(k1)s dans la définition des lèvres de la
fissure (%(k2)s).
-> Risque et Conseil :
Les mailles des lèvres doivent etre linéiques. Vérifiez que les mailles 
définies correspondent bien aux faces des éléments 2D qui s'appuient
sur la lèvre.
"""),

76: _("""
Le noeud %(k1)s du fond de fissure n'appartient à aucune des mailles
de la lèvre supérieure. 
-> Risque et Conseil :
Veuillez revoir les données.
"""), 

77: _("""
Le noeud %(k1)s du fond de fissure n'appartient à aucune des mailles
de la lèvre inférieure. 
-> Risque et Conseil :
Veuillez revoir les données.
"""), 

78: _("""
La tangente à l'origine n'est pas orthogonale à la normale :
   Normale aux lèvres de la fissure : %(r1)f %(r2)f %(r3)f
   Tangente à l'origine (= direction de propagation) :  %(r4)f %(r5)f %(r6)f
-> Risque et Conseil :
La tangente à l'origine est nécessairement dans le plan de la fissure, 
donc orthogonale à la normale fournie. Vérifier les données.
"""), 

79: _("""
La tangente à l'extrémité n'est pas orthogonale à la normale :
   Normale aux lèvres de la fissure : %(r1)f %(r2)f %(r3)f
   Tangente à l'origine (= direction de propagation) :  %(r4)f %(r5)f %(r6)f
-> Risque et Conseil :
La tangente à l'extrémité est nécessairement dans le plan de la fissure, 
donc orthogonale à la normale fournie. Vérifier les données.
"""), 

81: _("""
Il faut donner la direction de propagation en 2D
La direction par défaut n'existe plus.
-> Risque et Conseil :
Veuillez renseigner le mot-clé DIRECTION.
"""), 

82: _("""
Le mot-clé PROPAGATION est utilisé seulement pour le calcul de G avec
propagation lagrangienne.
-> Risque et Conseil :
Veuillez renseigner l'option G_LAGR ou G_LAGR_GLOB.
"""), 

83: _("""
Cette combinaison de lissage n'est pas programmée pour l'option : %(k1)s.
-> Risque et Conseil :
Veuillez consulter la doc U4.82.03 pour déterminer une combinaison de lissage
licite avec l'option désirée.
"""), 

84: _("""
Le degré des polynomes de Legendre doit etre inférieur ou égal au nombre
de noeuds du fond de fissure lorsque le lissage de G est de type
LEGENDRE et le lissage de THETA de type LAGRANGE.
"""), 

85: _("""
Le lissage de G doit etre de type LEGENDRE si le lissage de THETA
est de type LEGENDRE.
-> Risque et Conseil :
Veuillez redéfinir le mot-clé LISSAGE_G.
"""), 

86: _("""
L'option %(k1)s n'est pas permise avec le lissage LAGRANGE_REGU.
"""), 

87: _("""
Si la méthode LAGRANGE_REGU est utilisée pour le lissage, 
alors le lissage de G et de theta doivent etre de type LAGRANGE_REGU.
"""),

88: _("""
Le champ theta est obligatoire avec l'option %(k1)s.
-> Risque et Conseil :
Veuillez utiliser le mot-clé THETA_LAGR pour renseigner un champ theta.
"""), 

89: _("""
Le fond de fissure est obligatoire avec l'option CALC_K_G.
-> Risque et Conseil :
Veuillez renseigner le mot-clé FOND_FISS.
"""), 

90: _("""
L'usage des polynomes de Legendre dans le cas d'un fond de fissure clos
est interdit.
-> Risque et Conseil :
Veuillez redéfinir le mot-clé LISSAGE_THETA.
"""), 

92: _("""
Le mot-clef BORNES est obligatoire avec l'option  %(k1)s  !
"""), 

93: _("""
Accès impossible au champ : %(k1)s pour le numéro d'ordre : %(i1)d
"""), 

95: _("""
Accès impossible au mode propre champ : %(k1)s pour le numéro d'ordre : %(i1)d.
"""), 

96: _("""
Option non disponible actuellement.
"""), 

98: _("""
Récupération impossible de la normale dans le fond de fissure FOND_FISS.
-> Risque et Conseil :
Un problème a du se produire lors de la création de la structure de données 
FOND_FISS. Vérifiez les données dans DEFI_FOND_FISS.
"""), 
 
}
