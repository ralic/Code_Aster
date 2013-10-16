# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

4 : _(u"""
  -> Le modèle contient un mélange d'éléments finis 2D (plan Oxy) et 3D

  -> Risque & Conseil :
     Sur ce genre de modèle, on ne sait pas déterminer s'il est 2D ou 3D.
     Certains chargements ne seront pas possibles.
"""),

5 : _(u"""
 Le chargement de type %(k1)s est interdit en 2D.
"""),

6: _(u"""
Erreur d'utilisation :
 Le modèle contient un mélange d'éléments 2D (vivant dans le plan Oxy) et 3D.
 Le code n'a pas prévu ce cas de figure dans l'application du chargement demandé.

Risques et conseils :
 Il faut peut être émettre une demande d'évolution pour pouvoir traiter ce problème.
"""),

7 : _(u"""
Le modèle est de dimension %(i1)d . ARETE_IMPO s'applique sur des arêtes d'éléments 3D,
donc un modèle de dimension 3. Pour les arêtes d'éléments 2D utiliser FACE_IMPO.
"""),

8 : _(u"""
Pour un chargement de type LIAISON_COQUE, il faut avoir autant de noeuds dans les deux listes.
"""),

9 : _(u"""
Échec de l'appariement des deux listes de noeuds pour le chargement de type LIAISON_COQUE.

 Conseils :
   - Si la distance entre les deux surfaces à apparier est grande devant leurs dimensions, précisez l'isométrie qui permet de les superposer par l'intermédiaire des mots-clés CENTRE, ANGL_NAUT et TRAN.
"""),

10: _(u"""
Pour le chargement de type LIAISON_CHAMNO, on doit utiliser le mot clé CHAM_NO pour donner le CHAM_NO dont les composantes seront les coefficients de la relation linéaire.
"""),

11: _(u"""
Pour le chargement de type LIAISON_CHAMNO, il faut que le CHAM_NO dont les termes servent de coefficients à la relation linéaire à écrire ait été défini.
"""),

12: _(u"""
Pour le chargement de type LIAISON_CHAMNO, tous les coefficients donnés par le mot-clef CHAM_NO sont nuls.
"""),

17: _(u"""
 Pour le chargement courant, la liste des noeuds donnée est réduite à un seul terme et l'on ne fait aucun traitement.
"""),

23: _(u"""
 Il est impossible de calculer la tangente de la maille %(k1)s, des noeuds doivent être confondus.
"""),

24: _(u"""
 Il est impossible de calculer la normale de la maille %(k1)s, des noeuds doivent être confondus.
"""),

25: _(u"""
 Il n'est pas possible de calculer la normale d'un segment en 3d.
 Il ne doit pas y avoir de segment dans le groupe sur lequel vous appliquez la condition limite.
"""),

26: _(u"""
 Il est impossible de calculer la normale de la maille %(k1)s .
 Il y a un problème dans la définition de vos mailles: des arêtes doivent être confondues.
"""),

29: _(u"""
 L'angle formé par le vecteur normal courant à une face et le vecteur normal moyen, au noeud %(k1)s, est supérieur a 10 degrés et vaut %(k2)s degrés.
"""),

30: _(u"""
Erreur d'utilisation :
 La norme du vecteur normal (moyenne des normales des éléments concourants) est presque nulle.
 Les facettes concourantes au noeud  %(k1)s ne définissent pas une normale fiable.
 Il y a un problème dans la définition des mailles de bord .

Suggestion :
 Pensez à réorienter les mailles de bord avec l'opérateur MODI_MAILLAGE.
"""),

31 : _(u"""
 Erreur utilisateur:
    On cherche à imposer une condition aux limites sur le ddl %(k1)s du noeud %(k2)s.
    Mais ce noeud ne porte pas ce ddl.

    Conseils :
     - vérifiez le modèle et les conditions aux limites :
        - le noeud incriminé fait-il partie du modèle ?
        - le noeud porte-t-il le ddl que l'on cherche à contraindre ?
"""),

32 : _(u"""
 Il y a un problème sur une relation linéaire car les coefficients sont trop petits.
 Ce problème survient dans le cas de liaisons linéaires automatiques. Vérifiez les éventuelles
 alarmes émises précédemment dans des mots clefs comme LIAISON_MAILLE par exemple.
"""),

33: _(u"""
 Le noeud <%(k1)s> ne fait pas partie du modèle.
"""),

34: _(u"""
 Les relations suivantes sont redondantes et donc supprimées en appliquant le principe de
surcharge.
"""),

35: _(u"""
 Liste des noeuds en vis-à-vis de l'occurrence %(i1)d de LIAISON_GROUP :"""),

36: _(u"""    <%(k1)s> en  vis-à-vis de <%(k2)s>"""),

37 : _(u"""
 Le noeud de nom <%(k1)s> n'est connecté à aucune maille. Il est donc impossible de définir
le repère local pour appliquer DDL_POUTRE.
 Revoyez la définition de votre repère local.
"""),

38 : _(u"""
 Le noeud de nom <%(k1)s> est connecté à plus d'une maille. Il est donc impossible de définir
le repère local pour appliquer DDL_POUTRE.
 Revoyez la définition de votre repère local.
"""),

39 : _(u"""
 Le repère local que vous avez défini ne contient pas de maille attachée au noeud <%(k1)s>.
Il est donc impossible de définir le repère local pour appliquer DDL_POUTRE.
 Revoyez la définition de votre repère local.
"""),

40 : _(u"""
 La maille <%(k1)s>, attaché au noeud <%(k2)s> n'est pas de type "SEG".
Il est donc impossible de définir le repère local pour appliquer DDL_POUTRE.
"""),

41 : _(u"""
 La maille <%(k1)s>, attaché au noeud <%(k2)s> est de longueur nulle.
Il est donc impossible de définir le repère local pour appliquer DDL_POUTRE.
"""),

42: _(u"""
 Pour définir le mot-clef %(k1)s, on doit utiliser %(i1)d valeurs car nous sommes dans le
cas d'un modèle à %(i2)d dimensions.
"""),






45: _(u"""
Erreur utilisateur :
  Vous voulez contraindre le ddl %(k1)s sur un ensemble de noeuds,
  Mais ce ddl n'existe sur aucun de ces noeuds.
"""),

46 : _(u"""
 Lors de l'écriture de la relation linéaire rigidifiant la structure (LIAISON_SOLIDE ou RELA_CINE_BP),
 on a un problème dans le cas 3d où les noeuds sont alignés car deux noeuds sont confondus.
"""),

47 : _(u"""
 Lors de l'écriture de la relation linéaire rigidifiant la structure (LIAISON_SOLIDE ou RELA_CINE_BP),
 on a un problème dans le cas 3d où les noeuds forment un triangle.
 Le système résultant n'est pas inversible. Le triangle est trop distordu, revoyez votre maillage.
"""),

48: _(u"""
 Le concept CABLE_BP de nom %(k1)s ne contient pas de relations linéaires. L'option RELA_CINE est donc inutilisable.
"""),

49: _(u"""
 Le concept CABLE_BP de nom %(k1)s ne contient pas de contraintes. L'option SIGM_BPEL est donc inutilisable.
"""),

50: _(u"""
 On ne trouve pas de noeud assez près du noeud %(k1)s .
"""),

51 : _(u"""
 Il y a un conflit dans les vis-à-vis des noeuds.
 Le noeud  %(k1)s est à la fois le vis-à-vis du noeud %(k2)s et du noeud %(k3)s.
"""),

52 : _(u"""
 Il y a un conflit dans les vis-à-vis des noeuds.
 Le noeud  %(k1)s n'est l'image d'aucun noeud par la correspondance inverse.
"""),

53: _(u"""
 Vous avez donné une direction nulle pour l'axe de rotation.
"""),

61: _(u"""
 Le type d'onde S est interdit en 3D pour le chargement ONDE_PLANE, précisez SV ou SH.
"""),

62: _(u"""
 Les types d'onde SV et SH sont interdits en 2D pour le chargement ONDE_PLANE, on a pris le type S.
"""),

63: _(u"""
 Vous ne pouvez pas bloquer le déplacement tangent sur des faces d'éléments 3D.
 Utiliser DDL_IMPO ou LIAISON_DDL.
"""),

64 : _(u"""
  Vous définissez une charge avec %(k1)s sur un modèle de type %(k2)s, ce n'est pas possible.
"""),

65 : _(u"""
Le vecteur définissant l'axe de rotation a une composante non nulle suivant Ox ou Oz,
ce qui induit un chargement non axisymétrique. Avec une modélisation AXIS ou AXIS_FOURIER,
l'axe de rotation doit être dirigé suivant Oy.
"""),

66 : _(u"""
Les coordonnées du centre de rotation ont au moins une composante non nulle, ce qui induit
un chargement non axisymétrique. Avec une modélisation AXIS ou AXIS_FOURIER,
le centre de rotation doit être confondu avec l'origine.
"""),

67 : _(u"""
Le vecteur définissant l'axe de rotation a une composante non nulle suivant Ox ou Oy,
ce qui induit des forces centrifuges hors plan. Avec une modélisation C_PLAN ou D_PLAN,
l'axe de rotation doit être dirigé suivant Oz.
"""),

82: _(u"""
 Il faut au moins deux noeuds pour LIAISON_UNIF.
"""),

86: _(u"""
 La maille de nom %(k1)s n'est pas de type linéique (segments).
 Elle ne sera pas affectée par %(k2)s .
"""),

87: _(u"""
 La maille de nom %(k1)s n'est pas de type surfacique (triangles ou quadrangles).
 Elle ne sera pas affectée par %(k2)s .
"""),

88: _(u"""
 La maille de nom %(k1)s n'est pas de type volumique.
 Elle ne sera pas affectée par %(k2)s .
"""),

89: _(u"""
  -> Erreur dans les mailles du mot-clé facteur %(k1)s.
     Aucune maille n'est du bon type. Elles sont toutes ignorées.
"""),

97 : _(u"""
 Tous les coefficients de la relation linéaire sont strictement nuls.
 Cette erreur peut survenir si le maillage est incorrect (par exemple des noeuds confondus) ou si
vous affectez des coefficients nuls.
"""),

99 : _(u"""
Problème :
  Une relation linéaire entre ddls a un second membre de type "fonction".
  On ne peut pas la normaliser (afin que son plus grand coefficient soit 1.) car on ne
  sait pas "diviser" une fonction par un réel.

  Le plus grand coefficient de la relation est très différent de 1.  (<1.d-3 ou > 1.d3).
  Cette équation (non normalisée) peut conduire à des difficultés numériques lors de
  la résolution des systèmes linéaires.

Conseil :
  Utilisez le solveur MUMPS afin de contrôler la qualité de la résolutions des systèmes linéaires.
"""),

}
