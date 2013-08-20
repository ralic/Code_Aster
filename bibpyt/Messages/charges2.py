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
 Aucun noeud affecté ne connaît le DDL de nom <%(k1)s>
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

63: _(u"""
 Vous ne pouvez pas bloquer le déplacement tangent sur des faces d'éléments 3D.
 Utiliser DDL_IMPO ou LIAISON_DDL.
"""),

82: _(u"""
 Il faut au moins deux noeuds pour utiliser LIAISON_UNIF.
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
