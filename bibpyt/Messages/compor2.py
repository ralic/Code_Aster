# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
   SIMU_POINT_MAT : Le type de DEFORMATION choisi,  <%(k1)s>, est actuellement incompatible avec SUPPORT=POINT.
    On utilise donc SUPPORT=ELEMENT.
"""),

    2 : _(u"""
   SIMU_POINT_MAT : Erreur, on ne peut avoir à la fois SIGM et EPSI imposés sur la composante <%(k1)s>
"""),

    3 : _(u"""
   SIMU_POINT_MAT : Erreur, on doit avoir une seule composante donnée parmi  <%(k1)s>
"""),

    4 : _(u"""
   SIMU_POINT_MAT : Problème a l'inversion de la matrice jacobienne.
   On tente de subdiviser le pas de temps
"""),

    5 : _(u"""
   SIMU_POINT_MAT : nombre d'itérations maximum atteint.
   On tente de subdiviser le pas de temps
"""),

    6 : _(u"""
   POLYCRISTAL : nombre de phases trop grand (le nombre maximum de phases vaut actuellement 1000).
   Faire une demande d'évolution pour lever cette limitation si nécessaire.
"""),

    7 : _(u"""
   On ne peut pas utiliser les KIT_THM dans SIMU_POINT_MAT.
"""),

    8 : _(u"""
   DEFI_COMPOR : la somme des fractions volumiques est très différente de 1.0 : <%(r1).15E>
   Vérifiez FRAC_VOL pour toutes les occurrences du mot clé POLYCRISTAL.
"""),

    9 : _(u"""
Les déformations deviennent trop grandes : <%(r1)E>
=> GROT_GDEP sous COMPORTEMENT n'est plus valide.

Pour un calcul en grandes déformations
il faut utiliser GDEF_LOG ou SIMO_MIEHE.

Pour un calcul hyper-élastique, utiliser ELAS_HYPER.
"""),


    10 : _(u"""
Le redécoupage local du pas de temps n'est pas compatible avec <%(k1)s>
"""),

    11 : _(u"""
La rotation de réseau n'est pas compatible avec RUNGE_KUTTA. Utiliser l'intégration IMPLICITE.
"""),

    12 : _(u"""
  LA LOI ENDO_HETEROGENE N'EST COMPATIBLE QU'AVEC LE MODELE NON LOCAL GRAD_SIGM.
"""),

    13 : _(u"""
  LA MODELISATION GRAD_SIGM N'EST COMPATIBLE QU'AVEC LA LOI ENDO_HETEROGENE.
"""),

    14: _(u"""
 ENDO_HETEROGENE : Les critères entre KI et SY ne sont pas respectés ; baissez KI ou augmentez SY
"""),

    15: _(u"""
 MONOCRISTAL : la matrice d'interaction fournie n'est pas carrée : nombre lignes = <%(r1)E>, nombre colonnes = <%(r2)E>.
"""),

    16: _(u"""
 POLYCRISTAL : Il faut au maximum 5 mono cristaux différents sur l'ensemble des phases.  Ici,il y en a : <%(i1)i>.
"""),

    17: _(u"""
 MONOCRISTAL : la matrice d'interaction fournie ne comporte pas le bon nombre de systèmes.
 il en faut : <%(i1)i>.
"""),

    18: _(u"""
 MONOCRISTAL : la matrice d'interaction fournie n'est pas symétrique.
"""),

    19: _(u"""
 MONOCRISTAL : le nombre de composantes de n et m n'est pas correct :  <%(r1)E> au lieu de 6.
"""),

    20: _(u"""
 MONOCRISTAL : comme il y a  plusieurs familles de systèmes de glissement,
 il faut fournir une matrice d'interaction entre tous ces systèmes, de dimension  <%(i1)i>
"""),

    21: _(u"""
 MONOCRISTAL : pas de matrice jacobienne programmée actuellement pour  MONO_DD_FAT.
 Utiliser ALGO_INTE='NEWTON_PERT'
"""),

    22: _(u"""
   SIMU_POINT_MAT : Le type de DEFORMATION choisi,  <%(k1)s>, est incompatible avec GRAD_IMPOSE.
   GRAD_IMPOSE n'est utilisable qu'avec DEFORMATION='SIMO_MIEHE'.
"""),

    23: _(u"""
   Il y a incohérence entre le champ des variables internes et le comportement affecté sur les mailles (si vous ne l'avez pas précisé, on suppose par défaut
que le comportement est élastique).
   On ne peut pas modifier le champ des variables internes de manière automatique.
   Mais comme vous semblez être dans un cas autorisé (voir message précédent), la reprise de ce champ dans un calcul non-linéaire devrait bien se passer (correction automatique dans l'opérateur).
"""),

    24 : _(u"""
Erreur lors de la vérification de la cohérence entre les champs de variables internes.
Le maillage sur lequel s'appuie le modèle et le maillage du champ des variables internes ne sont pas les mêmes.
"""),

    27: _(u"""
Erreur lors de la vérification de la cohérence entre les champs de variables internes.

On a affiché ci-dessus la liste des mailles pour lesquelles le nombre de sous-points est différent.
"""),

    28 : _(u"""
Erreur lors de la vérification de la cohérence entre les champs de variables internes.

On a affiché ci-dessus la liste des mailles pour lesquelles il n'y a pas de comportement affecté.
"""),

    29 : _(u"""
Erreur lors de la vérification de la cohérence entre les champs de variables internes.

On a affiché ci-dessus la liste des mailles pour lesquelles le nombre de variables internes est différent.

Le code n'accepte de changement de comportement que dans quelques cas très particuliers :
    - LEMAITRE <-> VMIS_ISOT_XXXX
    - ELAS     <-> XXXX

Risques & conseils :
  Vérifiez le comportement affecté sur cette maille.
"""),

    30 : _(u"""
Erreur lors de la vérification de la cohérence entre les champs de variables internes.

On a affiché ci-dessus la liste des mailles pour lesquelles les comportements sont incompatibles.

Le code n'accepte de changement de comportement que dans quelques cas très particuliers :
    - LEMAITRE <-> VMIS_ISOT_XXXX
    - ELAS     <-> XXXX

Risques & conseils :
  Vérifiez le comportement affecté sur cette maille.

"""),

    31: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  Vous n'avez pas renseigné le même nombre d'éléments pour les mots clefs simple %(k2)s.
"""),

    32: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  La valeur <%(r1)E> a été renseignée pour le mot clef simple <%(k2)s>, au lieu d'une valeur strictement négative.
"""),

    33: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  La valeur du mot clef simple <BIOT_COEF> doit être comprise dans l'intervalle ]0,1].
  Or vous avez renseigné la valeur <%(r1)E>
"""),

    34: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  La valeur <%(r1)E> a été renseignée pour le mot clef simple <%(k2)s>, au lieu d'une valeur strictement positive.
"""),

    35: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  Il y a n = %(i2)d occurrences du mot clef simple <PRES_CONF>, il faut donc renseigner n+1 = %(i3)d
  éléments pour le mot clef simple <TABLE_RESU>. Or vous en avez renseigné %(i4)d
"""),

    36: _(u"""
  CALC_ESSAI_GEOMECA : Pour l'essai <%(k1)s>.
  Erreur lors du calcul du module de %(k2)s sécant maximal : pour les valeurs de paramètres matériau que vous
  avez choisies, la valeur par défaut du mot clef simple <%(k3)s> conduit à sortir du domaine d'élasticité du matériau.
  Il faut donc renseigner une valeur strictement inférieure à  <%(r1)E> pour <%(k3)s>
"""),

    37: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  Incohérence entre les valeurs saisies pour les mot clef simples <PRES_CONF> et <SIGM_IMPOSE>.
  On doit toujours avoir PRES_CONF + SIGM_IMPOSE < 0.
  Or vous avez renseigné <PRES_CONF=%(r1)E> et <SIGM_IMPOSE=%(r2)E>, soit PRES_CONF + SIGM_IMPOSE = %(r3)E
"""),

    38: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  La liste de valeurs renseignées pour le mot clef simple <%(k2)s> doit être %(k4)s .
  Or vous avez renseigné la liste suivante :
  %(k3)s
"""),

    39: _(u"""
  CALC_ESSAI_GEOMECA :
  Les seules lois de comportement autorisées pour <%(k1)s> sont les lois de sol suivantes :
  --> %(k2)s
  Or vous avez renseigné <RELATION='%(k3)s'> pour le mot clef facteur <COMPORTEMENT>.
"""),

    40: _(u"""
  CALC_ESSAI_GEOMECA : %(k1)s numéro %(k2)s.

  Pour le chargement :
    %(k3)s
    %(k4)s
  avec un nombre total de cycles :
    %(k5)s

  Le calcul a été mené jusqu'à %(i1)d cycles.

  Le critère de liquéfaction n'a pas été atteint.
"""),

    41: _(u"""
  CALC_ESSAI_GEOMECA : %(k1)s numéro %(k2)s.

  Pour le chargement :
    %(k3)s
    %(k4)s
  avec un nombre total de cycles :
    %(k5)s

  Le calcul s'est arrêté au cours du cycle numéro %(i1)d pour cause d'absence de convergence,
  les pas archivés avant l'arrêt ont été sauvegardés.

  Le critère de liquéfaction a été atteint au cours du cycle numéro %(i2)d.
"""),

    42: _(u"""
  CALC_ESSAI_GEOMECA : %(k1)s numéro %(k2)s.

  Pour le chargement :
    %(k3)s
    %(k4)s
  avec un nombre total de cycles :
    %(k5)s

  Le calcul s'est arrêté au cours du cycle numéro %(i1)d pour cause d'absence de convergence,
  les pas archivés avant l'arrêt ont été sauvegardés.

  Le critère de liquéfaction n'a pas été atteint.
"""),

    43: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef simple <TABLE_REF> pour la table <%(k1)s>.
  Vous avez indiqué dans cette table le TYPE suivant :
    <'%(k2)s'>
  Or on ne peut indiquer qu'un TYPE figurant parmi la liste de GRAPHIQUE demandée en sortie pour cet essai:
    <%(k3)s>
"""),

    44: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef simple <TABLE_REF> pour la table <%(k1)s>.
  La liste des paramètres d'une TABLE_REF doit nécessairement être <'TYPE','LEGENDE','ABSCISSE','ORDONNEE'>
  Or liste des paramètres de la table renseignée est :
  <%(k2)s>
"""),

    45: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef simple <TABLE_REF> pour la table <%(k1)s>.
  La colonne <%(k2)s> d'une TABLE_REF ne doit contenir qu'un élément de type chaîne de caractères.
"""),

    46: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef simple <TABLE_REF> pour la table <%(k1)s>.
  Les colonnes ABSCISSE et ORDONNEE d'une TABLE_REF doivent avoir même cardinal et contenir des réels.
"""),

    47: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  Incohérence entre les valeurs saisies pour les mot clef simples <PRES_CONF>, <SIGM_IMPO> et <SIGM_décharge>.
  On doit toujours avoir PRES_CONF + SIGM_IMPO <= SIGM_décharge .
  Or vous avez renseigné < PRES_CONF = %(r1)E> et <SIGM_IMPO = %(r2)E>, soit PRES_CONF + SIGM_IMPO = %(r3)E supérieur à %(r4)E
"""),

    48: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d).
  Incohérence entre les valeurs saisies pour les mot clef simples <PRES_CONF> et <SIGM_décharge>.
  On doit toujours avoir SIGM_décharge <= PRES_CONF.
  Or vous avez renseigné < SIGM_décharge = %(r1)E> et <PRES_CONF = %(r2)E>
"""),

    50 : _(u"""Maille: %(k1)-8s - Pas de points d'intégration"""),

    51 : _(u"""Maille: %(k1)-8s - Comportement précédent: %(k2)-16s - Comportement courant: %(k3)-16s"""),

    52 : _(u"""Maille: %(k1)-8s - Nombre de sous-points précédent: %(i1)d - Nombre de sous-points courant: %(i2)d"""),

    53 : _(u"""Maille: %(k1)-8s - Nombre de variables internes précédent: %(i1)d - Nombre de variables internes courant: %(i2)d"""),


    70: _(u"""
#---------------------------------------------------------------------------------------------------------------------------
# test pour analyser à l'aide de SIMU_POINT_MAT l'échec d'intégration du comportement sur la maille <%(k1)s>, point <%(i1)d>
#---------------------------------------------------------------------------------------------------------------------------
DEBUT()
# recopier MAT=DEFI_MATERIAU(...), DEFI_COMPOR(...),...

LIST=DEFI_LIST_REEL(DEBUT= %(r1).15E , INTERVALLE=_F(JUSQU_A= %(r2).15E , NOMBRE=1))
# liste = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=LIST,),ECHEC=_F(SUBD_NIVEAU=10,SUBD_PAS=4),)
"""),


    71: _(u"""
%(k1)s=DEFI_FONCTION(NOM_PARA='INST',VALE=( %(r1).15E , %(r2).15E, %(r3).15E,  %(r4).15E))
"""),

    72: _(u"""
RESU=SIMU_POINT_MAT ( INFO=1, MATER=MAT, INCREMENT=_F(LIST_INST=LIST),
                      EPSI_IMPOSE=_F(EPXX=EXX, EPYY=EYY,EPZZ=EPZZ,EPXY=EXY),
"""),

    73: _(u"""
                      SUPPORT='ELEMENT', MODELISATION='C_PLAN',
"""),

    74: _(u"""
RESU=SIMU_POINT_MAT ( INFO=1, MATER=MAT, INCREMENT=_F(LIST_INST=LIST),
                      EPSI_IMPOSE=_F(EPXX=EXX, EPYY=EYY,EPZZ=EPZZ,EPXY=EXY,EPXZ=EPXZ,EPYZ=EPYZ),
"""),

    75: _(u"""
                      EPSI_INIT=_F(EPXX= %(r1).15E , EPYY= %(r2).15E,  EPZZ= %(r3).15E,  EPXY= %(r4).15E,  EPXZ= %(r5).15E,  EPYZ= %(r6).15E ),
"""),

    76: _(u"""
                      SIGM_INIT=_F(SIXX= %(r1).15E , SIYY= %(r2).15E,  SIZZ= %(r3).15E,  SIXY= %(r4).15E,  SIXZ= %(r5).15E,  SIYZ= %(r6).15E ),
                      VARI_INIT=_F(VALE=(
"""),

    77: _(u"""
                      %(r1).15E,
"""),

    78: _(u"""
                      COMPORTEMENT=_F(RELATION= '%(k1)s',
"""),

    79: _(u"""
                      NEWTON=_F(REAC_ITER=1),
)
FIN()
"""),

    80: _(u"""
)
FIN()
"""),

    81: _(u"""
%(k1)s=DEFI_FONCTION(NOM_PARA='INST',VALE=( %(r1).15E, %(r2).15E, %(r3).15E, %(r4).15E))
"""),

    82: _(u"""
                  AFFE_VARC=(
"""),

    83: _(u"""
                              _F(NOM_VARC='%(k1)s',VALE_FONC=%(k2)s,VALE_REF=%(r1).15E),
"""),

    84: _(u"""
                              _F(NOM_VARC='%(k1)s',VALE_FONC=%(k2)s),
"""),


    85: _(u"""
                  ),
"""),

    86: _(u"""
                      )),
"""),

    87: _(u"""
                      COMPOR=%(k1)s,
"""),

    88: _(u"""
                      ITER_INTE_MAXI=%(i1)d, RESI_INTE_RELA=%(r1).15E, ALGO_INTE='%(k1)s',
"""),

    89: _(u"""
                      ITER_INTE_PAS=%(i1)d, PARM_THETA=%(r1).15E,
"""),

    90: _(u"""
                      DEFORMATION='%(k1)s',
"""),

    91: _(u"""
   SIMU_POINT_MAT : META_LEMA_ANI est interdit à cause de la matrice de Hill définie en repère cylindrique.
   Si la matrice de Hill est isotrope, utiliser SIMU_POINT_MAT avec SUPPORT='ELEMENT'.
   Sinon, utiliser STAT_NON_LINE sur un maillage adéquat.
"""),

    92: _(u"""
   SIMU_POINT_MAT : META_LEMA_ANI définit une matrice de Hill définie en repère cylindrique.
   Il faut donc vérifier que  la matrice de Hill est isotrope.
   Sinon, utiliser STAT_NON_LINE sur un maillage adéquat.
"""),

    93 : _(u"""
   Quand on utilise PETIT_REAC dans SIMU_POINT_MAT, il n'est pas possible d'imposer exactement une déformation donnée.
   Les déformations à la fin du calcul seront différentes de celles que l'on a donné, sauf en petites déformations.
   Si vous voulez simuler des grandes déformations, utilisez plutôt GDEF_LOG.
"""),

}
