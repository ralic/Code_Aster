#@ MODIF compor2 Messages  DATE 06/08/2012   AUTEUR CUVILLIE M.CUVILLIEZ 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg={

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

8 : _(u"""
   DEFI_COMPOR : la somme des fractions volumiques est très différente de 1.0 : <%(r1).15E>
   Vérifiez FRAC_VOL pour toutes les occurrences du mot clé POLYCRISTAL.
"""),

9 : _(u"""
Les déformations deviennent trop grandes : <%(r1)E>
=> GROT_GDEP sous COMP_INCR n'est plus valide.

Pour un calcul en grandes déformation sous COMP_INCR
il faut utiliser GDEF_HYPO_ELAS ou SIMO_MIEHE.

Pour un calcul hyper-élastique, utiliser COMP_ELAS.
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
Mot-clef : %(k1)s - occurrence  %(i1)d : comportement %(k2)s - nombre de variables internes : %(i2)d
Noms des variables internes :"""),

24: _(u"""   V%(i1)d : %(k1)s"""),

25: _(u"""
Pour les noms des variables internes du MONOCRISTAL, voir DEFI_COMPOR."""),

26: _(u"""
MONOCRISTAL : en grandes déformations, il y a 18 variables internes supplémentaires :
Le tableau des variables internes comporte, avant les 3 dernières variables internes habituelles :
FP : (1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),
FE : (1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),
"""),

27: _(u"""
Mot-clef : %(k1)s - nombre de grains  %(i1)d : localisation %(k2)s 
nombre d'occurrences de MONOCRISTAL différentes : %(i2)d - nombre de variables internes : %(i3)d
Noms des variables internes :"""),

28: _(u""" A partir de la variable interne %(i1)d : pour chaque grain : """),

29: _(u""" Dernière variable interne V%(i1)d : %(k1)s"""),

30: _(u""" ... jusqu'à V%(i1)d """),

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
  Erreur lors du calcul du module de cisaillement sécant maximal : pour les valeurs de paramètres matériau que vous 
  avez choisies, la valeur par défaut du mot clef simple <%(k2)s> conduit à sortir du domaine d'élasticité du matériau.
  Il faut donc renseigner une valeur strictement inférieure à  <%(r1)E> pour <%(k2)s>
"""),

37: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d). 
  Incohérence entre les valeurs saisies pour les mot clef simples <PRES_CONF> et <SIGM_IMPOSE>. 
  On doit toujours avoir PRES_CONF + SIGM_IMPOSE < 0.
  Or vous avez renseigné <PRES_CONF=%(r1)E> et <SIGM_IMPOSE=%(r2)E>, soit PRES_CONF + SIGM_IMPOSE = %(r3)E
"""),

38: _(u"""
  CALC_ESSAI_GEOMECA : Erreur dans la saisie du mot clef facteur <%(k1)s> (occurrence %(i1)d). 
  La liste de valeurs renseignées pour le mot clef simple <%(k2)s> doit être croissante. 
  Or vous avez renseigné la liste suivante :
  %(k3)s
"""),

39: _(u"""
  CALC_ESSAI_GEOMECA : 
  Les seules lois de comportement autorisées pour <%(k1)s> sont les lois de sol suivantes :
  --> %(k2)s
  Or vous avez renseigné <RELATION='%(k3)s'> pour le mot clef facteur <COMP_INCR>.
"""),

40: _(u"""
  CALC_ESSAI_GEOMECA : Bilan de l'essai %(k1)s.

  Pour les chargements suivants : 
    %(k2)s
  le critère de liquéfaction n'a pas été atteint au bout de NB_CYCLE = %(i1)d cycles.
  
  - Si vous avez demandé des graphiques en sortie (mot clef facteur 'GRAPHIQUE'), les valeurs qui correspondent à ces chargements
    ne seront pas reportées dans le graphique de type TYPE = '%(k3)s'
  - Si vous avez demandé des tables en sortie (mot clef simple 'TABLE_RESU'), les lignes des colonnes %(k4)s et %(k5)s qui 
    correspondent à ces chargements ne seront pas reportées dans les tables.

  Conseil : augmentez la valeur de NB_CYCLE
"""),

41: _(u"""
  CALC_ESSAI_GEOMECA : Bilan de l'essai %(k1)s.

  Pour les chargements suivants : 
    %(k2)s
  Le calcul s'est arrêté en erreur fatale au cours d'un cycle postérieur au premier cycle. Ces erreurs ont été interceptées, 
  et les résultats obtenus jusqu'au cycle précédent ont été sauvegardés. Ces erreurs sont supposées correspondre au dépassement 
  du critère de liquéfaction pour chacun de ces chargements.

"""),

42: _(u"""
  CALC_ESSAI_GEOMECA : Bilan de l'essai %(k1)s.

  Pour les chargements suivants : 
    %(k2)s
  Le calcul s'est arrêté en erreur fatale au cours du premier cycle.

  - Si vous avez demandé des graphiques en sortie (mot clef facteur 'GRAPHIQUE'), les valeurs qui correspondent à ces chargements
    ne seront pas reportées dans les graphiques.
  - Si vous avez demandé des tables en sortie (mot clef simple 'TABLE_RESU'), les valeurs qui correspondent à ces chargements
    ne seront pas reportées dans les tables.

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

}
