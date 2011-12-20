#@ MODIF utilitai3 Messages  DATE 20/12/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

2 : _(u"""
 l'utilisation de cette commande n'est légitime que si
 la configuration étudiée est du type "FAISCEAU_TRANS"
"""),

4 : _(u"""
 le nom d'un paramètre ne peut pas dépasser 16 caractères
"""),

5 : _(u"""
 le paramètre  %(k1)s  n'existe pas
"""),

6 : _(u"""
 seuls les paramètres de types réel, entier ou complexe sont traites
"""),

7 : _(u"""
 erreur DPVP_2
"""),

8 : _(u"""
 code retour non nul détecté
"""),

9 : _(u"""
 maillage autre que SEG2                                ou POI1
"""),

10 : _(u"""
 mailles ponctuelles                           plexus POI1 ignorées
"""),

11 : _(u"""
 le format doit être IDEAS
"""),

12 : _(u"""
 le maillage doit être issu d'IDEAS
"""),

13 : _(u"""
  maillage non issu d'IDEAS
"""),

14 : _(u"""
 avec le 2414, on ne traite pas les NUME_ORDRE
"""),

15 : _(u"""
 Problème lecture du fichier IDEAS
"""),

17 : _(u"""
 format  %(k1)s  inconnu.
"""),

18 : _(u"""
 NOM_CMP_IDEM est curieux :  %(k1)s
"""),

19 : _(u"""
 problème maillage <-> modèle
"""),

20 : _(u"""
 le champ de META_ELNO:ETAT_INIT(NUM_INIT) n'existe pas.
"""),

21 : _(u"""
 maillage et modèle incohérents.
"""),

22 : _(u"""
 pour TYPE_RESU:'EL..' il faut renseigner le mot clé modèle.
"""),

23 : _(u"""
Erreur :
  Aucun élément fini du ligrel '%(k1)s' ne sait calculer le
  paramètre: %(k2)s de l'option:  %(k3)s.
  Le champ par éléments que l'on veut créer est vide. On ne peut pas continuer.
"""),

24 : _(u"""
 option=  %(k1)s  incompatible avec TYPE_CHAM=  %(k2)s
"""),

25 : _(u"""
 OPERATION=  %(k1)s  seulement TYPE_CHAM= 'NOEU_GEOM_r'
"""),

26 : _(u"""
 OPERATION=  %(k1)s  incompatible avec TYPE_CHAM=  %(k2)s
"""),

27 : _(u"""
 grandeurs différentes pour : %(k1)s et : %(k2)s
"""),

28 : _(u"""
 il existe des doublons dans la liste d'instants de rupture
"""),

29 : _(u"""
 il faut donner plus d'un instant de rupture
"""),

30 : _(u"""
 il manque des températures  associées aux bases de résultats (mot-clé tempe)
"""),

31 : _(u"""
 le paramètre m de WEIBULL doit être le même pour toutes les bases résultats !
"""),

32 : _(u"""
 le paramètre SIGM_REFE de WEIBULL doit être le même pour toutes les bases résultats !
"""),

33 : _(u"""
 aucun numéro d'unité logique n'est associe a  %(k1)s
"""),

34 : _(u"""
 aucun numéro d'unité logique n'est disponible
"""),

35 : _(u"""
 action inconnue:  %(k1)s
"""),

36 : _(u"""
 Arrêt de la procédure de recalage : le paramètre m est devenu trop petit (m<1),
 vérifiez vos listes d'instants de rupture
"""),

37 : _(u"""
 les paramètres de la nappe ont été réordonnées.
"""),

38 : _(u"""
 type de fonction non connu
"""),

39 : _(u"""
 points confondus.
"""),

40 : _(u"""
 impossibilité : la maille  %(k1)s  doit être de type "SEG2" ou "SEG3"
 et elle est de type :  %(k2)s
"""),

42 : _(u"""
 le contour dont on doit calculer l'aire n'est pas fermé
"""),

43 : _(u"""
 le mot-clé "reuse" n'existe que pour l'opération "ASSE"
"""),

46 : _(u"""
 le groupe de mailles " %(k1)s " n'existe pas.
"""),

47 : _(u"""
 le groupe  %(k1)s  ne contient aucune maille.
"""),

48 : _(u"""
 on ne traite que des problèmes 2d.
"""),

49 : _(u"""
 la maille " %(k1)s " n'existe pas.
"""),

50 : _(u"""
 On doit donner un résultat de type "EVOL_THER" après le mot-clé "LAPL_PHI"
 du mot-clé facteur "CARA_POUTRE" dans la commande POST_ELEM pour calculer
 la constante de torsion.
"""),

51 : _(u"""
 Le nombre d'ordres du résultat  %(k1)s  nécessaire pour calculer la constante
 de torsion doit être égal a 1.
"""),

52 : _(u"""
 On n'arrive pas a récupérer le champ de températures du résultat  %(k1)s
"""),

53 : _(u"""
 La table "CARA_GEOM" n'existe pas.
"""),

54 : _(u"""
 On doit donner un résultat de type "EVOL_THER" après le mot-clé "LAPL_PHI_Y"
 du mot-clé facteur "CARA_POUTRE" dans la commande POST_ELEM pour calculer les
 coefficients de cisaillement et les coordonnées du centre de torsion.
"""),

55 : _(u"""
 On doit donner un résultat de type "EVOL_THER" après le mot-clé "LAPL_PHI_Z"
 du mot-clé facteur "CARA_POUTRE" dans la commande POST_ELEM pour calculer les
 coefficients de cisaillement et les coordonnées du centre de torsion.
"""),

56 : _(u"""
 Le nombre d'ordres du résultat  %(k1)s  nécessaire pour calculer les coefficients
 de cisaillement et les coordonnées du centre de torsion doit être égal a 1.
"""),

57 : _(u"""
 On doit donner un résultat de type "EVOL_THER" après le mot-clé "LAPL_PHI"
 du mot-clé facteur "CARA_POUTRE" dans la commande POST_ELEM pour calculer la
 constante de gauchissement.
"""),

58 : _(u"""
 Le nombre d'ordres du résultat  %(k1)s  nécessaire pour calculer la constante de
 gauchissement doit être égal a 1.
"""),

59 : _(u"""
 Il faut donner le nom d'une table issue d'un premier calcul avec l'option "CARA_GEOM"
 de  POST_ELEM après le mot-clé "CARA_GEOM" du mot-clé facteur "CARA_POUTRE".
"""),

60 : _(u"""
 Il faut obligatoirement définir l'option de calcul des caractéristiques de poutre
 après le mot-clé "option" du mot-clé facteur "CARA_POUTRE" de la commande POST_ELEM.
"""),

61 : _(u"""
 l'option  %(k1)s n'est pas admise après le mot-clé facteur "CARA_POUTRE".
"""),

62 : _(u"""
 il faut donner le nom d'un résultat de type EVOL_THER
 après le mot-clé LAPL_PHI du mot-clé facteur "CARA_POUTRE".
"""),

63 : _(u"""
 il faut donner le nom d'un résultat de type EVOL_THER
 après le mot-clé LAPL_PHI_Y du mot-clé facteur "CARA_POUTRE".
"""),

64 : _(u"""
 il faut donner le nom d'un résultat de type EVOL_THER
 après le mot-clé LAPL_PHI_Z du mot-clé facteur "CARA_POUTRE".
"""),

68 : _(u"""
 On attend un concept "MODE_MECA" ou "EVOL_ELAS" ou "EVOL_THER" ou "DYNA_TRANS"
 ou "EVOL_NOLI"
"""),

69 : _(u"""
 champ de vitesse donné
"""),

70 : _(u"""
 champ de déplacement donné
"""),

71 : _(u"""
 option masse cohérente.
"""),

72 : _(u"""
 calcul avec masse diagonale
"""),

73 : _(u"""
 type de champ inconnu.
"""),

75 : _(u"""
 On attend un concept "MODE_MECA" ou "EVOL_ELAS" ou "MULT_ELAS" ou "EVOL_THER"
 ou "DYNA_TRANS" ou "EVOL_NOLI"
"""),

76 : _(u"""
 Pour calculer les indicateurs globaux d'énergie, il faut donner un résultat
 issu de STAT_NON_LINE .
"""),

77 : _(u"""
 on attend un résultat de type "EVOL_NOLI" .
"""),

78 : _(u"""
 Le résultat  %(k1)s  doit comporter la relation de comportement au numéro
 d'ordre  %(k2)s  .
"""),

79 : _(u"""
 Le résultat  %(k1)s  doit comporter un champ de variables internes au numéro
 d'ordre  %(k2)s  .
"""),

80 : _(u"""
 Impossibilité : le volume du modèle traite est nul.
"""),

81 : _(u"""
 impossibilité : le volume du GROUP_MA  %(k1)s  est nul.
"""),

82 : _(u"""
 impossibilité : le volume de la maille  %(k1)s  est nul.
"""),

83 : _(u"""
 Erreur: les options de calcul doivent être identiques pour toutes les occurrences
 du mot clef facteur
"""),

84 : _(u"""
 on attend un concept "EVOL_NOLI"
"""),

85 : _(u"""
 erreur: le champ SIEF_ELGA n'existe pas
"""),

86 : _(u"""
 erreur: le champ VARI_ELGA n'existe pas
"""),

87 : _(u"""
 erreur: le champ DEPL_ELNO n'existe pas
"""),

88 : _(u"""
 erreur: le champ EPSG_ELGA n'existe pas
"""),

89 : _(u"""
 les 2 nuages : %(k1)s  et  %(k2)s  doivent avoir le même nombre de coordonnées.
"""),

90 : _(u"""
 les 2 nuages : %(k1)s  et  %(k2)s  doivent avoir la même grandeur associée.
"""),

91 : _(u"""
 il manque des composantes sur :  %(k1)s
"""),



93 : _(u"""
 seuls les types "réel" et "complexe" sont autorises.
"""),

94 : _(u"""
 MINMAX est toujours calculé sur TOUT le modèle pour les champs aux noeuds.
"""),

}
