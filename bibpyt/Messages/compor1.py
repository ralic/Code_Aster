#@ MODIF compor1 Messages  DATE 05/03/2012   AUTEUR PROIX J-M.PROIX 
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

1: _(u"""
 HUJPLA :: nombre de variables internes incorrect:
           NVI > NVIMAX
"""),

2: _(u"""
 HUJDDD :: on ne calcule pas DPSIDS pour K=4.
           - vérifiez la programmation -
"""),

3: _(u"""
 CAM_CLAY :: le coefficient de poisson et ou le module de YOUNG ne sont pas corrects
             dans la maille %(k1)s

             *** vérifiez la cohérence des données mécaniques suivantes :
                 E, nu, eO (indice des vides), KAPA
                 (contrainte volumique initiale) et KCAM la compressibilité
                 initiale. Si PTRAC et KCAM sont nuls, il faut initialiser les contraintes

                 il faut notamment vérifier ceci:

        NU = (TROIS*((UN+E0)*SIGMMO+KAPA*KCAM)-DEUXMU*KAPA)/
     &         (SIX*((UN+E0)*SIGMMO+KAPA*KCAM)+DEUXMU*KAPA)

        E = DEUXMU*(UN+NU)
 ***
"""),

4: _(u"""
 HUJEUX :: les modélisations autorisées sont 3D D_PLAN ou AXIS
"""),

5: _(u"""
 HUJEUX :: K différent de NBMECA pour le mécanisme isotrope
           - vérifiez la programmation -
"""),

6: _(u"""
 HUJEUX :: erreur inversion par pivot de Gauss
"""),

7: _(u"""
 HUJCRI :: EPSI_VP est trop grand:
           l'exponentielle explose
"""),

8: _(u"""
 HUJEUX :: mécanisme indéterminé
           - vérifiez la programmation -
"""),

9 : _(u"""
Arrêt suite à l'échec de l'intégration de la loi de comportement.
Vérifiez vos paramètres, la cohérence des unités.
Essayez d'augmenter ITER_INTE_MAXI, ou de subdiviser le pas de temps
localement via ITER_INTE_PAS.
"""),

10: _(u"""
 HUJKSI :: mot-clé inconnu
"""),

11: _(u"""
 HUJNVI :: modélisation inconnue
"""),

12: _(u"""
 HUJCI1 :: l'incrément de déformation est nul:
           on ne peut pas trouver le zéro de la fonction.
"""),

14: _(u"""
 HUJTID :: erreur dans le calcul de la matrice tangente
"""),

15: _(u"""
  Pour les poutres multi-fibres, l'utilisation de lois de comportement via
  ALGO_1D='DEBORST' nécessite d'avoir un seul matériau par poutre!
 """),

16 : _(u"""
Arrêt suite à l'échec de l'intégration de la loi de comportement.

Erreur numérique (overflow) : la plasticité cumulée devient très grande.
"""),

17 : _(u"""
  HUJCI1 :: Soit le zéro n'existe pas, soit il se trouve hors des
            bornes admissibles.
"""),

18 : _(u"""
  HUJCI1 :: Cas de traction à l'instant moins.
"""),

19 : _(u"""
  MONOCRISTAL :: écrouissage cinématique non trouvé.
"""),

20 : _(u"""
  MONOCRISTAL :: écoulement non trouvé.
"""),

21 : _(u"""
  MONOCRISTAL :: écrouissage isotrope non trouvé.
"""),

23 : _(u"""
  MONOCRISTAL :: la matrice d'interaction est définie avec
  4 coefficients. Ceci n'est applicable qu'avec 24 systèmes de
  glissement (famille BCC24).
"""),

24 : _(u"""
  MONOCRISTAL :: la matrice d'interaction est définie avec
  6 coefficients. Ceci n'est applicable qu'avec 12 systèmes de
  glissement.
"""),

25 : _(u"""
  MONOCRISTAL :: la matrice d'interaction est définie avec
  un nombre de coefficients incorrect :: il en faut 1, ou 4, ou 6.
"""),

26: _(u"""
 LETK - lklmat :: paramètres de la loi LETK non cohérents
"""),

27 : _(u"""
  comportement cristallin  : les coefficients matériau ne peuvent dépendre de la température.
"""),

28 : _(u"""
  comportement cristallin homogénéisé : les coefficients matériau ne peuvent dépendre de la température.
"""),

29: _(u"""
 LETK - lkdhds :: division par zéro - entrée en plasticité avec un déviateur  nul.
 le pas de temps est trop petit - augmenter le pas de temps pour augmenter le déviateur.
"""),

30: _(u"""
 LETK - lkds2h :: division par zéro - entrée en plasticité avec un déviateur nul.
 le pas de temps est trop petit - augmenter le pas de temps pour augmenter le déviateur.
"""),

31: _(u"""
 LETK - lkcaln :: division par zéro - entrée en plasticité avec un déviateur nul.
 le pas de temps est trop petit - augmenter le pas de temps pour augmenter le déviateur.
"""),

32: _(u"""
 VISC_CINx_CHAB :: pour la viscosité, renseigner le mot-clé LEMAITRE dans DEFI_MATERIAU.
 Si vous voulez seulement de l'élastoplasticité, il faut utiliser VMIS_CINx_CHAB.
"""),

33: _(u"""
 NMHUJ :: ELAS/ELAS_ORTH :: erreur de lecture des propriétés matériaux.
"""),

34: _(u"""
 HUJTID :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

35: _(u"""
 HUJDP :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

36: _(u"""
 HUJTEL :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

37: _(u"""
 HUJPOT :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

38: _(u"""
 HUJJID :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

39: _(u"""
 HUJIID :: ELAS/ELAS_ORTH :: cas non prévu.
"""),

40: _(u"""
 HUJELA :: ELAS/ELAS_ORTH :: cas non prévu.
"""),


41: _(u"""
 CAM_CLAY ::
 Pour la maille <%(k1)s> une des exponentielles pose un problème numérique.
 La subdivision du pas de temps au niveau global est déclenchée.
 Il faut pour cela l'autoriser avec la commande DEFI_LIST_INST.
 Information sur les bornes :
   Valeur max :   <%(r1)E>
   borne correspondant à <%(k2)s> : <%(r2)E>
   borne correspondant à <%(k3)s> : <%(r3)E>
"""),

42: _(u"""
 CAM_CLAY ::  Kcam et Ptrac doivent vérifier la relation suivante :

              KCAM > -K0 * PTRAC  ou KCAM > -(1+e0)/kapa * PTRAC
"""),


43: _(u"""
 Le numéro de loi de comportement choisi <%(i1)i> est hors des bornes 1-100
"""),


44: _(u"""
 Le type de déformation choisi <%(k1)s> est incompatible avec le comportement <%(k2)s>
"""),

45: _(u"""
 Le type d'algorithme d'intégration choisi : <%(k1)s> (sous COMP_INCR/%(k2)s) est incompatible avec le comportement <%(k3)s>.

Conseil :
Ne renseignez pas le mot-clé COMP_INCR/%(k2)s, afin de sélectionner l'algorithme par défaut.
"""),

46: _(u"""
 Le type de matrice tangente choisi <%(k1)s> est incompatible avec le comportement <%(k2)s>
"""),

47: _(u"""
 La modélisation choisie <%(k1)s> est incompatible avec le comportement <%(k2)s>. Pour modéliser
 des contraintes planes (ou des coques) avec ce comportement, on utilise ALGO_C_PLAN='DEBORST'
"""),

48: _(u"""
 La modélisation choisie <%(k1)s> est incompatible avec le comportement <%(k2)s>. Pour modéliser
 des contraintes 1D (barres, poutres) avec ce comportement, on utilise ALGO_1D='DEBORST'
"""),

49: _(u"""
 La modélisation choisie <%(k1)s> est incompatible avec le comportement <%(k2)s>
"""),

50: _(u"""
 Aucun comportement n'est défini sur la maille <%(k1)s>. Code_Aster a défini par défaut
  COMP_INCR='ELAS', DEFORMATION='PETIT'.
"""),

51: _(u"""
 La commande <%(k1)s> n'est pas prévue dans le traitement des mot-clé COMP_INCR / COMP_ELAS.
"""),

54: _(u"""
 ECRO_LINE : la pente d'écrouissage H et/ou le module de YOUNG E ne sont pas compatibles :
             H doit être strictement inférieur à E. Ici H=<%(r1)E>, et E=<%(r2)E>.
             Pour modéliser l'élasticité linéaire, il suffit de choisir SY grand, et H < E.
"""),

55: _(u"""
La <%(k1)s> dichotomie pour la loi IRRAD3M n'a pas trouvé de solution pour
le nombre d'itération donné <%(i1)d>.\n
Information pour le débogage
   Borne 0                 : <%(r1).15E>
   Borne 1                 : <%(r2).15E>
   Puissance N             : <%(r3).15E>
   Pas pour la recherche   : <%(r4).15E>
   RM                      : <%(r5).15E>
   EU                      : <%(r6).15E>
   R02                     : <%(r7).15E>
   Précision demandée      : <%(r8).15E>
Valeurs initiales
   N0                      : <%(r9).15E>
   Borne 0                 : <%(r10).15E>
   Borne 1                 : <%(r11).15E>
   Borne E                 : <%(r12).15E>
"""),

56: _(u"""
L'irradiation diminue au cours du temps. C'EST PHYSIQUEMENT IMPOSSIBLE.
Grandeurs au point de Gauss :
   Irradiation a t- : <%(r1).15E>
   Irradiation a t+ : <%(r2).15E>
"""),

57: _(u"""
Pour information
   Température a t- : <%(r1)E>
   Température a t+ : <%(r2)E>
"""),

58: _(u"""
Le franchissement du seuil de fluage ne se fait pas dans la tolérance donnée dans DEFI_MATERIAU
pour la loi IRRAD3M, par le mot clef TOLER_ET.
   Tolérance sur le franchissement du seuil : <%(r1)E>
   Erreur sur le franchissement du seuil    : <%(r2)E>
La subdivision du pas de temps est déclenchée.
Il faut pour cela l'autoriser avec le mot clef SUBD_METHODE de la commande STAT_NON_LINE.
"""),

59: _(u"""
 Attention: pas de couplage avec le coefficient de couplage CHI = 0, on retrouve la loi UMLV
"""),

60: _(u"""
Couplage: on ne fait pas dépendre E, MU et ALPHA de la température T, on prend T=0 comme pour ENDO_ISOT_BETON
"""),

61: _(u"""
Couplage: on fait dépendre E, MU et ALPHA de la température maximale Tmax, comme pour MAZARS
"""),

63 : _(u"""
   ATTENTION SR > 1    SR = %(r1)f
   SECHM %(r2)f    SECHP %(r3)f    W0 %(r4)f
"""),

64 : _(u"""
   ATTENTION SR < 0    SR = %(r1)f
   SECHM %(r2)f    SECHP %(r3)f    W0 %(r4)f
"""),

65 : _(u"""
   Attention dans la routine majpad la pression d'air dissous devient
   négative à la maille %(k1)s.
"""),

66 : _(u"""
La loi de comportement ENDO_SCALAIRE n'est disponible que pour la formulation
non locale GRAD_VARI, assurez vous que votre modélisation soit l'une des trois
suivantes : - D_PLAN_GRAD_VARI
            - AXIS_GRAD_VARI
            - 3D_GRAD_VARI
"""),
67 : _(u"""
Dans la définition du matériau RUPT_DUCT les coefficients de forme de la loi CZM_TRA_MIX doivent vérifier : COEF_EXTR <= COEF_PLAS
"""),

69 : _(u"""
Le type de déformations %(k1)s n'est pas compatible avec les modélisations SHB. Utilisez PETIT ou GROT_GDEP.
"""),

70 : _(u"""
Problème lors du traitement de l'occurrence numéro %(i1)d du mot-clé facteur %(k2)s :
  La donnée du mot-clé %(k1)s n'est pas cohérente avec le reste des données (MODELISATION, RELATION).
  Le mot-clé %(k1)s sera ignoré.
"""),
}
