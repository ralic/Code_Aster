#@ MODIF algorith Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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







3: _("""
 type de matrice inconnu.
"""),

4: _("""
 points confondus
"""),

5: _("""
 gradients confondus
"""),





10: _("""
 impossible de diagonaliser la matrice de raideur en choc
"""),

11: _("""
 puls(i) = 0. initialisation a puls0(i).
"""),

12: _("""
  -> L'option 'ACCE_ABSOLU' est incorrectement traitée en multi appuis.
     Sauf si les mouvements sont corrélés.
  -> Risque & Conseil :
     Assurez-vous que les mouvements sont décorrélés et indépendants.

"""),

13: _("""
 le vect_elem n'existe pas :  %(k1)s
"""),

14: _("""
 champ non renseigne. il faut avoir utilise corich('e',...) sur:  %(k1)s
"""),

15: _("""
 impossible
"""),

16: _("""
 les charges cinematiques sont pour l'instant proscrites avec feti
"""),

19: _("""
 stop 3
"""),

20: _("""
 on n'a pas pu extraire le premier champ des modes mecaniques.
"""),

21: _("""
 le noeud  %(k1)s  n'appartient pas au maillage :  %(k2)s
"""),

22: _("""
 le groupe  %(k1)s  n'appartient pas au maillage :  %(k2)s
"""),

23: _("""
 le noeud  %(k1)s  n'est pas un noeud support.
"""),

24: _("""
 le nombre de cas  doit etre superieur a deux pour etre  combine
"""),

25: _("""
 donnees incompatibles.
"""),

26: _("""
 le vecteur directeur du spectre est nul.
"""),

27: _("""
 cas du mono_appui : vous avez deja donne un spectre pour cette direction.
"""),

28: _("""
  erreur(s) rencontree(s) lors de la lecture des supports.
"""),

29: _("""
  vous avez deja donne un spectre pour le support  %(k1)s
"""),

30: _("""
 on ne peut pas traiter du mono-appui et du multi-appui simultanement.
"""),

31: _("""
 le noeud  %(k1)s  ne fait pas parti du maillage :  %(k2)s
"""),

32: _("""
  la numerotation n'est pas coherente avec le  modele generalise  si vous avez active l'option initial dans  nume_ddl_gene faite de meme ici !  on arrete tout
"""),

33: _("""
 dimensionement
"""),

34: _("""
 il y a incohrence entre la loi de couplage de defi_materiau  %(k1)s  et la loi de couplage dans stat_non_line  %(k2)s
"""),

35: _("""
 les champs " %(k1)s " et " %(k2)s " n'ont pas le meme domaine de definition.
"""),

36: _("""
 barsoum, hexa : bug !
"""),

37: _("""
 barsoum, penta : bug !
"""),

38: _("""
 barsoum, pyram : bug !
"""),

39: _("""
 barsoum, quad : bug !
"""),

40: _("""
 barsoum, tetra : bug !
"""),

41: _("""
 barsoum, tria : bug !
"""),

42: _("""
 beton_double_dp: increment de deformation plastique en traction negatif --> redecoupage auto du pas de temps
"""),

43: _("""
 beton_double_dp: increment de deformation plastique en compression negatif --> redecoupage auto du pas de temps
"""),

44: _("""
 integration elastoplastique de loi beton_double_dp : la condition d applicabilite sur la taille des elements n est pas respectee en compression.
"""),

45: _("""
 integration elastoplastique de loi beton_double_dp : la condition d applicabilite sur la taille des elements n est pas respectee en compression pour la maille:  %(k1)s
"""),

46: _("""
 integration elastoplastique de loi beton_double_dp : la condition d applicabilite sur la taille des elements n est pas respectee en traction.
"""),

47: _("""
 integration elastoplastique de loi beton_double_dp : la condition d applicabilite sur la taille des elements n est pas respectee en traction pour la maille:  %(k1)s
"""),

48: _("""
  -> Intégration élastoplastique de loi multi-critères beton_double_dp :
     la contrainte équivalente est nulle pour la maille %(k1)s: le calcul
     de la matrice tangente est impossible.
  -> Risque & Conseil :

"""),

49: _("""
 pour la loi beton_double_dp le parametre coef_elas_comp doit etre compris entre 0. et 100.
"""),

50: _("""
 pour la loi beton_double_dp le parametre long_cara doit etre strictement positif
"""),

51: _("""
 beton_double_dp: le cas des contraintes planes n esp pas traite pour ce modele.
"""),

52: _("""
 resultat n'est pas en evol_noli
"""),

53: _("""
 champ sief_elga non trouve
"""),

54: _("""
 champ epsp_elno non trouve
"""),

55: _("""
 champ vari_elno_elga non trouve
"""),

56: _("""
 aucun champ initial trouve
"""),

57: _("""
 le materiau depend de la temperature! il n'y a pas de champ de temperature ! le calcul est impossible
"""),






59: _("""
 thlag-gleg pas possible
"""),

60: _("""
 certains coefficients de masse ajoutee sont negatifs. verifiez l' orientation des normales des elements d' interface.convention adoptee : structure vers fluide
"""),

61: _("""
 certains coefficients d amortissement ajoute sont negatifs. possibilited instabilitede flottement
"""),

62: _("""
 erreur dans le calcul des valeurs propres de la matrice de raideur
"""),

63: _("""
 valeurs propres de la matrice de raideur non reelles
"""),

64: _("""
 valeurs propres de la matrice de raideur reelles negatives
"""),

65: _("""
 erreur dans la selection des valeurs propres de la matrice de raideur
"""),

66: _("""
 tailles des matrices incompatibles pour calcul matrice diagonale
"""),

67: _("""
 option secante non valide
"""),

68: _("""
 trop de familles de systemes de glissement. augmenter la limite actuelle (5)
"""),

69: _("""
 trop de familles de systemes de glissement. modifier gerpas
"""),

70: _("""
 nbsys=0
"""),

71: _("""
 tailles incompatibles pour le produit matrice * vecteur
"""),

72: _("""
 traitement non prevu pour le type d'obstacle demande
"""),

73: _("""
 obstacle de type discret mal defini (un angle > pi).
"""),






77: _("""
 pb a la resolution du systeme
"""),

78: _("""
 cas 2d impossible
"""),

79: _("""
 liaison de frottement incongrue
"""),






81: _("""
 erreur contact - trop de reac. geom.
"""),

82: _("""
 erreur pgmeur 1
"""),








89: _("""
 vecteur diagnostic absent (dvlp)
"""),

90: _("""
 operation inconnue sur le vecteur diagnostic (dvlp)
"""),

91: _("""
 acces incorrect au vecteur diagnostic (dvlp)
"""),

92: _("""
 cas impossible
"""),



94: _("""
 pas possible
"""),






96: _("""
 ce mot cle de modi_maillage attend un vecteur de norme non nulle.
"""),

97: _("""
 le mot cle repere de modi_maillage attend deux vecteurs non nuls orthogonaux.
"""),










}
