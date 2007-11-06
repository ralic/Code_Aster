#@ MODIF algorith Messages  DATE 05/11/2007   AUTEUR VIVAN L.VIVAN 
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

cata_msg = {

1: _("""
 La modélisation C_PLAN n'est pas compatible avec la loi de comportement ELAS_VMIS_PUIS.
"""),

3 : _("""
 type de matrice inconnu.
"""),

10 : _("""
 impossible de diagonaliser la matrice de raideur en choc
"""),

11 : _("""
 PULS(I) = 0.
 initialisation à PULS0(I).
"""),

13 : _("""
 le VECT_ELEM n'existe pas :  %(k1)s
"""),

14 : _("""
 champ non renseigné.
 il faut avoir utilisé CORICH('E',...) sur:  %(k1)s
"""),

15 : _("""
 impossible
"""),

16 : _("""
 les charges cinématiques sont pour l'instant proscrites avec FETI
"""),

19 : _("""
 stop 3
"""),

20 : _("""
 on n'a pas pu extraire le premier champ des modes mécaniques.
"""),

21 : _("""
 le noeud  %(k1)s  n'appartient pas au maillage :  %(k2)s
"""),

22 : _("""
 le groupe  %(k1)s  n'appartient pas au maillage :  %(k2)s
"""),

25 : _("""
 données incompatibles.
"""),

26 : _("""
 le vecteur directeur du spectre est nul.
"""),

27 : _("""
 cas du MONO_APPUI : vous avez déjà donné un spectre pour cette direction.
"""),

28 : _("""
  erreur(s) rencontrée(s) lors de la lecture des supports.
"""),

29 : _("""
  vous avez déjà donné un spectre pour le support  %(k1)s
"""),

30 : _("""
 on ne peut pas traiter du mono-appui et du multi-appui simultanément.
"""),

31 : _("""
 le noeud  %(k1)s  ne fait pas partie du maillage :  %(k2)s
"""),

32 : _("""
  la numérotation n'est pas cohérente avec le modèle généralisé
  si vous avez activé l'option INITIAL dans NUME_DDL_GENE faites de meme ici !
"""),

33 : _("""
 calcul des options RIGI_MECA_TANG, RAPH_MECA et FULL_MECA
 en mécanique des milieux poreux avec couplage THM
 ---> erreur de dimensionnement
"""),

34 : _("""
 il y a incohérence entre :
    la loi de couplage de DEFI_MATERIAU : %(k1)s
 et la loi de couplage de STAT_NON_LINE : %(k2)s
"""),

35 : _("""
 les champs " %(k1)s " et " %(k2)s " n'ont pas le meme domaine de définition.
"""),

36 : _("""
 BARSOUM, erreur dans le traitement des mailles %(k1)s
"""),

42 : _("""
 BETON_DOUBLE_DP: incrément de déformation plastique en traction négatif
 --> redécoupage automatique du pas de temps
"""),

43 : _("""
 BETON_DOUBLE_DP: incrément de déformation plastique en compression négatif
 --> redécoupage automatique du pas de temps
"""),

44 : _("""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n'est pas respectée en compression.
"""),

45 : _("""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des elements
 n'est pas respectée en compression pour la maille:  %(k1)s
"""),

46 : _("""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n est pas respectée en traction.
"""),

47 : _("""
 integration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilite sur la taille des éléments
 n'est pas respectée en traction pour la maille:  %(k1)s
"""),

48 : _("""
  -> Intégration élastoplastique de loi multi-critères BETON_DOUBLE_DP :
     la contrainte équivalente est nulle pour la maille %(k1)s
     le calcul de la matrice tangente est impossible.
  -> Risque & Conseil :
"""),

49 : _("""
 pour la loi BETON_DOUBLE_DP :
 le paramètre COEF_ELAS_COMP doit etre compris entre 0. et 100.
"""),

50 : _("""
 pour la loi BETON_DOUBLE_DP :
 le parametre LONG_CARA doit etre strictement positif
"""),

51 : _("""
 BETON_DOUBLE_DP:
 le cas des contraintes planes n'est pas traité pour ce modèle.
"""),

52 : _("""
 le résultat n'est pas un EVOL_NOLI
"""),

53 : _("""
 champ SIEF_ELGA non trouvé
"""),

54 : _("""
 champ EPSP_ELNO non trouvé
"""),

55 : _("""
 champ VARI_ELNO_ELGA non trouvé
"""),

56 : _("""
 aucun champ initial trouvé
"""),

57 : _("""
 le matériau dépend de la température 
 il n'y a pas de champ de température
 le calcul est impossible
"""),

59 : _("""
 THLAG-GLEG pas possible
"""),

60 : _("""
 certains coefficients de masse ajoutée sont négatifs.
 verifiez l'orientation des normales des éléments d'interface.
 convention adoptée : structure vers fluide
"""),

61 : _("""
 certains coefficients d'amortissement ajouté sont négatifs.
 possibilité d'instabilité de flottement
"""),

62 : _("""
 erreur dans le calcul des valeurs propres de la matrice de raideur
"""),

63 : _("""
 valeurs propres de la matrice de raideur non réelles
"""),

64 : _("""
 valeurs propres de la matrice de raideur réelles négatives
"""),

65 : _("""
 erreur dans la sélection des valeurs propres de la matrice de raideur
"""),

66 : _("""
 tailles des matrices incompatibles pour calcul matrice diagonale
"""),

67 : _("""
 option SECANTE non valide
"""),

68 : _("""
 trop de familles de systèmes de glissement.
 augmenter la limite actuelle (5)
"""),

69 : _("""
 trop de familles de systèmes de glissement.
 modifier GERPAS
"""),

70 : _("""
 NBSYS=0
"""),

71 : _("""
 tailles incompatibles pour le produit matrice * vecteur
"""),

72 : _("""
 traitement non prévu pour le type d'obstacle demandé
"""),

73 : _("""
 obstacle de type discret mal défini (un angle est supérieur à pi).
"""),

77 : _("""
 problème à la résolution du système
"""),

78 : _("""
 cas 2D impossible
"""),

79 : _("""
 liaison de frottement incongrue
"""),

81 : _("""
 erreur contact - trop de réactualisations géométriques
"""),

82 : _("""
 erreur de programmation
"""),

89 : _("""
 vecteur diagnostic absent (dvlp)
"""),

90 : _("""
 opération inconnue sur le vecteur diagnostic (dvlp)
"""),

91 : _("""
 accès incorrect au vecteur diagnostic (dvlp)
"""),

92 : _("""
 cas impossible
"""),

94 : _("""
 pas possible
"""),

96 : _("""
 ce mot cle de MODI_MAILLAGE attend un vecteur de norme non nulle.
"""),

97 : _("""
 le mot cle REPERE de MODI_MAILLAGE attend deux vecteurs non nuls orthogonaux.
"""),

}
