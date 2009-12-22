#@ MODIF algorith6 Messages  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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

1 : _("""
 GAMMA_T et GAMMA_C ne doivent pas être égal à 1 en même temps.
"""),

2 : _("""
 -> La valeur de SYC %(r1)s ne permet pas de respecter GAMMA_C < 1.
 -> Conseil : Choisissez une valeur de SYC inférieure à %(r2)s
"""),

3 : _("""
 -> Les valeurs des paramètres de la loi GLRC_DM entraîne un seuil d'endommagement nul.
 -> Conseil : Modifier les valeurs des paramètres matériaux
"""),

13 : _("""
 dimension du problème inconnue
"""),

16 : _("""
 le fond de fissure d'un maillage 2d ne peut être défini par des mailles
"""),

17 : _("""
 les mailles à modifier doivent être de type "SEG3" ou "POI1"
"""),

18 : _("""
 le fond de fissure d'un maillage 2d est défini par un noeud unique
"""),

19 : _("""
  -> Code Aster a détecté des mailles de type différent lors de la
     correspondance entre les maillages des deux modèles (mesuré/numérique).
     Ce cas n'est pas prévu, Code Aster initialise la correspondance au noeud
     le plus proche.
  -> Risque & Conseil :
     ???
"""),

20 : _("""
 nombre noeuds mesuré supérieur au nombre de noeuds calculé
"""),

21 : _("""
 NOEU_CALCUL non trouvé
"""),

22 : _("""
 NOEU_MESURE non trouvé
"""),

23 : _("""
 nombre de noeuds différent
"""),

24 : _("""
 traitement manuel correspondance : un couple à la fois
"""),

25 : _("""
 échec projection
"""),

26 : _("""
 norme vecteur dir. nulle
"""),

27 : _("""
 le nombre des coefficients de pondération est supérieur
 au nombre de vecteurs de base
"""),

28 : _("""
 le nombre des coefficients de pondération est inférieur
 au nombre de vecteurs de base
 le dernier coefficient est affecté aux autres
"""),

29 : _("""
 le nombre des fonctions de pondération est supérieur
 au nombre de vecteurs de base
"""),

30 : _("""
 le nombre des fonctions de pondération est inférieur
 au nombre de vecteurs de base
 la dernière fonction est affectée aux autres
"""),

31 : _("""
 le nombre dabscisses d'une des fonctions d'interpolation
 n'est pas identique au nombre d'abscisses du premier point
 de mesure expérimental 
"""),

32 : _("""
  le critère d'égalite de la liste d'abscisses du premier dataset 58
  et de la liste d'abscisses d une des fonctions de pondération
  n'est pas verifié 
"""),

33 : _("""
 incompatibilité NOM_PARA et données mesurées 
"""),

37 : _("""
 pas de numéro d'ordre pour le concept  %(k1)s 
"""),

52 : _("""
 itérations cycliques :
 changement de configuration ou variation trop importante
 du deplacement physique à l'issue de la dernière itération
 Conseil: diminuez le pas de temps
"""),

53 : _("""
 pas de convergence de l'algorithme de NEWTON :
 - en  %(k1)s  iterations
 - à l'instant  %(k2)s
 il faut réduire la rigidité normale, ou le jeu.
"""),

54 : _("""
 dvp : trop de noeuds
"""),

55 : _("""
 THETA = 1 ou 0.5 
"""),

56 : _("""
 fluence commandée et FLUX_PHI différent de 1
"""),

57 : _("""
 fluence décroissante (PHI<0)
"""),

58 : _("""
 relation ASSE_COMBU 1d sans loi de fluence appropriée
"""),

59 : _("""
 erreur dir. grandissement
"""),

60 : _("""
 CAM_CLAY :
 la porosité donnee dans CAM_CLAY doit etre la meme que dans THM_INIT
"""),

61 : _("""
 BARCELONE :
 il faut que la contrainte hydrostatique soit supérieure
 à la  pression de cohesion -KC*PC 
"""),

62 : _("""
 ITER_INTE_MAXI insuffisant lors du calcul de la borne
"""),

63 : _("""
 CAM_CLAY :
 le cas des contraintes planes n'est pas traité pour ce modèle.
"""),

64 : _("""
 CAM_CLAY :
 il faut que la contrainte hydrostatique soit supérieure
 a la pression initiale PA 
"""),

66 : _("""
 pour l'instant, on ne traite pas le cas des contraintes planes
 dans le modele de CHABOCHE à une variable cinématique.
"""),

67 : _("""
 N doit etre strictementpositif.
"""),

68 : _("""
 paramètre UN_SUR_K égal à zéro cas incompatible avec VISC_CINX_CHAB
"""),

69 : _("""
 loi VISC_CINX_CHAB
 on doit obligatoirement avoir UN_SUR_M = zéro
"""),

78 : _("""
 F reste toujours négative.
"""),

79 : _("""
 F reste toujours positive.
"""),

80 : _("""
 pb interp vari entiere ?? 
"""),

81 : _("""
 utiliser ALGO_1D="DEBORST" sous COMP_INCR pour le comportement  %(k1)s 
"""),

86 : _("""
 erreur de programmation 1
"""),

87 : _("""
 loi de comportement inexistante
"""),

88 : _("""
 erreur dans le type de comportement
"""),

92 : _("""
 pas de contraintes planes
"""),

96 : _("""
 GREEN deformation required for ELAS_HYPER material
"""),

}
