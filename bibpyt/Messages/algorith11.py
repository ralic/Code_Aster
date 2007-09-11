#@ MODIF algorith11 Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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

1: _("""
 le sup de KMOD0 est nul
 on prend le sup de KMOD
"""),

2: _("""
 le sup de KMOD est nul.
"""),

3: _("""
 la variable AMOR est nulle
"""),

4: _("""
 erreur de dimension (dvlp)
"""),

5: _("""
 force normale nulle.
"""),

6: _("""
 somme des "impacts-ecrouissage" < somme des "glissement"
"""),

7: _("""
 "NOM_CAS" n'est pas une variable d'accès d'un résultat de type "EVOL_THER".
"""),

8: _("""
 "NUME_MODE" n'est pas une variable d'accès d'un résultat de type "EVOL_THER".
"""),

9: _("""
 "NUME_MODE" n'est pas une variable d'accès d'un résultat de type "MULT_ELAS".
"""),

10: _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "MULT_ELAS".
"""),

11: _("""
 "NOM_CAS" n'est pas une variable d'accès d'un resultat de type "FOURIER_ELAS".
"""),

12: _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "FOURIER_ELAS".
"""),

13: _("""
 "NOM_CAS" n'est pas une variable d'accès d'un resultat de type "FOURIER_THER".
"""),

14: _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "FOURIER_THER".
"""),

15: _("""
 "RESU_INIT" est obligatoire
"""),

16: _("""
 "MAILLAGE_INIT" est obligatoire
"""),

17: _("""
 "resu_final" est obligatoire
"""),

18: _("""
 "maillage_final" est obligatoire
"""),

19: _("""
 3 valeurs pour "TRAN"
"""),

20: _("""
 TYPCAL invalide :  %(k1)s 
"""),

24: _("""
 absence de potentiel permanent
"""),

25: _("""
 le modele fluide n'est pas thermique
"""),

26: _("""
 le modele interface n'est pas thermique
"""),

27: _("""
 modèle fluide incompatible avec le calcul de masse ajoutée
 seules les modelisations PLAN ou 3D ou AXIS sont utilisées
"""),

28: _("""
 on ne trouve pas de champ de température CHTN
"""),

29: _("""
 le nombre d'amortissement modaux est différent du nombre de modes dynamiques
"""),

30: _("""
 il n y a pas le meme nombre de modes retenus
 dans l'excitation modale et dans la base modale
"""),

31: _("""
 il faut autant d'indices en i et j
"""),

32: _("""
 avec SOUR_PRESS et SOUR_FORCE, il faut deux points/ddls d'application
"""),

33: _("""
 mauvais accord entre le nombre d'appuis et le nombre de valeur dans le mot-clé: NUME_ORDRE_I
"""),

34: _("""
 il faut autant de noms de composante que de noms de noeud
"""),

35: _("""
  vous avez oublié de préciser le mode statique
"""),

36: _("""
  mode statique non- nécessaire
"""),

37: _("""
 la fréquence min doit etre plus faible que la fréquence max
"""),

38: _("""
 trop de points dans la liste.
"""),

39: _("""
 segment nul
"""),

40: _("""
 la base locale semble fausse
"""),

41: _("""
 la discrétisation du fond de fissure est grossière par rapport à la courbure du fond de fissure
 possibilité de résultats faux
 il faudrait raffiner le maillage autour du fond de fissure.
"""),

42: _("""
 nombre de points d'intersection impossible.
"""),

43: _("""
 problème de dimension : ni 2D, ni 3D
"""),


45: _("""
 inter douteuse
"""),

46: _("""
 trop de points d'intersection
"""),

47: _("""
 problème de decoupage à 3 points
"""),

48: _("""
 problème de decoupage à 4 points
"""),

49: _("""
 uniquement C_PLAN/D_PLAN disponible pour XFEM
"""),

51: _("""
 mailles manquantes
"""),

52: _("""
 point de FOND_FISS sans maille de surface rattachée
"""),

53: _("""
 problème dans l'orientation des normales a fond_fiss
 verifier la continuité des mailles de FOND_FISS
"""),

54: _("""
 segment de fond_fiss sans maille de surface rattachée
"""),

55: _("""
 augmentez NXMAFI
"""),

56: _("""
  -> Lors de l'enregistrement du champ d'archivage du contact, il s'est avéré
     que les valeurs de contact au noeud %(k1)s différents selon la maille sur
     laquelle se trouve ce noeud.
  -> Risque & Conseil :
     Ce message est normal si le contact est activé sur la fissure.
"""),

61: _("""
  -> Lors de l'orientation des points du fond de fissure, le point du fond de
     fissure initial (PFON_INI) est trop loin du fond de fissure.
  -> Risque & Conseil :
     Le point initial qui en résulte amène surement à une orientation du fond
     de fissure erronée.
     Veuillez redéfinir le point du fond de fissure initial (mot clé PFON_INI).
"""),

62: _("""
 PFON_INI = PT_ORIGINE
"""),

63: _("""
 problème dans l'orientation du fond de fissure : PT_ORIGIN mal choisi.
"""),

64: _("""
 Tous les points du fond de fissure sont des points de bord
 Assurez-vous du bon choix des parametres d'orientation de fissure
 et de PFON_INI.
"""),

65: _("""
 PFON_INI semble etre un point de fin de fissure selon l'orientation choisie
 assurez-vous du bon choix de PFON_INI
"""),

66: _("""
 la methode "UPWIND" est en cours d'implémentation.
"""),

67: _("""
 les aretes de la maille  %(k1)s  ( %(k2)s ) ont  %(k3)s  points d'intersection
 avec l'iso-zéro de  %(k4)s
"""),

68: _("""
 problème pour récupérer AR_MIN dans la table "CARA_GEOM"
"""),

69: _("""
 ARMIN négatif ou nul
"""),

70: _("""
 augmenter NXPTFF
"""),

71: _("""
 le critère de modification de l'enrichissement Heaviside servant à éviter
 les pivots nuls a abouti à un cas de figure qui semble bizarre
 Normalement, on doit avoir un hexaèdre coupé dans un coin (3 points d'intersection)
 On a ici un  %(k1)s avec  %(k2)s  points d'intersection.
"""),

72: _("""
 aucune arete sur laquelle LSN s'annule
"""),

73: _("""
 taille limite D10 non définie
"""),

74: _("""
 échec de la recherche de zéro (NITER)
"""),

75: _("""
 échec de la recherche de zéro (bornes)
"""),

76: _("""
 F(XMIN) non négative
"""),

77: _("""
 f=0 : augmenter ITER_INTE_MAXI
"""),

78: _("""
 polynome non résolu
"""),

79: _("""
 pas d'interpolation possible
"""),

81: _("""
 STOP_SINGULIER=DECOUPE nécessite la subdivision automatique du pas de temps (SUBD_PAS).
"""),
82: _("""
 NMVPIR erreur direction grandissement
 Angle ALPHA %(k1)s
 Angle BETA  %(k2)s
"""),
83: _("""
 Arret par manque de temps CPU.
"""),

85: _("""
 On veut affecter un comportement %(k1)s avec la relation %(k2)s
 sur une maille deja affectée par un autre comportement %(k3)s %(k4)s
"""),
86: _("""
 Perturbation trop petite, calcul impossible
"""),
87: _("""
 Champ déjà existant
 Le champ %(k1)s à l'instant %(r1)g est remplacé par le champ %(k2)s à l'instant %(r2)g avec la précision %(r3)g.
"""),

88: _("""
 arret débordement assemblage : ligne 
"""),

90: _("""
 arret débordement assemblage : colonne 
"""),

92: _("""
 arret nombre de sous-structures invalide : 
 il en faut au minimum : %(i1)d 
 vous en avez défini   : %(i2)d 
"""),

93: _("""
 arret nombre de nom de sous-structures invalide :
 il en faut exactement : %(i1)d 
 vous en avez défini   : %(i2)d 
"""),

94: _("""
 nombre de MACR_ELEM invalide :
 sous_structure %(k1)s
 il en faut exactement : %(i2)d 
 vous en avez défini   : %(i1)d 
"""),

95: _("""
 nombre d'angles nautiques invalide
 sous_structure %(k1)s 
 il en faut exactement :  %(i2)d 
 vous en avez défini   : %(i1)d 
"""),

96: _("""
 nombre de translations invalide
 sous_structure %(k1)s
 il en faut exactement :  %(i2)d 
 vous en avez defini   : %(i1)d 
"""),

97: _("""
 nombre de liaison definies invalide
 il en faut au minimum : %(i2)d 
 vous en avez defini   : %(i1)d 
"""),

98: _("""
 nombre de mot-clés invalide
 numéro liaison: %(i1)d
 mot-clé       : %(k1)s 
 il en faut exactement : %(i3)d 
 vous en avez defini   : %(i2)d 
"""),

99: _("""
 sous-structure indéfinie
 numéro liaison: %(i1)d
 nom sous-structure: %(k1)s 
"""),

}
