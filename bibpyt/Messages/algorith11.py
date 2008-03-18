#@ MODIF algorith11 Messages  DATE 18/03/2008   AUTEUR CNGUYEN C.NGUYEN 
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
 le sup de KMOD0 est nul
 on prend le sup de KMOD
"""),

2 : _("""
 le sup de KMOD est nul.
"""),

3 : _("""
 la variable AMOR est nulle
"""),

4 : _("""
 erreur de dimension (dvlp)
"""),

5 : _("""
 force normale nulle.
"""),

6 : _("""
 somme des "impacts-ecrouissage" < somme des "glissement"
"""),

7 : _("""
 "NOM_CAS" n'est pas une variable d'accès d'un résultat de type "EVOL_THER".
"""),

8 : _("""
 "NUME_MODE" n'est pas une variable d'accès d'un résultat de type "EVOL_THER".
"""),

9 : _("""
 "NUME_MODE" n'est pas une variable d'accès d'un résultat de type "MULT_ELAS".
"""),

10 : _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "MULT_ELAS".
"""),

11 : _("""
 "NOM_CAS" n'est pas une variable d'accès d'un resultat de type "FOURIER_ELAS".
"""),

12 : _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "FOURIER_ELAS".
"""),

13 : _("""
 "NOM_CAS" n'est pas une variable d'accès d'un resultat de type "FOURIER_THER".
"""),

14 : _("""
 "INST" n'est pas une variable d'accès d'un resultat de type "FOURIER_THER".
"""),

15 : _("""
 "RESU_INIT" est obligatoire
"""),

16 : _("""
 "MAILLAGE_INIT" est obligatoire
"""),

17 : _("""
 "resu_final" est obligatoire
"""),

18 : _("""
 "maillage_final" est obligatoire
"""),

20 : _("""
 TYPCAL invalide :  %(k1)s 
"""),

24 : _("""
 absence de potentiel permanent
"""),

25 : _("""
 le modele fluide n'est pas thermique
"""),

26 : _("""
 le modele interface n'est pas thermique
"""),

27 : _("""
 modèle fluide incompatible avec le calcul de masse ajoutée
 seules les modelisations PLAN ou 3D ou AXIS sont utilisées
"""),




29 : _("""
 le nombre d'amortissement modaux est différent du nombre de modes dynamiques
"""),

30 : _("""
 il n y a pas le meme nombre de modes retenus
 dans l'excitation modale et dans la base modale
"""),

31 : _("""
 il faut autant d'indices en i et j
"""),

32 : _("""
 avec SOUR_PRESS et SOUR_FORCE, il faut deux points/ddls d'application
"""),

33 : _("""
 mauvais accord entre le nombre d'appuis et le nombre de valeur dans le mot-clé: NUME_ORDRE_I
"""),

34 : _("""
 il faut autant de noms de composante que de noms de noeud
"""),

35 : _("""
  vous avez oublié de préciser le mode statique
"""),

36 : _("""
  mode statique non- nécessaire
"""),

37 : _("""
 la fréquence min doit etre plus faible que la fréquence max
"""),

73 : _("""
 le parametre materiau taille limite d10 n'est pas defini
"""),

74 : _("""
 échec de la recherche de zéro (NITER)
"""),

75 : _("""
 échec de la recherche de zéro (bornes)
"""),

76 : _("""
 La valeur de F(XMIN) doit être négative.
"""),

77 : _("""
 f=0 : augmenter ITER_INTE_MAXI
"""),

79 : _("""
 pas d'interpolation possible
"""),

81 : _("""
 STOP_SINGULIER=DECOUPE nécessite la subdivision automatique du pas de temps (SUBD_PAS).
"""),

82 : _("""
 NMVPIR erreur direction grandissement
 Angle ALPHA %(k1)s
 Angle BETA  %(k2)s
"""),

83 : _("""
 Arret par manque de temps CPU.
"""),

85 : _("""
 On veut affecter un comportement %(k1)s avec la relation %(k2)s
 sur une maille deja affectée par un autre comportement %(k3)s %(k4)s
"""),

86 : _("""
 Perturbation trop petite, calcul impossible
"""),

87 : _("""
 Champ déjà existant
 Le champ %(k1)s à l'instant %(r1)g est remplacé par le champ %(k2)s à l'instant %(r2)g avec la précision %(r3)g.
"""),

88 : _("""
 arret débordement assemblage : ligne 
"""),

90 : _("""
 arret débordement assemblage : colonne 
"""),

92 : _("""
 arret nombre de sous-structures invalide : 
 il en faut au minimum : %(i1)d 
 vous en avez défini   : %(i2)d 
"""),

93 : _("""
 arret nombre de nom de sous-structures invalide :
 il en faut exactement : %(i1)d 
 vous en avez défini   : %(i2)d 
"""),

94 : _("""
 nombre de MACR_ELEM invalide :
 sous_structure %(k1)s
 il en faut exactement : %(i2)d 
 vous en avez défini   : %(i1)d 
"""),

95 : _("""
 nombre d'angles nautiques invalide
 sous_structure %(k1)s 
 il en faut exactement :  %(i2)d 
 vous en avez défini   : %(i1)d 
"""),

96 : _("""
 nombre de translations invalide
 sous_structure %(k1)s
 il en faut exactement :  %(i2)d 
 vous en avez defini   : %(i1)d 
"""),

97 : _("""
 nombre de liaison definies invalide
 il en faut au minimum : %(i2)d 
 vous en avez defini   : %(i1)d 
"""),

98 : _("""
 nombre de mot-clés invalide
 numéro liaison: %(i1)d
 mot-clé       : %(k1)s 
 il en faut exactement : %(i3)d 
 vous en avez defini   : %(i2)d 
"""),

99 : _("""
 sous-structure indéfinie
 numéro liaison: %(i1)d
 nom sous-structure: %(k1)s 
"""),

}
