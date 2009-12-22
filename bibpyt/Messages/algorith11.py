#@ MODIF algorith11 Messages  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
La valeur supérieure de KMOD0 est nulle.
On prend la valeur supérieure KMOD.
"""),

2 : _("""
La valeur supérieure de KMOD est nulle.
"""),

3 : _("""
La variable AMOR est nulle.
"""),

5 : _("""
La force normale est nulle.
"""),

6 : _("""
La somme des impacts écrouissage est inférieure à la somme des glissements.
"""),

7 : _("""
NOM_CAS n'est pas une variable d'accès d'un résultat de type EVOL_THER.
"""),

8 : _("""
NUME_MODE n'est pas une variable d'accès d'un résultat de type EVOL_THER.
"""),

9 : _("""
NUME_MODE n'est pas une variable d'accès d'un résultat de type MULT_ELAS.
"""),

10 : _("""
INST n'est pas une variable d'accès d'un resultat de type MULT_ELAS.
"""),

11 : _("""
NOM_CAS n'est pas une variable d'accès d'un resultat de type FOURIER_ELAS.
"""),

12 : _("""
INST n'est pas une variable d'accès d'un resultat de type FOURIER_ELAS.
"""),

13 : _("""
NOM_CAS n'est pas une variable d'accès d'un resultat de type FOURIER_THER.
"""),

14 : _("""
INST n'est pas une variable d'accès d'un resultat de type FOURIER_THER.
"""),

15 : _("""
Le mot-clef RESU_INIT est obligatoire.
"""),

16 : _("""
Le mot-clef MAILLAGE_INIT est obligatoire.
"""),

17 : _("""
Le mot-clef RESU_FINAL est obligatoire.
"""),

18 : _("""
Le mot-clef MAILLAGE_FINAL est obligatoire.
"""),

24 : _("""
Absence de potentiel permanent.
"""),

25 : _("""
Le modèle fluide n'est pas thermique.
"""),

26 : _("""
Le modèle interface n'est pas thermique.
"""),

27 : _("""
Le modèle fluide est incompatible avec le calcul de masse ajoutée.
Utilisez les modélisations PLAN ou 3D ou AXIS.
"""),

29 : _("""
Le nombre d'amortissement modaux est différent du nombre de modes dynamiques.
"""),

30 : _("""
Il n y a pas le même nombre de modes retenus  dans l'excitation modale et 
dans la base modale
"""),

31 : _("""
Il faut autant d'indices en i et j.
"""),

32 : _("""
Avec SOUR_PRESS et SOUR_FORCE, il faut deux points par degré de liberté d'application
"""),

33 : _("""
Mauvais accord entre le nombre d'appuis et le nombre de valeur dans le mot-clé NUME_ORDRE_I
"""),

34 : _("""
Il faut autant de noms de composantes que de noms de noeuds.
"""),

35 : _("""
Précisez le mode statique.
"""),

36 : _("""
Le mode statique n'est pas nécessaire.
"""),

37 : _("""
La fréquence minimale doit être plus petite que la fréquence maximale.
"""),

73 : _("""
Le paramètre matériau taille limite D10 n'est pas défini.
"""),

74 : _("""
Echec de la recherche de zéro (nombre maximum d'itérations atteint).
"""),

75 : _("""
Echec de la recherche de zéro (bornes atteintes).
"""),

76 : _("""
La valeur de F(XMIN) doit être négative.
"""),

77 : _("""
Augmentez ITER_INTE_MAXI.
"""),

79 : _("""
Pas d'interpolation possible.
"""),

81 : _("""
STOP_SINGULIER=DECOUPE nécessite la subdivision automatique du pas de temps (SUBD_PAS).
"""),

82 : _("""
Erreur de la direction de glissmeent dans NMVPIR.
 Angle ALPHA: %(k1)s
 Angle BETA : %(k2)s
"""),

83 : _("""
Arrêt par manque de temps CPU.
"""),

86 : _("""
La perturbation est trop petite, calcul impossible.
"""),

87 : _("""
Champ déjà existant
Le champ %(k1)s à l'instant %(r1)g est remplacé par le champ %(k2)s à l'instant %(r2)g avec la précision %(r3)g.
"""),

88 : _("""
Arrêt débordement assemblage : ligne. 
"""),

90 : _("""
Arrêt débordement assemblage : colonne. 
"""),

92 : _("""
Arrêt pour nombre de sous-structures invalide : 
 Il en faut au minimum : %(i1)d 
 Vous en avez défini   : %(i2)d 
"""),

93 : _("""
Arrêt pour nombre de noms de sous-structures invalide :
 Il en faut exactement : %(i1)d 
 Vous en avez défini   : %(i2)d 
"""),

94 : _("""
Arrêt pour nombre de MACR_ELEM invalide :
 Sous-structure %(k1)s
 Il en faut exactement : %(i2)d 
 Vous en avez défini   : %(i1)d 
"""),

95 : _("""
Arrêt pour nombre d'angles nautiques invalide :
 Sous-structure %(k1)s 
 Il en faut exactement : %(i2)d 
 Vous en avez défini   : %(i1)d 
"""),

96 : _("""
Arrêt pour nombre de translations invalide :
 Sous-structure %(k1)s 
 Il en faut exactement : %(i2)d 
 Vous en avez défini   : %(i1)d  
"""),

97 : _("""
Arrêt pour nombre de liaisons définies invalide :
 Il en faut exactement : %(i2)d 
 Vous en avez défini   : %(i1)d 
"""),

98 : _("""
Arrêt pour nombre de mot-clés invalide :
 Numéro liaison : %(i1)d
 Mot-clé        : %(k1)s 
 Il en faut exactement : %(i3)d 
 Vous en avez défini   : %(i2)d
"""),

99 : _("""
Arrêt pour sous-structure indéfinie :
 Numéro liaison    : %(i1)d
 Nom sous-structure: %(k1)s 
"""),

}
