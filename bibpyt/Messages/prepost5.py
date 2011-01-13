#@ MODIF prepost5 Messages  DATE 13/01/2011   AUTEUR PELLET J.PELLET 
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

def _(x) : return x

cata_msg = {

1 : _("""
 L'option %(k1)s est deja calculee pour le numero d'ordre %(k2)s.
 On la recalcule car les donnees peuvent etre differentes.
"""),

2 : _("""
Champ inexistant SIEF_ELGA ou SIEF_ELGA numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

3 : _("""
Champ inexistant DEPL numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

4 : _("""
Champ inexistant %(k1)s numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

5 : _("""
Option illicite pour le resultat %(k1)s numero d'ordre %(k2)s pour le calcul de l'option %(k3)s
"""),

6 : _("""
Numero d'ordre trop grand %(k1)s pour le calcul de l'option %(k2)s
"""),

7 : _("""
Option illicite pour le resultat %(k1)s numero d'ordre trop grand %(k2)s pour le calcul de l'option %(k3)s
"""),

8 : _("""

 la taille memoire   necessaire au vecteur de travail dans   lequel nous stockons les composantes   u et v du vecteur tau est trop importante   par rapport a la place disponible.
 taille disponible :  %(i1)d
 taille necessaire :  %(i2)d
"""),

10 : _("""
 le noeud traite  n'est associe a aucune maille volumique.
 numero du noeud =  %(i1)d
 nombre de mailles attachees au noeud =  %(i2)d
"""),

16 : _("""
 appel erronenumero d'ordre %(i1)d code retour de rsexch : %(i2)d
 pb cham_no %(k1)s
"""),

19 : _("""
 nombre de noeud(s) elimine(s) du maillage  %(i1)d
"""),

20 : _("""
 nombre de maille(s) eliminee(s) du maillage  %(i1)d
"""),

21 : _("""
 le numero du groupe de mailles est trop grand:  %(i1)d
  le numero du groupe doit etre inferieur a  %(i2)d
"""),

25 : _("""
  on ne trouve pas la composante  %(k1)s  dans la grandeur  %(k2)s
"""),

30 : _("""
  on ne trouve pas la maille  %(k1)s
"""),

31 : _("""
  on ne trouve pas le groupe  %(k1)s
"""),

32 : _("""
  le groupe  %(k1)s  ne contient aucune maille  %(k2)s
"""),

38 : _("""
  on ne trouve pas le noeud : %(k1)s
"""),

40 : _("""
  le groupe  %(k1)s ne contient aucun noeud  %(k2)s
"""),

41 : _("""
  le parametre  %(k1)s n'existe pas %(k2)s
"""),

45 : _("""
 noeud inconnu dans le fichier  ideas  noeud numero :  %(i1)d
"""),

46 : _("""
 element inconnu dans le fichier ideas element numero :  %(i1)d
"""),

54 : _("""
 probleme dans  nomta traitement de l'instant  %(r1)f
  recuperation de  %(k1)s
  pour le secteur  %(i1)d
"""),

57 : _("""
 probleme dans  nomta traitement de l'instant  %(r1)f
  recuperation de  %(k1)s
"""),

58 : _("""
 probleme dans  nomta traitement de l'instant  %(r1)f
  recuperation "numeli" pour  %(k1)s
"""),

59 : _("""
 probleme dans  nomta traitement de l'instant  %(r1)f
  recuperation "numeli" pour le secteur  %(i1)d
"""),

61 : _("""
 la composante  %(k1)s  n'existe dans aucun des champs %(k2)s
"""),

64 : _("""
 la valeur d'amortissement reduit est trop grande
 la valeur d'amortissement :  %(r1)f
  du mode propre  %(i1)d
  est tronquee au seuil :  %(r2)f
"""),

67 : _("""

 la taille memoire   necessaire au vecteur de travail   est trop importante   par rapport a la place disponible.
 taille disponible :  %(i1)d
 taille necessaire :  %(i2)d
"""),

68 : _("""

 la taille du vecteur  contenant les caracteristiques des   paquets de mailles est trop petite.
 nb de paquets maxi :  %(i1)d
 nb de paquets reels:  %(i2)d
"""),

70 : _("""

 la taille du vecteur  contenant les caracteristiques des   paquets de noeuds est trop petite.
 nb de paquets maxi :  %(i1)d
 nb de paquets reels:  %(i2)d
"""),

73 : _("""
 appel errone  resultat :  %(k1)s   archivage numero :  %(i1)d
   code retour de rsexch :  %(i2)d
   probleme champ :  %(k2)s
"""),

74 : _("""
 on ne trouve pas l'instant  %(r1)f  dans la table  %(k1)s
"""),

75 : _("""
 on trouve   plusieurs instants  %(r1)f  dans la table  %(k1)s
"""),

76 : _("""
 noeud non contenu dans une  maille sachant calculer l" option
 noeud numero :  %(i1)d
"""),

77 : _("""
 *** banque de donnees *** pour le type de geometrie  %(k1)s
  le couple de materiaux  %(k2)s
  ne se trouve pas dans la banque. %(k3)s
"""),

78 : _("""
 le calcul du rayon n'est pas assez precis.cupn0 =  %(r1)f
  cvpn0 =  %(r2)f
 cupn1 =  %(r3)f
  cvpn1 =  %(r4)f
 cupn2 =  %(r5)f
  cvpn2 =  %(r6)f
 flag =  %(i1)d
 cuon1 =  %(r7)f
  cuon2 =  %(r8)f
 cuon3 =  %(r9)f
 cvon1 =  %(r10)f
  cvon2 =  %(r11)f
 cvon3 =  %(r12)f
 rayon =  %(r13)f
  raymin =  %(r14)f
 (rayon - raymin) =  %(r15)f
 ((rayon-raymin)/raymin) =  %(r16)f
"""),

}
