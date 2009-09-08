#@ MODIF utilitai2 Messages  DATE 07/09/2009   AUTEUR PELLET J.PELLET 
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

4 : _("""
 problème pour récupérer les variables d'accès
"""),

5 : _("""
 on ne traite que des variables d'accès réelles
"""),

6 : _("""
 on ne traite que des paramètres réels
"""),

7 : _("""
 unite logique inexistante
"""),

8 : _("""
 fonction a une seule variable admis
"""),

10 : _("""
 on n'imprime pas encore de fonction de type " %(k1)s "      desole.
"""),

11 : _("""
 on ne sait pas imprimer une fonction de type " %(k1)s "      desole.
"""),

12 : _("""
 interpolation sur paramètres non permise
"""),

13 : _("""
 interpolation " %(k1)s " inconnue
"""),

14 : _("""
 " %(k1)s " type de fonction inconnu
"""),

16 : _("""
 interpolation non permise
"""),

17 : _("""
 on ne connait pas ce type d'interpolation:  %(k1)s
"""),

31 : _("""
 on ne trouve pas l'equation  %(k1)s  dans le "prof_chno"
"""),

35 : _("""
 il y a un bug
"""),

36 : _("""
 group_ma_interf: un element n'est ni tria3 ni tria6 ni quad4 ni quad8
"""),

37 : _("""
 group_ma_flu_str: un element n'est ni tria3 ni tria6 ni quad4 ni quad8
"""),

38 : _("""
 group_ma_flu_sol: un element n'est ni tria3 ni tria6 ni quad4 ni quad8
"""),

39 : _("""
 group_ma_sol_sol: un element n'est ni tria3 ni tria6 ni quad4 ni quad8
"""),





47 : _("""
 le fichier " %(k1)s " n'est relie a aucune unite logique.
"""),




52 : _("""
 ajout de l'option "SIEF_ELGA_DEPL", les charges sont-elles correctes ?
"""),

53 : _("""
 nombre max d'iterations atteint
"""),

54 : _("""
 la dimension d'espace doit etre <= 3
"""),

55 : _("""
 les points du nuage de depart sont tous en (0.,0.,0.).
"""),

56 : _("""
 le nuage de depart est vide.
"""),

57 : _("""
 les points du nuage de depart sont tous confondus.
"""),

58 : _("""
 les points du nuage de depart sont tous alignes.
"""),

59 : _("""
 les points du nuage de depart sont tous coplanaires.
"""),

60 : _("""
 methode inconnue :  %(k1)s
"""),

61 : _("""
 le descripteur_grandeur de compor ne tient pas sur un seul entier_code
"""),

62 : _("""
 erreur dans etenca
"""),

63 : _("""
 la composante relcom n'a pas ete affectee pour la grandeur compor
"""),

66 : _("""
 pas assez de valeurs dans la liste.
"""),

67 : _("""
 il faut des triplets de valeurs.
"""),

68 : _("""
 il n'y a pas un nombre pair de valeurs.
"""),

69 : _("""
 nombre de valeurs different  pour "noeud_para" et "vale_y"
"""),

70 : _("""
 il manque des valeurs dans  %(k1)s  ,liste plus petite que  %(k2)s
"""),

71 : _("""
 interpolation "log" et valeurs negatives sont incompatibles !
"""),

72 : _("""
 parametres non croissants
"""),

73 : _("""
 deux fonctions differentes affectee a la meme valeur de parametre.
"""),

74 : _("""
 deux listes de valeurs differentes affectee a la meme valeur de parametre.
"""),

75 : _("""
 les listes nume_lign et liste_x doivent contenir le meme nombre de termes
"""),

76 : _("""
 les noms des paramètres doivent etre différents
"""),

77 : _("""
 les listes d'abscisses et d'ordonnées doivent etre de memes longueurs
"""),

78 : _("""
 fonction incompatible avec  %(k1)s
"""),

79 : _("""
 les noms de chaque parametre doivent etre differents
"""),

80 : _("""
 un seul nume_ordre !!!
"""),

83 : _("""
 les noeuds debut et fin n appartiennent pas au maillage.
"""),

84 : _("""
 la fonction doit s appuyee sur un maillage pour lequel une abscisse curviligne est definie.
"""),

85 : _("""
 mauvaise definition des noeuds debut et fin
"""),

86 : _("""
 le nombre de champs à lire est supérieur a 100
"""),

87 : _("""
  -> Le maillage doit etre issu d'IDEAS pour garantir la cohérence entre
     le maillage et les résultats lus.

  -> Risque & Conseil :
     Vous récupérez des résultats au format IDEAS, ces résultats sont donnés
     aux noeuds par leur nom, et/ou aux mailles par leurs noms. Il faut
     vérifier que les résultats lus ont été obtenus avec le meme maillage
     que celui lu par aster (LIRE_MAILLAGE).
"""),

88 : _("""
  le mot clé MODELE est obligatoire pour un champ de type CHAM_ELEM
"""),

89 : _("""
  le format ENSIGHT n'accepte que le champ PRES
"""),

90 : _("""
 nflag etant egal a 0, on ne peut pas avoir plus d'un instant.
"""),

91 : _("""
 element non prevu  %(k1)s
"""),

93 : _("""
 evol_ther - champ temp uniquement
"""),

94 : _("""
 Champ non prevu :  %(k1)s
 Demandez l'évolution
"""),

95 : _("""
  %(k1)s  et  %(k2)s  : nombre de composantes incompatible.
"""),

97 : _("""
Erreur Utilisateur :
  On n'a pu lire aucun champ dans le fichier.
  La structure de données créée est vide.

Risques & Conseils :
  Si le fichier lu est au format Ideas, et si la commande est LIRE_RESU,
  le problème vient peut-etre d'une mauvaise utilisation (ou d'une abscence d'utilisation)
  du mot clé FORMAT_IDEAS. Il faut examiner les "entetes" des DATASET du fichier à lire.
"""),

98 : _("""
 on n'a pas lu tous les champs.
"""),

}
