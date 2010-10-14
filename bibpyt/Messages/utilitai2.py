#@ MODIF utilitai2 Messages  DATE 11/10/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

4 : _("""
 Il y a un problème pour récupérer les variables d'accès.
"""),

5 : _("""
 Seules les variables d'accès réelles sont traitées.
"""),

6 : _("""
 Seuls les paramètres réels sont traités.
"""),

7 : _("""
 L'unité logique est inexistante.
"""),

8 : _("""
  Les fonctions à une seule variable sont admises.
"""),

10 : _("""
  Les fonctions de type " %(k1)s " ne sont pas encore imprimées.
"""),

11 : _("""
  Les fonctions de type " %(k1)s " ne sont pas imprimées.
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
  Le fichier " %(k1)s " n'est relié a aucune unité logique.
"""),




52 : _("""
 ajout de l'option "SIEF_ELGA_DEPL", les charges sont-elles correctes ?
"""),

53 : _("""
 Le nombre maximum d'iterations est atteint.
"""),

54 : _("""
  La dimension de l'espace doit être inférieur ou égal à 3.
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
  Pour le format ENSIGHT, le mot-clé MODELE est obligatoire.
"""),

89 : _("""
  Le format ENSIGHT n'accepte que le champ de type PRES.
"""),

91 : _("""
  Le type d'élément %(k1)s n'est pas prevu.
"""),

94 : _("""
  Le champ %(k1)s n'est pas prévu.
  Vous pouvez demander l'évolution.
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

}
