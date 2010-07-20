#@ MODIF modelisa4 Messages  DATE 19/07/2010   AUTEUR PELLET J.PELLET 
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
  -> Le modèle contient un mélange d'éléments finis 2D (plan XOY) et 3D

  -> Risque & Conseil :
     Sur ce genre de modèle, on ne sait pas déterminer s'il est 2D ou 3D.
     Parfois, cela empeche de faire le "bon choix".
"""),

5 : _("""
 mot-cle  %(k1)s  interdit en 2d
"""),

6 : _("""
  -> Phase de vérification du maillage : présence de noeuds orphelins.
     Les noeuds orphelins sont des noeuds qui n'appartiennent à aucune maille.
"""),

7 : _("""
 certains noeuds connectent plus de 200 mailles. ces mailles ne sont pas verifiees.
"""),

8 : _("""
  -> Phase de vérification du maillage : présence de mailles doubles (ou triples, ...)
     Les mailles multiples sont des mailles de noms différents qui ont la meme connectivité
     (elles s'appuient sur les memes noeuds).

  -> Risque & Conseil :
     Le risque est de modéliser 2 fois (ou plus) l'espace. On peut par exemple avoir
     un modèle 2 fois trop lourd ou 2 fois trop rigide.
     Remarque : les mailles concernées sont imprimées dans le fichier "message".
     Sur ce maillage, il est imprudent d'affecter des quantités avec le mot clé TOUT='OUI'.
"""),

9 : _("""
  -> Phase de vérification du maillage : présence de mailles aplaties.

  -> Risque & Conseil :
     Vérifiez votre maillage. La présence de telles mailles peut conduire à des
     problèmes de convergence et nuire à la qualité des résultats.
"""),

10 : _("""
 - chckma phase de verification du maillage - mailles degenerees
"""),






13 : _("""
 seule la grandeur neut_f est traitee actuellement.
"""),

14 : _("""
 les champs de cham_f et cham_para n'ont pas la meme discretisation noeu/cart/elga/elno/elem.
"""),






16 : _("""
 avec "noeud_cmp", il faut donner un nom et une composante.
"""),

17 : _("""
 pour recuperer le champ de geometrie, il faut utiliser le mot cle maillage
"""),

18 : _("""
 le mot-cle type_champ =  %(k1)s n'est pas coherent avec le type du champ extrait :  %(k2)s _ %(k3)s
"""),

19 : _("""
 on ne peut extraire qu'1 numero d'ordre. vous en avez specifie plusieurs.
"""),

24 : _("""
 arret sur erreur(s), normale non sortante
"""),

25 : _("""
  la liste : %(k1)s  a concatener avec la liste  %(k2)s  doit exister
"""),

26 : _("""
  on ne peut pas affecter la liste de longueur nulle %(k1)s  a la liste  %(k2)s  qui n'existe pas
"""),

27 : _("""
 la concatenation de listes de type  %(k1)s  n'est pas encore prevue.
"""),

28 : _("""
 <coefal> le numero de correlation et/ou le type de reseau passes dans le fichier de commande ne  sont pas coherents avec le fichier .70
"""),

29 : _("""
 <coefam> le numero de correlation et/ou le type de reseau passes dans le fichier de commande ne  sont pas coherents avec le fichier .70
"""),

30 : _("""
 <coefam> ce type de reseau n est pas encore implante dans le code
"""),

31 : _("""
 <coefra> le numero de correlation et/ou le type de reseau passes dans le fichier de commande ne  sont pas coherents avec le fichier .71
"""),

32 : _("""
 <coefra> ce type de reseau n est pas encore implante dans le code
"""),

33 : _("""
 <coefrl> le numero de correlation et/ou le type de reseau passes dans le fichier de commande ne  sont pas coherents avec le fichier .71
"""),

35 : _("""
 jacobien negatif
"""),

36 : _("""
 La normale de la maille %(k1)s est nulle
"""),

37 : _("""
 on essaie de creer ou d'agrandir le ligrel de charge avec un nombre de termes negatif ou nul
"""),





39 : _("""
 probleme rencontre lors de l interpolation d une des deformees modales
"""),

40 : _("""
 probleme rencontre lors de l interpolation d une des fonctions
"""),

41 : _("""
 probleme dans le cas 3d ou les noeuds sont alignes, la distance separant 2 noeuds non-identiques de la liste est trop petite
"""),

42 : _("""
  -> Mélange de mailles quadratiques avec des QUAD8. Aster supprime la liaison
     sur le noeud milieu des QUAD8
  -> Risque & Conseil :
     Le problème de contact avec des mailles quadratiques est mal traité dans Aster, vous risquez d'obtenir des
     pressions de contact oscillantes entre noeuds milieux et noeuds sommets. Essayez, dans la mesure du possible,
     d'utiliser des éléments linéaires.
"""),

43 : _("""
 incoherence car aucun noeud n'a de ddl drz et la routine traite le cas 2d ou il y a au-moins un ddl drz
"""),

44 : _("""
 incoherence car aucun noeud n'a de ddl derotation drx et dry et drz et la routine traite le cas 3d ou il y a au-moins un noeud ayant ces 3 ddls
"""),

















50 : _("""
 la maille :  %(k1)s  n'est pas affectee par un element fini.
"""),






53 : _("""
 le noeud d application de l excitation n est pas un noeud du maillage.
"""),

54 : _("""
 le noeud d application de l excitation ne doit pas etre situe au bord du domaine de definition du maillage.
"""),

55 : _("""
 la fenetre excitee deborde du domaine de definition du maillage.
"""),

56 : _("""
 la demi-fenetre excitee en amont du noeud central d application n est pas definie.
"""),

57 : _("""
 la demi-fenetre excitee en amont du noeud central d application deborde du domaine de definition du maillage.
"""),

58 : _("""
 les demi-fenetres excitees en aval et en amont du noeud central d application ne sont pas raccordees.
"""),

59 : _("""
 la demi-fenetre excitee en aval du noeud central d application n est pas definie.
"""),

60 : _("""
 la demi-fenetre excitee en aval du noeud central d application deborde du domaine de definition du maillage.
"""),

61 : _("""
 les fonctions interpretees doivent etre tabulees auparavant
"""),

62 : _("""
 nappe interdite pour definir le flux
"""),

63 : _("""
  on deborde a gauche
"""),

64 : _("""
 prolongement gauche inconnu
"""),

65 : _("""
  on deborde a droite
"""),

66 : _("""
 prolongement droite inconnu
"""),

67 : _("""
  on est en dehors des bornes
"""),

68 : _("""
 les mailles de type  %(k1)s ne sont pas traitees pour la selection des noeuds
"""),

69 : _("""
 Erreur d'utilisation :
   On cherche à nommer un objet en y insérant un numéro.
   Le numéro %(i1)d est trop grand vis à vis de la chaine de caractère.

 Risque et Conseil :
   Vous avez atteint la limite de ce que sait faire le code
   (trop de poursuites, de pas de temps, de pas d'archivage, ...)
"""),

70 : _("""
 erreur : deux noeuds du cable sont confondus on ne peut pas definir le cylindre.
"""),

71 : _("""
 immersion du cable no %(k1)s  dans la structure beton : le noeud  %(k2)s  se trouve a l'exterieur de la structure
"""),

72 : _("""
 maille degeneree
"""),








76 : _("""
 le vecteur normal est dans le plan tangent
"""),

77 : _("""
  %(k1)s  mot cle lu " %(k2)s " incompatible avec " %(k3)s "
"""),

78 : _("""
 lecture 1 :erreur de lecture pour %(k1)s
"""),

79 : _("""
 lecture 1 :item > 24 car  %(k1)s
"""),

80 : _("""
  %(k1)s  le groupe  %(k2)s  est vide
"""),

81 : _("""
  %(k1)s  erreur de syntaxe : mot cle " %(k2)s " non reconnu
"""),

82 : _("""
  %(k1)s  mot cle " %(k2)s " ignore
"""),

83 : _("""
  le vecteur est perpendiculaire à la poutre.
"""),

84 : _("""
  La poutre présente une ou plusieurs branches: cas non permis.
  Essayez de créer des groupes de mailles différents pour
  chaque branche et de les orienter indépendemment.
"""),





89 : _("""
 mot cle wohler non trouve
"""),




91 : _("""
 mot cle manson_coffin non trouve
"""),

92 : _("""
 lecture 1 : ligne lue trop longue : %(k1)s
"""),

93 : _("""
  Probleme lors de la lecture du fichier maillage
  numéro de la derniere ligne traitée : %(i1)d

  -> Risque & Conseil :
  Verifiez si le mot clé FIN est présent à la fin du fichier.
"""),

94 : _("""
  Probleme lors de la lecture du fichier maillage
  Le fichier à lire est vide.

  -> Risque & Conseil :
  Vérifiez la valeur mise derrière le mot clé UNITE et
  que cette valeur par défaut correspond au type "mail" dans ASTK
"""),











97 : _("""
 le nom du groupe  %(k1)s  est tronque a 8 caracteres
"""),

98 : _("""
 il faut un nom apres "nom="
"""),




}
