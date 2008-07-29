#@ MODIF calculel4 Messages  DATE 28/07/2008   AUTEUR PELLET J.PELLET 
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

8 : _("""
 le resultat  %(k1)s  n'existe pas
"""),

10 : _("""
 Erreur d'utilisation :
   On ne trouve pas de variables de commandes ('TEMP', 'HYDR', ...) :
   Option: %(k2)s  type_element: %(k3)s )

 Risques & conseils :
   La cause la plus fréquente de cette erreur est d'avoir oublié de
   renseigner AFFE_MATERIAU/AFFE_VARC.
   (Ou de n'avoir renseigné que AFFE_VARC/VALE_REF sans avoir renseigné EVOL ou CHAMP_GD)
"""),

11 : _("""
 Erreur d'utilisation lors de l'affectation des variables de commande (AFFE_MATERIAU/AFFE_VARC):
   Pour la variable de commande %(k1)s,
   Vous avez oublié d'utiliser l'un des 2 mots clés CHAMP_GD ou EVOL.
   L'abscence de ces 2 mots clés n'est permise que pour NOM_VARC='TEMP' (modélisations THM)
"""),

12 : _("""
 Erreur de programmation (catalogue des éléments finis) :
 Les éléments finis ayant l'attribut VOLU_FINI='OUI' ne peuvent créer que des
 matrices élémentaires non-symétriques.
"""),

13 : _("""
 Erreur d'utilisation (AFFE_MATERIAU/AFFE_VARC) :
  Le maillage associé au calcul (%(k1)s) est différent de celui associé
  aux champs (ou evol_xxxx) affectés dans AFFE_MATERIAU/AFFE_VARC (%(k2)s).

 Conseil :
  Il faut corriger AFFE_MATERIAU.
"""),




41 : _("""
 erreur_01
"""),

42 : _("""
 erreur_02
"""),

43 : _("""
 le nom_para n'existe pas
"""),

44 : _("""
 0 ligne trouvee pour le nom_para
"""),

45 : _("""
 plusieurs lignes trouvees
"""),

46 : _("""
 code retour de "tbliva" inconnu
"""),

47 : _("""
 type_resu inconnu:  %(k1)s
"""),

48 : _("""
 erreur calcul alpha0 :champ depl elastique non trouve
"""),

49 : _("""
 erreur : le champ depl elastique n'existe pas
"""),

50 : _("""
 erreur: le champ sief_elga_depl n'existe pas
"""),

51 : _("""
 methode zac : accommodation et chargement non radial --> methode non appliquable
"""),

53 : _("""
 longueurs des modes locaux incompatibles entre eux.
"""),

54 : _("""
 aucuns noeuds sur lesquels projeter.
"""),

55 : _("""
 pas de mailles a projeter.
"""),

56 : _("""
  %(k1)s  pas trouve.
"""),

57 : _("""
 il n'y a pas de mailles a projeter.
"""),

58 : _("""
 les maillages a projeter sont ponctuels.
"""),

59 : _("""
 maillages 1 differents.
"""),

60 : _("""
 maillages 2 differents.
"""),

61 : _("""
 probleme dans l'examen de  %(k1)s
"""),

62 : _("""
 aucun numero d'ordre dans  %(k1)s
"""),

63 : _("""
 On n'a pas pu projeter le champ %(k1)s de la sd_resultat %(k2)s
 vers la sd_resultat %(k3)s pour le numéro d'ordre %(i1)d
"""),

64 : _("""
 Aucun champ projete.
"""),

65 : _("""
  maillages non identiques :  %(k1)s  et  %(k2)s
"""),

66 : _("""
 pas de chmate
"""),

67 : _("""
 erreur dans etanca pour le probleme primal
"""),

68 : _("""
 erreur dans etenca pour le probleme dual
"""),

69 : _("""
 On ne trouve pas la variable de commande :  %(k1)s  pour la maille:  %(k2)s
"""),

79 : _("""
 La grandeur :  %(k1)s  n'existe pas dans le catalogue des grandeurs.
"""),

80 : _("""
 le nom de la grandeur  %(k1)s  ne respecte pas le format xxxx_c
"""),

81 : _("""
 probleme dans le catalogue des grandeurs simples, la grandeur complexe %(k1)s
 ne possede pas le meme nombre de composantes que son homologue réelle %(k2)s
"""),

82 : _("""
 Probleme dans le catalogue des grandeurs simples, la grandeur %(k1)s
 ne possede pas les memes champs que son homologue reelle %(k2)s
"""),

83 : _("""
 erreur: le calcul des contraintes ne fonctionne que pour le phenomene mecanique
"""),

84 : _("""
 erreur numeros des noeuds bords
"""),

85 : _("""
 erreur: les elements supportes sont tria3 ou tria6
"""),

86 : _("""
 erreur: les elements supportes sont quad4 ou quad8 ou quad9
"""),

87 : _("""
 maillage mixte tria-quad non supporte pour l estimateur zz2
"""),

88 : _("""
 erreur: les mailles supportees sont tria ou quad
"""),

89 : _("""
 Erreur: un element du maillage possede tous ses sommets sur une frontiere.
 Il faut au moins un sommet interne.
 Pour pouvoir utiliser ZZ2 il faut remailler le coin de telle facon que
 tous les trg aient au moins un sommet interieur.
"""),

91 : _("""
 On ne trouve pas de routine te0npq.
 npq doit etre compris entre 1 et 600 ici : npq = %(k1)s
"""),

92 : _("""
  relation :  %(k1)s  non implantee sur les poulies
"""),

93 : _("""
  deformation :  %(k1)s  non implantee sur les poulies
"""),

94 : _("""
 l'attribut:  %(k1)s  n'existe pas pour le type:  %(k2)s
"""),

95 : _("""
 Erreur de programmation ou d'utilisation :
   On ne trouve pas dans les arguments de la routine calcul de champ a associer
   au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

96 : _("""
 Erreur de programmation :
 on n'a pas pu extraire toutes les cmps voulues du champ global associe
 au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

97 : _("""
 TOUT = OUI obligatoire avec  %(k1)s
"""),

98 : _("""
 on n'a pas pu récupérer le paramètre THETA dans le résultat  %(k1)s
 valeur prise pour THETA: 0.57
"""),

99 : _("""
 récupération d'une valeur de THETA illicite dans le résultat  %(k1)s
 valeur prise pour THETA: 1.
"""),

}
