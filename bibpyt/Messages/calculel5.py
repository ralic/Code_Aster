#@ MODIF calculel5 Messages  DATE 05/10/2009   AUTEUR DESOZA T.DESOZA 
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

2 : _("""
 pour les options de thermique, il y a encore a travailler !!
"""),

3 : _("""
Erreur Utilisateur :
  On cherche à regrouper des éléments finis en "paquets" de façon à ce que la taille de leurs
  matrices élémentaires ne puisse pas dépasser la taille des blocs définis dans :
  DEBUT / MEMOIRE / TAILLE_BLOC

  Malheureusement, la taille indiquée ne permet pas de mettre 1 seul élément dans le paquet.

Conseil :
  Il faut augmenter la valeur du paramètre  DEBUT / MEMOIRE / TAILLE_BLOC
"""),

4 : _("""
 !! probleme creation cham_elem nul dans alchml !!
"""),












8 : _("""
 il faut renseigner le mot clé MAILLE
"""),

10 : _("""
 nbnocp est trop grand, contacter l'assistance
"""),

11 : _("""
 le parametre est a valeurs de type  " %(k1)s "  et la valeur de reference de type  " %(k2)s ".
"""),

12 : _("""
 TYPE_TEST inconnu
"""),

13 : _("""
 le champ  %(k1)s  est a valeurs de type  " %(k2)s "  et la valeur de reference de type  " %(k3)s ".
"""),

14 : _("""
 le champ  %(k1)s  est de type inconnu.
"""),










20 : _("""
 le GROUP_NO  %(k1)s  contient  %(k2)s  noeuds
"""),

21 : _("""
 le GROUP_MA  %(k1)s  contient  %(k2)s  mailles
"""),














27 : _("""
 ! pas de lumpe en 3d p2: hexa20_d --> face8_d !
"""),






















34 : _("""
 ! p2 obligeatoire avec terme source non nul !
"""),






38 : _("""
  il faut definir un champ de vitesse
"""),

39 : _("""
 la grandeur pour la variable:  %(k1)s  doit etre:  %(k2)s  mais elle est:  %(k3)s
"""),

40 : _("""
 Erreur utilisateur.
 Le champ de variables internes initiales n'a pas le bon nombre de sous-points.
 Maille : %(k1)s
 Nombre de sous-points a l'instant '-' : %(i1)d
 Nombre de sous-points a l'instant '+' : %(i2)d
"""),

41 : _("""
 pas de variables internes initiales pour la maille  %(k1)s
"""),

42 : _("""
 comportements incompatibles :  %(k1)s  et  %(k2)s  pour la maille  %(k3)s
"""),

43 : _("""
PROJ_CHAMP (ou LIAISON_MAILLE) :
  Le noeud %(k1)s de coordonnées (%(r1)f,%(r2)f,%(r3)f) est projeté à la distance %(r4)f"""),

44 : _("""
 ! le champ doit etre un cham_elem !
"""),

45 : _("""
 ! longueurs des modes locaux imcompatibles entre eux !
"""),

46 : _("""
 ! terme normalisation global nul !
"""),

48 : _("""
 PROJ_CHAMP (ou LIAISON_MAIL) :
 Nombre de noeuds projetés sur des mailles un peu distantes : %(i1)d.
 (la distance à la maille est supérieure à 1/10ième du diamètre de la maille)

 Le noeud %(k1)s est projeté le plus loin à la distance %(r1)f"""),


49 : _("""
 LIAISON_MAIL :
 La relation linéaire destinée à éliminer le noeud esclave %(k1)s est une tautologie
 car la maille maitre en vis à vis de ce noeud possède ce meme noeud dans sa connectivité.
 On ne l'écrit donc pas.
"""),

50 : _("""
 Présence de coques orthotropes, les mots clés ANGL_REP ou VECTEUR
 du mot clé facteur REPE_COQUE ne sont pas traités.
"""),

51 : _("""
 Le repère de post-traitement a été défini dans la commande AFFE_CARA_ELEM mot clé facteur COQUE.
 Il est conseillé de définir ce repère à l'aide du mot clé ANGL_REP ou VECTEUR du mot clé
 facteur REPE_COQUE de la commande CALC_ELEM.
"""),

52 : _("""
 Présence de GRILLE dans la modélisation, les mots clés ANGL_REP ou VECTEUR
 du mot clé facteur REPE_COQUE ne sont pas traités.
"""),

53 : _("""
 La super_maille %(k1)s n'existe pas dans le maillage %(k2)s.
"""),

54 : _("""
 La maille %(k1)s doit etre une maille de peau de type QUAD ou TRIA
 car on est en 3D et elle est de type %(k2)s.
"""),

55 : _("""
 L'un des mots-cles ANGL_REP ou VECTEUR est à fournir pour l'option ARCO_ELNO_SIGM.
"""),

56 : _("""
 La combinaison 'fonction multiplicatrice' et 'chargement de type fonction' n'est pas autorisée car
 votre chargement %(k1)s contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP).
 On poursuit sans tenir compte de la fonction multiplicatrice.
"""),

57 : _("""
 La combinaison de chargements de meme type n'est pas autorisée car l'un des chargements
 contient une charge exprimée par une formule.
 Pour réaliser cette combinaison, vous devez transformer votre charge 'formule' en charge 'fonction'
 (via l'opérateur DEFI_FONCTION ou CALC_FONC_INTERP)
"""),

58 : _("""
 La combinaison de chargements de type 'déformation initiale' n'a aucun sens physique.'
"""),

59 : _("""
 La combinaison de chargements de type 'pesanteur' n'a aucun sens physique.'
"""),

60 : _("""
 La combinaison de chargements de type 'rotation' est déconseillée.
 Veuillez plutot utiliser un chargement de type 'force interne'.
"""),

63 : _("""
 Il faut au moins 2 numéros d'ordre pour traiter l'option %(k1)s
"""),

64 : _("""
 les champs ne sont pas de la meme grandeur:  type du cham_no  %(k1)s
   type du cham_no_affe  %(k2)s
"""),

65 : _("""
 composante non definie dans  la grandeur.  composante:  %(k1)s
"""),

66 : _("""

 le nombre de composantes affectees n'est pas egal  au nombre de composantes a affecter
 occurence de affe numero %(i1)d
 nbre de cmp affectees :  %(i2)d
 nbre de cmp a affecter :  %(i3)d
"""),

67 : _("""
 erreurs donneesle GROUP_MA  %(k1)s
  n'a pas le meme nombre de mailles  que le GROUP_MA  %(k2)s
"""),

68 : _("""
 erreurs donneesle GROUP_MA  %(k1)s
  n'a pas les memes types de maille  que le GROUP_MA  %(k2)s
"""),

69 : _("""
 erreurs donnees : la maille  %(k1)s  du maillage  %(k2)s
  n'est pas la translation de la  maille  %(k3)s
  du maillage  %(k4)s
    vecteur translation :  %(r1)f %(r2)f %(r3)f
"""),

70 : _("""
 l'instant  de calcul  %(r1)f  n'existe pas dans  %(k1)s
"""),

71 : _("""
 plusieurs numeros d'ordre trouves pour l'instant  %(r1)f
"""),

72 : _("""
 cette commande est reentrante :   sd resultat en sortie     %(k1)s
    sd resultat "resu_final"  %(k2)s
"""),

73 : _("""
 la sd resultat en sortie  %(k1)s
  doit contenir qu'un seul nume_ordre %(k2)s
"""),

74 : _("""
 manque le champ  %(k1)s  dans la sd resultat  %(k2)s
  pour le nume_ordre  %(i1)d
"""),

76 : _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s
    elrefa  :  %(k3)s
    famille :  %(k4)s
"""),

78 : _("""
 on ne sait pas encore decouper le type_element :  %(k1)s  en sous-elements %(k2)s
    elrefa :  %(k3)s
"""),

83 : _("""
 ecla_pg : champ vide nom_cham:  %(k1)s  nume_ordre :  %(i1)d
"""),





85 : _("""
 pb liste de mailles carte : %(k1)s  numero entite : %(i1)d
  position ds liste : %(i2)d
  numero de maille  : %(i3)d
"""),

87 : _("""
 famille non disponible
 maille de reference  %(k1)s
"""),

}
