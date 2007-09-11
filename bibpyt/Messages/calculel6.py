#@ MODIF calculel6 Messages  DATE 11/09/2007   AUTEUR REZETTE C.REZETTE 
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

cata_msg={











9: _("""
  Erreur d'utilisation :
  Vous avez utilisé le mot clé AFFE_MATERIAU/AFFE/TEMP_REF
  alors que la température de référence doit etre fournie avec le mot clé :
  AFFE_MATERIAU/AFFE_VARC/VALE_REF= ...     (NOM_VARC='TEMP)
"""),

10: _("""
  Option inconnue %(k1)s
"""),















13: _("""
 interpolation deformations  anelastiques : evol_noli: %(k1)s instant: %(r1)f
 icoret: %(i1)d
"""),








15: _("""
  l'element diagonal u( %(i1)d , %(i2)d ) de la factorisation est nul. %(k1)s
 la solution et les estimations d' erreurs ne peuvent etre calculees. %(k2)s
"""),

16: _("""
 interpolation temperature:evol_ther: %(k1)s nom symbolique: %(k2)s
 instant: %(r1)f
 icoret: %(i1)d
"""),

17: _("""
 recherche nbre de cmp: erreur:  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s
"""),













20: _("""
 recherche nbre de cmp: erreur: grandeur ligne numero  %(i1)d  de nom  %(k1)s
 grandeur colonne numero  %(i2)d
  de nom  %(k2)s
 grandeur mere numero  %(i3)d
  de nom  %(k3)s
"""),

21: _("""
 recherche nbre de cmp: erreur: grandeur %(i1)d a un code inconnu:  %(i2)d
"""),

22: _("""
 recherche nbre d entiers codes  %(k1)s grandeur numero  %(i1)d  de nom  %(k2)s
"""),













25: _("""
 recherche nbre d entiers codes grandeur ligne numero  %(i1)d  de nom  %(k1)s
 grandeur colonne numero  %(i2)d de nom  %(k2)s
 grandeur mere numero  %(i3)d de nom  %(k3)s
"""),

26: _("""
 recherche nbre d entiers codes grandeur %(i1)d a un code inconnu:  %(i2)d
"""),

27: _("""
 acces impossible  champ :  %(k1)s , nume_ordre :  %(i1)d
"""),

28: _("""
 acces impossible au mode propre champ :  %(k1)s , nume_ordre :  %(i1)d
"""),








34: _("""
 famille non disponible    type de maille  %(k1)s
    famille d'integration  %(i1)d
"""),































41: _("""
 famille non disponible    type de maille  %(k1)s
"""),

42: _("""
 ! prise en compte de l'erreur !
 ! sur cl de type echange_paroi n'a ! %(i1)d
 ! pas ete encore implantee          ! %(i2)d
"""),

43: _("""
 ! le mot cle excit contient !! plusieurs occurences de type flux lineaire ! %(i1)d
 !   seule la derniere sera prise en compte   ! %(i2)d
"""),

44: _("""
 ! le mot cle excit contient !! plusieurs occurences de type echange    ! %(i1)d
 ! seule la derniere sera prise en compte  ! %(i2)d
"""),

45: _("""
 ! le mot cle excit contient !! plusieurs occurences de type source     ! %(i1)d
 ! seule la derniere sera prise en compte  ! %(i2)d
"""),

46: _("""
 ! champ temperature !! vide pour numero ordre ! %(i1)d
"""),

47: _("""
 ! champ flux_elno_temp !! vide pour numero ordre ! %(i1)d
"""),

49: _("""
 erreurs donnees composante inconnue  %(k1)s  pour la grandeur  %(k2)s
"""),

50: _("""
 Préparation des variables de commande :
 Dans le CHAM_MATER %(k1)s et pour la variable de commande %(k2)s,
 on ignore la composante %(k3)s
"""),

51: _("""
 erreurs donnees composante inconnue  %(k1)s
"""),

52: _("""
 Erreur Utilisateur :

 Variables internes initiales non coherentes (nb sous-points) avec le comportement choisi.
 Pour la maille : %(k1)s
  nb sous-points "k-1" :  %(i1)d
  nb sous-points "k"   :  %(i2)d
"""),

53: _("""

 variables internes initiales :  pas le nombre de composantes voulu par le comportement  pour la maille  nomail
  attendu par le comportement :  %(i1)d
  trouve sur la maille :  %(i2)d
"""),

54: _("""
 Utilisation d'un mot clé obsolète : AFFE_CHAR_MECA/TEMP_CALCULEE

 L'une des charges contient un chargement thermique (TEMP_CALCULEE).
 L'utilisation de la température comme variable de commande en mécanique doit
 maintenant se faire en utilisant AFFE_MATERIAU/AFFE_VARC/NOM_VARC='TEMP'.

 Néanmoins, jusqu'à la version 9.1 (incluse), les 2 syntaxes sont acceptées.

 Conseil :
 Déplacer le chargement thermique de AFFE_CHAR_MECA/TEMP_CALCULEE vers
 AFFE_MATERIAU/AFFE_VARC
"""),

55: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Le CHAM_MATER %(k1)s contient des variables de commandes (AFFE_VARC).
 Une des charges contient un chargement thermique (TEMP_CALCULEE).

 Conseil :
 Déplacer le chargement thermique de AFFE_CHAR_MECA/TEMP_CALCULEE vers
 AFFE_MATERIAU/AFFE_VARC
"""),

56: _("""
 Erreur d'utilisation (rcmaco/alfint) :
 Le CHAM_MATER %(k1)s contient des variables de commandes (AFFE_MATERIAU/AFFE_VARC).
 Un des matériaux du CHAM_MATER contient un coefficient de dilation ALPHA=f(TEMP).
 Mais la température n'est pas fournie sous AFFE_MATERIAU/AFFE_VARC

 Conseil :
 Renseignez le chargement thermique à l'aide de AFFE_MATERIAU/AFFE_VARC/NOM_VARC='TEMP'
"""),

57: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Pour la variable de commande %(k1)s, il y a une incohérence du
 nombre de "sous-points" entre le CARA_ELEM %(k2)s
 et le CHAM_MATER %(k3)s.

 Conseil :
 N'avez-vous pas défini plusieurs CARA_ELEM conduisant à des nombres de
 "sous-points" différents (COQUE_NCOU, TUYAU_NCOU, ...) ?
"""),

58: _("""
 Erreur de programmation :
 Pour la variable de commande %(k1)s, on cherche à utiliser la famille
 de points de Gauss '%(k2)s'.
 Mais cette famille n'est pas prévue dans la famille "liste" (MATER).

 Contexte de l'erreur :
    option       : %(k3)s
    type_element : %(k4)s

 Conseil :
 Emettez une fiche d'anomalie
"""),

59: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Dans le CHAM_MATER %(k1)s et pour la variable de commande %(k2)s,
 on a trouvé la composante 'TEMP_INF'.
 Cela veut sans doute dire que vous avez oublié de "préparer"
 la variable de commande 'TEMP' avec   CREA_RESU / OPERATION='PREP_VRC2'
"""),

60: _("""
 Erreur d'utilisation (préparation des variables de commande) :
 Dans le CHAM_MATER %(k1)s et pour la variable de commande %(k2)s,
 la liste donnée pour le mot clé VALE_REF n'a pas la bonne longueur.
"""),


61:_("""
 Erreur de programmation (fointa) :
    Le type de la fonction est invalide : %(k1)s
"""),

62: _("""
 Erreur de programmation (fointa) :
    Pour l'interpolation de la fonction %(k1)s,
    il manque le paramètre %(k2)s
"""),

63: _("""
 Erreur lors de l'interpolation (fointa) de la fonction %(k1)s :
 Code retour: %(i1)d
"""),

64: _("""
 Variables internes en nombre différent aux instants '+' et '-' pour la maille %(k1)s
 Instant '-' : %(i1)d
 Instant '+' : %(i2)d
"""),

65: _("""
 Le nombre de charges fourni par l'utilisateur %(i1)d est différent du
 nombre de charges trouvées dans la SD_resultat %(i2)d
"""),

66: _("""
 Le couple (charge-fonction) fourni par l'utilisateur n'est pas présent dans la SD_resultat.
 On poursuit le calcul avec le chargement fourni par l'utilisateur.
   Charge   (utilisateur) : %(k1)s
   Fonction (utilisateur) : %(k2)s
   Charge   (SD_resultat) : %(k3)s
   Fonction (SD_resultat) : %(k4)s

"""),

67: _("""
 Erreur utilisateur :
   Un calcul élémentaire nécessite une ou plusieurs variables de commande (CVRC).
   Sur la maille : %(k1)s, on ne trouve pas le bon nombre de "CVRC" :
   On attend : %(i2)d "CVRC",  mais on n'en trouve que : %(i1)d

 Conseil :
   Vérifier les occurences de AFFE_MATERIAU/AFFE_VARC pour la maille concernée.
"""),

68: _("""
 la liste des composantes fournies a nocart est incorrecte.
 composantes dans catalogue:
"""),

69: _("""
   %(k1)s
"""),

70: _("""
 composantes dans editgd:
"""),

71: _("""
   %(k1)s
"""),

72: _("""

"""),

73: _("""
 ! jacobien negatif en 3d !
"""),

74: _("""
 element :  %(i1)d jacobien :  %(r1)f
 attention le calcul d erreur est faux si
 la maille n est pas correctement orientee
"""),

75: _("""
 Probleme de parallelisation des calculs élémentaires avec FETI. Imcompatiblité
 entre LIGRELs dans la routine CALCUL.
 CONSEIL: Essayer de passer en séquentiel ou de changer de solveur linéaire.
"""),

76: _("""
 Probleme de parallelisation des calculs élémentaires avec FETI. Imcompatiblité
 LIGREL/numéro de maille dans la routine CALCUL.
 CONSEIL: Essayer de passer en séquentiel ou de changer de solveur linéaire.
"""),

77: _("""
 pb lors de l'affectation du champ: %(k1)s
 des valeurs n'ont pas ete recopiees dans le cham_elem final (perte d'information ?)
 ce probleme peut etre du a l'utilisation du mot cle TOUT='OUI'.
 on peut verifier le champ produit avec info=2

"""),

78: _("""
 Lois de comportement différentes pour la maille %(k3)s :
 - loi de comportement extraite de la SD Résultat   : %(k1)s
 - loi de comportement fournie à l'opérateur CALC_G : %(k2)s

--> Risques & conseils :
On doit généralement utiliser la meme loi de comportement entre le calcul et le
post-traitement. On peut utiliser deux comportements différents, mais alors 
l'utilisateur doit etre vigilant sur l'interprétation des résultats(cf.U2.05.01).
Si plusieurs comportements sont définis sur la structure, le comportement à 
indiquer dans CALC_G est celui du matériau dans lequel la fissure se développe. 
Dans ce cas, ce message d'alarme est quand meme émis mais le résultat est bien cohérent.
"""),

}
