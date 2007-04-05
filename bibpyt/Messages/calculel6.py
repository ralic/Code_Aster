#@ MODIF calculel6 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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















































10: _("""
  option inconnue %(k1)s
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








51: _("""
 erreurs donnees composante inconnue  %(k1)s
"""),

52: _("""

 variables internes initiales   non coherentes (nb sous-points) avec le comportement  pour la maille  nomail
  nb sous-points "k-1" :  %(i1)d
  nb sous-points "k" :  %(i2)d
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
 Le CHAM_MATER %(k1)s contient des variables de commandes (AFFE_VARC).
 Une des charges contient un chargement thermique (TEMP_CALCULEE).

 Conseil :
 Déplacer le chargement thermique de AFFE_CHAR_MECA/TEMP_CALCULEE vers
 AFFE_MATERIAU/AFFE_VARC
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
 on ne sait pas quoi faire de la composante %(k3)s

 Conseils :
 Si le problème concerne la composante 'TEMP_INF', c'est peut-etre parce
 que vous avez oublié d'utiliser CREA_RESU / OPERATION='PREP_VRC2'
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


}
