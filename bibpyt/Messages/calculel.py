#@ MODIF calculel Messages  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

2: _("""
Erreur Utilisateur :
 Quand on utilise AFFE_CHAR_CINE/EVOL_IMPO, c'est le champ de l'evol_xxx correspondant
 au 1er instant qui impose sa "loi" : tous les ddls de ce champ seront imposés pour tous
 les instants du calcul.

 Malheureusement, on ne trouve pas un ddl dans l'evol_xxx %(k1)s :
   instant : %(r1)f  noeud : %(i1)d  cmp : %(k2)s

Risques & conseils :
 Assurez-vous que l'évolution imposée %(k1)s concerne les memes ddls pour tous les instants.
"""),


3: _("""
 la grandeur :  %(k1)s  n existe pas dans le catalogue des grandeurs.
"""),

4: _("""
 incoherence des maillages : %(k1)s  et  %(k2)s
"""),

5: _("""
 Erreur de programmation (ou d'utilisation ?) :
   Le changement de discrétisation : %(k1)s n'est pas encore programmé.
 Risques et conseils :
   Il y a peut-etre une demande d'évolution à émettre ...
"""),

8: _("""
 sur la maille %(k1)s le calcul est thermo mécanique. Or il manque la température de référence.
 On ne peut donc pas calculer de déformation thermique.
"""),

11: _("""
 le mode_local:  %(k1)s  ne doit pas etre vecteur ou matrice.
"""),

12: _("""
 le mode_local:  %(k1)s  ne doit pas etre "diff__".
"""),

14: _("""
  incompatibilite des type_champ ("elga"/"elno")  pour l option :  %(k1)s  entre les 2 type_elem :  %(k2)s  et  %(k3)s
"""),

15: _("""
 sur la maille %(k1)s le calcul est thermo mécanique. Or il manque le parametre matériau
 %(k2)s . On ne peut donc pas calculer de déformation thermique. 
"""),

16: _("""
 type de maille indisponible
"""),

17: _("""
 type de champ inconnu
"""),

18: _("""
 les parties réelle et imaginaire du champ à assembler ne sont pas du même type
 l'un est un CHAM_NO et l'autre un CHAM_ELEM
"""),

20: _("""
 le champ de grandeur  %(k1)s  ne respecte pas le format xxxx_r
"""),

21: _("""
 les champs réel et imaginaire à assembler ne contiennent pas la même grandeur
"""),

22: _("""
 problème dans le catalogue des grandeurs simples
 la grandeur %(k1)s  ne possède pas le même nombre de champs que son homologue complexe %(k2)s
"""),

23: _("""
 problème dans le catalogue des grandeurs simples
 la grandeur  %(k1)s  ne possède pas les mêmes champs que son homologue complexe  %(k2)s
"""),

24: _("""
 les champs à assembler n'ont pas la même longueur
"""),

26: _("""
 longueurs des CHAM_ELEM incompatibles
"""),

27: _("""
 CHAM_ELEM à combiner incompatible
"""),

28: _("""
  %(k1)s  indisponible
"""),

29: _("""
 option inconnue au catalogue :  %(k1)s
"""),

30: _("""
  -> Le TYPE_ELEMENT %(k1)s  ne sait pas encore calculer l'OPTION:  %(k2)s.
     On ne calcule donc rien sur les éléments de ce type.

  -> Risque & Conseil :
     Vérifiez que l'option incomplètement calculée est bien une option de post-traitement
     et que le déficit de calcul n'entraine pas de résultats faux.
"""),

31: _("""
  la température n'est pas correctement renseignée
"""),

34: _("""
 le calcul de l'option :  %(k1)s
 n'est possible pour aucun des types d'éléments du LIGREL.
"""),

37: _("""
 Erreur dans la lecture des CHAR_CINE ou dans les CHAR_CINE
"""),

38: _("""
 la carte concerne aussi des mailles tardives qui sont oubliées
"""),

42: _("""
 Erreur Programmeur:
 Incohérence fortran/catalogue
 TYPE_ELEMENT :  %(k1)s
 OPTION       :  %(k2)s
 La routine texxxx.f correspondant au calcul élémentaire ci-dessus est bugguée
 Elle écrit en dehors de la zone allouée au paramètre (OUT) %(k3)s.

"""),

47: _("""
  le CHAM_ELEM:  %(k1)s  n'existe pas.
"""),

48: _("""
 le CHAM_ELEM: %(k1)s  n'a pas le même nombre de composantes dynamiques sur tous ses éléments.
"""),

49: _("""
 le CHAM_ELEM : %(k1)s a des sous-points.
"""),

52: _("""
 la cmp: %(k1)s  n'appartient pas à la grandeur: %(k2)s
"""),

53: _("""
 option : %(k1)s  inexistante dans les catalogues.
"""),

54: _("""
 Le paramètre:  %(k1)s  de l'option:  %(k2)s  n'est pas connu des TYPE_ELEM du LIGREL:  %(k3)s
"""),

55: _("""
 Erreur utilisateur :
   On cherche à créer un CHAM_ELEM mais sur certains points, on ne trouve pas la composante : %(k1)s
 Risques & conseils :
   Si la commande que vous exécutez comporte le mot clé PROL_ZERO='OUI', vous devriez peut-etre l'utiliser.
"""),

56: _("""
 le LIGREL contient des mailles tardives
"""),

57: _("""
 nombres de points différents pour la maille:  %(k1)s
 CHAM_ELEM de :  %(k2)s
"""),

58: _("""
 il manque la composante : %(k1)s  sur la maille : %(k2)s
"""),

67: _("""
 grandeur:  %(k1)s  inconnue au catalogue.
"""),

68: _("""
 numéro de maille invalide     :  %(k1)s  (<1 ou >nbma)
"""),

69: _("""
 numéro de point invalide      :  %(k1)s  (<1 ou >nbpt)
 pour la maille                :  %(k2)s
"""),

70: _("""
 numéro de sous_point invalide :  %(k1)s  (<1 ou >nbspt)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
"""),

71: _("""
 numéro de composante invalide :  %(k1)s  (<1 ou >nbcmp)
 pour la maille                :  %(k2)s
 pour le point                 :  %(k3)s
 pour le sous-point            :  %(k4)s
"""),

91: _("""
 incohérence des familles de points de Gauss pour la maille  %(k1)s
 ( %(k2)s / %(k3)s )
"""),

92: _("""
 type scalaire du CHAM_NO :  %(k1)s  non réel.
"""),

93: _("""
 type scalaire du NUME_DDL :  %(k1)s  non réel.
"""),

99: _("""
 melange de CHAM_ELEM_S et CHAM_NO_S
"""),
}
