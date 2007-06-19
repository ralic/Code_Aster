#@ MODIF modelisa9 Messages  DATE 19/06/2007   AUTEUR REZETTE C.REZETTE 
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
1: _("""
 il manque le parametre  %(k1)s dans la table %(k2)s
 .sa presence est indispensable a la  creation d'un champ elementaire. %(k3)s
"""),

2: _("""
 le parametre  %(k1)s de la table %(k2)s
 est incompatible a la  creation d'un  champ elementaire constant. %(k3)s
"""),

3: _("""
 il manque le parametre  %(k1)s dans la table %(k2)s
 .sa presence est indispensable a la  creation d'un champ  %(k3)s
"""),

4: _("""
 le parametre  %(k1)s de la table %(k2)s
 n'est valable que pour la  creation d'un champ  %(k3)s
"""),

5: _("""
 incoherence entre maille et point dans la table %(k1)s maille : %(k2)s
 point  : %(i1)d
 nombre de points de la maille: %(i2)d
"""),

6: _("""
 plusieurs affectations  pour le meme point d'une maille
  dans la table %(k1)s
 maille: %(k2)s
 point : %(i1)d
"""),

7: _("""
 plusieurs affectations  pour le meme sous-point dans la table %(k1)s
 maille: %(k2)s
 point : %(i1)d
 sous-point : %(i2)d
"""),

8: _("""
 plusieurs affectations  pour la meme maille dans la table %(k1)s
 maille: %(k2)s
"""),


9: _("""
Erreur utilisateur :
   Pour le materiau : %(k1), on cherche à redéfinir un mot clé déjà défini : %(k2)
"""),


10: _("""
Erreur utilisateur :
   Comportement 'HUJEUX'
   Non convergence pour le calcul de la loi de comportement (NB_ITER_MAX atteint).

Conseil :
   Augmenter NB_ITER_MAX (ou diminuer la taille des incréments de charge)

"""),

11: _("""
 mocle facteur non traite :mclf %(k1)s
"""),


12: _("""
 manque le parametre  %(k1)s 
"""),

13: _("""
 pour la maille  %(k1)s 
"""),

14: _("""
 
"""),

15: _("""
 pas de freq initiale definie : on prend la freq mini des modes calcules 
   %(r1)f 
"""),

16: _("""
 pas de freq finale definie : on prend la freq max des modes calcules   %(r1)f 
"""),

17: _("""
 votre freq de coupure   %(r1)f 
"""),

18: _("""
 est inferieure a celle  du modele de turbulence adopte :  %(r1)f 
"""),

19: _("""
 on prend la votre. 
"""),

20: _("""
 votre freq de coupure :   %(r1)f 
"""),

21: _("""
 est superieure a celle  du modele de turbulence adopte :   %(r1)f 
"""),

22: _("""
 on prend celle du modele. 
"""),

23: _("""
 erreur dans les donnees mot cle facteur  %(k1)s  occurence  %(i1)d 
"""),

24: _("""
 le maillage est "plan" ou "z_cst"
"""),

25: _("""
 le maillage est "3d"
"""),

26: _("""
 il y a  %(i1)d  valeurs pour le mot cle  %(k1)s il en faut  %(i2)d 
"""),

27: _("""
 erreur dans les donnees mot cle facteur  %(k1)s  occurence  %(i1)d 
 pour le mot cle  %(k2)s 
  le noeud n'existe pas  %(k3)s 
"""),

28: _("""
 erreur dans les donnees mot cle facteur  %(k1)s  occurence  %(i1)d 
 pour le mot cle  %(k2)s 
  le group_no n'existe pas  %(k3)s 
"""),

29: _("""
 trop de noeuds dans le group_no mot cle facteur  %(k1)s  occurence  %(i1)d 
   noeud utilise:  %(k2)s 
"""),

30: _("""
 Le MODELE doit etre de type mécanique.
"""),

}
