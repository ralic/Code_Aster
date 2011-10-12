#@ MODIF utilitai6 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _(u"""
 La grandeur fournie : %(k1)s ne figure pas dans le catalogue des grandeurs
"""),

2 : _(u"""
 Erreur utilisateur (CREA_CHAMP/AFFE) :
   Le type du champ que l'on cherche à créer (réel, entier, complexe, fonction)
   n'est pas compatible avec le mot clé utilisé (VALE, VALE_I, VALE_C, VALE_F).

 Il faut respecter la correspondance suivante :
    - champ réel        -> VALE
    - champ complexe    -> VALE_C
    - champ entier      -> VALE_I
    - champ fonction    -> VALE_F
"""),

3 : _(u"""
 la liste de composantes et la liste des valeurs n'ont pas la même dimension
 occurence de AFFE numéro  %(i1)d
"""),

4 : _(u"""
 une composante n'appartient pas à la grandeur
 occurence de AFFE numéro  %(i1)d
 grandeur   :  %(k1)s
 composante :  %(k2)s
"""),

5 : _(u"""
 le NUME_DDL en entrée ne s'appuie pas sur la même grandeur que celle de la commande
 grandeur associee au NUME_DDL : %(k1)s
 grandeur de la commande       :  %(k2)s
"""),

11 : _(u"""
 une composante n'appartient pas à la grandeur
 grandeur   :  %(k1)s
 composante :  %(k2)s
"""),

12 : _(u"""
 variable inconnue:  %(k1)s  pour le résultat :  %(k2)s
"""),

13 : _(u"""
 problème rencontré lors de la recherche de la variable :  %(k1)s
         debut :  %(k2)s
           fin :  %(k3)s
"""),

14 : _(u"""
 interpolation non permise
 valeur à interpoler : %(r1)f
 borne inferieure    : %(r2)f
 borne superieure    : %(r3)f
"""),

15 : _(u"""
 il faut donner :
    - une MAILLE ou un GROUP_MA
    - un NOEUD ou un GROUP_NO ou un point
"""),

16 : _(u"""
 interpolation impossible
 instant à interpoler:  %(r1)f
"""),

17 : _(u"""
 interpolation impossible
 instant à interpoler:  %(r1)f
 borne inférieure    :  %(r2)f
"""),

18 : _(u"""
 interpolation impossible
 instant à interpoler:  %(r1)f
 borne supérieure    : %(r2)f
"""),

19 : _(u"""
 CHAM_NO inexistant pour l'accès %(k1)s sur le résultat %(k2)s
 pour le NUME_ORDRE %(i1)d
 instant à interpoler %(r1)f
"""),

25 : _(u"""
 CHAM_ELEM inexistant pour l'accès %(k1)s sur le résultat %(k2)s
 pour le NUME_ORDRE %(i1)d
 instant à interpoler %(r1)f
"""),

37 : _(u"""
 la fonction  %(k1)s  a  %(i1)d arguments
 le maximum exploitable est  %(i2)d
"""),

38 : _(u"""
 il y a  %(i1)d paramètre(s) identique(s) dans la définition de la nappe
"""),

44 : _(u"""
 trop d'amortissements modaux
 nombre d'amortissements :  %(i1)d
 nombre de modes         :  %(i2)d
"""),

47 : _(u"""
 erreur dans la recherche du noeud
 nom du noeud    :  %(k1)s
 nom du maillage :  %(k2)s
"""),

48 : _(u"""
 methode de newton
 exposant de la loi   = %(r1)f
 nombre d''iterations = %(i1)d
 residu fonction = %(r2)f
 residu f/df = %(r3)f
 precision = %(r4)f
"""),

51 : _(u"""
 pas de champ correspondant à l'instant demandé.
 resultat  %(k1)s
 accès "INST_INIT" : %(r1)f
"""),

52 : _(u"""
 plusieurs champs correspondant à l'instant demandé
 resultat  %(k1)s
 acces "INST_INIT" : %(r1)f
 nombre : %(i1)d
"""),

53 : _(u"""
 le premier instant de rupture n'est pas dans la liste des instants de calcul
 premier instant de rupture =  %(r1)f
 premier instant de calcul  =  %(r2)f
"""),

54 : _(u"""
 le dernier instant de rupture n'est pas dans la liste des instants de calcul
 dernier instant de rupture =  %(r1)f
 dernier instant de calcul  =  %(r2)f
"""),

55 : _(u"""
 parametres initiaux de weibull
 exposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 contrainte de reference = %(r3)f
"""),

56 : _(u"""
 statistiques recalage :
 nombre d'iterations  = %(i1)d
 convergence atteinte = %(r1)f
"""),

57 : _(u"""
 les abscisses %(k1)s %(k2)s ne sont pas monotones.
"""),

58 : _(u"""
 les abscisses %(k1)s %(k2)s ont été réordonnées.
"""),

59 : _(u"""
 l'ordre des abscisses %(k1)s %(k2)s a été inversé.
"""),

60 : _(u"""
 homogénéité du champ de matériaux pour weibull
 nombre de rc weibull trouvees =  %(i1)d
 les calculs sont valables pour  un seul comportement weibull %(k1)s
 on choisit la premiere relation du type weibull %(k2)s
"""),

61 : _(u"""
 paramètres de la rc weibull_fo
 exposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 contrainte de référence conventionnelle = %(r3)f
"""),

62 : _(u"""
 parametres de la rc weibull
 exposant de la loi      = %(r1)f
 volume de reference     = %(r2)f
 contrainte de reference = %(r3)f
"""),

68 : _(u"""
 type de numérotation non connue
 numérotation: %(k1)s
"""),

72 : _(u"""
 trop de mailles dans le GROUP_MA
 maille utilisée:  %(k1)s
"""),

77 : _(u"""
Concept résultat %(k1)s :
le numéro d'ordre %(i1)d est inconnu.
"""),

78 : _(u"""
Concept résultat %(k1)s :
le numéro d'archivage %(i1)d est supérieur au max %(i2)d.
"""),

79 : _(u"""
Concept résultat %(k1)s :
le numéro de rangement %(i1)d est supérieur au max %(i2)d.
"""),

80 : _(u"""
Concept résultat %(k1)s :
la variable %(k2)s est inconnue pour le type %(k3)s.
"""),

82 : _(u"""
 pas de champ trouvé pour la fréquence  %(r1)f
"""),

83 : _(u"""
 plusieurs champs trouvés pour la fréquence  %(r1)f
 nombre de champs trouvés  : %(i1)d
"""),

84 : _(u"""
 le "NOM_PARA_RESU"  %(k1)s n'est pas un paramètre du résultat  %(k2)s
"""),

89 : _(u"""
 erreur dans les données
 le paramètre  %(k1)s n'existe pas
"""),

90 : _(u"""
 erreur dans les données
 le paramètre %(k1)s n'est pas trouvé
"""),

93 : _(u"""
 le paramètre  %(k1)s n'existe pas dans la table %(k2)s
 il est nécessaire
 veuillez consulter la documentation de la commande
"""),

99 : _(u"""
 erreur dans les données
 paramètre :  %(k1)s
 plusieurs valeurs trouvees
 pour le paramètre : %(k3)s
 et le paramètre   : %(k4)s
"""),

}
