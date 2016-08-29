# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

    1 : _(u"""
 L'option %(k1)s est déjà calculée pour le numéro d'ordre %(k2)s.
 On la recalcule car les données peuvent être différentes.
"""),

    2 : _(u"""
Champ inexistant SIEF_ELGA ou SIEF_ELGA numéro d'ordre %(k1)s pour le calcul de l'option %(k2)s
"""),

    3 : _(u"""
Champ inexistant DEPL numéro d'ordre %(k1)s pour le calcul de l'option %(k2)s
"""),

    4 : _(u"""
 Il n'existe aucun noeud lié à la liste de de mailles ou au groupe de mailles fournit à IMPR_RESU.

 Conseil : Vérifiez les mots-clés MAILLE ou GROUP_MA fournit à IMPR_RESU.
"""),

    8 : _(u"""

 la taille mémoire   nécessaire au vecteur de travail dans   lequel nous stockons les composantes   u et v du vecteur TAU est trop importante   par rapport a la place disponible.
 taille disponible :  %(i1)d
 taille nécessaire :  %(i2)d
"""),

    10 : _(u"""
 le noeud traité n'est associé à aucune maille volumique.
 numéro du noeud =  %(i1)d
 nombre de mailles attachées au noeud =  %(i2)d
"""),

    16 : _(u"""
 appel erroné numéro d'ordre %(i1)d code retour  : %(i2)d
 Problème CHAM_NO %(k1)s
Séchage moins %(r2)f  Séchage plus %(r3)f
"""),

    19 : _(u"""
 nombre de noeud(s) éliminé(s) du maillage  %(i1)d
"""),

    20 : _(u"""
 nombre de maille(s) éliminée(s) du maillage  %(i1)d
"""),

    21 : _(u"""
 le numéro du groupe de mailles est trop grand:  %(i1)d
  le numéro du groupe doit être inférieur a  %(i2)d
"""),

    25 : _(u"""
  on ne trouve pas la composante  %(k1)s  dans la grandeur  %(k2)s
"""),

    30 : _(u"""
  on ne trouve pas la maille  %(k1)s
"""),

    31 : _(u"""
  On ne trouve pas le groupe %(k1)s.
"""),

    32 : _(u"""
  le groupe  %(k1)s  ne contient aucune maille  %(k2)s
"""),

    38 : _(u"""
  on ne trouve pas le noeud : %(k1)s
"""),

    40 : _(u"""
  le groupe  %(k1)s ne contient aucun noeud  %(k2)s
"""),

    41 : _(u"""
  le paramètre  %(k1)s n'existe pas %(k2)s
"""),

    45 : _(u"""
 noeud inconnu dans le fichier  IDEAS  noeud numéro :  %(i1)d
"""),

    46 : _(u"""
 élément inconnu dans le fichier IDEAS élément numéro :  %(i1)d
"""),

    47 : _(u"""
 Erreur d'utilisation IMPR_RESU FORMAT='CASTEM' :
   Le format CASTEM n'autorise pas les noms de groupes de plus de
   8 caractères. Les noms plus longs sont tronqués.
   Ici, deux groupes sont identiques si on les tronquent : %(k1)s

"""),

    48 : _(u"""
Erreur utilisateur dans la commande EXTR_RESU / RESTREINT :
  La structure de donnée contient des champs par éléments.
  Le mot clé MODELE est obligatoire.
"""),

    49 : _(u"""
Erreur utilisateur dans la commande EXTR_RESU / RESTREINT :
  Le maillage associé à certains champs de %(k1)s est %(k2)s.
  Il devrait être %(k3)s.
"""),








    57 : _(u"""
 problème dans  le traitement de l'instant  %(r1)f
  récupération de  %(k1)s
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    58 : _(u"""
 problème dans  le traitement de l'instant  %(r1)f
  récupération  pour  %(k1)s
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    59 : _(u"""
 problème dans  le traitement de l'instant  %(r1)f
  récupération pour le secteur  %(i1)d
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    61 : _(u"""
 la composante  %(k1)s  n'existe dans aucun des champs %(k2)s
"""),

    64 : _(u"""
 la valeur d'amortissement réduit est trop grande
 la valeur d'amortissement :  %(r1)f
  du mode propre  %(i1)d
  est tronquée au seuil :  %(r2)f
"""),

    67 : _(u"""

 la taille mémoire   nécessaire au vecteur de travail   est trop importante   par rapport a la place disponible.
 taille disponible :  %(i1)d
 taille nécessaire :  %(i2)d
"""),

    68 : _(u"""

 la taille du vecteur  contenant les caractéristiques des   paquets de mailles est trop petite.
 nombre de paquets max :  %(i1)d
 nombre de paquets réels:  %(i2)d
"""),

    70 : _(u"""

 la taille du vecteur  contenant les caractéristiques des   paquets de noeuds est trop petite.
 nombre de paquets max :  %(i1)d
 nombre de paquets réels:  %(i2)d
"""),

    73 : _(u"""
 appel erroné  résultat :  %(k1)s   archivage numéro :  %(i1)d
   code retour :  %(i2)d
   problème champ :  %(k2)s
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    74 : _(u"""
 on ne trouve pas l'instant  %(r1)f  dans la table  %(k1)s
"""),

    75 : _(u"""
 on trouve   plusieurs instants  %(r1)f  dans la table  %(k1)s
"""),

    76 : _(u"""
 noeud non contenu dans une  maille sachant calculer l" option
 noeud numéro :  %(i1)d
"""),

    77 : _(u"""
banque de données pour le type de géométrie  %(k1)s
  le couple de matériaux  %(k2)s
  ne se trouve pas dans la banque. %(k3)s
"""),

    78 : _(u"""
 le calcul du rayon n'est pas assez précis.
    %(r1)f
    %(r2)f
    %(r3)f
    %(r4)f
    %(r5)f
    %(r6)f
    %(i1)d
    %(r7)f
    %(r8)f
    %(r9)f
    %(r10)f
    %(r11)f
    %(r12)f
    %(r13)f
    %(r14)f
    (%(r15)f
    %(r16)f

Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

}
