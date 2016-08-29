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
 La modélisation C_PLAN n'est pas compatible avec la loi de comportement ELAS_VMIS_PUIS.
"""),

    3 : _(u"""
 type de matrice inconnu.
"""),

    13 : _(u"""
 le VECT_ELEM n'existe pas :  %(k1)s
"""),




    21 : _(u"""
 le noeud  %(k1)s  n'appartient pas au maillage :  %(k2)s
"""),

    25 : _(u"""
 données incompatibles.
"""),

    32 : _(u"""
  la numérotation n'est pas cohérente avec le modèle généralisé
  si vous avez activé l'option INITIAL dans NUME_DDL_GENE faites de même ici !
"""),

    33 : _(u"""
Calcul des options en mécanique des milieux poreux avec couplage THM
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    34 : _(u"""
 il y a incohérence entre :
    la loi de couplage de DEFI_MATERIAU : %(k1)s
 et la loi de couplage de STAT_NON_LINE : %(k2)s
"""),

    35 : _(u"""
 les champs " %(k1)s " et " %(k2)s " n'ont pas le même domaine de définition.
"""),

    36 : _(u"""
 BARSOUM, erreur dans le traitement des mailles %(k1)s
"""),

    42 : _(u"""
 BETON_DOUBLE_DP: incrément de déformation plastique en traction négatif
 --> redécoupage automatique du pas de temps
"""),

    43 : _(u"""
 BETON_DOUBLE_DP: incrément de déformation plastique en compression négatif
 --> redécoupage automatique du pas de temps
"""),

    44 : _(u"""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n'est pas respectée en compression.
"""),

    45 : _(u"""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n'est pas respectée en compression pour la maille:  %(k1)s
"""),

    46 : _(u"""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n est pas respectée en traction.
"""),

    47 : _(u"""
 intégration élastoplastique de la loi BETON_DOUBLE_DP :
 la condition d'applicabilité sur la taille des éléments
 n'est pas respectée en traction pour la maille:  %(k1)s
"""),

    48 : _(u"""
  -> Intégration élastoplastique de loi multicritères BETON_DOUBLE_DP :
     la contrainte équivalente est nulle pour la maille %(k1)s
     le calcul de la matrice tangente est impossible.
  -> Risque & Conseil :
"""),

    52 : _(u"""
  Le nombre de modes et de degré de liberté d'interface sont différents.
"""),

    53 : _(u"""
  Le nombre de modes dynamiques est %(i1)d,
  ce n'est pas un multiple du nombre de composante.
"""),

    54 : _(u"""
  Le nombre de modes statiques est %(i1)d,
  ce n'est pas un multiple du nombre de composante.
"""),





    60 : _(u"""
 certains coefficients de masse ajoutée sont négatifs.
 vérifiez l'orientation des normales des éléments d'interface.
 convention adoptée : structure vers fluide
"""),

    61 : _(u"""
 certains coefficients d'amortissement ajouté sont négatifs.
 possibilité d'instabilité de flottement
"""),

    67 : _(u"""
 option SECANTE non valide
"""),

    68 : _(u"""
 trop de familles de systèmes de glissement.
 augmenter la limite actuelle (5)
"""),

    69 : _(u"""
 trop de familles de systèmes de glissement.
Ce message est un message d'erreur développeur.
Contactez le support technique.
"""),

    70 : _(u"""
 Le nombre de système de glissement est égal à 0
"""),

    74 : _(u"""
 Le couplage FLUAGE/ENDO_ISOT_BETON n'est pas valide en THM.
"""),

    96 : _(u"""
 ce mot clé de MODI_MAILLAGE attend un vecteur de norme non nulle.
"""),

    97 : _(u"""
 le mot clé REPERE de MODI_MAILLAGE attend deux vecteurs non nuls orthogonaux.
"""),

}
