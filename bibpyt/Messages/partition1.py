# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

    1: _(u"""
 Il y a moins de sous-domaines (%(i1)d) que de processeurs participant au calcul (%(i2)d).

 Conseils :
   - augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
   - diminuez le nombre de processeurs du calcul
"""),

    17 : _(u"""
  La partition %(k1)s que vous utilisez pour partitionner le modèle %(k2)s en sous-domaines a été construite sur un autre modèle (%(k3)s).

  Conseil : vérifiez la cohérence des modèles.
"""),


    91 : _(u"""
   La sous-structuration n'est compatible qu'avec un mode de parallélisme centralisé.

   Conseil :
     - Renseignez le mot-clé PARTITION/PARALLELISME de AFFE_MODELE (ou MODI_MODELE) avec 'CENTRALISE'
  """),

    93 : _(u"""
 Il y a moins de mailles (%(i1)d) dans le modèle que de processeurs participant au calcul (%(i2)d).

 Conseils :
   - vérifiez qu'un calcul parallèle est approprié pour votre modèle
   - diminuez le nombre de processeurs du calcul
"""),

    97 : _(u"""
Erreur d'utilisation pour le parallélisme :
 Le mode de répartition des éléments entre les différents processeurs (PARTITION / PARALLELISME='GROUP_ELEM')
 ne peut pas être utilisé ici car il y a moins de groupes d'éléments (%(i1)d) que de processeurs (%(i2)d).
 En d'autres termes, il n'y a pas assez d'éléments à répartir (le modèle est trop petit).

 Conseils :
   - diminuez le nombre de processeurs du calcul
   - changez le mode de distribution des mailles avec le mot-clé PARTITION / PARALLELISME de l'opérateur
     AFFE_MODELE (ou MODI_MODELE)
"""),

    98: _(u"""
  La maille de numéro:  %(i1)d appartient à plusieurs sous-domaines !
"""),

    99 : _(u"""
 Le paramètre CHARGE_PROC0_SD du mot-clé facteur PARTITION est mal renseigné.
 Il faut qu'il reste au moins un sous domaine par processeur une fois affectés tous les sous-domaines du processeur 0.

 Conseils :
   - laissez le mot-clé CHARGE_PROC0_SD à sa valeur par défaut
   - diminuez le nombre de processeurs du calcul ou bien augmentez le nombre de sous-domaines de la partition du mot-clé PARTITION
"""),

}
