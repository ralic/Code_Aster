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

    1 : _(u"""
   Arrêt par manque de temps CPU pendant les itérations de Newton, au numéro d'instant < %(i1)d >
      - Temps moyen par itération de Newton : %(r1)f
      - Temps CPU restant                   : %(r2)f
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseil :
      - Augmentez le temps CPU.
"""),

    2 : _(u"""
   Arrêt par manque de temps CPU au numéro d'instant < %(i1)d >
      - Temps moyen par pas de temps        : %(r1)f
      - Temps CPU restant                   : %(r2)f
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseil :
      - Augmentez le temps CPU.
"""),

    3 : _(u"""
   Arrêt suite à l'échec de l'intégration de la loi de comportement.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseils :
      - Vérifiez vos paramètres, la cohérence des unités.
      - Essayez d'augmenter ITER_INTE_MAXI, ou de subdiviser le pas de temps localement via ITER_INTE_PAS.
"""),

    4 : _(u"""
   Arrêt pour cause de matrice non inversible.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseils :
      - Vérifiez vos conditions aux limites.
      - Vérifiez votre modèle, la cohérence des unités.
      - Si vous faites du contact, il ne faut pas que la structure ne "tienne" que par le contact.
"""),

    5 : _(u"""
   Arrêt pour cause d'échec lors du traitement du contact discret.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    6 : _(u"""
   Arrêt pour cause de matrice de contact singulière.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    7 : _(u"""
   Arrêt pour cause d'absence de convergence avec le nombre d'itérations requis dans l'algorithme non-linéaire de Newton.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseils :
   - Augmentez ITER_GLOB_MAXI.
   - Réactualisez plus souvent la matrice tangente.
   - Raffinez votre discrétisation temporelle.
   - Essayez d'activer la gestion des événements (découpe du pas de temps par exemple) dans la commande DEFI_LIST_INST.
"""),

    8  : _(u"""
   Arrêt par échec dans le pilotage.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    9  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur la géométrie.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    10  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur le seuil de frottement.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    11  : _(u"""
   Arrêt par échec dans la boucle de point fixe sur le statut de contact.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.
"""),

    12 : _(u"""
   Arrêt pour cause d'absence de convergence avec le nombre d'itérations requis dans le solveur linéaire itératif.
   La base globale est sauvegardée. Elle contient les pas archivés avant l'arrêt.

   Conseils :
   - Augmentez le nombre maximum d'itérations (NMAX_ITER).
   - Utilisez un préconditionneur plus précis ou changez d'algorithme.
"""),

    50 : _(u"""Arrêt par échec de l'action <%(k1)s>  pour le traitement de l'évènement <%(k2)s>. """),
    51 : _(u"""Arrêt demandé pour le déclenchement de l'évènement <%(k1)s>. """),

    60 : _(u"""
   Les forces de contact sont mal définies dans le domaine de Fourier.

   Conseil :
   - Augmenter le nombre d'harmoniques des forces de contact (NB_HARM_NONL).
"""),

    61 : _(u"""
   Le dernier terme de la série entière est nul. On force le rayon de convergence à 1.
"""),

    62 : _(u"""
   Correction échouée !!!
"""),

    63 : _(u"""
   Le nombre d'harmoniques des forces de contact doit être supérieur ou égale au nombre d'harmoniques des autres variables.
   NB_HARM_NONL >= NB_HARM_LINE
"""),

    64 : _(u"""
   -----------------------------------------------------------------------
   ||    Erreur absolue   ||    Erreur relative  || Max. coefficient   ||
   -----------------------------------------------------------------------
   || %(r1)19.8e || %(r2)19.8e || %(r3)19.8e ||
   -----------------------------------------------------------------------
"""),

    65 : _(u"""
   La solution périodique est stable
   --------------------------------------------------------------------------
"""),

    66 : _(u"""
   La solution périodique est instable
   --------------------------------------------------------------------------
"""),

    67 : _(u"""
   --------------------- Numéro ordre : %(i1)d -----------------------------------
"""),

    68 : _(u"""Le mot-clé GROUP_NO du mot-clé facteur CHOC ne doit contenir qu'un noeud."""),

}
