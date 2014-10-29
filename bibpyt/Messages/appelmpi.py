# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    1 : _(u"""%(i2)4d alarme a été émise sur le processeur #%(i1)d.
"""),

    2 : _(u"""%(i2)4d alarmes ont été émises sur le processeur #%(i1)d.
"""),





    5: _(u"""
 Erreur lors de l'appel à une fonction MPI.
 Les détails de l'erreur devraient être affichés ci-dessus.
"""),

    6: _(u"""
 Problème lié a la distribution parallèle de calculs au sein d'une macro-commande.
 Le paramètre %(k1)s=%(i1)d est incohérent.

 Conseil:
 =======
   * Contacter l'équipe de développement.
"""),

    8: _(u"""
 Problème lié a la distribution parallèle de calculs au sein d'une macro-commande.
 Parmi les paramètres suivants, au moins un est incohérent.
     - %(k1)s=%(i1)d,
     - %(k2)s=%(i2)d,
     - %(k3)s=%(i3)d.

 Conseil:
 =======
   * Contacter l'équipe de développement.
"""),

    80 : _(u"""  Le processeur #0 demande d'interrompre l'exécution."""),

    81 : _(u"""  On demande au processeur #%(i1)d de s'arrêter ou de lever une exception."""),

    82 : _(u"""  On signale au processeur #0 qu'une erreur s'est produite."""),

    83 : _(u"""  Communication de type '%(k1)s' annulée."""),

    84 : _(u"""  Le processeur #%(i1)d a émis un message d'erreur."""),


    92 : _(u"""  On signale au processeur #0 qu'une exception a été levée."""),

    94 : _(u"""
    Il n'y a plus de temps pour continuer.
    Le calcul sera interrompu à la fin de la prochaine itération, du prochain
    pas de temps ou de la prochaine commande, ou bien brutalement par le système.

    On accorde un délai de %(r1).0f secondes pour la prochaine communication.

    Conseil :
        Augmentez la limite en temps du calcul.
"""),

    95 : _(u"""
    Tous les processeurs sont synchronisés.
    Suite à une erreur sur un processeur, l'exécution est interrompue.
"""),

    96 : _(u"""
  Le processeur #%(i1)d n'a pas répondu dans le délai imparti.
"""),

    97 : _(u"""
    Le délai d'expiration de la communication est dépassé.
    Cela signifie que le processeur #0 attend depuis plus de %(r1).0f secondes,
    ce qui n'est pas normal.
"""),

    99 : { 'message' : _(u"""

    Au moins un processeur n'est pas en mesure de participer à la communication.
    L'exécution est donc interrompue.

"""), 'flags' : 'DECORATED',
           },

}
