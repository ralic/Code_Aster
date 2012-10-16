#@ MODIF appelmpi Messages  DATE 15/10/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={
1 : _(u"""%(i2)4d alarme a été émise sur le processeur #%(i1)d.
"""),

2 : _(u"""%(i2)4d alarmes ont été émises sur le processeur #%(i1)d.
"""),

3: _(u"""
 En parallèle, il faut au moins un sous-domaine par processeur.
"""),

5: _(u"""
 Erreur MPI: %(k1)s
"""),

80 : _(u"""  Le processeur #0 demande d'interrompre l'exécution."""),

81 : _(u"""  On demande au processeur #%(i1)d de s'arrêter ou de lever une exception."""),

82 : _(u"""  On signale au processeur #0 qu'une erreur s'est produite."""),

83 : _(u"""  Communication de type '%(k1)s' annulée."""),

84 : _(u"""  Le processeur #%(i1)d a émis un message d'erreur."""),


92 : _(u"""  On signale au processeur #0 qu'une exception a été levée."""),



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
