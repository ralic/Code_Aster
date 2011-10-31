#@ MODIF subdivise Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

# Pour la méthode de subdivision

cata_msg={


1: _(u"""
  On tente de découper le pas de temps par la méthode %(k1)s.
"""),

2: _(u"""
  On ne tente pas de découper le pas de temps car la fonctionnalité n'est pas activée.
  Pour activer la découpe du pas de temps, utilisez la commande DEFI_LIST_INST.
"""),

3: _(u"""
    Le nombre maximal <%(i1)d> de niveaux de subdivision est atteint.
    Conseils :
     - augmentez SUBD_NIVEAU en mode MANUEL. 
     - ajustez SUBD_PAS_MINI en mode AUTO ou MANUEL.
"""),


10: _(u"""
    Essai de découpage en <%(i1)d> pas.
   """),

11: _(u"""
    Découpe uniforme    
    Le pas de temps vaut : <%(r1)13.6G> (découpe uniforme)
   """),

20: _(u"""
    Découpe non-uniforme 
    Le premier pas de temps vaut: <%(r1)13.6G>
    Les suivant valent          : <%(r2)13.6G>
   """),

50: _(u"""
    Le pas de temps minimum <%(r1)13.6G> (PAS_MINI ou SUBD_PAS_MINI) est atteint.
    Conseils :
     - diminuez SUBD_PAS_MINI dans la gestion de la découpe
     - diminuez PAS_MINI dans la gestion de la liste d'instants
     - ajustez SUBD_NIVEAU si vous êtes en mode MANUEL pour la gestion de la découpe.
"""),

60: _(u"""
  Echec dans la tentative de découper le pas de temps.
"""),

61: _(u"""
  On peut découper le pas de temps.
"""),

99: _(u"""Avec PREDICTION = 'DEPL_CALCULE', la subdivision du pas de temps
n'est pas autorisée. """),


}
