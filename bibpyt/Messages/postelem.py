#@ MODIF postelem Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
# RESPONSABLE DELMAS


cata_msg={

1: _(u"""
 Un instant demandé dans POST_ELEM, option CHAR_LIMITE n'est pas présent dans le résultat <%(k1)s>.
"""),

2: _(u"""
 Impossible de trouver les déplacements de numéro d'ordre %(i1)d dans POST_ELEM, option CHAR_LIMITE.
"""),

3: _(u"""
 Le résultat <%(k1)s> utilisé dans POST_ELEM, option CHAR_LIMITE n'a pas été produit par un STAT_NON_LINE avec pilotage.
 Vérifiez que vous utilisez le bon résultat.
"""),

4: _(u"""
 Avec le mot-clé RESULTAT, il faut renseigner NOM_CHAM pour identifier le champ sur lequel réaliser le post-traitement.
"""),

11: _(u"""
 Un instant demandé dans POST_ELEM, option TRAV_EXT n'est pas présent dans le résultat <%(k1)s>.
"""),

12: _(u"""
 Impossible de trouver les déplacements <DEPL> de numéro d'ordre %(i1)d dans POST_ELEM, option TRAV_EXT.
"""),

13: _(u"""
 Impossible de trouver les forces nodales <FORC_NODA> de numéro d'ordre %(i1)d dans POST_ELEM, option TRAV_EXT.
"""),

}

