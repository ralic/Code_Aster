#@ MODIF adaptation Messages  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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

# Pour la méthode d'adaptation du pas de temps

cata_msg={

1: _(u"""
  Adaptation du pas de temps.
"""),

2: _(u"""
    Pour la méthode d'adaptation de type <%(k1)s>, le pas de temps calculé vaut <%(r1)19.12e>.
"""),

3: _(u"""
    Pour la méthode d'adaptation de type <%(k1)s>, le critère n'est pas vérifié. Le pas de temps n'est pas adapté.
"""),

4: _(u"""
    Aucun critère d'adaptation n'est vérifié. On garde le pas de temps <%(r1)19.12e>.
"""),

5: _(u"""
    Sur tous les critères d'adaptation, le plus petit pas de temps vaut <%(r1)19.12e>.
"""),

6: _(u"""
    Après ajustement sur les points de passage obligatoires, le plus petit pas de temps vaut <%(r1)19.12e>.
"""),

10 : _(u"""
    On maintient la découpe du pas de temps à <%(r1)19.12e>.
"""),

11 : _(u"""
    La valeur du pas de temps retenu <%(r1)19.12e> est inférieure à PAS_MINI.
"""),

12 : _(u"""
    La valeur du pas de temps <%(r1)19.12e> est supérieure à PAS_MAXI <%(r2)19.12e>.
    On limite le pas de temps à PAS_MAXI <%(r2)19.12e>.
"""),

13 : _(u"""
 On a dépassé le nombre maximal de pas de temps autorisé.
"""),
}
