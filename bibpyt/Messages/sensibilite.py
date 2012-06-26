#@ MODIF sensibilite Messages  DATE 25/06/2012   AUTEUR ABBAS M.ABBAS 
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

cata_msg = {


3 : _(u"""
 La dérivée de %(k1)s par rapport à %(k2)s est introuvable.
"""),

15 : _(u"""
 Le comportement %(k1)s n'est pas autorisé en sensibilité
"""),


52 : _(u"""
 Actuellement, on ne sait dériver que les 'POU_D_E'.
"""),

53 : _(u"""
 En thermo élasticité, le calcul des dérivées de g est pour le moment incorrect.
"""),

54 : _(u"""
 Avec un chargement en déformations (ou contraintes) initiales, le calcul
 des dérivées de g est pour le moment incorrect.
"""),

55 : _(u"""
 Le calcul de dérivée n'a pas été étendu à la plasticité.
"""),

57 : _(u"""
 Le calcul de dérivée n'a pas été prévu pour les variables de commande de séchage ou d'hydratation.
"""),


}
