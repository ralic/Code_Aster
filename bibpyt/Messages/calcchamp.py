# coding=utf-8
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
# person_in_charge: nicolas.sellenet at edf.fr

cata_msg = {

    1 : _(u"""
 Le champ %(k1)s est déjà présent dans la structure de données
 à tous les instants demandés.
 Aucun calcul ne sera donc réalisé pour cette option.

Conseil :
 Si vous souhaitez réellement calculer à nouveau cette option,
 créez une nouvelle structure de données.
"""),

    2 : _(u"""
 L'option %(k1)s nécessaire au calcul de l'option %(k2)s est
 manquante dans les structures de données résultat %(k3)s et
 %(k4)s pour le numéro d'ordre %(i1)d.

 Le calcul de cette option n'est donc pas possible.
 L'option demandée n'est calculable sur les éléments du modèle.
"""),

3 : _(u"""
La sortie EPSI_%(k2)s de CALC_CHAMP est incorrecte pour décrire les
composantes de la déformation en grandes déformations.

Conseil:
Avec des déformations de type %(k1)s,
il est préférable d'utiliser EPSG_%(k2)s
"""),

4 : _(u"""
Les contributions de l'amortissement liées à la vitesse pour les 
réactions nodales sont négligées dans la version actuelle du code.
"""),
}
