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
# person_in_charge: mickael.abbas at edf.fr

cata_msg = {

    7 : _(u"""Échec lors du calcul des modes empiriques pour l'estimation du domaine réduit."""),    

    9 : _(u"""Les bases ne sont pas définies sur le même maillage."""),

   10 : _(u"""Les bases ne sont pas définies sur le maillage %(k1)s."""),

   11 : _(u"""Le modèle doit être le même sur tous les modes des bases."""),

   12 : _(u"""Le GROUP_NO %(k1)s fait déjà partie du maillage."""),

   13 : _(u"""Le GROUP_MA %(k1)s fait déjà partie du maillage."""),

   16 : _(u"""Les modes empiriques ne sont pas des champs du type attendu."""),

   17 : _(u"""Les modes empiriques ne sont pas des champs gradients du type attendu (on attend %(k1)s). """),

   20 : _(u"""Calcul du domaine réduit sur la base %(k1)s. """),

   21 : _(u"""Création des groupes dans le maillage pour l'estimation du domaine réduit."""),  

   22 : _(u"""Nombre d'éléments dans le domaine réduit: %(i1)d"""), 

   23 : _(u"""Nombre de noeuds sur l'interface du domaine réduit: %(i1)d"""),

   24 : _(u"""Création des structures de données pour l'estimation du domaine réduit."""), 

   25 : _(u"""Nombre total de points magiques pour l'estimation du domaine réduit: %(i1)d"""),
}
