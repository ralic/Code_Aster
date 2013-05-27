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
# person_in_charge: josselin.delmas at edf.fr

cata_msg={


3 : _(u"""
  Toutes vos zones de contact sont en mode RESOLUTION='NON'.
  Le mode REAC_GEOM = 'SANS' est forcé.
"""),

4 : _(u"""
  Toutes vos zones de contact sont en mode RESOLUTION='NON'.
  Le mode ALGO_RESO_GEOM = 'POINT_FIXE' est forcé.
"""),

11 : _(u"""
Au moins une des mailles de contact que vous avez définies est de dimension %(i1)i, or la dimension de votre problème est : %(i2)i.
Cette maille n'est donc pas une maille de bord. Il doit y avoir une erreur dans votre mise en données.

Conseil :
Vérifiez votre AFFE_MODELE et le type de vos mailles dans la définition des surfaces de contact.
"""),

12 : _(u"""
Contact avec formulation continue.
Votre modèle contient des surfaces de contact qui s'appuient sur un mélange d'éléments axisymétriques et non axisymétriques.
Cela n'a pas de sens. Toute la modélisation doit être axisymétrique.

Conseil :
Vérifiez votre AFFE_MODELE et le type de vos mailles dans la définition des surfaces de contact.
"""),

13 : _(u"""
Contact méthodes maillées.
La zone de contact numéro %(i1)i contient %(i2)i noeuds communs aux surfaces maîtres et esclaves.
Vérifiez la définition de vos surfaces de contact ou bien renseignez un des mots-clés SANS_NOEUD/SANS_GROUP_NO/SANS_MAILLE/SANS_GROUP_MA.
"""),

14 : _(u"""
Contact méthode continue.
  -> Une zone de contact est définie sur une modélisation axisymétrique. Le Jacobien
     est nul car un noeud de la surface de contact esclave appartient à l'axe.
     La pression de contact (degré de liberté LAGS_C) risque d'être erronée.
  -> Conseil :
     Il faut changer de schéma d'intégration et utiliser 'GAUSS'.
"""),

15 : _(u"""
Contact formulation discrète.
Les zones de contact numéro %(i1)i et numéro %(i2)i ont %(i3)i noeuds communs à leurs surfaces esclaves. Cela peut parfois conduire à une matrice de contact singulière.

Si le calcul venait à échouer, vérifiez la définition de vos surfaces de contact ou bien renseignez un des mots-clés SANS_NOEUD/SANS_GROUP_NO/SANS_MAILLE/SANS_GROUP_MA.
"""),

16 : _(u"""
Contact formulation continue.
Les zones de contact numéro %(i1)i et numéro %(i2)i ont %(i3)i noeuds communs à leurs surfaces esclaves : c'est interdit.
Conseil :
 - changez vos surfaces de contact.
"""),

}
