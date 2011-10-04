#@ MODIF supervis2 Messages  DATE 03/10/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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

def _(x) : return x

cata_msg={

1 : _("""
Lecture du fichier pick.1..."""),

3 : _("""%(k1)-8s %(k2)-16s ignoré"""),

5 : _("""
Erreur inattendue lors de l'exécution de la commande '%(k1)s'.
Merci de signaler cette anomalie.

Traceback :

%(k2)s
%(k3)s
"""),

6 : _("""
Erreur lors du chargement du catalogue du matériau '%(k1)s'.
"""),

7 : _("""
Erreur dans le catalogue du matériau '%(k1)s'.

Il n'est pas possible d'avoir plusieurs occurrences pour le
mot-clé facteur '%(k2)s'.
"""),


8 : _("""

L'opération de retassage de la base GLOBALE (mot clé RETASSAGE="OUI"
dans la commande FIN) est inutile lorsque l'on sauvegarde cette 
dernière au format HDF (mot clé FORMAT_HDF="OUI" dans la commande FIN).

"""),
}
