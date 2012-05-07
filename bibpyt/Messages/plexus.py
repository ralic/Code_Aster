#@ MODIF plexus Messages  DATE 07/05/2012   AUTEUR ASSIRE A.ASSIRE 
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

1 : _(u"""La macro-commande MACR_EUROPLEXUS ne fonctionne pas encore en poursuite"""),

2:  _(u"""Le mot-clé %(k1)s n'existe pas"""),

3 : _(u"""Le mot-clé GROUP_MA est obligatoire dans AFFE_MODELE"""),

4 : _(u"""Le type de section de poutre %(k1)s n'est pas encore pris en compte dans le passage vers Europlexus"""),

5 : _(u"""Le type de section de barre %(k1)s n'est pas encore pris en compte dans le passage vers Europlexus"""),

6 : _(u"""La modélisation %(k1)s n'est pas disponible dans CALC_EUROPLEXUS"""),

7 : _(u"""Le mot-clé FONC_MULT est obligatoire pour le chargement de type PRES_REP"""),

9 : _(u"""Il faut avoir au moins un des mots-clés DDL_IMPO et PRES_REP dans AFFE_CHAR_MECA"""),

10: _(u"""
Les vecteurs y_local des GROUP_MA %(k1)s
calculés à partir des angles nautiques ne sont pas identiques.
Veuillez imposer directement VECT_Y dans AFFE_CARA_ELEM si vous
êtes sur de l'orientation
"""),

11: _(u"""
Les vecteurs y_local des mailles du GROUP_MA %(k1)s
calculés à partir des angles nautiques ne sont pas identiques.
Veuillez imposer directement VECT_Y dans AFFE_CARA_ELEM si vous
êtes sur de l'orientation
"""),

12: _(u"""
Le mot-clé facteur FONC_PARASOL est obligatoire quand le mot-clé RIGI_PARASOL e
est renseigné dans AFFE_CARA_ELEM
"""),

13: _(u"""
Les déplacements imposés non nuls dans DDL_IMPO ne sont pas autorisés
"""),

14 : _(u"""
Le fichier MED contenant les résultats d'Europlexus est introuvable.
L'exécution d'Europlexus s'est probablement mal déroulée
"""),

15 : _(u"""En présence du mot-clé %(k1)s dans AFFE_CARA_ELEM
le mot-clé %(k2)s devrait être présent dans CALC_EUROPLEXUS.
"""),

16 : _(u"""En présence du mot-clé %(k1)s dans CALC_EUROPLEXUS
le mot-clé %(k2)s est obligatoire dans AFFE_CARA_ELEM.
"""),

}
