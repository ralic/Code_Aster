#@ MODIF plexus Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _(u"""La macro-commande MACR_EUROPLEXUS ne fonctionne pas encore en poursuite"""),

2:  _(u"""Le mot-cle %(k1)s n'existe pas"""),

3 : _(u"""Le mot-cle GROUP_MA est obligatoire dans AFFE_MODELE"""),

4 : _(u"""Le type de section de poutre %(k1)s n'est pas encore pris en compte dans le passage vers Europlexus"""),

7 : _(u"""Le mot-cle FONC_MULT est obligatoire pour le chargement de type PRES_REP"""),

9 : _(u"""Il faut avoir au moins un des mots-cles DDL_IMPO et PRES_REP dans AFFE_CHAR_MECA"""),

10: _(u"""
Les vecteurs y_local des group_ma %(k1)s
calcul�s � partir des anhgles nautiques ne sont pas identiques.
Veuillez imposer directement VECT_Y dans AFFE_CARA_ELEM si vous
etes sur de l'orientation
"""),

11: _(u"""
Les vecteurs y_local des mailles du group_ma %(k1)s
calcul�s � partir des anhgles nautiques ne sont pas identiques.
Veuillez imposer directement VECT_Y dans AFFE_CARA_ELEM si vous
etes sur de l'orientation
"""),

12: _(u"""
Le mot-cle facteur FONC_PARASOL est obligatoire quand le mot-cle RIGI_PARASOL e
est renseign� dans AFFE_CARA_ELEM
"""),

13: _(u"""
Les deplacements impos�s non nuls dans DDL_IMPO ne sont pas autoris�s
"""),

14 : _(u"""
Le fichier MED contenant les r�sultats d'Europlexus est introuvable.
L'ex�cution d'Europlexus s'est probablement mal d�roul�e
"""),

15 : _(u"""En pr�sence du mot-cl� %(k1)s dans AFFE_CARA_ELEM
le mot-cl� %(k2)s devrait �tre pr�sent dans CALC_EUROPLEXUS.
"""),

16 : _(u"""En pr�sence du mot-cl� %(k1)s dans CALC_EUROPLEXUS
le mot-cl� %(k2)s est obligatoire dans AFFE_CARA_ELEM.
"""),

}
