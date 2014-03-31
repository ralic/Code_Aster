# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

1 : _(u"""
Pour que CALC_EUROPLEXUS fonctionne il faut ajouter DEBUG=_F(HIST_ETAPE='OUI')
dans la commande DEBUT.
Remarque : CALC_EUROPLEXUS ne fonctionne pas en POURSUITE"""),

2:  _(u"""Le mot-clé %(k1)s n'existe pas"""),

3 : _(u"""Le mot-clé GROUP_MA est obligatoire dans AFFE_MODELE"""),

4 : _(u"""Le type de section de poutre %(k1)s n'est pas encore pris en compte dans le passage vers Europlexus"""),

5 : _(u"""Le type de section de barre %(k1)s n'est pas encore pris en compte dans le passage vers Europlexus"""),

6 : _(u"""La modélisation %(k1)s n'est pas disponible dans CALC_EUROPLEXUS"""),

7 : _(u"""Le mot-clé FONC_MULT est obligatoire pour le chargement de type PRES_REP"""),

8 : _(u"""Le concept EVOL_NOLI %(k1)s ne possède pas de mot-clé CARA_ELEM"""),

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

17 : _(u"""On ne peut pas fournir un état initial de contraintes sur les éléments POU_D_EM.
"""),

18 : _(u"""Le mot clé %(k1)s du concept CARA_ELEM n'est pas pris en compte par CALC_EUROPLEXUS'
"""),
19 : _(u"""Le type de charge %(k1)s n'est pas pris en compte par CALC_EUROPLEXUS'
"""),
20 : _(u"""Les groupes de mailles auxquels le matériau %(k1)s est affecté n'ont pas tous la même relation.
           Voir mot-clé COMPORTEMENT.
"""),

21 : _(u"""On ne trouve pas les caractéristiques élastiques du matériau %(k1)s.
"""),
22 : _(u"""La valeur du mot-clé RELATION du matériau %(k1)s est différente de GLRC_DAMAGE.
"""),
23 : _(u"""On ne trouve pas le mot-clé BETON dans le matériau %(k1)s.
           Ce mot-clé est indispensable à la loi GLRC_DAMAGE.
"""),

}
