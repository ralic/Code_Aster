#@ MODIF irrad3m Messages  DATE 17/12/2007   AUTEUR FLEJOU J-L.FLEJOU 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE FLEJOU J-L.FLEJOU

def _(x) : return x

cata_msg={

1: _("""
La <%(k1)s> dichotomie pour la loi IRRAD3M n'a pas trouvée de solution pour
le nombre d'itération donné <%(i1)d>.\n
Info pour le debug
   Borne 0                 : <%(r1).15E>
   Borne 1                 : <%(r2).15E>
   Puissance N             : <%(r3).15E>
   Pas pour la recherche   : <%(r4).15E>
   RM                      : <%(r5).15E>
   EU                      : <%(r6).15E>
   R02                     : <%(r7).15E>
   Précision demandée      : <%(r8).15E>
Valeurs initiales
   N0                      : <%(r9).15E>
   Borne 0                 : <%(r10).15E>
   Borne 1                 : <%(r11).15E>
   Borne E                 : <%(r12).15E>
"""),

2: _("""
L'irradiation diminue au cours du temps. C'EST PHYSIQUEMENT IMPOSSIBLE.
Grandeurs au point de Gauss :
   Irradiation a t- : <%(r1).15E>
   Irradiation a t+ : <%(r2).15E>
"""),

3: _("""
Pour info
   Température a t- : <%(r1)E>
   Température a t+ : <%(r2)E>
"""),

4: _("""
Le franchissement du seuil de fluage ne se fait pas dans la tolérence donnée dans DEFI_MATERIAU
pour la loi IRRAD3M, par le mot clef TOLER_ET.
   Tolérence sur le franchissement du seuil : <%(r1)E>
   Erreur sur le franchissement du seuil    : <%(r2)E>
La subdivision du pas de temps au niveau global est déclanchée.
Il faut pour cela l'autoriser avec le mot clef SUBD_METHODE de la commande STAT_NON_LINE.
""")
}
