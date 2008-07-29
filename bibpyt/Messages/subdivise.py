#@ MODIF subdivise Messages  DATE 28/07/2008   AUTEUR FLEJOU J-L.FLEJOU 
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

def _(x) : return x

# Pour la méthode de subdivision

cata_msg={

# Plus de messages pour développeur ==> ASSERT

# Messages utilisateurs
9: _("""La subdivisition utilise la méthode EXTRAPOLE.
Il n'y a pas eu de détection de convergence alors que les critères sont atteints.
Cela peut arriver, si vous avez du contact avec une réactualisation géométrique.
Aucune subdivision n'est faite. Pour information :
   Pour l'itération %(i1)d, RESI_GLOB_RELA = <%(r1)E> et doit être inférieure à <%(r2)E>
   Pour l'itération %(i1)d, RESI_GLOB_MAXI = <%(r3)E> et doit être inférieure à <%(r4)E>"""),

10: _("""Le nombre maximale d'itérations autorisées ITER_GLOB_* est atteint.
La méthode de subdivision ne peut pas faire d'extrapolation.
La subdivision UNIFORME est déclenchée. Cela peut etre du à une oscillation de l'erreur ou à une divergence.
   Nombre d'intervalle             = <%(i1)d>
   Niveau de subdivision           = <%(i2)d>
   Ratio sur le premier intervalle = <%(r1)E>
   Pas de Temps actuel             = <%(r2)E>"""),

11: _("""La méthode de subdivision ne peut pas faire d'extrapolation.
Il n'y a pas de convergence et la méthode de subdivision trouve un nombre
d'itération à convergence < au nombre donné sous le mot clé CONVERGENCE.
La subdivision UNIFORME est déclanchée.
   Nombre d'intervalle             = <%(i1)d>
   Niveau de subdivision           = <%(i2)d>
   Ratio sur le premier intervalle = <%(r1)E>
   Pas de Temps actuel             = <%(r2)E>
Pour information :
   La régression est faite sur <%(k1)s>
   La méthode calcule <%(r3)d> pour <%(r4)d>"""),

12: _("""Le nombre maximal de niveau de subdivision est atteint.
      SUBD_NIVEAU doit etre >= <%(i1)d>
      SUBD_NIVEAU est de     = <%(i2)d>
Conseil :
   Augmenter SUBD_NIVEAU. Il est également possible d'ajuster SUBD_PAS_MINI pour
   imposer un incrément de temps en-dessous duquel on ne peut plus subdiviser.
   Si les 2 mots clefs SUBD_NIVEAU et SUBD_PAS_MINI sont utilisés la subdivision
   s'arrete dès que l'un des 2 critères est vérifié."""),

13: _("""Méthode de subdivision : %(k1)s"""),

14: _("""La subdivision est forcée, méthode UNIFORME.
   Nombre d'intervalle             = <%(i1)d>
   Niveau de subdivision           = <%(i2)d>
   Ratio sur le premier intervalle = <%(r1)E>
   Pas de Temps actuel             = <%(r2)E>"""),

15: _("""Le pas minimal de la subdivision est atteint.
   Pas de Temps actuel          = <%(r1)E>
   Pas de Temps minimum imposé  = <%(r2)E>
   Niveau de subdivision        = <%(i1)d>
   Méthode de subdivision       = <%(k1)s>
Conseil :
   Diminuer SUBD_PAS_MINI. Il est également possible d'ajuster SUBD_NIVEAU pour
   indiquer le nombre successif de subdivision d'un pas de temps.
   Si les 2 mots clefs SUBD_NIVEAU et SUBD_PAS_MINI sont utilisés la subdivision
   s'arrete dès que l'un des 2 critères est vérifié."""),

16: _("""Méthode extrapolation, convergence prévue en %(i1)d itérations pour un maximum autorisé de %(i2)d."""),

17: _("""Itération %(i1)d, poursuite autorisée."""),

18: _("""Subdivision du pas de temps en %(i1)d intervalles. Le ratio sur le premier intervalle est de %(r1)e.
   Niveau de subdivision = <%(i2)d>
   Pas de Temps actuel   = <%(r2)E>"""),

}
