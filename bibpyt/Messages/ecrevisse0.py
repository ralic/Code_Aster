#@ MODIF ecrevisse0 Messages  DATE 21/04/2010   AUTEUR BOTTONI M.BOTTONI 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={
1: _("""
    Informations extraites d'Aster
    %(k1)s:
    Inst : %(r1)f
    Absc(T)        : %(r2)f - %(r3)f
    Min T          : %(r4)f
    Max T          : %(r5)f
    Absc(position) : %(r6)f - %(r7)f
"""),

2: _("""
    Informations en entree d'Ecrevisse
    Inst : %(r1)f
    Absc(T)   : %(r2)f - %(r3)f
    Min Temperature moyenne   : %(r4)f
    Max Temperature moyenne   : %(r5)f
    Absc(Ouverture) : %(r6)f - %(r7)f
    Min Ouverture   : %(r8)e
    Max Ouverture   : %(r9)e
    Min Glissement  : %(r10)e
    Max Glissement  : %(r11)e
"""),

3: _("""
 INSTANT : %(r1)f. Il n'y a pas de resultat thermique, on ne lance pas Ecrevisse...
"""),

4: _("""
 INSTANT : %(r1)f. Les ouvertures sont trop faibles %(r2)f, on ne lance pas Ecrevisse...
"""),

5: _("""
 INSTANT : %(r1)f. Les temperatures sont trop fortes %(r2)f, on ne lance pas Ecrevisse...
"""),

6: _("""
 INSTANT : %(r1)f. Les temperatures sont trop faibles %(r2)f, on ne lance pas Ecrevisse...
"""),

7: _("""
 INSTANT : %(r1)f. Le differentiel de pression est trop faible %(r2)f, on ne lance pas Ecrevisse...
"""),

8: _("""
 INSTANT : %(r1)f. On lance Ecrevisse...
"""),

9: _("""
 INSTANT : %(r1)f. Probleme dans la recuperation des resultats Ecrevisse...
"""),


11: _("""
 Erreur systeme : impossible de generer le fichier de donnees pour Ecrevisse!
"""),

12: _("""
 Impossible de creer le repertoire de travail pour le logiciel Ecrevisse : %(k1)s
"""),

13: _("""
 L'executable indique par le mot-cle LOGICIEL n'existe pas!
"""),

14: _("""
 Impossible de faire un lien symbolique, on copie l'executable Ecrevisse
"""),

15: _("""
 Impossible de copier l'executable Ecrevisse
"""),

16: _("""
 Lancement de l'execution d'Ecrevisse...
"""),

17: _("""
 Fin de l'execution d'Ecrevisse.
"""),

18: _("""
 Il n'y a pas de fichiers resultats d'Ecrevisse.
 On renvoit une table vide.
 Penser a verifier que le debit soit etabli et non nul.
"""),

20: _("""
 Il faut au minimum %(i1)d temps dans la liste d'instants
"""),

22: _("""
 Il faut renseigner la temperature de reference dans AFFE_MATERIAU.
"""),

23: _("""
 ATTENTION : l'ancienne version du couplage Aster-Ecrevisse (qui utilise le flux de chaleur)
 ne marche que avec fissures horizontales et verticales!!
 L'angle theta forme avec la verticale est egal a  %(r1)f.
"""),

24: _("""
 ERREUR : copyfile %(k1)s --> %(k2)s
"""),

30: _("""
 Nombre de decoupage d'un pas de temps atteint. On arrete le processus.
 Tous les instants converges sont conserves.
   entre l'instant %(r1)f et l'instant %(r2)f
   MACR_ECREVISSE/MACR_CONVERGENCE/SUBD_NIVEAU : %(i1)d
"""),

31: _("""
 Pas de temps mini atteint lors du decoupage d'un pas de temps. On arrete le processus.
 Tous les instants converges sont conserves.
   entre l'instant %(r1)f et l'instant %(r2)f, on ne peut pas inserer l'instant %(r3)f
   MACR_ECREVISSE/MACR_CONVERGENCE/SUBD_PAS_MINI : %(r4)f
"""),

32: _("""
 Non convergence, iteration %(i1)d, ajout d'un pas temps dans l'intervalle de temps [ %(r1)f , %(r2)f ]
 Insertion de l'instant %(r3)f
"""),

33: _("""
 Le NUME_ORDRE_MIN %(i1)d qui correspond a l'instant %(r1)f est <= %(i2)d ]
 La convergence est forcee.
"""),

34: _("""
 CONVERGENCE MACR_ECREVISSE - Instant de calcul : %(r1)f
   Erreur en Temperature : %(r2)f ; Ecart en Temperature : %(r3)f
   Erreur en Pression    : %(r4)f ; Ecart en Pression    : %(r5)f
   Erreur Temp-Press     : %(r6)f  ]
"""),

34: _("""
 CONVERGENCE MACR_ECREVISSE - Instant de calcul : %(r1)f
   Erreur en Temperature : %(r2)f ; Ecart en Temperature : %(r3)f
   Erreur en Pression    : %(r4)f ; Ecart en Pression    : %(r5)f
   Erreur Temp-Press     : %(r6)f  ]
"""),

35: _("""
'CONVERGENCE MACR_ECREVISSE - Instant de calcul : %(r1)f
    Nature du Critere : %(k1)s
    Valeur du critere : %(k2)s
    Convergence : %(k3)s
"""),

36: _("""
'CONVERGENCE MACR_ECREVISSE - 1er Instant de calcul : %(r1)f
   Pas de calcul de critere.
"""),

37: _("""
'Convergence atteinte a l'instant de calcul %(r1)f, on passe au pas de temps suivant'
"""),

38: _("""
'ATTENTION : au moins pour une valeur des cotes, le glissement relatif des deux points
correspondants sur les levres de le fissure est plus grand que la dimension des mailles adjacentes '
"""),

39: _("""
'ATTENTION : au moins pour une valeur des cotes, la distance entre deux points
correspondants sur les levres de la fissure est plus petite que l'ouverture remanente'
"""),

40: _("""
'ATTENTION : il n' y a pas le meme nombre de noeuds sur les levres de la fissure.
"""),

41: _("""
'INFO : Le calcul d'ecoulement a ete fait avec un ouverture de fissure egale a l'ouverture remanante pour %(r1)f points'
"""),

42: _("""
'Le parametre TORTUOSITE doit etre strictement positif (>0)'
"""),
}
