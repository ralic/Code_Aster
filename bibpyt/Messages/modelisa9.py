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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {
    1 : _(u"""
Erreur utilisateur dans la commande CREA_CHAMP / EXTR / TABLE :
   Dans la table %(k1)s, pour créer un champ de type %(k2)s,
   certains paramètres sont obligatoires et d'autres sont interdits :

     NOEU :
       obligatoire : NOEUD
       interdit    : MAILLE, POINT, SOUS_POINT
     ELGA :
       obligatoire : MAILLE, POINT
       interdit    : NOEUD
     ELNO :
       obligatoire : MAILLE, NOEUD (ou POINT)
     ELEM :
       obligatoire : MAILLE
       interdit    : NOEUD, POINT
"""),

    2 : _(u"""
 Erreur utilisateur dans la commande CREA_CHAMP / EXTR / TABLE  :
   Les paramètres de la table doivent être :
     - soit : MAILLE, NOEUD, POINT, SOUS_POINT
     - soit le nom d'une composante de la grandeur.

   L'une (au moins) des valeurs de la liste : %(ktout)s
   n'est pas le nom d'une composante de la grandeur.

 Conseil :
   Si la table fournie provient de la commande CREA_TABLE / RESU, il faut
   probablement utiliser la commande CALC_TABLE + OPERATION='EXTR' pour
   éliminer les colonnes RESULTAT, NUME_ORDRE, ....
"""),

    3 : _(u"""
Alarme de programmation :
   Plusieurs matériaux ont été affectés sur la maille %(k1)s.
   Le programme cherche un paramètre matériel (%(k2)s) sans préciser le matériau.
   On utilise le premier matériau de la liste affectée sur la maille.

Risques et conseils :
   Le comportement du code est dangereux car les résultats dépendent de l'ordre  
   des matériaux affectés dans la commande AFFE_MATERIAU.
   Il faut sans doute émettre une demande de correction du code.
"""),





    5 : _(u"""
 Erreur utilisateur :
   On cherche à créer un CHAM_ELEM / ELNO à partir d'une table (%(k1)s).
   La maille  %(k2)s a %(i2)d noeuds mais dans la table,
   une ligne concerne un noeud "impossible" :
   noeud de numéro local  %(i1)d  ou noeud de nom %(k3)s
"""),

    6 : _(u"""
Erreur utilisateur :
   Plusieurs valeurs dans la table %(k1)s pour :
   Maille: %(k2)s
   Point : %(i1)d  ou Noeud : %(k3)s
   Sous-point : %(i2)d
"""),




    8 : _(u"""
Erreur :
   On cherche à transformer un CHAM_ELEM en carte.
   CHAM_ELEM : %(k1)s  carte : %(k2)s
   Pour la maille numéro %(i3)d le nombre de points (ou de sous-points) est > 1
   ce qui est interdit.
   Point:       %(i1)d
   Sous-point : %(i2)d
"""),

    9 : _(u"""
Erreur utilisateur :
   Pour le matériau : %(k1)s, on cherche à redéfinir un mot clé déjà défini :
   %(k2)s
"""),

    10 : _(u"""
Erreur utilisateur :
   Comportement 'HUJEUX'
   Non convergence pour le calcul de la loi de comportement (NB_ITER_MAX
atteint).

Conseil :
   Augmenter NB_ITER_MAX (ou diminuer la taille des incréments de charge)

"""),

    11 : _(u"""
 mot-clé facteur non traite :  %(k1)s
"""),

    15 : _(u"""
 pas de FREQ initiale définie : on prend la fréquence mini des modes calcules
   %(r1)f
"""),

    16 : _(u"""
 pas de fréquence finale définie : on prend la fréquence max des modes calcules
 %(r1)f
"""),

    17 : _(u"""
 votre fréquence de coupure   %(r1)f
"""),

    18 : _(u"""
 est inférieure a celle  du modèle de turbulence adopte :  %(r1)f
"""),

    19 : _(u"""
 on prend la votre.
"""),

    20 : _(u"""
 votre fréquence de coupure :   %(r1)f
"""),

    21 : _(u"""
 est supérieure a celle  du modèle de turbulence adopte :   %(r1)f
"""),

    22 : _(u"""
 on prend celle du modèle.
"""),

    23 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
"""),

    24 : _(u"""
 le maillage est "plan" ou "z_CSTE"
"""),

    25 : _(u"""
 le maillage est "3d"
"""),

    26 : _(u"""
 il y a  %(i1)d  valeurs pour le mot clé  %(k1)s il en faut  %(i2)d
"""),

    27 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
 pour le mot clé  %(k2)s
  le noeud n'existe pas  %(k3)s
"""),

    28 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
 pour le mot clé  %(k2)s
  le GROUP_NO n'existe pas  %(k3)s
"""),

    29 : _(u"""
 trop de noeuds dans le GROUP_NO mot clé facteur  %(k1)s  occurrence  %(i1)d
   noeud utilise:  %(k2)s
"""),

    31 : _(u"""
 poutre : occurrence %(i2)d :
 "CARA" nombre de valeurs entrées:  %(i2)d
 "VALE" nombre de valeurs entrées:  %(i3)d
 vérifiez vos données

"""),

    32 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
"""),





    35 : _(u"""
 il y a  %(i1)d  valeurs pour le mot clé  ANGL_NAUT il en faut  %(i2)d
"""),

    36 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
"""),

    39 : _(u"""
 il y a  %(i1)d  valeurs pour le mot clé  %(k1)s il en faut  %(i2)d
"""),

    40 : _(u"""
 erreur dans les données mot clé facteur  %(k1)s  occurrence  %(i1)d
"""),

    43 : _(u"""
 il y a  %(i1)d  valeurs pour le mot clé  %(k1)s il en faut  %(i2)d
"""),

    44 : _(u"""
 fichier MED :  %(k1)s maillage :  %(k2)s erreur effoco numéro  %(i1)d
"""),

    51 : _(u"""
 fichier MED :  %(k1)s maillage :  %(k2)s erreur efouvr numéro  %(i1)d
"""),

    52 : _(u"""
 fichier MED :  %(k1)s maillage :  %(k2)s erreur efferm numéro  %(i1)d
"""),

    53 : _(u"""

 l'identifiant d'une maille dépasse les 8 caractères autorisés:
   %(k1)s
 maille      : %(k2)s
 PREF_MAILLE : %(k3)s
"""),

    54 : _(u"""
 l'utilisation de 'PREF_NUME' est recommandée.
"""),

    55 : _(u"""
 comportement : %(k1)s non trouve
"""),

    56 : _(u"""
 pour la maille  %(k1)s
"""),

    57 : _(u"""
Erreur d'utilisation pour la commande CREA_MAILLAGE / CREA_GROUP_MA :
  Le programme tente de créer une maille appelée %(k1)s
  mais cette maille a déjà été crée par une occurrence précédente de ce mot clé facteur.

Risques et conseils :
  Pour éviter cette erreur, vous pouvez changer la valeur
  de PREF_MAILLE pour chaque occurrence du mot clé CREA_GROUP_MA.
  """),




    59 : _(u"""
 erreur lors de la définition de la courbe de traction : %(k1)s
 le premier point de la courbe de traction %(k2)s a pour abscisse:  %(r1)f

"""),

    60 : _(u"""
 erreur lors de la définition de la courbe de traction :%(k1)s
 le premier point de la courbe de traction %(k2)s a pour ordonnée:  %(r1)f

"""),

    61 : _(u"""
 Erreur lors de la définition de la courbe de traction : %(k1)s

 la courbe de traction doit satisfaire les conditions suivantes :
 - les abscisses (déformations) doivent être strictement croissantes,
 - la pente entre 2 points successifs doit être inférieure a la pente
   élastique (module de Young) entre 0 et le premier point de la courbe.

 pente initiale (module de Young) :   %(r1)f
 pente courante                  :   %(r2)f
 pour l'abscisse                 :   %(r3)f

"""),

    62 : _(u"""
 Courbe de traction : %(k1)s points presque alignés. Risque de PB dans
 STAT_NON_LINE en particulier en C_PLAN
  pente initiale :     %(r1)f
  pente courante:      %(r2)f
  précision relative:  %(r3)f
  pour l'abscisse:     %(r4)f

"""),

    63 : _(u"""
 erreur lors de la définition de la courbe de traction %(k1)s
 le premier point de la fonction indicée par :  %(i1)d de la nappe  %(k2)s
 a pour abscisse:  %(r1)f

"""),

    64 : _(u"""
 erreur lors de la définition de la courbe de traction %(k1)s
 le premier point de la fonction indicée par :  %(i1)d de la nappe  %(k2)s
 a pour ordonnée:  %(r1)f

"""),

    65 : _(u"""
 erreur lors de la définition de la courbe de traction %(k1)s
 pente initiale :   %(r1)f
 pente courante:    %(r2)f
 pour l'abscisse:  %(r3)f

"""),



    74 : _(u"""
 comportement :%(k1)s non trouvé
"""),

    75 : _(u"""
Le matériau '%(k1)s' n'a pas été affecté à la maille %(k2)s.

Conseil:
    Vérifiez que le mot-clé facteur '%(k1)s' a bien été renseigné dans
    DEFI_MATERIAU et que ce matériau a bien été affecté (par AFFE_MATERIAU)
    sur cette partie du modèle.
"""),

    77 : _(u"""
 manque le paramètre  %(k1)s
"""),

    78 : _(u"""
 pour la maille  %(k1)s
"""),

    80 : _(u"""
 Il n'y a aucun mot clé défini sous le comportement %(k1)s dans le fichier de
 commandes.
 Ce mot clé facteur n'apparaît donc pas dans l'écho des commandes.
 Ce mot clé facteur est inutile.
"""),

    81 : _(u"""
  La maille de nom %(k1)s n'est pas de type SEG3 ou SEG4,
  elle ne sera pas affectée par %(k2)s
"""),

    82 : _(u"""
  GROUP_MA : %(k1)s
"""),

    83 : _(u"""
Le nom du comportement '%(k1)s' est trop long, il dépasse les 32 caractères autorisés.
"""),

    84 : _(u"""
Le nom du paramètre >>%(k1)s<< est trop long, il dépasse les 16 caractères autorisés,
Le nom sera tronqué à 16 caractères.
"""),

    92 : _(u"""
Erreur Europlexus
   Le mot-clé GROUP_MA_SEG2 est incompatible avec EUROPLEXUS = 'OUI'.
"""),

    93 : _(u"""
Erreur Europlexus
   Toutes les occurrences de RIGI_PARASOL doivent avoir la même valeur pour le
   mot clef EUROPLEXUS. La valeur du mot clef EUROPLEXUS à l'occurrence %(i1)d
   est différente de sa valeur à l'occurrence numéro 1.
"""),

    94 : _(u"""
     On ne peut pas appliquer un cisaillement 2d sur une modélisation 3D
"""),
    95 : _(u"""
     ERREUR: l'auto-spectre est a valeurs négatives
"""),
    96 : _(u"""
Erreur Europlexus
   Europlexus ne gère pas les MAILLES, mais seulement les POINTS.
   Le problème vient de la maille %(k1)s.
"""),

    97 : _(u"""
Erreur Europlexus
   Données incorrectes. Les dimensions des objets ne sont pas cohérentes (Erreur
   Fortran : acearp)
"""),

    98 : _(u"""
Erreur Europlexus
   Pour accéder aux valeurs nécessaires à Europlexus, il faut que dans la
   commande AFFE_CARA_ELEM, pour le mot clef facteur RIGI_PARASOL, la valeur
   du mot clef EUROPLEXUS soit 'OUI' dans toutes les occurrences.
"""),

}
