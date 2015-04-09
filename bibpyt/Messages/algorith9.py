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
   La projection d'un vecteur complexe sur une base de RITZ n'est pas développée dans l'opérateur PROJ_VECT_BASE.
   Contactez l'équipe de développement CODE-ASTER.
 """),

    2 : _(u"""
 la méthode de Newmark est programmée sous sa forme implicite:
 le paramètre BETA ne doit pas être nul.
 """),

    4 : _(u"""
 valeur de THETA illicite
"""),

    10 : _(u"""
 nombre de vecteurs demandé trop grand
 on prend tous les modes du concept MODE_MECA
"""),

    12 : _(u"""
 La borne inférieure est incorrecte.
"""),

    15 : _(u"""
 Le pas (%(r3)f) est plus grand que l'intervalle [%(r1)f, %(r2)f].
"""),

    16 : _(u"""
 Le pas est nul.
"""),

    17 : _(u"""
 Le nombre de pas est négatif.
"""),

    18 : _(u"""
 Les matrices assemblées généralisées doivent avoir un stockage plein (cf. NUME_DDL_GENE)
"""),

    19 : _(u"""
 COEF_VAR_AMOR non nul et amortissement non présent
"""),

    26 : _(u"""
 le modèle est obligatoire
"""),

    27 : _(u"""
 impossible de combiner les mots clés CHARGE et VECT_ASSE en dehors des ondes planes
"""),

    28 : _(u"""
 concept réentrant : "RESULTAT" doit porter le même nom que la sortie
"""),

    29 : _(u"""
 concept réentrant : "RESULTAT" est d'un type différent
"""),

    30 : _(u"""
 argument en double pour "NOM_CHAM"
"""),

    34 : _(u"""
 les matrices ne possèdent pas toutes la même numérotation
"""),

    39 : _(u"""
 base modale et MATR_ASSE avec numérotations différentes
"""),

    40 : _(u"""
  type de matrice inconnu:  %(k1)s
"""),

    41 : _(u"""
 base modale et VECT_ASSE avec  numérotations différentes
"""),

    42 : _(u"""
 la base constituée ne forme pas une famille libre
"""),

    43 : _(u"""
 le nombre de valeurs doit être pair.
"""),

    44 : _(u"""
 trop d'arguments pour "NOM_CHAM"
"""),

    45 : _(u"""
 pour calculer une ACCE_ABSOLU, il faut "ACCE_MONO_APPUI"
"""),

    46 : _(u"""
 pour restituer sur un squelette, il faut "MODE_MECA"
"""),

    47 : _(u"""
 mots-clés 'SOUS_STRUC' et 'SQUELETTE' interdits
"""),

    48 : _(u"""
 le mot-clé 'MODE_MECA' doit être présent
"""),

    49 : _(u"""
 l'instant de récupération est en dehors du domaine de calcul.
"""),

    50 : _(u"""
 la fréquence  de récupération n'a pas été calculée.
"""),

    51 : _(u"""
 Vous avez demandé de restituer sur une fréquence (mot-clé FREQ) pour un concept transitoire
 sur base généralisée. Pour ce type de concept vous devez utiliser le mot-clé 'INST'.
"""),
    52 : _(u"""
 Vous avez demandé de restituer sur un instant (mot-clé INST) pour un concept harmonique
 sur base généralisée. Pour ce type de concept vous devez utiliser le mot-clé 'FREQ'.
"""),

    55 : _(u"""
 mauvaise définition de l'interspectre.
"""),

    56 : _(u"""
 le "NB_PTS" doit être une puissance de 2.
"""),

    57 : _(u"""
 si les mots-clés NUME_ORDRE et AMOR_REDUIT sont utilisés,
 il faut autant d'arguments pour l'un et l'autre
"""),

    58 : _(u"""
 le concept MODE_MECA d'entrée doit être celui correspondant à la base modale initiale
 pour le calcul de couplage fluide-structure
"""),

    60 : _(u"""
 tous les modes non couplés étant retenus, le nombre d'arguments valide
 pour le mot-clé AMOR_REDUIT est la différence entre le nombre de modes
 de la base modale initiale et le nombre de modes pris en compte pour
 le couplage fluide-structure
"""),

    61 : _(u"""
 les numéros d'ordre fournis ne correspondent pas à des modes non perturbés
"""),

    62 : _(u"""
 option symétrie : la dimension de POINT et AXE_1 doit être identique.
"""),

    63 : _(u"""
 option symétrie : AXE_2 est inutile en 2D, il est ignoré.
"""),

    64 : _(u"""
 option symétrie : la dimension de POINT et AXE_2 doit être identique.
"""),
    65 : _(u"""
 La dimension de la matrice de norme n'est pas compatible 
 avec la dimension de l'espace d'observation. Vérifiez la
 cohérence dimensionnelle de vos informations.
"""),
    66 : _(u"""
 La matrice de projection renseignée ne semble pas correspondre aux maillages
 associés aux modèles numérique et expérimental. 
"""),
    67 : _(u"""
 Les données expérimentales ne contiennent pas d'information quant au maillage
 expérimental associé, où elles sont incohérentes. On ne peut pas construire la 
 matrice d'observation.
"""),
    68 : _(u"""
 Vos données expérimentales semblent comporter des degrés de liberté de Lagrange.
 On ne peut pas traiter ce cas. Le code s'arrête.
"""),
    69 : _(u"""
 on ne sait pas traiter le champ de type:  %(k1)s
 champ :  %(k2)s
"""),
    70 : _(u"""
 Vous n'avez pas assez de mesures correspondant à la liste de fréquences demandées.
"""),
    71 : _(u"""
 Problème lors de la construction de la matrice d'observation. Le code s'arrête.
 Contactez l'assistance technique.
"""),
    74 : _(u"""
 attention, mode sur amorti
"""),

    75 : _(u"""
 attention, mode instable
"""),

    81 : _(u"""
 le vecteur directeur est nul.
"""),

    84 : _(u"""
 précision machine dépassée
"""),











    91 : _(u"""
 le nombre de noeuds mesuré doit être inférieur au nombre de noeuds du modèle
"""),

    92 : _(u"""
 maille SEG2 non trouvée
"""),

    93 : _(u"""
 intégration élastoplastique de loi BETON_DOUBLE_DP :
 pas de convergence lors de la projection au sommet des cônes de traction et de compression
 --> utiliser le redécoupage automatique du pas de temps.
"""),

    94 : _(u"""
 intégration élastoplastique de loi BETON_DOUBLE_DP :
 pas de convergence lors de la résolution pour NSEUIL =  %(k1)s
 --> utiliser le redécoupage automatique du pas de temps.
"""),

    95 : _(u"""
 non convergence à la maille:  %(k1)s
"""),

    96 : _(u"""
 la saturation n'est pas une variable interne pour la loi de couplage  %(k1)s
"""),

    97 : _(u"""
 la pression de vapeur n'est pas une variable interne pour la loi de couplage  %(k1)s
"""),

    99 : _(u"""
 la variable  %(k1)s  n'existe pas dans la loi CJS en 2D
"""),

    100 : _(u"""
 Vous ne pouvez pas mélanger deux modélisations avec et sans dépendance
des paramètres matériau à la température (mots-clés ELAS, ELAS_FO).
"""),

}
