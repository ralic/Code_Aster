# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
Erreur utilisateur dans la commande CREA_RESU / AFFE :
 Le maillage associé au mot clé CHAM_GD           : %(k1)s
 est différent de celui associé au mot clé MODELE : %(k2)s
"""),

    2 : _(u"""
 L'état initial défini n'est pas plastiquement admissible pour le modèle LETK.
 L'état initial de contraintes est erroné ou les propriétés matériaux ne sont pas adaptées au problème posé.
 Le calcul s'arrête en erreur fatale par précautions.
"""),

    3 : _(u"""
 REPERE='COQUE' ne traite que les champs aux éléments, pas les champs aux noeuds.
 On arrête le calcul.
"""),

    4 : _(u"""
 Le repère utilisateur défini par VECT_X et VECT_Y ne peut être utilisé qu'en 3D.
"""),


    6 : _(u"""
 il faut définir NOM_CMP
"""),

    7 : _(u"""
 Il faut définir 3 angles nautiques.
"""),

    8 : _(u"""
 L'origine doit être définie par 3 coordonnées.
"""),

    9 : _(u"""
 L'axe z est obligatoire en 3D.
"""),

    10 : _(u"""
 En 2D, seules les premières 2 coordonnées sont considérées pour l'origine.
"""),

    11 : _(u"""
 L'axe z n'a pas de sens en 2D. Le mot-clé AXE_Z est inutile.
"""),

    13 : _(u"""
  -> Lors du passage au repère cylindrique, un noeud a été localisé sur l'axe
     du repère cylindrique. Code_Aster utilise dans ce cas le centre de gravité de
     l'élément pour le calcul de la matrice de passage en repère cylindrique.
  -> Risque & Conseil :
     Si ce centre de gravité se trouve également sur l'axe du repère, le calcul
     s'arrête en erreur fatale.
"""),


    15 : _(u"""
 les modélisations autorisées sont 3D et D_PLAN et AXIS
"""),

    16 : _(u"""
 le choix des paramètres ne correspond pas à l'un des modèles CJS
"""),

    17 : _(u"""
 la loi CJS ne converge pas
"""),

    18 : _(u"""
 la loi CJS ne converge pas avec le nombre maximal d'itérations (intégration locale)
"""),

    20 : _(u"""
 modélisation inconnue
"""),

    22 : _(u"""
 vecteur de norme nulle
"""),

    23 : _(u"""
 Le type de maille %(k1)s n'est pas prévu.
"""),

    24 : _(u"""
 la maille doit être de type TETRA4, TETRA10, PENTA6, PENTA15, HEXA8 ou HEXA20.
 ou TRIA3-6 ou QUAD4-8
 or la maille est de type :  %(k1)s .
"""),

    26 : _(u"""
  %(k1)s  groupe inexistant
"""),

    27 : _(u"""
 La maille  %(k1)s  de type  %(k2)s  est invalide pour ORIE_FISSURE.
"""),

    28 : _(u"""
 Le groupe de mailles pour ORIE_FISSURE est invalide.
"""),

    29 : _(u"""
 Dans le groupe à réorienter pour ORIE_FISSURE,
 Il existe des mailles 2d et 3d.
 C'est interdit.
"""),

    30 : _(u"""
Erreur d'utilisation pour MODI_MAILLAGE / ORIE_FISSURE :
  On ne peut pas orienter les mailles.

Risques & conseils :
  Les mailles à orienter doivent correspondre à une mono couche d'éléments (2D ou 3D).
  Ces éléments doivent s'appuyer sur d'autres éléments de même dimension (2D ou 3D)
  pour pouvoir déterminer la direction transverse à la couche.
  Consulter la documentation d'utilisation pour plus de détails.
"""),










    43 : _(u"""
 pas de calcul sur le critère de Rice disponible
"""),

    44 : _(u"""
 cette commande doit nécessairement avoir le type EVOL_THER.
"""),

    45 : _(u"""
 seuls les champs de fonctions aux noeuds sont évaluables:  %(k1)s
"""),

    46 : _(u"""
 nous traitons les champs de réels et de fonctions: . %(k1)s
"""),

    47 : _(u"""
 le nom symbolique du champ à chercher n'est pas licite. %(k1)s
"""),

    48 : _(u"""
 plusieurs instants correspondent à celui spécifié sous AFFE
"""),

    49 : _(u"""
 NUME_FIN inférieur à NUME_INIT
"""),

    50 : _(u"""
 CMP non traitée
"""),

    54 : _(u"""
  incrément de déformation cumulée (DP) = - %(k1)s
"""),

    55 : _(u"""
 erreur d'intégration
 - essai d(intégration  numéro  %(k1)s
 - convergence vers une solution non conforme
 - incrément de déformation cumulée négative = - %(k2)s
 - redécoupage du pas de temps
"""),

    56 : _(u"""
  erreur
  - non convergence à l'itération max  %(k1)s
  - convergence régulière mais trop lente
  - erreur >  %(k2)s
  - redécoupage du pas de temps
"""),

    57 : _(u"""
  erreur
  - non convergence à l'itération max  %(k1)s
  - convergence irrégulière & erreur >  %(k2)s
  - redécoupage du pas de temps
"""),

    58 : _(u"""
  erreur
  - non convergence à l'itération max  %(k1)s
  - erreur >  %(k2)s
  - redécoupage du pas de temps
"""),

    59 : _(u"""
  la transformation géométrique est singulière pour la maille : %(k1)s
  (jacobien = 0.)
"""),









    63 : _(u"""
 on n'imprime que des champs réels
"""),

    64 : _(u"""
  %(k1)s CHAM_NO déjà existant
"""),

    65 : _(u"""
 appel erroné a RSEXCH
"""),

    66 : _(u"""
 calcul du transitoire : choc en phase transitoire - pas de solution trouvée.
 utiliser l'option ETAT_STAT = NON
"""),

    79 : _(u"""
 pas de valeurs propres trouvées
"""),


    80 : _(u"""
 le champ %(k1)s associé à la grandeur de type %(k2)s ne peut pas être utilisé dans une
 structure de données de type %(k3)s
"""),

}
