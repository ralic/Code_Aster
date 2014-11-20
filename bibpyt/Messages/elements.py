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
 AFFE_CARA_ELEM : mot clé GENE_TUYAU
 problème : OMEGA est différent de OMEGA2
 OMEGA  = %(r1)f
 OMEGA2 = %(r2)f
"""),

    3 : _(u"""
Vous utilisez des éléments de type GRILLE_MEMBRANE. Le mot-clé ANGL_REP de la commande AFFE_CARA_ELEM
permet d'indiquer la direction des armatures.
La projection de ce vecteur directeur dans le plan de certains des éléments de GRILLE_MEMBRANE est nulle.

Conseil :
  Vérifiez les données sous le mot clef ANGL_REP de la commande AFFE_CARA_ELEM.
"""),

    4 : _(u"""
Problème dans le calcul de l'option FORC_NODA / REAC_NODA :

Le nombre de sous-point du champ de contrainte contenu dans la SD n'est
pas cohérent avec ce qui a été défini dans AFFE_CARA_ELEM.

Il est probable que le champ de contrainte a été extrait sur un seul sous-point.

Il est impératif d'utiliser un champ de contrainte complet pour le calcul de FORC_NODA.
"""),

    5 : _(u"""
 problème de maillage TUYAU :
 pour une maille définie par les noeuds N1 N2 N3,
 le noeud N3 doit être le noeud milieu
"""),

    6 : _(u"""
  GENE_TUYAU
  il faut donner un vecteur non colinéaire au tuyau
"""),

    7 : _(u"""
  -> L'angle du coude est trop grand
     ANGLE     = %(r1)f
     ANGLE MAX = %(r2)f
  -> Risque & Conseil : mailler plus fin
"""),

    8 : _(u"""
La raideur tangente de la section est nulle.
Vérifier votre matériau, vous avez peut être défini un matériau élastoplastique parfait.

Risque & Conseil : mettre un léger écrouissage peut permettre de passer cette difficulté.
"""),


    9 : _(u"""
 il faut renseigner le coefficient E_N  dans les cas des déformations planes et de l'asymétrie
 on ne regarde donc que le cas des contraintes planes.
"""),

    10 : _(u"""
 Sous-programme CHPVER :
 le champ  %(k1)s n'a pas le bon type :
   type autorisé  :%(k2)s
   type du champ  :%(k3)s
"""),

    11 : _(u"""
 La modélisation utilisée n'est pas traitée.
"""),

    12 : _(u"""
 Le nombre de couche doit être obligatoirement supérieur à zéro.
"""),



    14 : _(u"""
 Le type d'élément %(k1)s n'est pas prévu.
"""),

    15 : _(u"""
 La nature du matériau %(k1)s n'est pas traitée.
 Seules sont considérées les natures : ELAS, ELAS_ISTR, ELAS_ORTH.
"""),

    18 : _(u"""
 le nombre de noeuds d'un tuyau est différent de 3 ou 4
"""),

    20 : _(u"""
 Aucun type d'éléments ne correspond au type demandé.
"""),

    21 : _(u"""
 prédicteur ELAS hors champs
"""),

    22 : _(u"""
Erreur :
   Le calcul du chargement dû l'hydratation n'est pas programmé pour le type d'élément %(k1)s.

Conseil :
  Pour pouvoir continuer le calcul, B_ENDOGE doit être nul. Le chargement sera nul.
  Il faut émettre une demande d'évolution pour que ce chargement soit pris en compte.
"""),

    23 : _(u"""
Erreur :
   Le calcul du chargement dû au séchage n'est pas programmé pour le type d'élément %(k1)s.

Conseil :
  Pour pouvoir continuer le calcul, K_DESSIC doit être nul. Le chargement sera nul.
  Il faut émettre une demande d'évolution pour que ce chargement soit pris en compte.
"""),

    24 : _(u"""
 dérivées de "MP" non définies
"""),

    25 : _(u"""
 On passe en mécanisme 2.
"""),

    26 : _(u"""
 Chargement en mécanisme 2 trop important.
 À vérifier.
"""),

    27 : _(u"""
 On poursuit en mécanisme 2.
"""),

    28 : _(u"""
 décharge négative sans passer par le mécanisme 1
 diminuer le pas de temps
"""),

    29 : _(u"""
 on revient en mécanisme 1
"""),

    30 : _(u"""
 pas de retour dans le mécanisme 1 trop important
 diminuer le pas de temps
"""),

    31 : _(u"""
 type d'élément  %(k1)s  incompatible avec  %(k2)s
"""),

    32 : _(u"""
 le comportement %(k1)s est inattendu
"""),

    34 : _(u"""
 élément non traité  %(k1)s
"""),





    36 : _(u"""
 nombre de couches négatif ou nul :  %(k1)s
"""),

    37 : _(u"""
 Sous-programme CHPVER :
 le champ  %(k1)s n'a pas la bonne grandeur :
   grandeur autorisée  :%(k2)s
   grandeur du champ   :%(k3)s
"""),


    38 : _(u"""
 Élément de poutre %(k1)s :
 Vous faites des calculs avec l'option GROT_GDEP. Lors de la réactualisation de la géométrie,
 un angle d'orientation de la poutre %(k1)s varie de plus de PI/8.
 Angle concerné %(k2)s avec un saut de %(r1)f degrés.

 * Cela peut arriver lorsque l'axe de la poutre correspond à l'axe global Z. Dans ce cas le
 calcul des angles définissant l'orientation de la poutre peut présenter une indétermination.
 -> Risque & Conseils :
    Des problèmes de convergence peuvent survenir.
    a) Essayez de définir une poutre qui n'est pas exactement verticale en déplaçant légèrement
       un des noeuds.
    b) Essayez de modifiez votre maillage, pour qu'au cours du calcul, l'axe de la poutre ne soit
       jamais l'axe Z global.

 * Cela peut être due à une instabilité de type flambement, déversement, ...
 -> Risque & Conseils :
    Des problèmes de convergence peuvent survenir.
    L'utilisation du pilotage peut permettre de passer cette instabilité.
"""),


    40 : _(u"""
  -> L'axe de référence pour le calcul du repère local est normal à un
     au moins un élément de plaque.
  -> Risque & Conseil :
     Il faut modifier l'axe de référence (axe X par défaut) en utilisant
     ANGL_REP ou VECTEUR.

"""),

    41 : _(u"""
 impossibilité :
 vous avez un matériau de type "ELAS_COQUE" et vous n'avez pas défini la raideur de membrane,
 ni sous la forme "MEMB_L", ni sous la forme "M_LLLL".
"""),

    42 : _(u"""
  Le comportement matériau %(k1)s n'est pas disponible pour ce type de modélisation

   Conseils :
   * S'il s'agit de ELAS_HYPER changez votre modélisation massif 2D,3D.
   * S'il s'agit de ELAS_GLRC utilisez la modélisation DKTG
   * S'il s'agit de ELAS_MEMBRANE utilisez la modélisation MEMBRANE&GRILLE_MEMBRANE
   * Si vous modélisez un comportement anisotrope de plaque/coque, utilisez soit
      ELAS_COQUE ou ELAS_ORTH.
   * Dans le cas ELAS_ORTH n'oubliez pas de définir DEFI_COMPOSITE
      Vous pouvez aussi utiliser ELAS_ORTH isotropie transverse définissant correctement les paramètres matériaux
   * Dans le cas ELAS_COQUE vous n'avez pas besoin de définir DEFI_COMPOSITE. Mais attention :
      ELAS_COQUE donne les propriétés matériaux (membrane, flexion) dans le repère
      intrinsèque de la coque.
"""),

    43 : _(u"""
 impossibilité :
 vous avez un matériau de type "ELAS_COQUE" et le déterminant de la sous matrice de Hooke relative au cisaillement est nul.
"""),

    44 : _(u"""
 Le comportement matériau %(k1)s n'est pas traité.
"""),

    45 : _(u"""
 Le comportement matériau %(k1)s n'est pas traité.

Conseil :
 Pour définir une COQUE_3D orthotrope, il ne faut pas utiliser
 la commande DEFI_COMPOSITE.
 Seule la définition du comportement ELAS_ORTH est nécessaire.
"""),


    46 : _(u"""
 nombre de couches négatif ou nul
"""),

    48 : _(u"""
 impossibilité, la surface de l'élément est nulle.
"""),

    50 : _(u"""
 comportement élastique inexistant
"""),

    52 : _(u"""
  -> Le type de comportement %(k1)s n'est pas prévu pour le calcul de
     SIEF_ELGA avec chargement thermique.
"""),

    53 : _(u"""
Erreur utilisateur :
  Température sur la maille: %(k1)s : il manque la composante "TEMP_MIL"
"""),

    55 : _(u"""
 ELREFA inconnu:  %(k1)s
"""),

    58 : _(u"""
 la nature du matériau  %(k1)s  nécessite la définition du coefficient  B_ENDOGE dans DEFI_MATERIAU.
"""),

    62 : _(u"""
 GROUP_MA :  %(k1)s  inconnu dans le maillage
"""),









    66 : _(u"""
 Si vous avez renseigné le mot-clé NOEUD_ORIG, donnez un groupe de mailles sous GROUP_MA ou une liste de mailles
 sous MAILLE. On ne réordonne pas les groupes de noeuds et les listes de noeuds.
"""),

    67 : _(u"""
 Le groupe de noeuds %(k1)s n'existe pas.
"""),


    68 : _(u"""
 Le noeud origine ou extrémité %(k1)s ne fait pas partie des mailles de la ligne.
"""),

    69 : _(u"""
 Le noeud origine ou extrémité  %(k1)s  n'est pas une extrémité de la ligne.
"""),

    70 : _(u"""
 Alarme DEFI_GROUP / CREA_GROUP_NO / OPTION='NOEUD_ORDO' :
   Le groupe de mailles spécifié forme une ligne fermée (NOEUD_ORIG et NOEUD_EXTR identiques).
   Vous n'avez pas renseigné le mot clé VECT_ORIE. La ligne est donc orientée arbitrairement.
"""),

    71 : _(u"""
 Erreur utilisateur :
   On cherche à orienter une ligne (un ensemble de segments).
   La recherche du noeud "origine" de la ligne échoue.

 Conseil :
   La ligne est peut-être une ligne fermée (sans extrémités).
   Il faut alors utiliser les mots clés GROUP_NO_ORIG et GROUP_NO_EXTR
   (ou NOEUD_ORIG et NOEUD_EXTR).

"""),

    72 : _(u"""
 GROUP_NO orienté : noeud origine =  %(k1)s
"""),

    73 : _(u"""
 Le GROUP_MA :  %(k1)s n'existe pas.
"""),




    77 : _(u"""
 le noeud extrémité  %(k1)s  n'est pas le dernier noeud
"""),





    83 : _(u"""
 Le type des mailles des lèvres doit être quadrangle ou triangle.
"""),


    87 : _(u"""
 Mauvaise définition de MP1 et MP2
"""),

    88 : _(u"""
 Option %(k1)s n'est pas disponible pour l'élément %(k2)s et la loi de comportement %(k3)s
"""),

    90 : _(u"""
Erreur de programmation :
   L'attribut NBSIGM n'est pas défini pour cette modélisation.
Solution :
   Il faut modifier le catalogue phenomene_modelisation__.cata pour ajouter NBSIGM pour cette modélisation.
"""),

    91 : _(u"""
   Le comportement est %(k1)s mais l'option DEFI_COMPOSITE est manquant.

   Conseils :
   1. Il faut utiliser ELAS_COQUE si vous ne voulez pas utiliser DEFI_COMPOSITE. Mais
attention : ELAS_COQUE donne les propriétés matériaux (membrane, flexion) dans le repère
intrinsèque de la coque.
   2. Sinon ELAS_ORTH pour les plaques/coques demandent d'activer DEFI_COMPOSITE
pour préciser les orientations des couches
"""),

    92 : _(u"""
   Le comportement  %(k1)s n'est pas pris charge actuellement par la modélisation.

   Conseils :
      Vous pouvez aussi utiliser ELAS_ORTH ou ELAS_COQUE  isotropie transverse définissant correctement les paramètres matériaux.
      Attention avec ELAS_ORTH vous devez absolument définir DEFI_COMPOSITE pour préciser l'orientation des couches.
      Attention ELAS_COQUE donne les propriétés matériaux (membrane, flexion) dans le repère
      intrinsèque de la coque.
"""),


    93 : _(u"""
  Avertissement :
  Le comportement  %(k1)s que vous utilisez les propriétés matériaux (membrane, flexion) dans le repère
  intrinsèque de la coque. Vous devez donc vous assurez des valeurs entrées sinon utilisez ELAS_ORTH et DEFI_COMPOSITE
  s'il s'agit d'une coque multi couche.
"""),
}
