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




    3 : _(u"""
 La conversion d'un champ par éléments aux noeuds en un champ
 aux noeuds a été demandé.
 Or le champ par éléments aux noeuds contient des
 sous-points alors qu'un champ aux noeuds ne peut pas en contenir.
 Les mailles contenant des sous-points vont être exclues.

 Conseil :
   Si vous souhaitez convertir le champ sur les mailles ayant des
   sous-points, vous devez d'abord extraire le champ par éléments
   aux noeuds sur un sous-point avec la commande POST_CHAMP.
"""),

    4 : _(u"""
 Vous avez demandé le calcul d'un champ aux noeuds sur des éléments
 de structure. Mais les repères locaux de certaines mailles entourant
 des noeuds sur lesquels vous avez demandés le calcul ne sont pas
 compatibles (Au maximum, on a %(r1)g degrés d'écart entre les angles
 nautiques définissant ces repères).

 Risque & Conseil :
   Il se peut que vous obteniez des résultats incohérents.
   Il est donc recommandé de passer en repère global les champs
   utiles au calcul du champ aux noeuds.
"""),

    5 : _(u"""
 vecteur axe de norme nulle
"""),

    6 : _(u"""
 axe non colinéaire à v1v2
"""),

    7 : _(u"""
 Problème norme de axe
"""),

    8 : _(u"""
Cette grandeur ne peut pas accepter plus de %(i2)d composantes (%(i1)d fournies).
"""),

    9 : _(u"""
 dimension  %(k1)s  inconnue.
"""),

    10 : _(u"""
 maillage obligatoire.
"""),

    11 : _(u"""
 on ne peut pas créer un champ de VARI_R avec le mot clé facteur AFFE
 (voir u2.01.09)
"""),

    12 : _(u"""
 mot clé AFFE/NOEUD interdit ici.
"""),

    13 : _(u"""
 mot clé AFFE/GROUP_NO interdit ici.
"""),

    14 : _(u"""
 type scalaire non traité :  %(k1)s
"""),

    15 : _(u"""
 incohérence entre nombre de composantes et nombre de valeurs
"""),

    16 : _(u"""
 il faut donner un champ de fonctions
"""),

    17 : _(u"""
 les paramètres doivent être réels
"""),

    18 : _(u"""
 maillages différents
"""),

    20 : _(u"""
Erreur utilisateur dans la commande CREA_CHAMP.
 le champ  %(k1)s n'est pas de type réel
"""),

    21 : _(u"""
 on ne traite que des "CHAM_NO" ou des "CHAM_ELEM".
"""),

    22: _(u"""
 la programmation prévoit que les entiers sont codés sur plus de 32 bits
 ce qui n'est pas le cas sur votre machine
"""),

    23 : _(u"""
 on ne trouve aucun champ.
"""),

    24 : _(u"""
 Lors de la recopie du champ "%(k1)s" vers le champ "%(k2)s" utilisant
 le NUME_DDL "%(k3)s" certaines composantes de "%(k2)s" ont du être
 mises à zéro.

 En effet, certaines parties attendues dans le champ "%(k2)s" n'étaient
 pas présentes dans "%(k1)s", elles ont donc été mises à zéros.

 Ce problème peut survenir lorsque la numérotation du champ "%(k1)s"
 n'est pas intégralement incluse dans celle de "%(k2)s".
"""),







    26 : _(u"""
Erreur utilisateur dans la commande CREA_RESU / TYPE_MAXI= ...
  La commande exige que tous les CHAM_NO pour lesquels on veut extraire
  le maximum aient la même numérotation pour leurs composantes.
  Ce n'est pas le cas pour cette structure de donnée SD_RESULTAT.
"""),

    27 : _(u"""
 il faut donner un maillage.
"""),

    28 : _(u"""
 Le champ : %(k1)s ne peut pas être assemblé en :  %(k2)s
"""),

    29 : _(u"""
Erreur utilisateur :
 La structure de donnée résultat %(k1)s est associée au maillage %(k2)s
 Mais la structure de donnée nume_ddl %(k3)s est associée au maillage %(k4)s
 Il n'y a pas de cohérence.
"""),





    31 : _(u"""
 NOM_CMP2 et NOM_CMP de longueur différentes.
"""),

    32: _(u"""
 Grandeur incorrecte pour le champ : %(k1)s
 grandeur proposée :  %(k2)s
 grandeur attendue :  %(k3)s
"""),

    33 : _(u"""
 le mot-clé 'COEF_C' n'est applicable que pour un champ de type complexe
"""),

    34 : _(u"""
 développement non réalisé pour les champs aux éléments. vraiment désolé !
"""),

    35 : _(u"""
Erreur utilisateur dans la commande CREA_CHAMP.
 le champ  %(k1)s n'est pas de type complexe
"""),


    37 : _(u"""
Erreur utilisateur dans la commande CREA_CHAMP.
Les options 'R2C' et 'C2R' ne sont autorisées que pour les CHAM_NO.
Le champ %(k1)s n'est pas un champ aux noeuds.
"""),

    40 : _(u"""
 structure de données inexistante : %(k1)s
"""),

    41 : _(u"""
 duplication "maillage" du .LTNT, objet inconnu:  %(k1)s
"""),

    42 : _(u"""
 type de sd. inconnu :  %(k1)s
"""),

    43 : _(u"""
 numérotation absente  problème dans la matrice  %(k1)s
"""),






    46 : _(u"""
 la fonction doit s appuyer sur un maillage pour lequel une abscisse curviligne a été définie.
"""),

    47 : _(u"""
  le mot clé : %(k1)s n est pas autorise.
"""),

    49 : _(u"""
  DISMOI :
  la question : " %(k1)s " est inconnue
"""),





    51 : _(u"""
 il n y a pas de NUME_DDL pour ce CHAM_NO
"""),

    52 : _(u"""
 type de charge inconnu
"""),


    54 : _(u"""
 trop d objets
"""),

    55 : _(u"""
 champ inexistant: %(k1)s
"""),


    56 : _(u"""
 Le partitionneur SCOTCH a fait remonter l'erreur %(i1)d. Veuillez contacter l'équipe de
 développement.
 Pour contourner ce problème, vous pouvez néanmoins:
   - changer de partitionneur (METHODE=KMETIS ou PMETIS),
"""),


    57 : _(u"""
  DISMOI :
  la question n'a pas de réponse sur une grandeur de type matrice GD_1 x GD_2
"""),

    59 : _(u"""
  DISMOI :
  la question n'a pas de sens sur une grandeur de type matrice GD_1 x GD_2
"""),

    60 : _(u"""
  DISMOI :
  la question n'a pas de sens sur une grandeur de type composée
"""),

    63 : _(u"""
 phénomène inconnu :  %(k1)s
"""),

    65 : _(u"""
 le type de concept : " %(k1)s " est inconnu
"""),

    66 : _(u"""
 le phénomène :  %(k1)s  est inconnu.
"""),

    68 : _(u"""
 type de résultat inconnu :  %(k1)s  pour l'objet :  %(k2)s
"""),

    69 : _(u"""
 le résultat composé ne contient aucun champ
"""),

    70 : _(u"""
 TYPE_MAILLE inconnu.
"""),

    71 : _(u"""
 mauvaise récupération de NEMA
"""),

    72 : _(u"""
 on ne traite pas les noeuds tardifs
"""),

    73 : _(u"""
 grandeur inexistante
"""),

    74 : _(u"""
 composante de grandeur inexistante
"""),

    75 : _(u"""
 problème avec la réponse  %(k1)s
"""),

    76 : _(u"""
 les conditions aux limites autres que des ddls bloques ne sont pas admises
"""),

    77 : _(u"""
 unité logique  %(k1)s , problème lors du close
"""),













    81 : _(u"""
  erreur à l'appel de METIS
  plus aucune unité logique libre
"""),

    82 : _(u"""
 méthode d'intégration inexistante.
"""),




    84 : _(u"""
 interpolation  %(k1)s  non implantée
"""),

    85 : _(u"""
 recherche " %(k1)s " inconnue
"""),

    86 : _(u"""
 l'intitule " %(k1)s " n'est pas correct.
"""),

    87 : _(u"""
 le noeud " %(k1)s " n'est pas un noeud de choc.
"""),

    88 : _(u"""
 nom de sous-structure et d'intitulé incompatible
"""),

    89 : _(u"""
 le noeud " %(k1)s " n'est pas un noeud de choc de l'intitule.
"""),

    90 : _(u"""
 le noeud " %(k1)s " n'est pas compatible avec le nom de la sous-structure.
"""),

    91 : _(u"""
 le paramètre " %(k1)s " n'est pas un paramètre de choc.
"""),

    92 : _(u"""
 le noeud " %(k1)s " n'existe pas.
"""),

    93 : _(u"""
 la composante " %(k1)s " du noeud " %(k2)s " n'existe pas.
"""),

    94 : _(u"""
 type de champ inconnu  %(k1)s
"""),

    95 : _(u"""
 "INTERP_NUME" et ("INST" ou "LIST_INST") non compatibles
"""),

    96 : _(u"""
 "INTERP_NUME" et ("FREQ" ou "LIST_FREQ") non compatibles
"""),

    99 : _(u"""
 objet %(k1)s inexistant
"""),

}
