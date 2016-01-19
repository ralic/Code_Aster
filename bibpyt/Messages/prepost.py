# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

    2 : _(u"""

Attention :
  Vous avez demandé l'impression d'un CHAM_ELEM de VARI_R :
  on complète le nombre de composantes V1, V2, ... avec des zéros
  pour que toutes les mailles aient le même nombre de composantes.

"""),

    3 : _(u"""
 Dans le cas d'un défaut SEMI_ELLIPSE, les mots-clés suivants doivent être présents :

   - EPAIS_MDB : épaisseur du métal de base
   - MATER_MDB : matériau constituant le métal de base
"""),

    4 : _(u"""
 La méthode %(k1)s est illicite
"""),

    5 : _(u"""
 Dans le cas d'un défaut de type ELLIPSE, la longueur du défaut n'est pas en accord :

   - Décalage >= 0 : avec la table coté métal de base,
   - Décalage <  0 : avec les tables définies cote revêtement et coté métal de base.
"""),

    6 : _(u"""
 Attention: le nombre de points de la table TABL_MECA_MDB est de %(i1)d. Il doit
 être égale a 5.
"""),

    7 : _(u"""
  Dans le cas d'un défaut ELLIPSE le mot-clé TABL_MECA_REV est obligatoire.
"""),

    8 : _(u"""
 prolongement à gauche exclu
"""),

    9 : _(u"""
 prolongement à droite exclu
"""),

    10 : _(u"""
 phénomène non valide
"""),

    11 : _(u"""
 la macro-commande POST_ERREUR prend en charge seulement le phénomène MECANIQUE.
"""),

    12 : _(u"""
 une et une seule fonction doit être associée à chaque GROUP_MA, pour la composante %(k1)s.
"""),

    13 : _(u"""
 Les valeurs non existantes du champ %(k1)s lues sur le maillage donné
 sont considérées nulles.
"""),

    14 : _(u"""
 Le NUME_DDL a été déterminé à partir de la matrice de rigidité %(k1)s.
"""),

    21 : _(u"""
 Intersection Droite / Cercle
 pas d'intersection trouvée
"""),

    28 : _(u"""
 volume supérieur à 1.d6
"""),

    31 : _(u"""
 structure de données RESULTAT inconnue  %(k1)s
"""),

    36 : _(u"""
 Le champ de:  %(k1)s  a des éléments ayant des sous-points.
 Ces éléments ne seront pas traités.
"""),

    38 : _(u"""
 le vecteur défini sous le mot clé ACTION/AXE_Z a une norme nulle.
"""),

    39 : _(u"""
 Le résultat généralisé en entrée (%(k1)s) est issu d'un calcul en sous-structuration,
 et non pas d'un calcul standard avec une simple projection sur base modale.

 Les opérations de restitution par POST_GENE_PHYS sont actuellement limitées aux calculs
 du dernier type (projections simples).

 Conseil :
 Utilisez l'un des deux opérateurs REST_GENE_PHYS ou REST_SOUS_STRUCT pour restituer sur
 base physique vos résultats en coordonnées généralisées.
"""),

    40 : _(u"""
 La base modale de projection %(k1)s, référencée dans le résultat généralisé, est
 différente de celle donnée en entrée sous le mot-clé MODE_MECA (%(k2)s).

 Vérifiez que les coordonnées généralisées peuvent être restituées sur cette dernière
 base.
"""),

    41 : _(u"""
 Pour l'observation no. %(i1)d :

 La restitution du champ %(k1)s n'est possible que si il a préalablement été calculé sur
 la base de projection. Or, dans la base %(k3)s, le champ %(k2)s n'a pas été trouvé.

 Conseil :
 Essayez d'enrichir la base %(k3)s avec un appel préalable à CREA_CHAMP, option %(k2)s.
"""),

    42 : _(u"""
 Pour l'observation no. %(i1)d :

 Il n'a pas été possible de trouver le composant %(k1)s dans le champ demandé %(k2)s.

 Conseil :
 Si vous ignorez les noms des composants de votre champ, il est possible d'appeler
 l'opérateur sans spécifier le mot-clé NOM_CMP. Cela permet d'en restituer la totalité.
"""),

    43 : _(u"""
 Pour l'observation no. %(i1)d :

 Vous avez demandé le champ : %(k2)s. Néanmoins, il n'existe aucune information
 dans le résultat généralisé %(k1)s quant à la présence d'un chargement
 dynamique en multi-appuis.

 Si la structure est entraînée par une accélération en mono-appui, il faudra
 renseigner le mot-clé ACCE_MONO_APPUI afin de prendre en compte cette accélération
 dans le champ ACCE_ABSOLU.

 Le résultat obtenu correspond donc au champ %(k3)s, en relatif.
"""),

    44 : _(u"""
 Pour l'observation no. %(i1)d :

 Vous avez demandé le champ : %(k2)s. Néanmoins, il n'existe aucune information
 dans le résultat généralisé %(k1)s quant à la présence d'un chargement
 dynamique en multi-appuis.

 Il n'est actuellement pas possible de récupérer déplacements et vitesses absolues
 en mono-appui. Pour l'accélération absolue, ceci est possible mais il est impératif
 de compléter avec le mot-clé ACCE_MONO_APPUI pour restituer le champ ACCE_ABSOLU.

 Le résultat obtenu correspond donc au champ %(k3)s, en relatif.
"""),

    45 : _(u"""
 Pour l'observation no. %(i1)d :

 Vous avez demandé de restituer l'accélération absolue (ACCE_ABSOLU) en précisant la
 fonction de l'accélération en mono-appui ainsi que sa direction.

 Le résultat généralisé %(k1)s a été calculé en présence des chargements en
 multi-appuis. L'accélération restituée est le cumul de l'accélération relative
 et les différentes accélérations d'entraînement en mono ainsi qu'en multi-appuis.
"""),


    46 : _(u"""
 erreur dans la création du fichier de maillage au format GIBI.
 Celui-ci ne contient pas d'objet de type maillage.

 Risque & Conseil:
 Assurez vous que votre procédure GIBI sauvegarde bien les objets du maillage (pile 32 dans le formalisme GIBI)
"""),

    51 : _(u"""
 l'option de calcul " %(k1)s " n'existe pas dans la structure de données %(k2)s
"""),

    52 : _(u"""
 le champ " %(k1)s " pour l'option de calcul " %(k2)s ", n'a pas été notée
 dans la structure de données %(k3)s
"""),

    53 : _(u"""
 la dimension du problème est invalide : il faut : 1d, 2d ou 3d.
"""),

    54 : _(u"""
 nombre de noeuds supérieur au maximum autorisé : 27.
"""),

    55 : _(u"""
 objet &&GILIRE.INDIRECT inexistant
 problème à la lecture des points
"""),

    56 : _(u"""
 type de maille :  %(k1)s  inconnu de la commande PRE_GIBI.
"""),

    57 : _(u"""
 nombre d'objets supérieur au maximum autorisé : 99999.
"""),

    59 : _(u"""
 le maillage GIBI est peut être erroné :
 il est écrit : "NIVEAU RREUR N_ERR"  avec N_ERR est >0 .
 on continue quand même, mais si vous avez des problèmes plus loin ...
"""),

    60 : _(u"""
 arrêt sur erreur(s)
"""),

    69 : _(u"""
 problème à l'ouverture du fichier
"""),

    70 : _(u"""
 problème à la fermeture du fichier
"""),

    74 : _(u"""
 la variable  %(k1)s  n'existe pas
"""),

    75 : _(u"""
 pas d'impression du champ
"""),

    76 : _(u"""
  -> Il y a des groupes de noeuds dans le maillage %(k1)s.
     Ils  n'apparaîtront pas dans le fichier géométrie ENSIGHT.
     Seuls des groupes de mailles peuvent y être intégrés.
"""),

    77 : _(u"""
  incompatibilité entre les GREL
"""),

    79 : _(u"""
  le nombre de couches est supérieur à 1
"""),

    80 : _(u"""
 on traite les TRIA7 QUAD9 en oubliant le noeud centre
"""),

    83: _(u"""
 Certaines composantes sélectionnées ne font pas partie du LIGREL
"""),

    84 : _(u"""
 Élément PYRAM5 non disponible dans IDEAS
"""),

    85 : _(u"""
 Élément PYRAM13 non disponible dans IDEAS
"""),

    86 : _(u"""
 On traite les PENTA18 en oubliant le(s) noeud(s)
 au centre et les SEG4 en oubliant les 2 noeuds centraux.
"""),

    87 : _(u"""
 On ne sait pas imprimer le champ de type:  %(k1)s
 champ :  %(k2)s
"""),

    90 : _(u"""
 On ne sait pas imprimer le champ  %(k1)s  au format  %(k2)s
"""),

    91 : _(u"""
 On ne sait pas imprimer au format 'MED' les cartes de type %(k1)s
"""),

    92 : _(u"""
 On ne sait pas imprimer au format 'RESULTAT' les champs de type "carte".
"""),

    97 : _(u"""
 On ne sait pas imprimer les champs de type " %(k1)s "
"""),

    98 : _(u"""
 Le champ:  %(k1)s  a des éléments ayant des sous-points.
 Il est écrit avec un format différent.
"""),

    99 : _(u"""
 Le champ:  %(k1)s  a des éléments ayant des sous-points.
 Ces éléments ne seront pas écrits.
"""),

}
