# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mickael.abbas at edf.fr

cata_msg = {

    1 : _(u"""Création de la matrice des clichés."""),

    2 : _(u"""Sauvegarde des %(i1)d modes empiriques dans la base empirique %(k1)s."""),

    4 : _(u"""Comptage du nombre de modes empiriques à sélectionner."""), 

    5 : _(u"""On a %(i1)d valeurs singulières comprises entre %(r1)13.6G et %(r2)13.6G . Avec les paramètres, on a retenu %(i2)d modes empiriques."""),   

    6 : _(u"""Le critère de sélection des valeurs singulières ne permet pas d'extraire au moins un mode empirique. Il faut changer la tolérance ou le nombre de modes."""),

    7 : _(u"""Calcul des modes empiriques par décomposition aux valeurs singulières."""),

    8 : _(u"""Échec lors du calcul des modes empiriques."""),

    9 : _(u"""Initialisation de toutes les structures de données."""),

   10 : _(u"""Lecture de tous les paramètres de la commande."""),

   11 : _(u"""On ne trouve pas de champ de type %(k1)s dans la structure de données résultat."""),

   12 : _(u"""Préparation de la numérotation des modes linéiques."""),

   13 : _(u"""On a détecté %(i1)d tranches pour la définition des modes linéiques."""),

   14 : _(u"""Initialisation des structures de données pour les paramètres du calcul."""),

   16 : _(u"""Méthode de construction de la base empirique: %(k1)s """),

   17 : _(u"""Nombre de modes maximum de la base empirique: %(i1)d """),

   18 : _(u"""Lecture des paramètres pour la méthode POD ou POD_INCR."""),

   19 : _(u"""Vérifications de la conformité de la structure de données résultat utilisée de nom %(k1)s."""),

   20 : _(u"""On ne peut utiliser des bases empiriques qu'avec des maillages tridimensionnels."""),
 
   21 : _(u"""Un mode empirique de type %(k1)s ne doit contenir que %(i1)d composantes."""),
 
   22 : _(u"""Un mode empirique contient des conditions limites dualisés (AFFE_CHAR_THER ou AFFE_CHAR_MECA). 
              Ce n'est pas possible, utilisez AFFE_CHAR_CINE"""),

   23 : _(u"""Un mode empirique contient une composante au noeud %(k1)s qui n'est pas utilisable."""),

   24 : _(u"""Paramètres généraux de DEFI_BASE_REDUITE."""),

   25 : _(u"""Initialisation des structures de données pour les paramètres POD du calcul."""),

   26 : _(u"""Initialisation des structures de données pour les paramètres GLOUTON du calcul."""),

   27 : _(u"""Lecture des paramètres pour la méthode GLOUTON."""),

   30 : _(u"""Vérification des paramètres pour le calcul non-linéaire avec réduction de modèle."""),

   31 : _(u"""La base empirique ne repose pas sur le même maillage sur le calcul non-linéaire."""),

   32 : _(u"""La base empirique n'est pas construite sur le bon type de champ."""),

   33 : _(u"""Le groupe de noeuds %(k1)s ne fait pas parti du maillage."""),

   34 : _(u"""La recherche linéaire est interdite avec la réduction de modèle."""),

   35 : _(u"""La base empirique est vide."""),

   36 : _(u"""Création de la structure de données pour réaliser le calcul non-linéaire avec réduction de modèle."""),

   37 : _(u"""Initialisation pour réaliser le calcul non-linéaire avec réduction de modèle."""),

   38 : _(u"""Création de la table pour sauver les coordonnées réduites."""),

   39 : _(u"""Sauvegarde des coordonnées réduites pour le calcul non-linéaire avec réduction de modèle."""),

   40 : _(u"""Résolution du problème réduit."""),

   41 : _(u"""Lecture des paramètres pour réaliser le calcul non-linéaire avec réduction de modèle."""),

   42 : _(u"""Suppression de la structure de données pour réaliser le calcul non-linéaire avec réduction de modèle."""),

   43 : _(u"""Initialisation de la structure de données pour le calcul multi-paramétrique avec réduction de modèle."""),

   44 : _(u"""Évaluation des coefficients pour la valeur %(i1)d des paramètres."""),

   45 : _(u"""Valeur du coefficient réel pour le vecteur de nom %(k1)s : %(r1)19.12e."""),

   46 : _(u"""Valeur du coefficient complexe pour le vecteur de nom %(k1)s : (%(r1)19.12e,%(r2)19.12e)."""),

   47 : _(u"""Valeur du coefficient réel pour la matrice de nom %(k1)s : %(r1)19.12e."""),

   48 : _(u"""Valeur du coefficient complexe pour la matrice de nom %(k1)s : (%(r1)19.12e,%(r2)19.12e)."""),

   50 : _(u"""Initialisation de la structure de données pour résoudre le système %(k1)s."""),

   51 : _(u"""Valeur des paramètres pour la variation d'indice %(i1)d."""),

   52 : _(u"""Le paramètre %(k1)s vaut %(r1)19.12e."""),

   60 : _(u"""Calcul par la méthode GLOUTON."""),

   61 : _(u"""Calcul du mode empirique %(i1)d."""),

   62 : _(u"""Calcul du second membre du système complet."""),

   63 : _(u"""Calcul de la matrice du système complet."""),

   64 : _(u"""Post-traitement et sauvegardes."""),

   65 : _(u"""Résolution du système complet."""),

   66 : _(u"""Calcul du second membre du système réduit."""),

   67 : _(u"""Calcul de la matrice du système réduit."""),

   81 : _(u"""Initialisation de la structure de données pour la variation des coefficients pour le calcul multi-paramétrique."""),

   82 : _(u"""Initialisation de la structure de données pour l'évaluation des coefficients pour le calcul multi-paramétrique."""),

   90 : _(u"""Initialisation de la structure de données des coefficients pour le calcul multi-paramétrique (second membre)."""),

   91 : _(u"""Initialisation de la structure de données des coefficients pour le calcul multi-paramétrique (matrice)."""),

   92 : _(u"""Calcul du mode empirique initial."""),

   93 : _(u"""Évaluation des coefficients pour la valeur initiale des paramètres."""),

   94 : _(u"""Copie de la valeur des paramètres initiaux."""),

}
