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
# person_in_charge: jacques.pellet at edf.fr

cata_msg = {

    1: _(u"""
 le LIGREL :  %(k1)s  ne contient pas d éléments finis
"""),

    2: _(u"""
 l'option  %(k1)s  n'existe pas.
"""),

    3: _(u"""
 Erreur utilisateur :
   Le maillage associé au champ: %(k1)s  (%(k3)s)
   est différent de celui associe au LIGREL:  %(k2)s  (%(k4)s)
"""),

    4: _(u"""
  erreur programmeur : appel a calcul, le champ: %(k1)s  est un champ "in" et un champ "out".
"""),

    5: _(u"""
 la grandeur associée au champ  %(k1)s : %(k2)s
 n'est pas celle associée au paramètre  %(k3)s : %(k4)s  (option: %(k5)s
"""),

    6: _(u"""
  on n'arrive pas a étendre la carte:  %(k1)s
"""),

    7: _(u"""
 famille de points de Gauss "liste" interdite: %(k1)s
"""),

    8: _(u"""
 Erreur :
   Le code cherche à utiliser dans un calcul élémentaire un CHAM_ELEM "étendu" (VARI_R ou sous-points).
   La programmation de la routine exchml.f ne sait pas encore traiter ce cas.
 Conseil :
   Il y a peut-être lieu d'émettre une demande d'évolution pour traiter ce cas.
"""),

    9: _(u"""
 problème noeud tardif pour un champ à représentation constante
"""),

    10: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le CARA_ELEM.
"""),

    11: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le CHAM_MATER.
"""),

    12: _(u"""
 Le calcul de l'option %(k1)s n'est pas possible. Il manque le MODELE.
"""),

    13: _(u"""
  erreur lors d'une extraction:
  le champ associe au paramètre :  %(k1)s  n'est pas dans la liste des champs paramètres.
"""),

    14: _(u"""
 Erreur développeur :
 L'option que l'on calcule ne connaît pas le paramètre :  %(k1)s
 Erreur probable dans un catalogue(typelem)
"""),
    15: _(u"""
 le paramètre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s
"""),

    16: _(u"""
 le paramètre: %(k1)s  n'est pas un paramètre de l'option: %(k2)s  pour le type_élément:  %(k3)s
"""),

    17: _(u"""
 on ne trouve pas dans les arguments de la routine CALCUL de champ à associer au paramètre: %(k1)s
  - option: %(k2)s
  - type_élément: %(k3)s
"""),

    18: _(u"""
Erreur utilisateur dans un calcul élémentaire de forces réparties :
  On n'a pas trouvé toutes les composantes voulues du champ pour le paramètre : %(k1)s
   - option        : %(k2)s
   - type_élément  : %(k3)s
   - maille        : %(k4)s
  On a trouvé un noeud sur lequel il existe des composantes mais pas toutes.
  On ne peut pas continuer

Risques et conseils :
  Si le champ provient de CREA_CHAMP/AFFE, vérifier que vous avez bien affecté FX,FY [FZ]
"""),

    19: _(u"""
Erreur dans un calcul élémentaire :
  On n'a pas trouvé toutes les composantes voulues du champ pour le paramètre : %(k1)s
   - option        : %(k2)s
   - type_élément  : %(k3)s
   - maille        : %(k4)s

Remarque :
  On a imprimé ci-dessus, une liste de booléens indiquant les composantes trouvées
  sur la maille.
"""),

    20: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Le matériau est nécessaire sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CHAM_MATER dans la commande courante.
  * Dans la commande AFFE_MATERIAU, avez-vous affecté un matériau sur la maille incriminée ?
"""),

    21: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques de "coque" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques de "coque"
    sur la maille incriminée ?
"""),

    22: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques de "poutre" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques de "poutre"
    sur la maille incriminée ?
"""),

    23: _(u"""
Erreur utilisateur dans un calcul élémentaire :
  Des caractéristiques d'"orientation" sont nécessaires sur la maille : %(k4)s
  - option de calcul élémentaire : %(k2)s
  - type_élément                 : %(k3)s

Conseils :
  * Peut-être avez-vous oublié de renseigner le mot clé CARA_ELEM dans la commande courante.
  * Dans la commande AFFE_CARA_ELEM, avez-vous affecté des caractéristiques d'"orientation"
    sur la maille incriminée ?
"""),

    24 : _(u"""
 Erreur d'utilisation :
   On ne trouve pas de variables de commandes ('TEMP', 'HYDR', ...) :
   Option: %(k2)s  type_élément: %(k3)s )

 Risques & conseils :
   La cause la plus fréquente de cette erreur est d'avoir oublié de
   renseigner AFFE_MATERIAU/AFFE_VARC.
   (Ou de n'avoir renseigné que AFFE_VARC/VALE_REF sans avoir renseigné EVOL ou CHAM_GD)
"""),



    26 : _(u"""
 Erreur utilisateur :
    On ne trouve pas la variable de commande :  %(k1)s
    pour la maille                : %(k2)s
    pour l'instant de calcul      : '%(k3)s'

 Conseils :
    Les variables de commande sont des variables connues a priori qui influencent
    le calcul du comportement des matériaux (exemple : la température).

    Lorsque le comportement mécanique dépend d'une variable de commande, il faut que l'utilisateur
    la fournisse au calcul.
    Cela se fait via la commande AFFE_MATERIAU / AFFE_VARC.

    Les variables de commande les plus utilisées sont :
      'TEMP'  : la température
      'HYDR'  : l'hydratation
      'SECH'  : le séchage
      'CORR'  : la corrosion
      'IRRA'  : l'irradiation

    Attention au fait que les variables de commandes doivent pouvoir être calculées pour TOUS
    les instants du calcul. Pour cela, si on utilise une structure de données EVOL_XXX pour
    renseigner une variable de commande (AFFE_MATERIAU/AFFE_VARC/EVOL), il faut faire attention
    à utiliser éventuellement les mots clés PROL_GAUCHE et PROL_DROIT.
"""),

    27 : _(u"""
 On ne trouve pas de routine te0NPQ.
 NPQ doit être compris entre 1 et 600 ici : NPQ = %(k1)s
"""),

    28 : _(u"""
 l'attribut:  %(k1)s  n'existe pas pour le type:  %(k2)s
"""),

    29 : _(u"""
 Erreur de programmation ou d'utilisation :
   On ne trouve pas dans les arguments de la routine calcul de champ a associer
   au paramètre: %(k1)s  (option: %(k2)s  type_élément: %(k3)s )
"""),

    30 : _(u"""
 Erreur de programmation :
 on n'a pas pu extraire toutes les composantes voulues du champ global associe
 au paramètre: %(k1)s  (option: %(k2)s  type_élément: %(k3)s )
"""),

    31: _(u"""
 Erreur de programmation :
 Pour la variable de commande %(k1)s, on cherche à utiliser la famille
 de points de Gauss '%(k2)s'.
 Mais cette famille n'est pas prévue dans la famille "liste" (MATER).

 Contexte de l'erreur :
    option       : %(k3)s
    type_élément : %(k4)s

 Conseil :
 Émettez une fiche d'anomalie
"""),

    32: _(u"""
 Erreur utilisateur :
   Un calcul élémentaire nécessite une ou plusieurs variables de commande (CVRC).
   Sur la maille : %(k1)s, on ne trouve pas le bon nombre de "CVRC" :
   On attend : %(i2)d "CVRC",  mais on n'en trouve que : %(i1)d

 Conseil :
   Vérifier les occurrences de AFFE_MATERIAU/AFFE_VARC pour la maille concernée.
"""),

    33 : _(u"""
 le mode_local:  %(k1)s  ne doit pas être vecteur ou matrice.
"""),

    34 : _(u"""
 le mode_local:  %(k1)s  ne doit pas être "DIFF__".
"""),

    35 : _(u"""
Erreur utilisateur concernant le parallélisme des calculs élémentaires :
  La partition des éléments du modèle a été faite sur %(i1)d processeurs.
  Mais maintenant, le nombre de processeurs disponibles est de %(i2)d.

Conseil :
  Lors de la poursuite du calcul, il faut utiliser la commande MODI_MODELE
  pour modifier la partition du modèle afin qu'elle soit cohérente avec le nombre
  de processeurs disponibles.
"""),

    36 : _(u"""
  incompatibilité des type_champ ("ELGA"/"ELNO")  pour l option :  %(k1)s  entre les 2 TYPE_ELEM :  %(k2)s  et  %(k3)s
"""),

    37 : _(u"""
Erreur utilisateur :
  -> Le TYPE_ELEMENT %(k1)s  ne sait pas encore calculer l'option:  %(k2)s.

  -> Risques & Conseils :
   * Si vous utilisez une commande de "calcul" (THER_LINEAIRE, STAT_NON_LINE, ...), il n'y a pas
     moyen de contourner ce problème. Il faut changer de modélisation ou émettre une demande d'évolution.

   * Si c'est un calcul de post-traitement, vous pouvez sans doute "éviter" le problème
     en ne faisant le post-traitement que sur les mailles qui savent le faire.
     Pour cela, il faut sans doute utiliser un mot clé de type "GROUP_MA".
     S'il n'y en a pas, il faut faire une demande d'évolution.
"""),

    38 : _(u"""
 le calcul de l'option :  %(k1)s
 n'est possible pour aucun des types d'éléments du LIGREL.
"""),

    39 : _(u"""
 incohérence des maillages : %(k1)s  et  %(k2)s
"""),

    40 : _(u"""
 Erreur Programmeur:
 Incohérence fortran/catalogue
 TYPE_ELEMENT :  %(k1)s
 OPTION       :  %(k2)s
 La routine texxxx.f correspondant au calcul élémentaire ci-dessus est erronée
 Elle écrit en dehors de la zone allouée au paramètre (OUT) %(k3)s.

"""),

    41 : _(u"""
Alarme utilisateur :
  Le TYPE_ELEMENT %(k1)s devrait calculer l'option:  %(k2)s,
  mais il ne le fait pas encore (la programmation est manquante).

  Pour l'instant, l'élément est ignoré pour le calcul de cette option,
  ce qui peut entraîner des résultats faux.

-> Risques & Conseils :
   * Cette situation est très dangereuse. Il y a un risque de résultats faux.
   * Si ce manque dans la programmation vous semble important, il faut émettre
     une demande d'évolution.
"""),

    42 : _(u"""
Erreur :
  Le TYPE_ELEMENT %(k1)s devrait calculer l'option :  %(k2)s,
  mais il ne le fait pas encore (la programmation est manquante).
  Le calcul n'est possible que dans les cas "triviaux" : chargement nul, ...

-> Risques & Conseils :
   * Si ce manque dans la programmation vous semble important, il faut émettre
     une demande d'évolution.
"""),

    43 : _(u"""
Erreur de programmation :
    On ne trouve pas le triplet ( %(k1)s )
    correspondant à (terme élémentaire, élément de référence, famille).
Conseils :
    Vérifiez le catalogue d'éléments.
    L'élément de référence ou la famille de points de Gauss ne sont pas définis.
"""),

    44: _(u"""
 Erreur lors du calcul de la pression: la maille n'est que partiellement affectée.
"""),

    45 : _(u"""
 Matériau : %(k1)s non affecté par la commande AFFE_MATERIAU.
"""),

    46 : _(u"""
 Il y a une incohérence entre les paramètres matériaux et la loi de comportement.
 On ne trouve pas les paramètres matériaux associés à la relation %(k1)s
"""),

    47 : _(u"""
 pour la maille %(k1)s.

 Conseils :
    Vérifiez la mise en données de la commande DEFI_MATERIAU : il faut définir
    les paramètres matériau pour la relation utilisée.
    Vérifiez que la maille %(k1)s est bien affectée par le matériau.
"""),


}
