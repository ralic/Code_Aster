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

    1 : _(u"""
Pour que CALC_EUROPLEXUS fonctionne il faut ajouter DEBUG=_F(HIST_ETAPE='OUI')
dans la commande DEBUT.
Remarque : CALC_EUROPLEXUS ne fonctionne pas en POURSUITE"""),

    2:  _(u"""Le mot-clé %(k1)s n'existe pas"""),

    3 : _(u"""
La modélisation %(k1)s est affecté à une partie du modèle fourni à CALC_EUROPLEXUS.
Cependant CALC_EUROPLEXUS ne peut fonctionner que si on n'utilise pas le mot-clé GROUP_MA
dans AFFE_MODELE pour affecter cette modélisation aux éléments concernés.
"""),

    4 : _(u"""
La valeur %(k3)s n'est pas permise pour le mot-clé %(k2)s de AFFE_CARA_ELEM/%(k1)s.
Voici la liste des valeurs permises :
 %(k4)s
"""),

    5 : _(u"""
Le chargement de type %(k1)s ne doit pas être associé à une fonction multiplicatrice,
car il correspond à un chargement EPX de type constant.
"""),

    6 : _(u"""La modélisation %(k1)s n'est pas disponible dans CALC_EUROPLEXUS"""),

    7 : _(u"""Le mot-clé FONC_MULT est obligatoire pour le chargement de type %(k1)s"""),

    8 : _(u"""La caractéristique %(k1)s n'est pas permise pour le mot-clé %(k2)s de AFFE_CARA_ELEM."""),

    9 : _(u"""
Vous tentez d'affecter via AFFE_CARA_ELEM deux modélisations différents sur le GROUP_MA %(k3)s :
%(k1)s et %(k2)s.

Il y a de grandes chances que vous ayez affecté une rigidité ou un amortissement sur un élément discret sur
lequel une masse est déjà affectée."""),

    10: _(u"""
Le mot-clé ANGL_NAUT n'est pas autorisé pour définir les orientations des poutres.
"""),

    11: _(u"""
Les vecteurs y_local des mailles du GROUP_MA %(k1)s
calculés à partir des angles nautiques ne sont pas identiques.
Veuillez imposer directement VECT_Y dans AFFE_CARA_ELEM si vous
êtes sur de l'orientation.
"""),

    12: _(u"""
On ne trouve pas de valeur à associer à la caractéristique %(k1)s pour le mot-clé %(k2)s de AFFE_CARA_ELEM
sur le groupe de maille %(k3)s.
Conseil : s'il s'agit d'éléments discrets de type raideur ou amortissement, vérifiez que vous
avez bien renseigné une occurrence de FONC_PARASOL avec ce groupe dans GROUP_MA.
S'il s'agit de poutres, vérifiez que vous avez bien déclaré une orientation au groupe de mailles
et explicitement à celui-ci.
Une déclaration d'une de ces informations sur un groupe de mailles contenant les mailles du groupe %(k3)s
ne permet pas à CALC_EUROPLEXUS de retrouver les informations.
"""),

    13 : _(u"""
    Lancement d'Europlexus en utilisant %(k1)s
"""),


    14 : _(u"""
Le fichier MED contenant les résultats d'Europlexus est introuvable ou vide.
L'exécution d'Europlexus s'est probablement mal déroulée.
"""),

    15 : _(u"""Les différentes occurrences de RIGI_PARASOL dans AFFE_CARA_ELEM n'ont pas la même
liste de caractéristiques (mot-clé CARA).
CALC_EUROPLEXUS ne sait pas traiter de tels cas.
"""),

    16 : _(u""" Dans l'une des occurrences du mot-clé facteur %(k1)s du concept issue de AFFE_CARA_ELEM,
les informations données ne peuvent être transmises à Europlexus car il n'est pas aussi riche que Code_Aster
pour la présente fonctionnalité.
Pour le mot-clé %(k2)s, les valeurs en positions %(i1)d et %(i2)d de VALE doivent être égales pour que la
traduction vers Europlexus soit possible.
"""),

    17 : _(u"""
Vous avez demander de fournir à EPX un état initial de contraintes, cependant cette fonctionnalité
n'est pas développée pour certains éléments du modèle. Le champ de contraintes du résultat donné
en état initial ne sera pas transmis à EPX pour les éléments affectés des modélisations suivantes :

%(k1)s.
"""),

    18 : _(u"""Le mot clé %(k1)s du concept CARA_ELEM n'est pas pris en compte par CALC_EUROPLEXUS'
"""),
    19 : _(u"""Le type de charge %(k1)s n'est pas pris en compte par CALC_EUROPLEXUS'
"""),

    20 : _(u"""Le concept %(k1)s donné par %(k2)s n'a pas été trouvé.'
"""),

    21 : _(u"""
Dans l'occurrence %(i1)d du mot-clé COURBE, NOM_COURBE dépasse.%(i2)d caractères.
Le nom de la colonne correspondante sera tronqué dans la table. 
"""),

    22 : _(u"""La valeur du mot-clé %(k1)s du matériau %(k2)s est différente de %(k3)s.
"""),

    23 : _(u"""
Il existe une ou plusieurs mailles de type %(k1)s dans le groupe %(k2)s auquel
la modélisation %(k3)s est affectée.
Or ce type de maille n'est pas accepté pour cette modélisation dans CALC_EUROPLEXUS.
"""),

    24 : _(u"""CALC_EUROPLEXUS ne traite aucune modélisation du phénomène %(k1)s.
"""),
    25 : _(u"""
Aucun type de mailles contenu dans le groupe %(k1)s n'est disponible dans CALC_EUROPLEXUS
pour la modélisation %(k2)s.
"""),
    26 : _(u"""
Le groupe %(k1)s est déclaré dans le modèle, mais on ne trouve pas dans le concept
CARA_ELEM les informations complémentaires le concernant, indispensables à sa prise
en compte dans le calcul.
"""),

    27 : _(u"""
Le mot-clé %(k1)s n'est pas disponible dans CALC_EUROPLEXUS pour les chargements de type %(k2)s.
"""),

    28 : _(u"""
La valeur du mot-clé %(k1)s du type de chargement %(k2)s n'est pas égale à la valeur imposée :
Valeur trouvée : %(r1)f
Valeur imposée : %(r2)f
"""),

    29 : _(u"""
CALC_EUROPLEXUS : CHARGEMENTS/LIAISONS

CALC_EUROPLEXUS ne sait pas traiter une occurrence du mot-clé %(k1)s de AFFE_CHAR_MECA.
Au moins %(i1)d mots-clé sont présents dans la même occurrence alors qu'il en faut au plus %(i2)d
parmi la liste suivante :
%(k2)s

Solution de contournement : séparez les différents mots-clé dans plusieurs occurrences de %(k1)s.

Remarque : Si vous souhaitez faire un blocage avec DDL_IMPO et que ce message vous arrête, c'est que
vous avez associé une fonction multiplicatrice au chargement en question. Le problème doit disparaître
si vous enlevez cette fonction (FONC_MULT de EXCIT).

"""),

    30 : _(u"""
La valeur du mot-clé %(k1)s du type de chargement %(k2)s n'est autorisée :
Valeur trouvée : %(k3)s
Valeur(s) autorisée(s) : %(k4)s
"""),

    31 : _(u"""
On ne trouve pas le paramètre %(k1)s pour la loi %(k2)s dans le matériau %(k3)s. Ce paramètre est obligatoire.
"""),

    32 : _(u"""
Aucun matériau n'est affecté au groupe %(k1)s.
"""),

    33 : _(u"""
On ne trouve pas le mot-clé %(k1)s dans le matériau %(k2)s. Il est indispensable au comportement %(k3)s.
"""),

    34 : _(u"""
Le groupe %(k1)s est donné dans AFFE_CARA_ELEM ou dans COMPORTEMENT, mais n'est pas présent dans le MODELE.
"""),

    35 : _(u"""
CALC_EUROPLEXUS : Traitement de RIGI_PARASOL.
Le groupe de maille %(k1)s existe déjà dans le maillage, vérifiez qu'il contient bien la maille %(k2)s
et elle seule. Si ce n'est pas le cas, le calcul risque d'échouer ou de donner les résultats faux.

Remarque : Si vous avez lancez deux CALC_EUROPLEXUS avec le même maillage et le même CARA_ELEM, il est
normal d'obtenir cette alarme car les groupes de mailles sont créés lors du premier CALC_EUROPLEXUS.
"""),

    36 : _(u"""
La loi de comportement GLRC_DAMAGE est affectée au groupe %(k1)s.
Cette loi nécessite de définir une orientation sur les éléments de ce groupe de maille,
ce qui n'est pas le cas actuellement.
Pour donner cette information il faut renseigner le mot-clé VECTEUR ou le mot-clé ANGL_REP
dans AFFE_CARA_ELEM pour le mot-clé facteur COQUE.

Remarque : Il est possible que vous n'ayez pas utilisé le même groupe de maille dans AFFE_MATERIAU et
dans AFFE_CARA_ELEM. Ceci est obligatoire pour que CALC_EUROPLEXUS fonctionne.
"""),

    37 : _(u"""
Le résultat %(k1)s donné en état initial comporte plusieurs %(k2)s.
CALC_EUROPLEXUS ne sait pas traiter de tels cas.
"""),

    38 : _(u"""
CALC_EUROPLEXUS/COURBE :
Le champ %(k1)s ne possède pas de composante %(k2)s.
"""),

    39 : _(u"""
CALC_EUROPLEXUS/COURBE :
La quantité de données présente dans le fichier de sortie de suivi de point ne concorde
pas avec les instructions du fichier de commande.
Une erreur s'est certainement produite lors du traitement de ce mot-clé par EPX.

Conseil : regarder le message de sortie d'EPX dans le fichier .mess pour comprendre
ou le problème se situe.
"""),

    40 : _(u"""
CALC_EUROPLEXUS : Traitement des matériaux associés au comportement VMIS_ISOT_TRAC

Les premières valeurs issues de la fonction %(k1)s, argument du paramètre %(k2)s
de mot-clé TRACTION de DEFI_MATERIAU sont incompatibles avec le module d'Young donné dans ELAS.
Contrainte divisée par le module = %(r1)f
Déformation                      = %(r2)f
"""),

    41 : _(u"""
CALC_EUROPLEXUS : FONC_PARASOL

Le groupe de maille %(k1)s est présent dans plusieurs occurrence de FONC_PARASOL.
Ceci est interdit. Toutes les informations relatives à ce groupe doivent être
données dans la même occurrence.
"""),

    42 : _(u"""
CALC_EUROPLEXUS : RIGI_PARASOL

Il ne peut pas encore y avoir plusieurs occurrences de RIGI_PARASOL dans
le CARA_ELEM donné à CALC_EUROPLEXUS.
"""),


    43 : _(u"""
CALC_EUROPLEXUS : RIGI_PARASOL

Il ne peut pas encore y avoir plusieurs groupes de mailles pour le mot-clé
GROUP_MA_POI1 de RIGI_PARASOL.
"""),

    44 : _(u"""
CALC_EUROPLEXUS/LIRE_EUROPLEXUS ne sait pas encore traiter le mot-clé %(k1)s
d'AFFE_CARA_ELEM.
"""),

    45 : _(u"""
Le mot-clé %(k1)s n'est pas autorisé dans CALC_EUROPLEXUS pour le mot-clé
facteur %(k2)s d'AFFE_CARA_ELEM.
"""),

    46 : _(u"""
Le paramètre %(k1)s est présent dans le mot-clé %(k2)s du matériau %(k3)s.
Il n'existe pas de traduction dans le matériau EPX correspondant.
"""),

    47 : _(u"""
Vous avez demandé à ce que le champ de variables internes soit pris en
compte dans l'état initial du calcul EPX. La transformation de ce champ
de Code_Aster vers EPX n'est pas programmée pour le comportement  %(k1)s.
Les valeurs de ce champ dans l'état initial sont donc nulles pour les mailles
sur lesquelles ce comportement est affecté.
"""),

    48 : _(u"""
Vous avez demandé à ce que le champ de variables internes soit pris en
compte dans l'état initial du calcul EPX. Cependant la transformation
du champ de variables internes de Code_Aster vers EPX n'est programmée
pour aucun des comportements présents dans le calcul.

La commande VARI_INT = 'OUI' n'a ici aucun effet et est donc inutile.
"""),

    49 : _(u"""
Le paramètre %(k1)s est présent dans le mot-clé %(k2)s du matériau %(k3)s.
Sa valeur est différente de la valeur imposée :
Valeur imposée  : %(r1)f
Valeur présente : %(r2)f
"""),

    50 : _(u"""
Le fichier correspondant à l'unité %(i1)d donnée dans UNITE_MED de LIRE_EUROPLEXUS
n'est pas un fichier MED ou est absent.
"""),

    51 : _(u"""
Le fichier correspondant à l'unité donnée dans UNITE_MED de LIRE_EUROPLEXUS
ne contient pas de champ %(k1)s. Il ne peut pas être traité.
"""),

    52 : _(u"""
CALC_EUROPLEXUS ne sait pas traduire une instance du mot-clé facteur %(k1)s d'AFFE_CARA_ELEM.
"""),

    53 : _(u"""
Le mot-clé CARA_ELEM est renseigné alors qu'un état initial est donné via ETAT_INIT.
Il n'est pas nécessaire de renseigner CARA_ELEM dans ce cas sauf si vous souhaitez
fournir des informations différentes de celles contenues dans le résultat de l'état initial.
"""),

    54 : _(u"""
Un champ de type %(k1)s contenu dans le fichier MED est porté par des éléments EUROPLEXUS de type %(k2)s.
LIRE_EUROPLEXUS ne sait pas traduire les données de ce champ. Les éléments du modèle
Code_Aster associés aux mêmes éléments du maillage, s'ils existent,
gardent des valeurs nulles sur le champ correspondant.
"""),

    55 : _(u"""
Un champ de variables internes contenu dans le fichier MED est associé à la loi
de comportement %(k1)s d'EUROPLEXUS.
LIRE_EUROPLEXUS ne sait pas traduire les données de ce champ. Les éléments du modèle
Code_Aster associés aux mêmes éléments du maillage, s'ils existent,
gardent des valeurs nulles sur le champ correspondant.
"""),

    56 : _(u"""
Dans une occurrence du mot-clé facteur %(k1)s de AFFE_CARA_ELEM, on de trouve pas
le mot-clé %(k2)s qui est obligatoire.
"""),

    57 : _(u"""
Échec de la transformation des variables internes d'EPX vers Code_Aster pour la loi %(k1)s.
Des mailles de bord sont certainement présentes parmi les mailles renseignées dans le 
mot-clé COMPORTEMENT pour cette loi.
"""),

}
