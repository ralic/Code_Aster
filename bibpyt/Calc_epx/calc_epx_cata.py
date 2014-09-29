# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: serguei.potapov at edf.fr

"""
Catalogues de la macro CALC_EUROPLEXUS
"""

"""
    DIRECTIVES EUROPLEXUS

    TYPE_DIR = 0 :
    Le nom de la directive n'est pas écrit dans le fichier de commande
    TYPE_DIR = 1 :
    Nom de la directive écrit + mot-clé TERM pour terminer la directive
    TYPE_DIR = 2 : Nom de la directive écrit (type le plus fréquent)
"""
cata_directives = {
    'DEBUT'  : {'TITRE' : 'FICHIER CREE PAR CALC_EUROPLEXUS DE CODE_ASTER',
                'TYPE_DIR' : 0,},
    'DIME'   : {'TITRE' : 'DIMENSIONNEMENT',
                'TYPE_DIR' : 1,},
    'GEOM'   : {'TITRE' : 'DEFINITION DE LA GEOMETRIE',
                'TYPE_DIR' : 1,},
    'COMPLEMENT': {'TITRE' : 'CARACTERISTIQUES DES ELEMENTS DE STRUCTURE',
                   'TYPE_DIR' : 2,},
    'FONC': {'TITRE' : 'DEFINITIONS DES FONCTIONS',
             'TYPE_DIR' : 2,},
    'MATE'   : {'TITRE' : 'DEFINITION DES MATERIAUX',
                'TYPE_DIR' : 2,},
    'ORIENTATION': {'TITRE' : 'ORIENTATION',
                    'TYPE_DIR' : 0,},
    'CHARGE' : {'TITRE' : 'DEFINITION DES CHARGEMENTS',
                'TYPE_DIR' : 2,},
    'LINK'   : {'TITRE' : 'DEFINITION DES CONDITIONS AUX LIMITES',
                'TYPE_DIR' : 2,},
    'ECRITURE'   : {'TITRE' : 'ECRITURES DES RESULTATS',
                    'TYPE_DIR' : 2,},
    'INIT'   : {'TITRE' : 'ETAT INITIAL',
                'TYPE_DIR' : 2,},
    'OPTION'   : {'TITRE' : 'OPTION DE CALCUL',
                  'TYPE_DIR' : 2,},
    'STRUCTURE'   : {'TITRE' : 'DEFINITION DES SOUS DOMAINES',
                     'TYPE_DIR' : 2,},
    'INTERFACE'   : {'TITRE' : 'OPTIONS DE CONNECTION ENTRE SOUS DOMAINES',
                     'TYPE_DIR' : 2,},
    'CALCUL'   : {'TITRE' : 'LANCEMENT DU CALCUL',
                  'TYPE_DIR' : 2,},
    'SUITE'   : {'TITRE' : 'POST-TRAITEMENT',
                 'TYPE_DIR' : 2,},
    'INFO_SORTIE'   : {'TITRE' : 'FICHIER SOURCE / FICHIER DE SORTIE',
                       'TYPE_DIR' : 0,},
    'SORTIE'   : {'TITRE' : 'CREATION DU FICHIER DE SORTIE',
                  'TYPE_DIR' : 2,},
    'FIN'   : {'TITRE' : 'FIN DU CALCUL',
               'TYPE_DIR' : 2,},
                  }
"""
    COMPORTEMENT

    LOI    : liste des mot-clés facteur du matériau nécessaires pour le
             comportement
    BESOIN : o (obligatoire), f (facultatif)
             précise si le mot-clé est obligatoire ou non
    REPEAT : y/n , précise si le mot-clé peut être répeté
    rq ; LOI, BESOIN et REPEAT sont des listes de mêmes longueurs
    NOM_EPX : nom EPX du comportement
    NB_VAR_ASTER : nombre de var. int. pour ce comportement dans aster
    NB_VAR_EPX   : nombre de var. int. pour ce comportement dans EPX
    TRANSFO      : True si une transformation est nécessaire (donc programmée)
                   pour passer des variables internes d'EPX à celles d'aster
                   Si False Vi_aster = V1_epx, si i <= NB_VAR_ASTER
                   les autres variables sont oubliées
    MC_FACT : très spécifique à GLRC_DAMAGE, définit le mot-clé EPX indiquant
              le nombre total des occurences des mot-clés répetables.
"""
cata_compor = {
    'ELAS' : {
        'LOI'    : ['ELAS'],
        'BESOIN' : ['o'],
        'REPEAT' : ['n'],
        'NOM_EPX' : 'LINE',
        'NB_VAR_ASTER' : 1,
        'NB_VAR_EPX'   : 2,
        'TRANSFO'      : False,
            },
    'GLRC_DAMAGE' : {
        'LOI'    : ['RELATION', 'BETON', 'NAPPE', 'CABLE_PREC',
                    'CISAIL_NL', 'LINER'],
        'BESOIN' : ['o', 'o', 'o', 'f', 'f', 'f'],
        'REPEAT' : ['n', 'n', 'y', 'n', 'n', 'y'],
        'NOM_EPX' : 'GLRC DAMA',
        'MC_FACT' : 'NLIT',
        'NB_VAR_ASTER' : 19,
        'NB_VAR_EPX'   : 25,
        'TRANSFO'      : True,
                    },
    #SPECIAL GLRC_DAMAGE
    'BETON' : {
        'LOI'    : ['ELAS', 'BETON_ECRO_LINE',],
        'BESOIN' : ['o', 'f',],
        'REPEAT' : ['n', 'n',],
               },
    'NAPPE' : {
        'LOI'    : ['ELAS', 'ECRO_LINE',],
        'BESOIN' : ['o', 'f',],
        'REPEAT' : ['n', 'n',],
               },
    'LINER' : {
        'LOI'    : ['ELAS', 'ECRO_LINE',],
        'BESOIN' : ['o', 'f',],
        'REPEAT' : ['n', 'n',],
               },
    'CABLE_PREC' : {
        'LOI'    : ['ELAS', 'ECRO_LINE',],
        'BESOIN' : ['o', 'f',],
        'REPEAT' : ['n', 'n',],
                    },
                }

"""
    PARAMETRES DES LOIS

    les clés de ce dictionnaire se construisent comme suit : comportement/loi
    (au sens de mot-clé dans le matériau)

    Rq : tous les mots-clés de ce dictionnaire sont des listes de mêmes
    longueurs à l'exception de NOM_EPX

    PARA : noms des paramètres de la loi dans Code_Aster
    PARA_EPX : noms des paramètres de la loi correspondant dans EPX
    BESOIN   : o/f , précise si le paramètre est obligatoire ou non
    TYPE     : reel, fonc(fonction) ou mfac(comme mot-clé facteur)

    POSI_PARA :
    Dans le cas les "loi" répetables, indique si le paramètre doit être
    positionné à l'intérieur de l'occurence du mot clé (valeur 1 ) ou à
    l'extérieur (0)(comme un paramètre classique du matériau EPX).
    Si ce mot-clé n'est pas donné, on considère que c'est 0 pour chaque
    paramètre.
        Rq : Il y a peu de chance pour que cela soit utile pour un autre
             comportement que GLRC_DAMAGE
    NOM_EPX : nom du mot-clé répetable dans EPX
"""
cata_lois = {
    'ELAS/ELAS' : {
        'PARA'     : ['E', 'NU', 'RHO', 'AMOR_ALPHA', 'AMOR_BETA'],
        'PARA_EPX' : ['YOUNG', 'NU', 'RO', 'KRAY', 'MRAY'],
        'BESOIN'   : ['o', 'o', 'o', 'f', 'f'],
        'TYPE'     : ['reel', 'reel', 'reel', 'reel', 'reel'],
                   },
    # SPECIAL GLRC_DAMAGE
    'GLRC_DAMAGE/CISAIL_NL' : {
        'PARA'     : ['BTD1', 'BTD2', 'TSD',],
        'PARA_EPX' : ['BTD1', 'BTD2', 'TSD',],
        'BESOIN'   : ['o', 'o', 'o',],
        'TYPE'     : ['reel', 'reel', 'reel',],
                               },
    'GLRC_DAMAGE/BETON' : {
        'PARA'     : ['MATER', 'EPAIS', 'GAMMA', 'QP1', 'QP2', 'C1N1',
                      'C1N2', 'C1N3', 'C2N1', 'C2N2', 'C2N3',
                      'C1M1', 'C1M2', 'C1M3', 'C2M1', 'C2M2',
                      'C2M3', 'OMT', 'EAT', 'BT1', 'BT2',
                      'MP1X', 'MP1Y', 'MP2X', 'MP2Y', 'MP1X_FO',
                      'MP1Y_FO', 'MP2X_FO', 'MP2Y_FO'],
        'PARA_EPX' : [None, 'H', 'GAMM', 'QP1', 'QP2', 'C1N1',
                      'C1N2', 'C1N3', 'C2N1', 'C2N2', 'C2N3',
                      'C1M1', 'C1M2', 'C1M3', 'C2M1', 'C2M2',
                      'C2M3', 'OMT', 'EAT', 'BT1', 'BT2',
                      'MP1X', 'MP1Y', 'MP2X', 'MP2Y', 'MP1X',
                      'MP1Y', 'MP2X', 'MP2Y'],
        'BESOIN'   : ['o', 'o', 'o', 'o', 'o', 'o',
                      'o', 'o', 'o', 'o', 'o',
                      'o', 'o', 'o', 'o', 'o',
                      'o', 'f', 'f', 'f', 'f',
                      'f', 'f', 'f', 'f', 'f',
                      'f', 'f', 'f',],
        'TYPE'     : ['mfac', 'reel', 'reel', 'reel', 'reel', 'reel',
                      'reel', 'reel', 'reel', 'reel', 'reel',
                      'reel', 'reel', 'reel', 'reel', 'reel',
                      'reel', 'reel', 'reel', 'reel', 'reel',
                      'reel', 'reel', 'reel', 'reel', 'fonc',
                      'fonc', 'fonc', 'fonc',],
                            },
    'GLRC_DAMAGE/NAPPE' : {
        'PARA'     : ['MATER', 'OMX', 'OMY', 'RX', 'RY', 'FS',],
        'PARA_EPX' : [None, 'OMX', 'OMY', 'RX', 'RY', 'FS',],
        'BESOIN'   : ['o', 'o', 'o', 'o', 'o', 'f',],
        'TYPE'     : ['mfac', 'reel', 'reel', 'reel', 'reel', 'reel',],
        'NOM_EPX'  : 'NAPP',
        'POSI_PARA': [1, 1, 1, 1, 1, 1,],
                           },
    'GLRC_DAMAGE/LINER' : {
        'PARA'     : ['MATER', 'OML', 'RLR',],
        'PARA_EPX' : [None, 'OMLR', 'RLR',],
        'BESOIN'   : ['o', 'o', 'o',],
        'TYPE'     : ['mfac', 'reel', 'reel',],
        'NOM_EPX'  : 'LINR',
        'POSI_PARA': [1, 1, 1,],
                           },
    'GLRC_DAMAGE/CABLE_PREC' : {
        'PARA'     : ['MATER', 'OMX', 'OMY', 'RX', 'RY', 'PREX', 'PREY'],
        'PARA_EPX' : [None, 'OMX', 'OMY', 'RX', 'RY', 'PREX', 'PREY'],
        'BESOIN'   : ['o', 'o', 'o', 'o', 'o', 'o', 'o'],
        'TYPE'     : ['mfac', 'reel', 'reel', 'reel', 'reel', 'reel', 'reel',],
        'NOM_EPX'  : 'PREC',
        'POSI_PARA': [1, 1, 1, 1, 1, 0, 0],
                                },
    'BETON/ELAS' : {
        'PARA'     : ['E', 'NU', 'RHO', 'AMOR_ALPHA', 'AMOR_BETA'],
        'PARA_EPX' : ['EB', 'NUB', 'RO', 'KRAY', 'MRAY'],
        'BESOIN'   : ['o', 'o', 'o', 'f', 'f'],
        'TYPE'     : ['reel', 'reel', 'reel', 'reel', 'reel'],
                    },
    'BETON/BETON_ECRO_LINE' : {
        'PARA'     : ['SYT', 'SYC',],
        'PARA_EPX' : ['FT', 'FC',],
        'BESOIN'   : ['o', 'f',],
        'TYPE'     : ['reel', 'reel',],
                               },
    'NAPPE/ELAS' : {
        'PARA'     : ['E',],
        'PARA_EPX' : ['EA',],
        'BESOIN'   : ['o',],
        'TYPE'     : ['reel',],
        'POSI_PARA': [1,],
                    },
    'NAPPE/ECRO_LINE': {
        'PARA'     : ['SY',],
        'PARA_EPX' : ['FY',],
        'BESOIN'   : ['o',],
        'TYPE'     : ['reel',],
        'POSI_PARA': [1,],
                        },
    'LINER/ELAS' : {
        'PARA'     : ['E', 'NU',],
        'PARA_EPX' : ['EA', 'NULR',],
        'BESOIN'   : ['o', 'o',],
        'TYPE'     : ['reel', 'reel',],
        'POSI_PARA': [1, 1,],
                    },
    'LINER/ECRO_LINE': {
        'PARA'     : ['SY',],
        'PARA_EPX' : ['FY',],
        'BESOIN'   : ['o',],
        'TYPE'     : ['reel',],
        'POSI_PARA': [1,],
                         },
    'CABLE_PREC/ELAS' : {
        'PARA'     : ['E',],
        'PARA_EPX' : ['EA',],
        'BESOIN'   : ['o',],
        'TYPE'     : ['reel',],
        'POSI_PARA': [1,],
                         },
    'CABLE_PREC/ECRO_LINE': {
        'PARA'     : ['SY',],
        'PARA_EPX' : ['FY',],
        'BESOIN'   : ['o',],
        'TYPE'     : ['reel',],
        'POSI_PARA': [1,],
                             },
                }

"""
    ORDRE DES PARAMETRES

    Ajouter un comportement uniquement dans le cas ou les paramètres
    (de POSI_PARA 0 ) doivent être dans un ordre spécial.

    Fonctionnement :
    Si le comportement n'est pas une clé de ce dictionnaire alors l'ordre est
    celui de la lecture des paramètres.
"""
cata_ordre_para = {
    'GLRC_DAMAGE' : [
        'H', 'EB', 'NUB', 'RO', 'NLIT', 'FC', 'C2M1', 'C2M2',
        'C2N2', 'BT1', 'C1N3', 'BT2',
        'C2N1', 'C2N3', 'FT', 'C1M2', 'C1N1', 'C1M3', 'C2M3',
        'OMT', 'QP2', 'C1N2', 'C1M1',
        'QP1', 'EAT', 'GAMM',
        'BTD1', 'BTD2', 'TSD', 'MP1X', 'MP1Y', 'MP2X', 'MP2Y',
        'KRAY', 'MRAY', 'PREX', 'PREY'],
                }

"""
    MODELISATIONS :

    MODE_EPX : Noms des modélisations EPX associées suivants le type de maille
    support. Dans certains cas (DIS_T et DIS_TR par exemple) les modélisations
    EPX correspondantes peuvent être multiples (on tranche avec le CARA_ELEM
    pour les discrets).

    ETAT_INIT : True si un état initial est compatible avec la modalisation

    RESU_ELEM : True si le résultat EPX sur ces éléments est écrit dans le
                fichier med de sortie.
    CONT_ASTER : Noms des composantes de contraintes dans Code_Aster si
                 contraintes il y a.
    MC_CARA    : Nom du mot-clé de AFFE_CARA_ELEM lié à la modélisation si
                 celle-ci en a besoin.
    MODI_REPERE : Type de changement de repère si besoin d'un changement de
                  .repère
"""

# Lors de l'ajout d'un type d'élément dont le champ SIEF_ELGA est formulé en
# effort, il est nécessaire de développer la transformation dans la methode
# LireEPX.prep_cont2effo de lire_europlexus_ops.py.

cata_modelisa = {
    'Q4GG' : {
        'MODE_EPX': {
            'TRIA3' : ['T3GS'],
            'QUAD4' : ['Q4GS'],
                     },
        'ETAT_INIT' : True,
        'RESU_ELEM' : True,
        'CONT_ASTER' : ['NXX', 'NYY', 'NXY', 'MXX', 'MYY', 'MXY', 'QX', 'QY'],
        'MC_CARA' : 'COQUE',
        'MODI_REPERE' : 'COQUE',
              },
    'POU_D_E' : {
        'MODE_EPX': {
            'SEG2' : ['POUT']
                    },
        'ETAT_INIT' : False,
        'RESU_ELEM' : False,
              },
    'BARRE' : {
        'MODE_EPX': {
            'SEG2' : ['BR3D']
                    },
        'ETAT_INIT' : True,
        'RESU_ELEM' : True,
        'CONT_ASTER' : ['N',],
        'MC_CARA' : 'BARRE',
              },
    'DIS_T' : {
        'MODE_EPX': {
            'POI1' : ['APPU', 'PMAT']
                    },
        'ETAT_INIT' : False,
        'RESU_ELEM' : False,
              },
    'DIS_TR' : {
        'MODE_EPX': {
            'POI1' : ['APPU', 'PMAT']
                    },
        'ETAT_INIT' : False,
        'RESU_ELEM' : False,
              },
                }

"""
    CARA_ELEM :

    TITRE : Titre a écrire dans le fichier de commande epx pour le mot-clé
            correspondant.
    DIRECTIVE : Nom de la directive dans laquelle les informations doivent être
                incluses.
    MOT_CLE_EPX : Mot-clé correspondant à l'information à traduire dans la
                  directive EPX.
    MOT_CLE_ASTER : Nom du mot-clé de AFFE_CARA_ELEM ou nom de la valeur à
                    chercher dans la liste du mot-clé CARA de AFFE_CARA_ELEM
                    permettant d'accéder aux données.
    CARA_ASTER : Nom des caractéristiques aster à récupérer dans le mot-clé
                 CARA (si plus de une caractéristique).
    CARA_EPX : Nom des caractéristiques nécessaires dans EPX (si MOT_CLE_EPX ne
               suffit pas).
    IS_VALE_ASTER : True si la caractéristique EPX correspondante est fournie
                    dans VALE de Code_Aster.
    MODE_EPX : A renseigner des le cas ou une même modelisation aster avec la
               meme maille support peut correspondre à plusieurs modelisations
               dans EPX (ex: DIS_T et DIS_TR).

"""
cata_cara_elem = {
    'INFO' : None,
    'MODELE' : None,
    'DISCRET' : [
        {
        'TITRE' :'MASSES AJOUTEES',
        'DIRECTIVE' : 'MATE',
        'MOT_CLE_EPX'  : 'MASSE',
        'MOT_CLE_ASTER' : 'M_T_D_N',
        'MODE_EPX':'PMAT',
         },
        {
        'TITRE' :'SUPPORT ELASTIQUE',
        'DIRECTIVE' : 'MATE',
        'MOT_CLE_EPX'  : 'SUPPORT',
        'MOT_CLE_ASTER' : 'K_TR_D_N',
        'CARA_EPX'   : ['KX', 'KY', 'KZ', 'NFKT', 'KRX', 'KRY', 'KRZ', 'NFKR'],
        'IS_VALE_ASTER': [True, True, True, False, True, True, True, False],
        'MODE_EPX':'APPU',
         },
        {
        'TITRE' :'SUPPORT ELASTIQUE',
        'DIRECTIVE' : 'MATE',
        'MOT_CLE_EPX'  : 'SUPPORT',
        'MOT_CLE_ASTER' : 'A_T_D_N',
        'CARA_EPX'   : ['AX', 'AY', 'AZ', 'NFAT',],
        'IS_VALE_ASTER': [True, True, True, False,],
        'MODE_EPX':'APPU',
         },
        {
        'TITRE' :'SUPPORT ELASTIQUE',
        'DIRECTIVE' : 'MATE',
        'MOT_CLE_EPX'  : 'SUPPORT',
        'MOT_CLE_ASTER' : 'A_TR_D_N',
        'CARA_EPX'   : ['AX', 'AY', 'AZ', 'NFAT', 'ARX', 'ARY', 'ARZ', 'NFAR'],
        'IS_VALE_ASTER': [True, True, True, False, True, True, True, False],
        'MODE_EPX':'APPU',
         },
                 ],
    'COQUE' : [
        {
        'TITRE' :'COQUES',
        'DIRECTIVE' : 'COMPLEMENT',
        'MOT_CLE_EPX': 'EPAIS',
        'MOT_CLE_ASTER' : 'EPAIS',
         }
               ],
    'POUTRE' : [
        {
        'TITRE' :'ELEMENTS POUTRES',
        'DIRECTIVE' : 'COMPLEMENT',
        'MOT_CLE_EPX': 'GEOP',
        'INFO_CLE'   : 'SECTION',
        'CARA_ASTER' : [' ', ' ', ' ', 'HY', 'HZ'],
        'CARA_EPX'   : ['VX', 'VY', 'VZ', 'AY', 'AZ'],
        'VERIF'      : {'SECTION' : ['RECTANGLE']},
         }
               ],
    'RIGI_PARASOL' : [
#    Ne pas prendre RIGI_PARASOL pour modèle : cas spécial.
        {
        'TITRE' :'RESSORTS DE SOL',
        'DIRECTIVE' : 'MATE',
        'MOT_CLE_EPX'  : 'SUPPORT',
        # traitement très spéciale pour RIGI_PARASOL
        'MOT_CLE_ASTER' : ['K_TR_D_N', 'A_T_D_N', 'A_TR_D_N',],
        'K_TR_D_N' : ['KX', 'KY', 'KZ', 'NFKT', 'KRX', 'KRY', 'KRZ', 'NFKR'],
        'A_T_D_N' : ['AX', 'AY', 'AZ', 'NFAT',],
        'A_TR_D_N' : ['AX', 'AY', 'AZ', 'NFAT', 'ARX', 'ARY', 'ARZ', 'NFAR'],
        'MODE_EPX':'APPU',
         }
                     ],
    'BARRE' : [
        {
        'TITRE' :'BARRES',
        'DIRECTIVE' : 'COMPLEMENT',
        'MOT_CLE_EPX': 'SECT',
        'MOT_CLE_ASTER' : 'A',
        'VERIF'      : {'SECTION' : ['GENERALE']},
         }
              ],
    'ORIENTATION'  : None,# utilisé dans la classe poutre
                 }

"""
    CHARGEMENTS

    DIRECTIVE : Nom de la directive dans laquelle transcrire les informations.
    MOT_CLE_EPX : Liste des mots-clés EPX correspondant au mot-clé facteur
                  aster.
    FONC_MULT : Si une fonction multiplicatrice est associée au chargement on
                donne le mot-clé définissant cette fonction dans EPX.
    ASTER : Nom du ou des mot-clés aster.
    EPX   : Nom du mot-clé EPX correspondant si besoin.
    COEF_MULT : Coefficient multiplicateur pour passer de Aster à EPX.
    VALE_IMPO : Valeur imposée dans Code_Aster pour avoir la correspondance
                dans EPX.
    ENTITE : Sur quoi s'appuie de chargement (GROUP_MA ou GROUP_NO).

"""

cata_charge = {
    'INFO'   : False,
    'MODELE' : False,
    'FORCE_COQUE' : {
        'DIRECTIVE'  : 'CHARGE',
        'MOT_CLE_EPX': ['1 FACTO 2', 'PRES COQU'],
        'FONC_MULT'  : 'TABLE',
        'ASTER'      : ['PRES'],
        'EPX'        : None,
        'COEF_MULT'  : -1,
        'ENTITE'     : ['GROUP_MA'],
                  },
    'DDL_IMPO' : {
        'DIRECTIVE' : 'LINK',
        'MOT_CLE_EPX': ['BLOQ'],
        'ASTER'     : ['DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'],
        'EPX'       : ['1', '2', '3', '4', '5', '6'],
        'VALE_IMPO' : 0.,
        'ENTITE'    : ['GROUP_MA', 'GROUP_NO'],
                  },
    'RELA_CINE_BP' : {
        'DIRECTIVE' : 'LINK',
        'MOT_CLE_EPX': ['RELA'],
        'ASTER'      : ['CABLE_BP'],
        'EPX'        : None,
        'MOT_CLE_VERIF' : ['SIGM_BPEL', 'RELA_CINE'],
        'VALE_VERIF' : ['NON', 'OUI'],
               },
              }


# Autres dictionnaires de correspondance.

cata_inst = {
    'PAS_NBRE' :'FREQ',
    'PAS_INST' : 'TFREQ'
          }

cata_courbe = {
    'PAS_NBRE_COURBE':'FREQ',
    'PAS_INST_COURBE' : 'TFREQ'
                 }

cata_champs = {
    'DEPL'     :'DEPLACEMENT',
    'VITE'     :'VITESSE',
    'ACCE'     :'ACCELERATION',
    'SIEF_ELGA':'CONT',
    'EPSI_ELGA':'EPST',
    'VARI_ELGA':'ECRO'
               }

cata_calcul = {
    'INST_INIT':'TINI',
    'PASFIX':'PASFIX',
    'INST_FIN':'TFIN',
    'NMAX':'NMAX'
               }

cata_compo = {}
for cham in ['DEPL', 'VITE', 'ACCE']:
    cata_compo[cham] = {
        'DX' : 1,
        'DY' : 2,
        'DZ' : 3,
        'DRX': 4,
        'DRY': 5,
        'DRZ': 6
                       }

cata_compo['SIEF_ELGA'] = {
    'SIXX' : 1,
    'SIYY' : 2,
    'SIZZ' : 3,
    'SIXY' : 4,
    'SIXZ' : 6,
    'SIYZ' : 5,
    'NXX'  : 1,
    'NYY'  : 2,
    'NXY'  : 3,
    'MXX'  : 4,
    'MYY'  : 5,
    'MXY'  : 6,
    'QX'   : 7,
    'QY'   : 8
                           }

cata_compo['EPSI_ELGA'] = {
    'EPXX' : 1,
    'EPYY' : 2,
    'EPZZ' : 3,
    'EPXY' : 4,
    'EPXZ' : 6,
    'EPYZ' : 5,
    'EXX'  : 1,
    'EYY'  : 2,
    'EXY'  : 3,
    'KXX'  : 4,
    'KYY'  : 5,
    'KXY'  : 6,
    'GAX'  : 7,
    'GAY'  : 8
                           }

cata_compo['VARI_ELGA'] = {}
for ii in range(1, 25):
    cata_compo['VARI_ELGA']['V%i'%ii] = ii

    
# Format med des champs depl, vite et acce
format_med = [
    {
    'NOM_CHAM_MED' : 'DEPL_001',
    'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
    'NOM_CMP_MED' : ('UX', 'UY', 'UZ', 'RX', 'RY', 'RZ'),
    'NOM_CHAM' :'DEPL'
     },
    {
    'NOM_CHAM_MED' : 'VITE_001',
    'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
    'NOM_CMP_MED' : ('VX', 'VY', 'VZ', 'RX', 'RY', 'RZ'),
    'NOM_CHAM' : 'VITE'
     },
    {
    'NOM_CHAM_MED' : 'ACCE_001',
    'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
    'NOM_CMP_MED' : ('GX', 'GY', 'GZ', 'RX', 'RY', 'RZ'),
    'NOM_CHAM' : 'ACCE'
     },
             ]
