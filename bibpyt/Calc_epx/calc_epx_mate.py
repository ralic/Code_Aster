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
Traitement des données matériaux et comportements pour CALC_EUROPLEXUS
"""

from Calc_epx.calc_epx_cata import cata_compor, cata_lois, cata_ordre_para
from Calc_epx.calc_epx_utils import recupere_structure, tolist, get_group_ma
from Utilitai.Utmess import UTMESS
from Calc_epx.calc_epx_struc import BLOC_DONNEES, BLOC_MATE, FONCTION


def export_mate(epx, CHAM_MATER, COMPORTEMENT, gmaInterfaces, dicOrthotropie):
    """
        Traitement des données matériaux et comportements
    """

    directive = 'MATE'
    # Recuperer la structure sous le mot_cle facteur AFFE de AFFE_MATER
    affe_mater = recupere_structure(CHAM_MATER, 'AFFE')
    affe_mater = tolist(affe_mater)

    # RELATTIONS AUTORISEES
    relations_autorisees = []
    for rela in cata_compor.keys():
        if cata_compor[rela].has_key('NOM_EPX'):
            relations_autorisees.append(rela)

    # ETAPE 1 : Recherche de la relation pour les GROUP_MA déclaré
    # dans COMPORTEMENT

    dic_comportement = {}
    gmaGLRC = []
    for comp in COMPORTEMENT:
        if comp['RELATION'] not in relations_autorisees:
            raise Exception("""La relation %s n'est pas programmée"""
                            % (comp['RELATION']))
        for gr in comp['GROUP_MA']:
            # EC refonte : on peut supprimer cela si on considère la loi
            # des surcharges
            if gr in dic_comportement:
                raise Exception('Une relation existe déjà pour le groupe %s'
                                % gr)
            # FIN EC
            dic_comportement[gr] = {'RELATION': comp['RELATION'],
                                    'MATER': None,
                                    'NOM_MATER': None,
                                    }
            if comp['RELATION'] == 'GLRC_DAMAGE':
                # GLRC impose de définir l'orientation :
                # on stocke dans gmaGLRC les GMA dont il faudra retrouver
                # l'orientation dans MODI_MAILLAGE/ORIE_NORM_COQUE
                gmaGLRC.append(gr)

    # ETAPE 2 : Recherche du materiau

    for affe in affe_mater:
        # Recuperer le concept du materiau defini par DEFI_MATERIAU
        concept_mater = affe['MATER']
        nom_mater = concept_mater.get_name()
        # Recuperer les group_ma concernes
        group_ma = get_group_ma(affe)
        for gr in group_ma:
            if gr in dic_comportement:
                dic_comportement[gr]['MATER'] = concept_mater
                dic_comportement[gr]['NOM_MATER'] = nom_mater
            # else :
            # EC : emet-on un message d'alarme ?

    # ETAPE 3 : Verification que tous les GROUP_MA ont un materiau
    #           creation des couples MATERIAU/RELATION et des group_ma associés
    dic_mate_rela = {}
    for gr in dic_comportement.keys():
        if dic_comportement[gr]['MATER'] == None:
            UTMESS('F', 'PLEXUS_32', gr)
        relation = dic_comportement[gr]['RELATION']
        nom_mater = dic_comportement[gr]['NOM_MATER']
        nom_mate_rela = nom_mater + '/' + relation
        if not dic_mate_rela.has_key(nom_mate_rela):
            dic_mate_rela[nom_mate_rela] = dic_comportement[gr]
            dic_mate_rela[nom_mate_rela]['GROUP_MA'] = []
        dic_mate_rela[nom_mate_rela]['GROUP_MA'].append(gr)

    # ETAPE 4 :

    nb_fonc = epx['FONC'].len_mcs()
    # EC : pas trop joli, sert a savoir le nombre de fonction deja declarees
    # en evitant de passer un argument supplémentaire
    liste_fonc = [' '] * nb_fonc
    mate_ordo = dic_mate_rela.keys()
    mate_ordo.sort()
    for mate_rela in mate_ordo:
        relation = dic_mate_rela[mate_rela]['RELATION']
        nom_mater = dic_mate_rela[mate_rela]['NOM_MATER']
        concept_mater = dic_mate_rela[mate_rela]['MATER']
        l_group = dic_mate_rela[mate_rela]['GROUP_MA']
        mate_epx = cata_compor[relation]['NOM_EPX']
        if cata_compor[relation].has_key('MC_FACT'):
            cle_bs = cata_compor[relation]['MC_FACT']
        else:
            cle_bs = None
        l_para = []
        l_vale = []
        l_bs = []

        l_cisail = False
        for i_loi, loi in enumerate(cata_compor[relation]['LOI']):
            besoin = cata_compor[relation]['BESOIN'][i_loi]
            donnees_loi = recupere_structure(concept_mater, loi)
            if not donnees_loi:
                if besoin == 'o':
                    UTMESS('F', 'PLEXUS_33', valk=(loi, nom_mater, relation))
                else:
                    continue
            donnees_loi = tolist(donnees_loi)

            if relation == 'GLRC_DAMAGE' and loi == 'RELATION':
                rela = donnees_loi[0]
                if relation != rela:
                    UTMESS('F', 'PLEXUS_22', valk=(loi, nom_mater, relation))
                continue

            rel_loi = relation + '/' + loi
            if cata_lois[rel_loi].has_key('NOM_EPX'):
                mot_cle_fact = True
                mot_cle_epx = cata_lois[rel_loi]['NOM_EPX']
            else:
                mot_cle_fact = False

            if (cata_compor[relation]['REPEAT'][i_loi] == 'n'
               and len(donnees_loi) > 1):
                raise Exception(
                    'Erreur de programmation, le mot cle ne peut pas etre repete')
            elif (cata_compor[relation]['REPEAT'][i_loi] == 'y'
                  and not mot_cle_fact):
                raise Exception(
                    'Erreur dev : un motclé repetable doit avoir NOM_EPX dans sa loi')

            # lecture des parametres
            if relation == 'GLRC_DAMAGE' and loi == 'CISAIL_NL':
                l_cisail = True
            for donnees in donnees_loi:
                l_para, l_vale, l_bs, liste_fonc = get_para_all(loi, relation,
                                                                l_para, l_vale, l_bs,
                                                                nom_mater, donnees,
                                                                liste_fonc)

        if cata_ordre_para.has_key(relation):
            ordre_para = cata_ordre_para[relation]
        else:
            ordre_para = None

        # ETAPE 5 : Construction du bloc de données
        titre = mate_rela
        if mate_epx == 'GLRC DAMA' and l_cisail:
            mate_epx += ' SHEA'
        bloc = BLOC_MATE(mate_epx, l_group, cara=l_para, vale=l_vale,
                         cle_bs=cle_bs, l_bs=l_bs, ordre_para=ordre_para,
                         titre=titre)
        epx[directive].add_bloc(bloc)

    # traiter les fonctions
    for dic_fonc in liste_fonc:
        if type(dic_fonc) is not dict:
            continue
        val = dic_fonc['VALE']
        ifo = dic_fonc['NUME']
        nom_aster = dic_fonc['NOM_FONC']
        cle_fonc = '%i LSQU 2 TABL' % (ifo)
        bloc_fonc = FONCTION(cle_fonc, val[0], val[1], nom_aster=nom_aster)
        epx['FONC'].add_bloc(bloc_fonc)

    if gmaInterfaces:
        mot_cle_epx = 'FANTOME'
        val_cle = 0.
        titre = 'INTERFACES'
        bloc = BLOC_DONNEES(mot_cle_epx, l_group=gmaInterfaces,
                            val_cle=val_cle, titre=titre)
        epx[directive].add_bloc(bloc)

    # traitement des orientations pour GLRC
    # on le fait ici car cela depend du materiau
    # DEFINITION REPERES ORTHOTROPIE QUI DOIT ETRE APRES MATE
    directive2 = 'ORIENTATION'
    for gma in gmaGLRC:
        if gma not in dicOrthotropie:
            UTMESS('F', 'PLEXUS_36', valk=(gma))
        vale = dicOrthotropie[gma]
        mot_cle_epx = 'COMP ORTS'
        val_cle = '%s %s %s' % (vale[0], vale[1], vale[2])
        bloc = BLOC_DONNEES(mot_cle_epx, l_group=gma, val_cle=val_cle,)
        epx[directive2].add_bloc(bloc)

    return epx


#-----------------------------------------------------------------------
def get_para_loi(loi, relation, l_para, l_vale, l_para1, l_vale1,
                 nom_mater, donnees, liste_fonc):
    """
        Lecture des paramètres de la loi 'loi'
    """
    cle = relation + '/' + loi
    if not cata_lois[cle].has_key('POSI_PARA'):
        posi_para = False
    else:
        posi_para = True
    for ipar, para in enumerate(cata_lois[cle]['PARA']):
        if donnees.has_key(para):
            para_epx = cata_lois[cle]['PARA_EPX'][ipar]
            type_donnee = cata_lois[cle]['TYPE'][ipar]
            if type_donnee == 'fonc':
                car_temp = donnees[para]
                nom_fonc = car_temp.get_name()
                ifonc = len(liste_fonc) + 1
                for dic_fonc in liste_fonc:
                    if type(dic_fonc) is dict:
                        nom = dic_fonc['NOM_FONC']
                        if nom == nom_fonc:
                            ifonc = dic_fonc['NUME']
                            break
                if ifonc > len(liste_fonc):
                    val = car_temp.Valeurs()
                    dic_fonc = {'VALE': val,
                                'NUME': ifonc,
                                'NOM_FONC': nom_fonc
                                }
                    liste_fonc.append(dic_fonc)
                vale = 'FONC %i' % ifonc
            elif type_donnee == 'mfac':
                raise Exception(
                    'Erreur de programmation : type de donnees mfac impossible')
            else:
                vale = donnees[para]
            if not posi_para:
                l_para.append(para_epx)
                l_vale.append(vale)
            elif cata_lois[cle]['POSI_PARA'][ipar] == 0:
                l_para.append(para_epx)
                l_vale.append(vale)
            else:
                l_para1.append(para_epx)
                l_vale1.append(vale)
        else:
            bes_para = cata_lois[cle]['BESOIN'][ipar]
            if bes_para == 'o':
                UTMESS('F', 'PLEXUS_31', valk=(para, loi, nom_mater))
    return l_para, l_vale, l_para1, l_vale1, liste_fonc
#-----------------------------------------------------------------------


def get_para_all(loi, relation, l_para, l_vale, l_bs,
                 nom_mater, donnees, liste_fonc):
    """
        Lecture des parametres de la loi 'loi'
        + traitement des parametres facteurs
    """
    rel_loi = relation + '/' + loi
    l_posi = True
    l_para1 = []
    l_vale1 = []
    if not cata_lois[rel_loi].has_key('POSI_PARA'):
        posi_para = 0
        l_posi = False
    for ipar, para in enumerate(cata_lois[rel_loi]['PARA']):
        if donnees.has_key(para):
            type_para = cata_lois[rel_loi]['TYPE'][ipar]
            para_epx = cata_lois[rel_loi]['PARA_EPX'][ipar]
            if l_posi:
                posi_para = cata_lois[rel_loi]['POSI_PARA'][ipar]
            if type(para_epx) is list:
                if rel_loi == 'VMIS_ISOT_TRAC/TRACTION':
                    l_para, l_vale = vmis_isot_trac(donnees, para, para_epx,
                                                    type_para, l_para, l_vale)
                else:
                    raise Exception("""
Pas de traitement special présent pour le couple relation/loi %s."""
                                    % rel_loi)
            elif type_para == 'fonc':
                car_temp = donnees[para]
                nom_fonc = car_temp.get_name()
                ifonc = len(liste_fonc) + 1
                for dic_fonc in liste_fonc:
                    if type(dic_fonc) is dict:
                        nom = dic_fonc['NOM_FONC']
                        if nom == nom_fonc:
                            ifonc = dic_fonc['NUME']
                            break
                if ifonc > len(liste_fonc):
                    val = car_temp.Valeurs()
                    dic_fonc = {'VALE': val,
                                'NUME': ifonc,
                                'NOM_FONC': nom_fonc
                                }
                    liste_fonc.append(dic_fonc)
                vale = 'FONC %i' % ifonc
                if posi_para == 0:
                    l_para.append(para_epx)
                    l_vale.append(vale)
                else:
                    l_para1.append(para_epx)
                    l_vale1.append(vale)
            elif type_para == 'reel':
                vale = donnees[para]
                if posi_para == 0:
                    l_para.append(para_epx)
                    l_vale.append(vale)
                else:
                    l_para1.append(para_epx)
                    l_vale1.append(vale)
            elif type_para == 'mfac':
                concept2 = donnees[para]

                nom_concept2 = concept2.get_name()
                for i, loi2 in enumerate(cata_compor[loi]['LOI']):
                    besoin = cata_compor[loi]['BESOIN'][i]
                    donnees2 = recupere_structure(concept2, loi2)
                    if not donnees2:
                        if besoin == 'o':
                            UTMESS('F', 'PLEXUS_33',
                                   valk=(loi2, nom_concept2, loi))
                        else:
                            continue
                    l_para, l_vale, l_para1, l_vale1, liste_fonc = get_para_loi(
                        loi2, loi, l_para,
                        l_vale, l_para1, l_vale1,
                        nom_concept2, donnees2,
                        liste_fonc)
            else:
                raise Exception(
                    'Erreur de programmation, TYPE = %s est interdit' % type_para)
        else:
            bes_para = cata_lois[rel_loi]['BESOIN'][ipar]
            if bes_para == 'o':
                UTMESS('F', 'PLEXUS_31', valk=(para, loi, nom_mater))
    if cata_lois[rel_loi].has_key('NOM_EPX'):
        nom_epx = cata_lois[rel_loi]['NOM_EPX']
        bloc_s = BLOC_DONNEES(nom_epx, cara=l_para1, vale=l_vale1)
        l_bs.append(bloc_s)
    return l_para, l_vale, l_bs, liste_fonc
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#                Routines de traitements spécifiques
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------


def vmis_isot_trac(donnees, para, para_epx, type_para, l_para, l_vale):
    """
        Transforme la fonction associée au paramètre SIGM de TRACTION afin
        d'obtenir les données attendues par EPX pour les mots-clés
        ELAS et TRAC du matériau VMIS ISOT.
    """
#   ajout des mots-clés
    l_para.extend(para_epx)
#   recuperation de la fonction associé à SIGM
    car_temp = donnees[para]
    val = car_temp.Valeurs()
    eps = val[0]
    sigm = val[1]
#   verif de la compatibilité de eps[0] et sigm[0] avec E
    if l_para[0][:4] != 'YOUN':
        raise Exception('Erreur de programmation')
    E = l_vale[0]
    diff = abs(sigm[0] / E - eps[0])
    if diff > 0.001 * eps[0]:
        nom_fonc = car_temp.get_name()
        UTMESS('F', 'PLEXUS_40', valk=[
               nom_fonc, para], valr=[sigm[0] / E, eps[0]])
    # ajout de la valeur de ELAS (limite élastique)
    l_vale.append(sigm[0])
    # préparation des valeurs de trac
    nb = len(eps)
    trac = [nb, ]
    for i in range(nb):
        trac.extend([sigm[i], eps[i]])
    l_vale.append(trac)

    return l_para, l_vale
