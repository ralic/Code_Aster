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
Traitement du modèle
"""
from Calc_epx.calc_epx_cata import cata_cara_elem
from Calc_epx.calc_epx_utils import get_group_ma, tolist
from Calc_epx.calc_epx_utils import float2str
import string
from Utilitai.Utmess import UTMESS
from Calc_epx.calc_epx_struc import BLOC_DONNEES        
from Utilitai.partition import MAIL_PY

def bloc_cara(typ_carel, l_elem, epx, l_group, directive, mot_cle_aster,
              mot_cle_epx, info_cle, cara_aster, cara_epx, is_vale_aster,
              mode_epx, mode_from_cara, titre, verif,
              dic_gr_cara_supp):
    """
        Analyse les données contenues dans une liste d'instances 'l_elem' du
        mot clé facteur 'typ_carel' de AFFE_CARA_ELEM.
        'l_elem' contient la plupart du temps un seul élément, sauf dans le
        cas de typ_carel='DISCRET'.
    """
    l_elem = tolist(l_elem)
    l_group = tolist(l_group)
    mot_cle_epx_select = None
    l_cara = []
    l_vale = []
    for elem in l_elem:
        for i_dic in range(len(directive)):

            if mot_cle_aster[i_dic]:
                if elem.has_key(mot_cle_aster[i_dic]):
                    val_cle = elem[mot_cle_aster[i_dic]]
                    vale = []
                    cara = []
                else:
                    cara_in = tolist(elem['CARA'])
                    if len(cara_in) != 1:
                        raise Exception('CARA_ELEM : cas non prevu')
                    if cara_in[0] != mot_cle_aster[i_dic]:
                        continue
                    valeur = tolist(elem['VALE'])
                    if is_vale_aster[i_dic]:
                        cara = cara_epx[i_dic]
                        vale = [None]*len(cara)
                        k_valeur = 0
                        for i_log, logi in enumerate(is_vale_aster[i_dic]):
                            if logi:
                                vale[i_log] = valeur[k_valeur]
                                k_valeur += 1
                        val_cle = ''
                    else:
                        val_cle = valeur[0]
                        vale = []
                        cara = []
            else:
                # pour l'instant il n'est pas nécessaire de boucler
                # si echec dans ce cas la
                val_cle = ''
                cara_in = tolist(elem['CARA'])
                vale_in = tolist(elem['VALE'])
                vale = [None]*len(cara_epx[i_dic])
                cara = cara_epx[i_dic]
                for i, car in enumerate(cara_in):
                    if not car in cara_aster[i_dic]:
                        UTMESS('F', 'PLEXUS_8', valk=(car, typ_carel))
                    val = vale_in[i]
                    index = cara_aster[i_dic].index(car)
                    vale[index] = val
            if None in vale:
                for i, val in enumerate(vale):
                    if val is None:
                        car = cara[i]
                        if len(l_group)!=1:
                            raise Exception("""Cas non traité. faire comme
                                            le cas else dans export_cara""")
                        gr_ma = l_group[0]
                        if not dic_gr_cara_supp.has_key(gr_ma):
                            UTMESS('F', 'PLEXUS_12', valk=(car, typ_carel, gr_ma))
                        if not car in dic_gr_cara_supp[gr_ma]:
                            UTMESS('F', 'PLEXUS_12', valk=(car, typ_carel, gr_ma))
                        vale[i] = dic_gr_cara_supp[gr_ma][car]
            #
            if info_cle[i_dic]:
                cle = elem[info_cle[i_dic]]
            else:
                cle = ''
            if verif[i_dic]:
                for mc_aster in verif[i_dic].keys():
                    if elem[mc_aster] not in  verif[i_dic][mc_aster]:
                        liste_ok = ', '.join(verif[i_dic][mc_aster])
                        UTMESS('F', 'PLEXUS_4', valk=(typ_carel, mc_aster,
                                                     elem[mc_aster], liste_ok))
            if mot_cle_epx_select == None:
                mot_cle_epx_select = mot_cle_epx[i_dic]
                directive_select = directive[i_dic]
                cle_select = cle
                val_cle_select = val_cle
                titre_select = titre[i_dic]
                mode_epx_select = mode_epx[i_dic]
            else:
                if (mot_cle_epx_select != mot_cle_epx[i_dic] or
                    directive_select != directive[i_dic] or
                    cle_select != cle or
                    val_cle_select != val_cle or
                    titre_select != titre[i_dic] or
                    mode_epx_select != mode_epx[i_dic]):
                    raise Exception('Erreur dev : Incohérence des donnees')
            l_cara.extend(cara)
            l_vale.extend(vale)
    if mot_cle_epx_select == None:
        raise Exception('Echec : lecture des caractéristiques élémentaires')
    bloc_donnees = BLOC_DONNEES(mot_cle_epx_select, l_group=l_group,
                                cle=cle_select, val_cle=val_cle_select,
                                cara=l_cara, vale=l_vale, titre=titre_select)
    epx[directive_select].add_bloc(bloc_donnees)
    if mode_epx_select is not None:
        for group in l_group:
            if not mode_from_cara.has_key(group):
                mode_from_cara[group] = mode_epx_select
            else:
                if mode_from_cara[group] != mode_epx_select:
                    UTMESS('F', 'PLEXUS_9', valk=(mode_from_cara[group],
                                                  mode_epx_select, group))
    return epx, mode_from_cara

def export_cara(cle, epx, donnees_cle, MAILLAGE, CARA_ELEM,
                 dic_gr_cara_supp, mode_from_cara):
    """
        Traite les données 'donnes_cle' contenues dans le mot clé facteur 'cle'
        de l'objet CARA_ELEM
    """
    from Cata.cata import DEFI_GROUP
    donnees_cle = tolist(donnees_cle)

    # recuperation des parametres
    [titre, directive, mot_cle_epx, mot_cle_aster, cara_aster,
    cara_epx, info_cle, is_vale_aster, mode_epx, verif] = recu_cara_cata(cle)


    if cle != 'RIGI_PARASOL':
        dic_gr_donnees = {}
        for elem in donnees_cle:
            l_group = get_group_ma(elem)
            for group in l_group:
                if not group in dic_gr_donnees.keys():
                    dic_gr_donnees[group] = []
                dic_gr_donnees[group].append(elem)

        for group in dic_gr_donnees.keys():
            l_elem = dic_gr_donnees[group]
            epx, mode_from_cara = bloc_cara(cle, l_elem, epx,
                           group, directive, mot_cle_aster, mot_cle_epx,
                           info_cle, cara_aster, cara_epx, is_vale_aster,
                           mode_epx, mode_from_cara, titre, verif,
                           dic_gr_cara_supp)
    
    else:
        MApyt = MAIL_PY()
        MApyt.FromAster(MAILLAGE)
        cara_parasol = None
        if len(donnees_cle)>1:
            UTMESS('F','PLEXUS_42')
        for elem in donnees_cle:
            group_ma_poi1 = get_group_ma(elem, 'GROUP_MA_POI1')
            if len(group_ma_poi1) !=1:
                UTMESS('F','PLEXUS_43')
            cara_in = tolist(elem['CARA'])
            if cara_parasol is not None:
                if cara_in != cara_parasol:
                    UTMESS('F', 'PLEXUS_15')
            else:
                cara_parasol = cara_in

            for group in group_ma_poi1:
                if not mode_from_cara.has_key(group):
                    mode_from_cara[group] = mode_epx[0]
                else:
                    raise Exception(
    'Une modélisation existe déjà pour le groupe %s dans mode_from_cara'%group)
        # verif des caractéristiques
        group_ma_poi1 = group_ma_poi1[0]
        l_cara = []
        for car in cara_parasol:
            if car not in mot_cle_aster[0]: UTMESS('F', 'PLEXUS_8',
                                                   valk=(car, cle))
            l_cara.extend(cata_cara_elem[cle][0][car])
        if not dic_gr_cara_supp.has_key(group_ma_poi1):
            UTMESS('F', 'PLEXUS_12', valk=('NKFT ou NFAT', cle, group_ma_poi1))
        for car in l_cara:
            if car.startswith('NF'):
                if car not in dic_gr_cara_supp[group_ma_poi1]:
                    UTMESS('F', 'PLEXUS_12', valk=(car, cle, group_ma_poi1))

        ressorts, amorts = CARA_ELEM.toEPX()
        crea_gr_ma = []
        li_mailles = []
        li_mailles.extend(ressorts.keys())
        li_mailles.extend(amorts.keys())
        li_mailles = list(set(li_mailles))
        
        for maille in li_mailles:
            # attention si pas d'amortissement (ou pas de raideurs)
            # amorts = {'        ': (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)}
            # on supprime cela de la liste li_mailles
            if maille.strip() == '': continue
            #
            group_ma = 'G_%s' %maille
            if not MApyt.gma.has_key(string.rstrip(group_ma)):
                crea_gr_ma.append({"MAILLE" : maille, "NOM" : group_ma})
            else:
                UTMESS('A', 'PLEXUS_35', valk=(group_ma, maille))

            l_vale = []
            for car in cara_parasol:
                if car.startswith('K_'):
                    val_raid = [float2str(x) for x in ressorts[maille]]
                    l_vale.extend(val_raid[:3])
                    l_vale.append(dic_gr_cara_supp[group_ma_poi1]['NFKT'])
                if car == 'K_TR_D_N':
                    l_vale.extend(val_raid[3:6])
                    l_vale.append(dic_gr_cara_supp[group_ma_poi1]['NFKR'])
                if car.startswith('A_'):
                    val_amor = [float2str(x) for x in amorts[maille]]
                    l_vale.extend(val_amor[:3])
                    l_vale.append(dic_gr_cara_supp[group_ma_poi1]['NFAT'])
                if car == 'A_TR_D_N':
                    l_vale.extend(val_amor[3:6])
                    l_vale.append(dic_gr_cara_supp[group_ma_poi1]['NFAR'])

            bloc_donnees = BLOC_DONNEES(mot_cle_epx[0], l_group=group_ma,
                                        cara=l_cara, vale=l_vale,
                                        titre=titre[0])
            epx[directive[0]].add_bloc(bloc_donnees)

        if crea_gr_ma != []:
            DEFI_GROUP(reuse=MAILLAGE,
                       MAILLAGE=MAILLAGE,
                       CREA_GROUP_MA=crea_gr_ma
                      )

    return epx, mode_from_cara

#-----------------------------------------------------------------------
def get_FONC_PARASOL(epx, FONC_PARASOL, dic_gr_cara_supp):
    """
        Récupère les fonctions présentes dans FONC_PARASOL
    """

    from Calc_epx.calc_epx_struc import FONCTION
    from Calc_epx.calc_epx_utils import tolist

    directive = 'FONC'
    # Cles de FONC_PARASOL dans l'ordre
    cles = ['NFKT', 'NFKR', 'NFAT', 'NFAR']
    # dictionnaire nom de la fonction : numero de fonction
    dic_fonc = {}
    ifonc = epx['FONC'].len_mcs()
    list_FONC_PARASOL = FONC_PARASOL.List_F()
    for inst_fonc in list_FONC_PARASOL:
        dic_gr = {}
        for cle in cles:
            if inst_fonc.has_key(cle):
                fonction = inst_fonc[cle]
                nom_aster = fonction.get_name()
                if not nom_aster in dic_fonc.keys():
                    ifonc += 1
                    dic_fonc[nom_aster] = ifonc
                    (temps, valeurs) = fonction.Valeurs()
                    cle_fonc = '%i TABL' %(ifonc)
                    bloc_fonc = FONCTION(cle_fonc, temps, valeurs, nom_aster)
                    epx[directive].add_bloc(bloc_fonc)
                dic_gr[cle] = dic_fonc[nom_aster]
        l_gr_ma = tolist(inst_fonc['GROUP_MA'])
        for gr in l_gr_ma:
            if dic_gr_cara_supp.has_key(gr):
                UTMESS('F','PLEXUS_41',valk=gr)
            dic_gr_cara_supp[gr] = dic_gr
        
    return dic_gr_cara_supp

#-----------------------------------------------------------------------
def recu_cara_cata(cle):
    """
        Récupère et met en forme les informations contenues dans le
        catalogue cata_cara_elem pour la clé 'cle'.
    """

    if not cata_cara_elem.has_key(cle):
        UTMESS('F','PLEXUS_44',valk = cle)
    titre = []
    directive = []
    mot_cle_epx = []
    mot_cle_aster = []
    cara_aster = []
    cara_epx = []
    info_cle = []
    is_vale_aster = []
    mode_epx = []
    verif = []
    for dic in cata_cara_elem[cle]:

        titre.append(dic['TITRE'])
        directive.append(dic['DIRECTIVE'])
        mot_cle_epx.append(dic['MOT_CLE_EPX'])

        if dic.has_key('MOT_CLE_ASTER'):
            mot_cle_aster.append(dic['MOT_CLE_ASTER'])
            cara_aster.append(None)
            if dic.has_key('IS_VALE_ASTER'):
                is_vale_aster.append(dic['IS_VALE_ASTER'])
            else:
                is_vale_aster.append(None)
        else:
            mot_cle_aster.append(None)
            if dic.has_key('CARA_ASTER'):
                cara_aster.append(dic['CARA_ASTER'])
            else:
                raise Exception('MOT_CLE_ASTER absent => CARA_ASTER  présent')
        if dic.has_key('CARA_EPX'):
            cara_epx.append(dic['CARA_EPX'])
        else:
            cara_epx.append(None)
        if dic.has_key('INFO_CLE'):
            info_cle.append(dic['INFO_CLE'])
        else:
            info_cle.append(None)
        if dic.has_key('MODE_EPX'):
            mode_epx.append(dic['MODE_EPX'])
        else:
            mode_epx.append(None)
        if dic.has_key('VERIF'):
            verif.append(dic['VERIF'])
        else:
            verif.append(None)

    return [titre, directive, mot_cle_epx, mot_cle_aster, cara_aster,
           cara_epx, info_cle, is_vale_aster, mode_epx, verif]
