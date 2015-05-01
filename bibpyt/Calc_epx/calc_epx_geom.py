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
# person_in_charge: serguei.potapov at edf.fr

"""
Traitement du modèle
"""
from Calc_epx.calc_epx_cata import cata_modelisa, mode_epx_fin
from Calc_epx.calc_epx_struc import BLOC_DONNEES
from Calc_epx.calc_epx_utils import recupere_structure, tolist, get_group_ma
from Utilitai.partition import MAIL_PY
import aster
import string
from Utilitai.Utmess import UTMESS
from Accas import _F
from Cata.cata import DEFI_GROUP


def export_modele(epx, MAILLAGE, MODELE, INTERFACES, mode_from_cara):
    """
        Traitement du concept MODELE et traduction pour EPX
        Traitement de l'objet INTERFACES
    """
    directive = 'GEOM'

    # Recuperer la structure sous le mot_cle facteur AFFE de AFFE_MODELE
    affe_modele = recupere_structure(MODELE, 'AFFE')
    affe_modele = tolist(affe_modele)

    # initialisation du dictionnaire qui contient les group_ma en fonction
    # de la modelisation
    epx_geom = {}

    MApyt = MAIL_PY()
    MApyt.FromAster(MAILLAGE)

    len_str_gr_med_max = 24
    gr_cr_noms_coupes = []
    veri_gr_from_cara = []
    ltyma = aster.getvectjev("&CATA.TM.NOMTM")
    modi_repere = {'COQUE': False}
    etat_init_cont = []
    for affe in affe_modele:
        modelisation = affe['MODELISATION']
        phenomene = affe['PHENOMENE']
        if phenomene != 'MECANIQUE':
            UTMESS('A', 'PLEXUS_24', valk=phenomene)
        if modelisation not in cata_modelisa.keys():
            UTMESS('A', 'PLEXUS_6', valk=modelisation)
        if not affe.has_key('GROUP_MA'):
            UTMESS('A', 'PLEXUS_3', valk=modelisation)
        if not cata_modelisa[modelisation]['ETAT_INIT']:
            etat_init_cont.append(modelisation)
        group_ma = get_group_ma(affe)
        if cata_modelisa[modelisation].has_key('MODI_REPERE'):
            type_modi = cata_modelisa[modelisation]['MODI_REPERE']
            modi_repere[type_modi] = True

        li_ty_ma_mode = cata_modelisa[modelisation]['MODE_EPX'].keys()

        nb_type_ma = len(li_ty_ma_mode)
        ltyma_maya = MAILLAGE.sdj.TYPMAIL.get()
        # vérification de la présence des différents type de mailles possibles
        # dans le groupe
        for gr in group_ma:
            lgeom = [False] * nb_type_ma
            l_ma_gr = MAILLAGE.sdj.GROUPEMA.get()[gr.ljust(24)]
            for m in l_ma_gr:
                typ_ok = False
                typ_m = ltyma[ltyma_maya[m - 1] - 1].strip()
                for i_typ, typma in enumerate(li_ty_ma_mode):
                    if typ_m == typma:
                        lgeom[i_typ] = True
                        typ_ok = True
                        break
                if not typ_ok:
                    UTMESS('F', 'PLEXUS_23', valk=(typ_m, gr, modelisation))
            if lgeom.count(True) == 0:
                UTMESS('F', 'PLEXUS_25', valk=(gr, modelisation))

            l_gr = len(gr)
            for i_typ, typma in enumerate(li_ty_ma_mode):
                if lgeom[i_typ] and lgeom.count(True) > 1:
                    ll = len(typma)
                    if l_gr <= len_str_gr_med_max - ll:
                        nom_gr = gr + typma
                    else:
                        nom_gr = gr[:len_str_gr_med_max - ll] + typma
                        num = 1
                        # traitement d'un cas vraiment peu probable mais pas
                        # impossible
                        while nom_gr in gr_cr_noms_coupes:
                            suffi = typma + "%s" % num
                            nom_gr = gr[
                                :len_str_gr_med_max - len(suffi)] + suffi
                            num += 1
                            if num == 20:
                                raise Exception(
                                    'Problème de noms de groupes de mailles')
                        gr_cr_noms_coupes.append(nom_gr)

                    if not MApyt.gma.has_key(string.rstrip(nom_gr)):
                        DEFI_GROUP(reuse=MAILLAGE, MAILLAGE=MAILLAGE,
                                   CREA_GROUP_MA=(
                                   _F(NOM=nom_gr, GROUP_MA=gr,
                                      TYPE_MAILLE=typma),
                                   ))
                elif lgeom[i_typ]:
                    nom_gr = gr
                else:
                    continue

                if len(cata_modelisa[modelisation]['MODE_EPX'][typma]) == 1:
                    mode_epx = cata_modelisa[
                        modelisation]['MODE_EPX'][typma][0]
                elif len(cata_modelisa[modelisation]['MODE_EPX'][typma]) == 0:
                    # elements a ne pas inclure dans GEOM
                    # face de 3D par exemple
                    continue
                else:
                    # cas ou la modelisation dépend du CARA_ELEM
                    mode_epx_dispo = cata_modelisa[
                        modelisation]['MODE_EPX'][typma]
                    if not gr in mode_from_cara.keys():
                        UTMESS('F', 'PLEXUS_26', valk=gr)
                    else:
                        veri_gr_from_cara.append(gr)
                    mode_epx = mode_from_cara[gr]
                    if mode_epx not in mode_epx_dispo:
                        raise Exception(
                            "Modélisation epx %s non permise pour la modélidation %s"
                            % (mode_epx, modelisation))

                if not epx_geom.has_key(mode_epx):
                    if cata_modelisa[modelisation].has_key('RESU_POIN'):
                        resu_poin = cata_modelisa[modelisation]['RESU_POIN']
                    else:
                        resu_poin = True
                    epx_geom[mode_epx] = {
                        'GROUP_MA': [],
                        'RESU_ELEM': cata_modelisa[modelisation]['RESU_ELEM'],
                        'RESU_POIN': resu_poin,
                    }
                epx_geom[mode_epx]['GROUP_MA'].append(nom_gr)

    # verif mode_from_cara
    for gr in mode_from_cara:
        if gr not in veri_gr_from_cara:
            UTMESS('F', 'PLEXUS_34', valk=gr)

    # liste comportant les modelisations definis dans le module GEOMETRIE
    # Ecriture sous format europlexus
    for mode_epx in epx_geom.keys():
        if mode_epx in mode_epx_fin:
            continue
        len_groups = len(epx_geom[mode_epx]['GROUP_MA'])
        if len_groups == 0:
            raise Exception('Erreur de programmation : liste de groupe vide')
        bloc_simple = BLOC_DONNEES(
            mode_epx, cara=epx_geom[mode_epx]['GROUP_MA'])
        epx[directive].add_bloc(bloc_simple)
    for mode_epx in mode_epx_fin:
        if mode_epx in epx_geom.keys():
            len_groups = len(epx_geom[mode_epx]['GROUP_MA'])
            if len_groups == 0:
                raise Exception('Erreur de programmation : liste de groupe vide')
            bloc_simple = BLOC_DONNEES(
                mode_epx, cara=epx_geom[mode_epx]['GROUP_MA'])
            epx[directive].add_bloc(bloc_simple)

    # INTERFACES
    listInterfaces = INTERFACES
    gmaInterfaces = []
    if listInterfaces:
        for interface in listInterfaces:
            Lgma1 = tolist(interface['GROUP_MA_1'])
            Lgma2 = tolist(interface['GROUP_MA_2'])
            gmaInterfaces.extend(Lgma1)
            gmaInterfaces.extend(Lgma2)
        bloc_simple = BLOC_DONNEES('CL3L', cara=gmaInterfaces)
        epx[directive].add_bloc(bloc_simple)

    return epx, epx_geom, gmaInterfaces, modi_repere, etat_init_cont
