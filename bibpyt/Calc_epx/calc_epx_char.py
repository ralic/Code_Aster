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
Traitement des chargements et des relations cinématiques
"""
import aster
from Calc_epx.calc_epx_utils import recupere_structure, tolist, get_group_ma
from Utilitai.Utmess import UTMESS
#-----------------------------------------------------------------------

def export_charge(epx, EXCIT, MAILLAGE):
    """
        Analyse et traduction pour EPX des données de chargement
        contenues dans l'objet EXCIT.
    """

    from Calc_epx.calc_epx_struc import FONCTION, BLOC_DONNEES
    from Calc_epx.calc_epx_cata import cata_charge, cata_liais

    excit_list = EXCIT.List_F()
    ifonc = epx['FONC'].len_mcs()

    for excit in excit_list:
        concept_charge = excit['CHARGE']
        if excit.has_key('FONC_MULT'):
            fonction = excit['FONC_MULT']
            nom_fonc_aster = fonction.get_name()
        else:
            fonction = None
        l_char_fact = False
        l_link_fonc = False

        list_char = recupere_structure(concept_charge)
        list_char = list_char.keys()
        # mots-clé de AFFE_CHAR_MECA
        for char in list_char:
            if char in ['INFO', 'MODELE']:
                continue
            elif char in cata_charge.keys():
                directive = 'CHARGE'
                cata = cata_charge
                l_char_fact = True
                type_char = recu_val('o', cata, char, 'TYPE_CHAR', None)
                if type_char[:4] == 'FACT':
                    l_char_fact = True
                    if fonction is None:
                        UTMESS('F', 'PLEXUS_7', valk=char)
                elif type_char[:4] == 'CONS':
                    raise Exception("""Type de charge pas encore testé
                            des aménagements sont certainement à faire.
                            Cette exeption peut être supprimée suite à cela.
                                    """)
                    if fonction:
                        UTMESS('F', 'PLEXUS_5', valk=char)
                else:
                    raise Exception("""Type de charge EPX non pris en
                                       compte : %s""" % type_char)
                if not epx[directive].get_mcfact(type_char):
                    objet = epx[directive].add_mcfact(type_char)
                else:
                    objet = epx[directive].get_mcfact(type_char)
            elif char in cata_liais.keys():
                directive = 'LINK'
                cata = cata_liais
                objet = epx[directive]
            else:
                UTMESS('F', 'PLEXUS_19', char)

            char_list = recupere_structure(concept_charge, char)
            char_list = tolist(char_list)

            mot_cle_epx = recu_val('o', cata, char, 'MOT_CLE_EPX', None)
            if len(mot_cle_epx) > 1:
                # choix du mot-clé :
                if char == 'DDL_IMPO':
                    if fonction:
                        mot_cle_epx = mot_cle_epx[1]
                    else:
                        mot_cle_epx = mot_cle_epx[0]
                else:
                    raise Exception('cas non traité')
            else:
                mot_cle_epx = mot_cle_epx[0]

            if directive == 'LINK':
                l_fonc = False
                if recu_val('o', cata, char, 'FONC_MULT', mot_cle_epx):
                    l_link_fonc = True
                    l_fonc = True
                    if fonction is None:
                        UTMESS('F', 'PLEXUS_7', valk=char)

            cle_aster = recu_val('o', cata, char, 'ASTER', mot_cle_epx)
            cle_epx = recu_val('o', cata, char, 'EPX', mot_cle_epx)
            if cle_epx is False and len(cle_aster) != 1:
                raise Exception("""Préciser EPX dans %s car la liste ASTER
                                possède plusieurs éléments.
                                """)
            entite = recu_val('f', cata, char, 'ENTITE', mot_cle_epx)
            if not entite:
                entite = []
            vale_impo = recu_val('f', cata, char, 'VALE_IMPO', mot_cle_epx)
            coef_mult = recu_val('f', cata, char, 'COEF_MULT', mot_cle_epx)
            mot_cle_verif = recu_val(
                'f', cata, char, 'MOT_CLE_VERIF', mot_cle_epx)
            if not mot_cle_verif:
                mot_cle_verif = []
                vale_verif = False
            else:
                vale_verif = recu_val(
                    'o', cata, char, 'VALE_VERIF', mot_cle_epx)
            nb_cle_max = recu_val('f', cata, char, 'NB_CLE_MAX', mot_cle_epx)
            if not nb_cle_max:
                nb_cle_max = 1

            # occurrences des mots-clé facteurs
            for ch in char_list:
                # EC pour l'instant on a que des cas a une valeur
                # li_vale = []
                info_epx = ''
                l_group = None
                l_cara = []
                l_vale = []
                nb_cle = 0
                for cle in ch.keys():
                    if cle in mot_cle_verif:
                        ind = mot_cle_verif.index(cle)
                        if ch[cle] not in  tolist(vale_verif[ind]):
                            UTMESS('F', 'PLEXUS_30', valk=(cle, char, ch[cle],
                                                ' '.join(tolist(vale_verif[ind]))))
                        continue
                    if cle in entite:
                        l_group = get_group_ma(ch, cle)
                        continue
                    if not cle in cle_aster:
                        UTMESS('F', 'PLEXUS_27', valk=(cle, char))
                    if char == 'RELA_CINE_BP':
                        if cle != cle_aster[0]:
                            raise Exception('Erreur avec RELA_CINE_BP')
                        cable_bp = ch[cle]
                        if ch.has_key('TYPE_EPX'):
                            type_epx = ch['TYPE_EPX']
                        else:
                            type_epx = 'ADHE'
#                        info_epx, l_cara = ecri_rela_cine(cable_bp, MAILLAGE,)
#                        l_vale = [''] * len(l_cara)
                        bloc_donnees = ecri_rela_cine(cable_bp, mot_cle_epx, type_epx)
                    else:
                        vale_tmp = ch[cle]
                    if nb_cle > nb_cle_max:
                        UTMESS(
                            'F', 'PLEXUS_29', valk=(char, ','.join(cle_aster)),
                            vali=(nb_cle, nb_cle_max))
                    ind = cle_aster.index(cle)
                    vale = ''
                    if vale_impo is not False:
                        if vale_tmp != vale_impo:
                            UTMESS('F', 'PLEXUS_28', valk=(cle, char),
                                   valr=(vale_tmp, vale_impo))
                    else:
                        vale = vale_tmp
                        if coef_mult:
                            vale = coef_mult * vale
                    if cle_epx:
                        info_epx += cle_epx[ind]
                    nb_cle += 1
                if directive == 'LINK' and l_fonc:
                    l_cara.append('FONC')
                    l_vale.append(ifonc + 1)
                if char != 'RELA_CINE_BP':
                    bloc_donnees = BLOC_DONNEES(mot_cle_epx, l_group=l_group,
                                                cle=info_epx, val_cle=vale,
                                                cara=l_cara, vale=l_vale)
                objet.add_bloc(bloc_donnees)
        if l_char_fact:
            # ajout de la fonction
            (temps, valeurs) = fonction.Valeurs()
            bloc_fonc = FONCTION('TABLE', temps, valeurs,
                                 nom_aster=nom_fonc_aster)
            objet.add_bloc(bloc_fonc)
        if l_link_fonc:
            ifonc += 1
            (temps, valeurs) = fonction.Valeurs()
            bloc_fonc = FONCTION('%s TABL' % ifonc, temps, valeurs,
                                 nom_aster=nom_fonc_aster)
            epx['FONC'].add_bloc(bloc_fonc)
#-----------------------------------------------------------------------


def recu_val(ch, cata, char, key, mot_cle_epx):
    """
        Récupère la valeur dans le cata.
        ch = 'o' si la clé key est forcément présente
    """
    if cata[char].has_key(key):
        if type(cata[char][key]) is dict:
            if mot_cle_epx is None:
                raise Exception("""Cas non prévu : pas de dictionnaire
                pour la clé %s
                """ % key)
            val = cata[char][key][mot_cle_epx]
            if val is None:
                val = False
        else:
            val = cata[char][key]
    else:
        if ch == 'o':
            raise Exception("""Mot-clé %s manquant dans cata_liais ou
            cata_charge pour la clé %s
            """ % (key, char))
        else:
            val = False
    return val
#-----------------------------------------------------------------------
def ecri_rela_cine(cabl_precont, cle_epx, type_epx):
    """
    Recherche des mots-clés de DEFI_CABLE_BP pour traduction en EPX (LCAB)
    """
    from Calc_epx.calc_epx_struc import BLOC_DONNEES, BLOC_DONNEES_SUP
    
    defi_cable_bp = recupere_structure(cabl_precont)
    # BETON COQUE
    gr_ma_bet = defi_cable_bp['GROUP_MA_BETON']
    bloc_betc = BLOC_DONNEES('BETC', l_group=gr_ma_bet)
    # CABLES
    defi_cable = defi_cable_bp['DEFI_CABLE']
    gr_ma_cab =[]
    for insta in defi_cable:
        gr_ma_cab.append(insta['GROUP_MA'])
    bloc_cabl = BLOC_DONNEES('CABL', l_group=gr_ma_cab)
    
    bloc_lcab = BLOC_DONNEES_SUP(cle_epx,[bloc_betc, bloc_cabl], cle=type_epx)
    
    return bloc_lcab
#-----------------------------------------------------------------------
def ecri_rela_cine_old(cabl_precont, MAILLAGE):
    """
    Ecriture des relations cinematiques contenues dans le concept cabl_precont
    """
    from Calc_epx.calc_epx_cata import cata_compo

    l_cara = []
    dic_ddl_impo = cata_compo['DEPL']

    nomnoe = aster.getvectjev(MAILLAGE.nom.ljust(8) + ".NOMNOE")
    dic_nomnoe = {}
    for i, noeu in enumerate(nomnoe):
        dic_nomnoe[noeu] = i + 1

    nom_cabl_pr = cabl_precont.nom.ljust(8)
    nb_rela = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLNR')[0]

    typ_coef = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLTC')[0]
    if typ_coef[:4] != 'REEL':
        raise Exception("Coefficients non reels")

    vec_sm = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLBE')
    vec_nb_coef = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLNT')
    pointeur = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLPO')
    vec_coef = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLCO')
    vec_nomnoe = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLNO')
    vec_nomddl = aster.getvectjev(nom_cabl_pr + '.LIRELA    .RLDD')

    for i_rela in range(nb_rela):
         #le second membre doit etre nul
        if vec_sm[i_rela] != 0.E0:
            raise Exception("Second memnbre non nul")
         #nb de coefficient de la relation
        nb_coef = vec_nb_coef[i_rela]
        nb_coef_temp = nb_coef
         #position du dernier terme de la realtion
        adr = pointeur[i_rela]
        for i_coef in range(nb_coef):
            if vec_coef[adr - nb_coef + i_coef] == 0.E0:
                nb_coef_temp -= 1
        l_cara.append('1 %s' % nb_coef_temp)
        for i_coef in range(nb_coef):
            coeff = vec_coef[adr - nb_coef + i_coef]
            if coeff != 0.E0:
                nomnoe_coef = vec_nomnoe[adr - nb_coef + i_coef]
                nomddl_coef = vec_nomddl[adr - nb_coef + i_coef]
                l_cara.append(' ' * 4 + str(coeff) + ' '
                              + str(dic_ddl_impo[nomddl_coef.rstrip()])
                              + ' ' + str(dic_nomnoe[nomnoe_coef]) + ' 0')
    return nb_rela, l_cara
#-----------------------------------------------------------------------
