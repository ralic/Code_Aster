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
Traitement des chargements et des relations cinématiques
"""
import aster
from Calc_epx.calc_epx_utils import recupere_structure, tolist, get_group_ma
from Calc_epx.calc_epx_cata import cata_charge
from Utilitai.Utmess import UTMESS
#-----------------------------------------------------------------------
def ecri_rela_cine(cabl_precont, MAILLAGE):
    """
    Ecriture des relations cinematiques contenues dans le concept cabl_precont
    """
    l_cara = []
    dic_ddl_impo = {}
    for i, cle in enumerate(cata_charge['DDL_IMPO']['ASTER']):
        dic_ddl_impo[cle] = cata_charge['DDL_IMPO']['EPX'][i]

    nomnoe = aster.getvectjev(MAILLAGE.nom.ljust(8)+".NOMNOE")
    dic_nomnoe = {}
    for i, noeu in enumerate(nomnoe):
        dic_nomnoe[noeu] = i+1

    nom_cabl_pr = cabl_precont.nom.ljust(8)
    nb_rela = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLNR')[0]

    typ_coef = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLTC')[0]
    if typ_coef[:4] != 'REEL':
        raise Exception("Coefficients non reels")

    vec_sm = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLBE')
    vec_nb_coef = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLNT')
    pointeur = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLPO')
    vec_coef = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLCO')
    vec_nomnoe = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLNO')
    vec_nomddl = aster.getvectjev(nom_cabl_pr+'.LIRELA    .RLDD')

    for i_rela in range(nb_rela):
        # le second membre doit etre nul
        if vec_sm[i_rela] != 0.E0:
            raise Exception("Second memnbre non nul")
        # nb de coefficient de la relation
        nb_coef = vec_nb_coef[i_rela]
        nb_coef_temp = nb_coef
        # position du dernier terme de la realtion
        adr = pointeur[i_rela]
        for i_coef in range(nb_coef):
            if vec_coef[adr-nb_coef+i_coef] == 0.E0:
                nb_coef_temp -= 1
        l_cara.append('1 %s'%nb_coef_temp)
        for i_coef in range(nb_coef):
            coeff = vec_coef[adr-nb_coef+i_coef]
            if coeff != 0.E0:
                nomnoe_coef = vec_nomnoe[adr-nb_coef+i_coef]
                nomddl_coef = vec_nomddl[adr-nb_coef+i_coef]
                l_cara.append(' '*4 +str(coeff)+' '
                              +dic_ddl_impo[nomddl_coef.rstrip()]
                              +' '+str(dic_nomnoe[nomnoe_coef])+' 0')
    return nb_rela, l_cara
#-----------------------------------------------------------------------
def export_charge(epx, EXCIT, MAILLAGE):
    """
        Analyse et traduction pour EPX des données de chargement
        contenues dans l'objet EXCIT.
    """

    from Calc_epx.calc_epx_struc import FONCTION, BLOC_DONNEES

    excit_list = EXCIT.List_F()

    for excit in excit_list:
        concept_charge = excit['CHARGE']
        if excit.has_key('FONC_MULT'):
            fonction = excit['FONC_MULT']
        else:
            fonction = None

        list_char = recupere_structure(concept_charge)
        list_char = list_char.keys()
        for char in list_char:
            if not cata_charge.has_key(char):
                UTMESS('F', 'PLEXUS_19', char)
            if cata_charge[char] == False:
                continue
            char_list = recupere_structure(concept_charge, char)
            char_list = tolist(char_list)
            directive = cata_charge[char]['DIRECTIVE']
            mot_cle_epx = cata_charge[char]['MOT_CLE_EPX']

            if len(mot_cle_epx) > 1:
                if len(mot_cle_epx) > 2:
                    raise Exception(
                "La liste MOT_CLE_EPX ne peut pas avoir plus de 2 éléments")
                mot_cle = mot_cle_epx[0]
                if not epx[directive].get_mcfact(mot_cle):
                    objet = epx[directive].add_mcfact(mot_cle)
                else:
                    UTMESS('F', 'PLEXUS_29', valk=(char))
            else:
                objet = epx[directive]

            nom_fonc = False
            if cata_charge[char].has_key('FONC_MULT'):
                nom_fonc = cata_charge[char]['FONC_MULT']
                nom_fonc_aster = fonction.get_name()
            vale_impo = False
            cle_aster = cata_charge[char]['ASTER']
            cle_epx = cata_charge[char]['EPX']
            entite = []
            if cata_charge[char].has_key('ENTITE'):
                entite = cata_charge[char]['ENTITE']
            if cata_charge[char].has_key('VALE_IMPO'):
                vale_impo = cata_charge[char]['VALE_IMPO']
            coef_mult = False
            if cata_charge[char].has_key('COEF_MULT'):
                coef_mult = cata_charge[char]['COEF_MULT']
            mot_cle_verif = []
            if cata_charge[char].has_key('MOT_CLE_VERIF'):
                mot_cle_verif = cata_charge[char]['MOT_CLE_VERIF']
                vale_verif = cata_charge[char]['VALE_VERIF']
            if nom_fonc and fonction is None:
                UTMESS('F', 'PLEXUS_7', valk=char)
            elif nom_fonc:
                (temps, valeurs) = fonction.Valeurs()
                objet.fonction = FONCTION(nom_fonc, temps, valeurs,
                                          nom_aster=nom_fonc_aster)
            for ch in char_list:
                # EC pour l'instant on a que des cas a une valeur
                # li_vale = []
                info_epx = ''
                l_group = None
                l_cara = []
                l_vale = []
                for cle in ch.keys():
                    if cle in mot_cle_verif:
                        ind = mot_cle_verif.index(cle)
                        if ch[cle] != vale_verif[ind]:
                            UTMESS('F', 'PLEXUS_30', valk=(cle, char, ch[cle],
                                                           vale_verif[ind]))
                        continue
                    if cle in entite:
                        l_group = get_group_ma(ch, cle)
                        continue
                    if not cle in cle_aster:
                        UTMESS('F', 'PLEXUS_27', valk=(cle, char))
                    if char == 'RELA_CINE_BP':
                        cable_bp = ch[cle]
                        info_epx, l_cara = ecri_rela_cine(cable_bp, MAILLAGE,)
                        l_vale = ['']*len(l_cara)
                        vale_tmp = ''
                    else:
                        vale_tmp = ch[cle]
                    ind = cle_aster.index(cle)
                    vale = ''
                    if vale_impo is not False:
                        if vale_tmp != vale_impo:
                            UTMESS('F', 'PLEXUS_28', valk=(cle, char),
                                           valr=(vale_tmp, vale_impo))
                    else:
                        vale = vale_tmp
                        if coef_mult is not False:
                            vale = coef_mult*vale
                    if cle_epx is not None:
                        info_epx += cle_epx[ind]
                bloc_donnees = BLOC_DONNEES(mot_cle_epx[-1], l_group=l_group,
                                            cle=info_epx, val_cle=vale,
                                            cara=l_cara, vale=l_vale)
                objet.add_bloc(bloc_donnees)
