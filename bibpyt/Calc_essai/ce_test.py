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
# person_in_charge: albert.alarcon at edf.fr

# Fichier comprenant une procédure de test de CALC_ESSAI avec les différentes
# options de calcul.

import aster
from Accas import _F, ASSD

from ce_calcul_expansion import CalcEssaiExpansion
from ce_calcul_identification import CalcEssaiIdentification, CalculInverse
from Utilitai.Utmess import UTMESS
from numpy import take


def TestCalcEssai(macro,
                  mess,
                  out_identification,
                  out_modifstru,
                  objects,
                  EXPANSION,
                  IDENTIFICATION,
                  MODIFSTRUCT,
                  GROUP_NO_CAPTEURS,
                  GROUP_NO_EXTERIEUR,
                  ):

    #
    # Transformation des objets Aster en classes python #
    # Test de ces classes                  #
    #

    # classe CalcEssaiObject
    # La classe MO recupere tous les concepts aster disponibles et
    # les range dans des classes python telles que Resultat, InterSpectre
    # Modele, CaraElem...

    #
    # 1) Test des fonctions de correlation #
    #
    if EXPANSION:
        calc_essai = CalcEssaiExpansion(macro, mess, objects)
        norm_num = None
        norm_exp = None

        # Transformation des objets Aster en classes Python
        resu_num = objects.resultats[EXPANSION['CALCUL'].nom]
        resu_exp = objects.resultats[EXPANSION['MESURE'].nom]

        # Set default indices for the modes
        if EXPANSION['NUME_MODE_CALCUL'] != 0:
            nume_mode_calcul = list(EXPANSION['NUME_MODE_CALCUL'])
        else:
            nume_mode_calcul = None
        if EXPANSION['NUME_MODE_MESURE'] != 0:
            nume_mode_mesure = list(EXPANSION['NUME_MODE_MESURE'])
        else:
            nume_mode_mesure = None

        parametres = {}
        if EXPANSION['RESOLUTION'] == 'LU':
            parametres['METHODE'] = 'LU'
        elif EXPANSION['RESOLUTION'] == 'SVD':
            parametres['METHODE'] = 'SVD'
            parametres['EPS'] = EXPANSION['EPS']

        calc_essai.setup(resu_num, nume_mode_calcul, resu_exp,
                         nume_mode_mesure, parametres)
        # Les concepts resultats sont srockes sous le nom RESU_+type de resu
        suffix = ['_NX', '_EX', '_ET', '_RD']
        calc_essai.calc_proj_resu(suffix, "RESU")

        resu_et = objects.resultats["RESU_ET"]
        resu_nx = objects.resultats["RESU_NX"]
        resu_rd = objects.resultats["RESU_RD"]

        calc_essai.calc_mac_mode(resu_et, resu_nx, resu_num.mass)

    #
    # 4) Test des fonctions de l'onglet identification #
    #
    if IDENTIFICATION:
        calcturb = CalcEssaiIdentification(objects, mess)

        inter_spec_name = IDENTIFICATION['INTE_SPEC'].nom
        obs_name = IDENTIFICATION['OBSERVABILITE'].nom
        com_name = IDENTIFICATION['COMMANDABILITE'].nom
        res_base = IDENTIFICATION['BASE'].nom

        calcturb.set_interspectre(objects.get_inter_spec(inter_spec_name))
        calcturb.set_observabilite(objects.get_mode_meca(obs_name))
        calcturb.set_commandabilite(objects.get_mode_meca(com_name))
        calcturb.set_base(objects.get_mode_meca(res_base))

        calcturb.set_alpha(IDENTIFICATION['ALPHA'])
        calcturb.set_epsilon(IDENTIFICATION['EPS'])
        calcturb.set_mcoeff(0.0)

        calcturb.calculate_force()

        calcturb.Sff.make_inte_spec(titre="Resultat identification : efforts",
                                    paras_out=out_identification)
        calcturb.Syy.make_inte_spec(titre="Resultat identification : mesure",
                                    paras_out=out_identification)
        calcturb.Syy_S.make_inte_spec(
            titre="Resultat identification : synthese",
            paras_out=out_identification)

    #
    # 5) Test des fonctions de l'onglet modif struct #
    #
    if MODIFSTRUCT:
        lance_modif_struct_calcul(macro, objects,
                                  MODIFSTRUCT,
                                  GROUP_NO_CAPTEURS,
                                  GROUP_NO_EXTERIEUR,
                                  out_modifstru)


class MessageBox:

    """!Classe qui permet d'ecrire dans un .mess separe"""

    def __init__(self, unite):
        self.unite = unite  # unite d'ecriture
        self.mess_file = open('fort.' + str(unite), 'w')

    def disp_mess(self, new_mess):
        """!Ecriture des messages dans le fichier sortie
        s'il existe et dans la fenetre de message"""
        self.mess_file.writelines(new_mess + '\n')

    def close_file(self):
        """ Ferme le fichier de message a la fin de l'utilisation"""
        self.mess_file.close()


def to_dict_lst(groups):
    """Transforme la liste renvoyée par le superviseur
    en une liste de dictionaires. Il est important que
    les degrés de liberté soient dans une liste."""
    res_lst = []
    for grp in groups:
        rdict = {"NOM": grp["GROUP_NO"],
                 "NOM_CMP": list(grp["NOM_CMP"])}
        res_lst.append(rdict)
    return res_lst


def lance_modif_struct_calcul(macro, ce_objects,
                              MODIFSTRUCT,
                              GROUP_NO_CAPTEURS,
                              GROUP_NO_EXTERIEUR,
                              out_modifstru):
    """Démarre le calcul CALC_ESSAI sur la structure modifiée.

       :param macro: la macro étape CALC_ESSAI.

       :param ce_objects: les objects, utilisés par le module CALC_ESSAI,
                              présents dans la mémoire JEVEUX au moment
                              de la macro étape.

       :param MODIFSTRUCT: la macro étape permettant le calcul de la structure
                           modifiée depuis le fichier de commande Aster.

       :param CAPTEURS: dictionaire (ou FACT) contenant les choix
                        de groupes de noeuds et leurs degrés de liberté
                        pour les capteurs.

       :param EXTERIEUR: dictionaire (ou FACT) contenant les choix
                          de groupes de noeuds et leurs degrés de liberté
                          pour les ddl exterieurs.

       :param out_modifstru: dictionaire (ou FACT) utilisé pour les résultats."""

    from Accas import _F
    from Calc_essai.ce_calcul_modifstruct import CalcEssaiModifStruct
    modif_struct = CalcEssaiModifStruct(macro, ce_objects,
                                        ce_objects.mess, out_modifstru)

    modif_struct.find_experimental_result_from(MODIFSTRUCT["MESURE"].nom)
    modif_struct.find_support_modele_from(MODIFSTRUCT["MODELE_SUP"].nom)
    modif_struct.set_stiffness_matrix(MODIFSTRUCT["MATR_RIGI"])
    modif_struct.set_method_name(MODIFSTRUCT["RESOLUTION"])

    modif_struct.set_sensor_groups(to_dict_lst(GROUP_NO_CAPTEURS))
    modif_struct.set_interface_groups(to_dict_lst(GROUP_NO_EXTERIEUR))

    modif_struct.set_modes_ide(MODIFSTRUCT["NUME_MODE_MESU"])
    modif_struct.set_modes_expansion(MODIFSTRUCT["NUME_MODE_CALCUL"])

    modif_struct.set_sumail_name("SUMAIL")   # nom super-maille par defaut
    modif_struct.find_modele_modif_from(MODIFSTRUCT["MODELE_MODIF"].nom)
    modif_struct.find_maillage_modif_from(MODIFSTRUCT["MODELE_MODIF"].nom)

    modif_struct.get_modele_support()
    modif_struct.calc_base_proj()

    modif_struct.set_param_condens(
        {'METHODE': 'SVD', 'EPS': 1.0E-5, 'REGUL': 'NON'})
    modif_struct.creation_modele_couple()

    mode_simult = 1  # methode utilisee : MODE_ITER_SIMULT
    calc_freq = _F(OPTION='PLUS_PETITE',
                   NMAX_FREQ=20,
                   SEUIL_FREQ=1.E-4,)
    calc_freq['SEUIL_FREQ'] = 1e-4

    modif_struct.calc_modes_modele_couple(mode_simult, calc_freq)


def get_ddl_extract(nom_resu):
    """ resu est le resultat de l'operateur OBSERVATION. On va y chercher les
    ddl filtres"""
    jdc = CONTEXT.get_current_step().jdc

    for etape in jdc.etapes:
        try:
            nom_etape = etape.sdnom
        except AttributeError:
            pass
        if nom_etape == nom_resu:
            grp_no_ma = etape.valeur['FILTRE']
            if type(grp_no_ma) != list and type(grp_no_ma) != tuple:
                grp_no_ma = [grp_no_ma]
            else:
                pass
            for dico in grp_no_ma:
                for ind1, ind2 in dico.items():
                    if type(ind2) != list and type(ind2) != tuple:
                        dico[ind1] = [ind2]

            return grp_no_ma
