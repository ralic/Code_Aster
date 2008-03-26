#@ MODIF meidee_test Meidee  DATE 26/03/2008   AUTEUR BODEL C.BODEL 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

# Fichier comprenant une procédure de test de Meidee avec les différentes
# options de calcul.

import aster
from Accas import _F, ASSD

from meidee_calcul_fludela import MeideeFludela
from meidee_calcul_correlation import MeideeCorrelation
from meidee_calcul_turbulent import CalculTurbulent, CalculInverse
from Numeric import take

def TestMeidee(macro,
               mess,
               out_fludela,
               out_turbulent,
               out_modifstru,
               objects,
               EXPANSION,
               FLUIDE_ELASTIQUE,
               TURBULENT,
               MODIFSTRUCT,
               GROUP_NO_CAPTEURS,
               GROUP_NO_EXTERIEUR,               
               ):

    #####################################################
    # Transformation des objets Aster en classes python #
    #              Test de ces classes                  #
    #####################################################

    # classe MeideeObjects
    # La classe MO recupere tous les concepts aster disponibles et
    # les range dans des classes python telles que Resultat, InterSpectre
    # Modele, CaraElem...
    
    
    ########################################
    # 1) Test des fonctions de correlation #
    ########################################
    if EXPANSION:
        meidee = MeideeCorrelation(macro,mess,objects)
        norm_num = None
        norm_exp = None

        # Transformation des objets Aster en classes Python
        resu_num = objects.resultats[EXPANSION['CALCUL'].nom]
        resu_exp = objects.resultats[EXPANSION['MESURE'].nom]
       
        # Set default indices for the modes
        if EXPANSION['NUME_MODE_CALCUL'] != 0:
            nume_mode_calcul = [ind-1 for ind in
                                list(EXPANSION['NUME_MODE_CALCUL'])]
        else:
            nume_mode_calcul = [0, 2, 3, 4, 5, 6]
        if EXPANSION['NUME_MODE_MESURE'] != 0:
            nume_mode_mesure = [ind-1 for ind in
                                list(EXPANSION['NUME_MODE_MESURE'])]
        else:
            nume_mode_mesure = [0, 2, 3, 4, 5]

        meidee.Setup(resu_num, resu_exp, norm_num, norm_exp)
        # Lancement de la macro MACRO_MEIDEE_PROJ
        # Les concepts resultats sont srockes sous le nom RESU_+type de resu
        # On realise l'expansion avec les quatre premiers modes de la base numerique
        # sur les trois premiers modes exp
        ind_mod_num = []
        ind_mod_exp = []


        # Selection des modes à étendre et des modes de la base d'expansion
        ind_mod_tmp  = take(resu_num.get_cara_mod(), nume_mode_calcul)[:,0].tolist()
        for j in ind_mod_tmp:
            ind_mod_num.append(int(j))
        freq_mod_num = take(resu_num.get_cara_mod(),nume_mode_calcul)[:,0].tolist()
        num_modes = (ind_mod_num,freq_mod_num)
        ind_mod_tmp  = take(resu_exp.get_cara_mod(),nume_mode_mesure)[:,0].tolist()
        for j in ind_mod_tmp:
            ind_mod_exp.append(int(j))
        freq_mod_exp  = take(resu_exp.get_cara_mod(),nume_mode_mesure)[:3,1].tolist()
        exp_modes = (ind_mod_exp,freq_mod_exp)
        
        meidee.calc_proj_resu(num_modes,exp_modes,meidee.resu_num, meidee.resu_exp,
                              "RESU")

        objects.recup_objects()
        resu_et  = objects.resultats["RESU_ET"]

        # Test des MAC_MODE (utilise la fonction calc_mac_mode)
        # NB : pour l'instant MAC_MODE ne marche pas avec les modes reduits
        tests = [("phi_et", ),("phi_et", "phi_num" ),("phi_exp", ),
                 ##("phi_exp", "phi_num_red"),
                 ("phi_num",)]
                 ##("phi_num_red",)

        for ind in range(len(tests)):
            meidee.get_mac(tests[ind],resu_num.obj,resu_exp.obj,norm_num,norm_exp)

        # Test des IMPR_RESU (format GMSH) on affiche tous les resu de MACRO_MEIDEE_PROJ
        meidee.prep_fichier(1,1,1,1)


    #############################################
    # 2) Test des fonctions de l'onglet fludela #
    #############################################
    if FLUIDE_ELASTIQUE:
        fludela = MeideeFludela(mess, out_fludela)

        # Transformation des objets Aster en classes Python
        resu_exp1 = objects.resultats[FLUIDE_ELASTIQUE['MESURE1'].nom]
        resu_exp2 = objects.resultats[FLUIDE_ELASTIQUE['MESURE2'].nom]
        resu_exp3 = objects.resultats[FLUIDE_ELASTIQUE['MESURE3'].nom] 

        fludela.set_results(resu_exp1, resu_exp2, resu_exp3)
        fludela.set_param_phy(rho_int=0, rho_ext=1000, diam_int=0.018, diam_ext=0.02)
        fludela.methode_globale = 'Granger'
        if FLUIDE_ELASTIQUE['RESU_EXPANSION'] == 'OUI':
            fludela.res_longeq = resu_et
        else:
            fludela.res_longeq = objects.resultats[FLUIDE_ELASTIQUE['BASE'].nom]
        fludela.pairs =  [([0], [0], [0]), ([1], [1], [1]), ([2], [2], [2])]
        list_vit = ['10']
        for vit in list_vit:
            fludela.set_speed(vit) # on fait le test pour une seule vitesse pour l'instant
            fludela.compute()

        # Ecriture des fichiers dans une table de resultats
        name = fludela.calc['U']
        #fabrique une copie du tableau de resultats correspondant a 1 vitesse (fludela.calc)
        data_tmp = fludela.save_data()
        data = {}
        keys1 = ["Ma rep dim  mod","Ma rep adim lin","Ka vit dim  mod","Ka vit adim lin"]
        keys2 = ['mass_ajou_dim_mod','mass_ajou_adim_lin','raid_ajou_dim_mod','raid_ajou_adim_lin']
        for col in range(4):
            key = keys1[col]
            obj = keys2[col]
            data[key] = data_tmp[obj]
        fludela.saved_data[name] = data
        fludela.save_to_file(name, data)
        fludela.save_to_table(fludela.saved_data,"Résultats Meidee fluide-elastique")

    ###############################################
    # 3) Test des fonctions de l'onglet turbulent #
    ###############################################
    if TURBULENT:
        calcturb = CalculTurbulent(objects, mess)

        inter_spec_name = TURBULENT['INTE_SPEC'].nom
        obs_name = TURBULENT['OBSERVABILITE'].nom
        com_name = TURBULENT['COMMANDABILITE'].nom
        res_base = TURBULENT['BASE'].nom

        calcturb.set_interspectre(objects.get_inter_spec(inter_spec_name))
        calcturb.set_observabilite(objects.get_resu(obs_name))
        calcturb.set_commandabilite(objects.get_resu(com_name))
        
        ddl_actifs_obs = get_ddl_extract(calcturb.res_obs.nom)
        ddl_actifs_com = get_ddl_extract(calcturb.res_com.nom)
        calcturb.set_extraction_ddl_obs(ddl_actifs_obs)
        calcturb.set_extraction_ddl_com(ddl_actifs_com)
        
        calcturb.set_base(objects.get_resu(res_base))
        
        calcturb.set_alpha(TURBULENT['ALPHA'])
        calcturb.set_epsilon(TURBULENT['EPS'])
        calcturb.set_mcoeff(0.0)

        calcturb.calculate_force()

        calcturb.Sff.make_inte_spec(titre="Résultat Meidee turbulent : efforts",
                                    paras_out = out_turbulent)
        calcturb.Syy.make_inte_spec(titre="Résultat Meidee turbulent : mesure",
                                      paras_out = out_turbulent)
        calcturb.Syy_S.make_inte_spec(titre="Résultat Meidee turbulent : synthèse",
                                      paras_out = out_turbulent)

    ##################################################
    # 4) Test des fonctions de l'onglet modif struct #
    ##################################################
    if MODIFSTRUCT:
       lance_modif_struct_calcul(macro, objects,
                                 MODIFSTRUCT,
                                 GROUP_NO_CAPTEURS,
                                 GROUP_NO_EXTERIEUR,
                                 out_modifstru)

def to_dict_lst(groups):
    """Transforme la liste renvoyée par le superviseur
    en une liste de dictionaires. Il est important que
    les degrés de liberté soient dans une liste."""
    res_lst = []
    for grp in groups:
        rdict = {"GROUP_NO" : grp["GROUP_NO"],
                 "NOM_CMP" : list(grp["NOM_CMP"])}
        res_lst.append(rdict)
    return res_lst

def lance_modif_struct_calcul(macro, meidee_objects,
                              MODIFSTRUCT,
                              GROUP_NO_CAPTEURS,
                              GROUP_NO_EXTERIEUR,
                              out_modifstru):
    """Démarre le calcul Meidee sur la structure modifiée.
       
       :param macro: la macro étape MACRO_VISU_MEIDEE.

       :param meidee_objects: les objects, utilisés par le module Meidee,
                              présents dans la mémoire JEVEUX au moment
                              de la macro étape MACRO_VISU_MEIDEE.

       :param MODIFSTRUCT: la macro étape permettant le calcul de la structure
                           modifiée depuis le fichier de commande Aster. 

       :param CAPTEURS: dictionaire (ou FACT) contenant les choix
                        de groupes de noeuds et leurs degrés de liberté
                        pour les capteurs.

       :param MS_EXTERNE: dictionaire (ou FACT) contenant les choix
                          de groupes de noeuds et leurs degrés de liberté
                          pour la structure externe.
                          
       :param RESMODIF: dictionaire (ou FACT) utilisé pour les résultats."""
    from Accas import _F       
    from Meidee.meidee_calcul_modifstruct import ModifStruct
    modif_struct = ModifStruct(macro, meidee_objects,
                               meidee_objects.mess, out_modifstru)

    modif_struct.find_experimental_result_from(MODIFSTRUCT["MESURE"].nom)
    modif_struct.find_support_modele_from(MODIFSTRUCT["MODELE_SUP"].nom)
    modif_struct.set_stiffness_matrix(MODIFSTRUCT["MATR_RIGI"])
    modif_struct.set_method_name(MODIFSTRUCT["RESOLUTION"])
    
    modif_struct.set_captor_groups(to_dict_lst(GROUP_NO_CAPTEURS))
    modif_struct.set_ms_externe_groups(to_dict_lst(GROUP_NO_EXTERIEUR))

    modif_struct.set_modes_ide(MODIFSTRUCT["NUME_MODE_MESU"])
    modif_struct.set_modes_expansion(MODIFSTRUCT["NUME_MODE_CALCUL"])

    modif_struct.find_maillage_couple_from(MODIFSTRUCT["MAILLE_MODIF"].nom)
    modif_struct.find_modele_couple_from(MODIFSTRUCT["MODELE_MODIF"].nom)

    modif_struct.calcul_mesure_support_corresp()

    modif_struct.calcul_condensation()
    mode_simult = 1
    calc_freq = _F(OPTION='PLUS_PETITE',
                   NMAX_FREQ=20,
                   SEUIL_FREQ=1.E-4,) 
    calc_freq['SEUIL_FREQ'] = 1e-4
    modif_struct.calcul_coupling_model_modes(mode_simult, calc_freq)
        
    
def get_ddl_extract(nom_resu):
    """ resu est le resultat de l'operateur OBSERVATION. On va y chercher les
    ddl filtres"""
    jdc = CONTEXT.get_current_step().jdc

    for etape in jdc.etapes:
        if etape.sdnom == nom_resu:
            grp_no_ma = etape.valeur['FILTRE']
            if type(grp_no_ma) != list and type(grp_no_ma) != tuple:
                grp_no_ma =  [grp_no_ma]
            else:
                pass
            for dico in grp_no_ma:
                for ind1,ind2 in dico.items():
                    if type(ind2) != list and type(ind2) != tuple :
                        dico[ind1] = [ind2]
                        
            return grp_no_ma
            















    
