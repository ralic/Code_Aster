#@ MODIF meidee_test Meidee  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 
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

# RESPONSABLE BODEL C.BODEL

# Fichier comprenant une procédure de test de Meidee avec les différentes
# options de calcul.

import aster
from Accas import _F, ASSD

## SUPRESSION (PROVISOIRE ?) DES ONGLETS IFS
#from meidee_calcul_fludela import MeideeFludela, MeideeTurbMonomod
from meidee_calcul_correlation import MeideeCorrelation
from meidee_calcul_turbulent import CalculTurbulent, CalculInverse
from numpy import take

def TestMeidee(macro,
               mess,
               #out_fludela,
               #out_meideeturb,
               out_identification,
               out_modifstru,
               objects,
               EXPANSION,
               #MEIDEE_FLUDELA,
               #MEIDEE_TURBULENT,
               IDENTIFICATION,
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
                
        meidee.setup(resu_num, nume_mode_calcul, resu_exp,
                     nume_mode_mesure,parametres)
        # Lancement de la macro MACRO_MEIDEE_PROJ
        # Les concepts resultats sont srockes sous le nom RESU_+type de resu
        suffix = ['_NX','_EX','_ET','_RD']
        meidee.calc_proj_resu(suffix,"RESU")

        resu_et = objects.resultats["RESU_ET"]
        resu_nx = objects.resultats["RESU_NX"]
        resu_rd = objects.resultats["RESU_RD"]

        meidee.calc_mac_mode( resu_et, resu_nx, resu_num.mass )

        ## Test des IMPR_RESU (format GMSH) on affiche tous les resu de MACRO_MEIDEE_PROJ
        #meidee.prep_fichier(resu_et, resu_rd)





    ###############################################
    # 4) Test des fonctions de l'onglet turbulent #
    ###############################################
    if IDENTIFICATION:
        calcturb = CalculTurbulent(objects, mess)

        inter_spec_name = IDENTIFICATION['INTE_SPEC'].nom
        obs_name = IDENTIFICATION['OBSERVABILITE'].nom
        com_name = IDENTIFICATION['COMMANDABILITE'].nom
        res_base = IDENTIFICATION['BASE'].nom

        calcturb.set_interspectre(objects.get_inter_spec(inter_spec_name))
        calcturb.set_observabilite(objects.get_mode_meca(obs_name))
        calcturb.set_commandabilite(objects.get_mode_meca(com_name))
        
        ddl_actifs_com = get_ddl_extract(calcturb.res_com.nom)
        ddl_actifs_obs = get_ddl_extract(calcturb.res_obs.nom)
        calcturb.set_extraction_ddl_obs(ddl_actifs_obs)
        calcturb.set_extraction_ddl_com(ddl_actifs_com)
        
        calcturb.set_base(objects.get_mode_meca(res_base))
        
        calcturb.set_alpha(IDENTIFICATION['ALPHA'])
        calcturb.set_epsilon(IDENTIFICATION['EPS'])
        calcturb.set_mcoeff(0.0)

        calcturb.calculate_force()

        calcturb.Sff.make_inte_spec(titre="Resultat Meidee turbulent : efforts",
                                    paras_out = out_identification)
        calcturb.Syy.make_inte_spec(titre="Resultat Meidee turbulent : mesure",
                                      paras_out = out_identification)
        calcturb.Syy_S.make_inte_spec(titre="Resultat Meidee turbulent : synthese",
                                      paras_out = out_identification)

    ##################################################
    # 5) Test des fonctions de l'onglet modif struct #
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
        rdict = {"NOM" : grp["GROUP_NO"],
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

       :param EXTERIEUR: dictionaire (ou FACT) contenant les choix
                          de groupes de noeuds et leurs degrés de liberté
                          pour les ddl exterieurs.
                          
       :param out_modifstru: dictionaire (ou FACT) utilisé pour les résultats."""

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

    modif_struct.set_sumail_name("SUMAIL")   # nom super-maille par defaut
    modif_struct.find_modele_couple_from(MODIFSTRUCT["MODELE_MODIF"].nom)
    modif_struct.find_maillage_modif_from(MODIFSTRUCT["MODELE_MODIF"].nom)

    modif_struct.calcul_mesure_support_corresp()

    modif_struct.set_param_condens({'METHODE':'SVD','EPS':1.0E-5,'REGUL':'NON'})
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
        try:
            nom_etape = etape.sdnom
        except AttributeError:
            pass
        if nom_etape == nom_resu:
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




## SUPRESSION (PROVISOIRE ?) DES ONGLETS IFS
##    #############################################
##    # 2) Test des fonctions de l'onglet fludela #
##    #############################################
##    if MEIDEE_FLUDELA:
##        fludela = MeideeFludela(mess, out_fludela)
##
##        # Transformation des objets Aster en classes Python
##        resu_exp1 = objects.resultats[MEIDEE_FLUDELA['MESURE1'].nom]
##        resu_exp2 = objects.resultats[MEIDEE_FLUDELA['MESURE2'].nom]
##        resu_exp3 = objects.resultats[MEIDEE_FLUDELA['MESURE3'].nom] 
##
##        fludela.set_results(resu_exp1, resu_exp2, resu_exp3)
##        fludela.set_param_phy(rho_int=0, rho_ext=1000, diam_int=0.018, diam_ext=0.02)
##        fludela.methode_globale = 'Granger'
##        fludela.res_longeq = objects.resultats[MEIDEE_FLUDELA['BASE'].nom]
##        fludela.pairs =  [([0], [0], [0]), ([1], [1], [1]), ([2], [2], [2])]
##        list_vit = ['10']
##        for vit in list_vit:
##            fludela.set_speed(vit) # on fait le test pour une seule vitesse pour l'instant
##            fludela.compute()
##
##        # Ecriture des fichiers dans une table de resultats
##        name = fludela.calc['U']
##        #fabrique une copie du tableau de resultats correspondant a 1 vitesse (fludela.calc)
##        data_tmp = fludela.save_data()
##        data = {}
##        keys1 = ["Ma rep dim  mod","Ma rep adim lin","Ka vit dim  mod","Ka vit adim lin"]
##        keys2 = ['mass_ajou_dim_mod','mass_ajou_adim_lin','raid_ajou_dim_mod','raid_ajou_adim_lin']
##        for col in range(4):
##            key = keys1[col]
##            obj = keys2[col]
##            data[key] = data_tmp[obj]
##        fludela.saved_data[name] = data
##        fludela.save_to_file(name, data)
##        fludela.save_to_table(fludela.saved_data,"Résultats Meidee fluide-elastique")
##
##
##    #############################################
##    # 3) Test des fonctions de l'onglet fludela #
##    #############################################
##    if MEIDEE_TURBULENT:
##        # utiliser les out_meideeturb
##        turbmonomod = MeideeTurbMonomod(objects,mess,out_meideeturb)
##        
##        inter_spec_name = MEIDEE_TURBULENT['INTE_SPEC'].nom
##        resu_num_name = MEIDEE_TURBULENT['BASE'].nom
##        resu_exp_name = MEIDEE_TURBULENT['MESURE'].nom
##        
##                
##        # Set default indices for the modes
##        if MEIDEE_TURBULENT['NUME_MODE_DECONV'] != 0:
##            nume_mode_deconv = [ind for ind in
##                                list(MEIDEE_TURBULENT['NUME_MODE_DECONV'])]
##        else:
##            nume_mode_calcul = None
##        if MEIDEE_TURBULENT['NUME_MODE_LOCAL'] != 0:
##            nume_mode_EML = [ind for ind in
##                                list(MEIDEE_TURBULENT['NUME_MODE_LOCAL'])]
##        else:
##            nume_mode_EML = None
##                
##        turbmonomod.set_res_longcorr(objects.get_resultats(resu_num_name))
##        turbmonomod.set_resultat_exp(objects.get_resultats(resu_exp_name))
##        turbmonomod.set_nume_deconv(nume_mode_deconv)
##        turbmonomod.set_nume_EML(nume_mode_EML)
##
##        param_corr = {}
##        param_corr['ld']=3.0
##        param_corr['lambdac']=3.5
##        param_corr['gammac']=70
##
##                
##        turbmonomod.set_param_gamma(param_corr)
##        
##        turbmonomod.set_param_phy(rho_int=1.3, rho_ext=1000, diam_int=0.009538, diam_ext=0.015878)
##        turbmonomod.set_interspectre(objects.get_inter_spec(inter_spec_name))
##        turbmonomod.set_type_intsp('DEPL')
##        
##        list_vit = ['10']
##        param0={}
##        param0['A']=5e+15
##        param0['Amin']=1e+11
##        param0['Amax']=1e+18
##        param0['PULSC']=100.
##        param0['PULSCmin']=50.
##        param0['PULSCmax']=150.
##        param0['BETA']=10.
##        param0['BETAmin']=5.
##        param0['BETAmax']=15.
##        param0['MU']=0.025
##        param0['MUmin']=0.01
##        param0['MUmax']=0.05
##        param0['poids1']=1.
##        param0['poids2']=0.1
##
##        for vit in list_vit:
##            turbmonomod.set_speed(vit) # on fait le test pour une seule vitesse pour l'instant
##            turbmonomod.compute()
##            turbmonomod.set_param_init_recal(param0)
##            turbmonomod.compute_interpolation()
            
