#@ MODIF meidee_test Meidee  DATE 06/03/2007   AUTEUR BODEL C.BODEL 
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
#

import meidee_fludela, meidee_correlation, meidee_turbulent, meidee_cata
from Numeric import take

def TestMeidee(macro,
               mess,
               out,
               objects,
               MODELE_COMMANDE,
               BASE,
               MODELE_MESURE,
               MESURE1,
               MESURE2,
               MESURE3,
               INTE_SPEC,
               ):

    #####################################################
    # Transformation des objets Aster en classes python #
    #              Test de ces classes                  #
    #####################################################

    # classe MeideeObjects
    # La classe MO recupere tous les concepts aster disponibles et
    # les range dans des classes python telles que Resultat, InterSpectre
    # Modele, CaraElem...
    
    resu_num  = objects.resultats[BASE.nom]
    resu_exp1 = objects.resultats[MESURE1.nom]
    resu_exp2 = objects.resultats[MESURE2.nom]
    resu_exp3 = objects.resultats[MESURE3.nom]
    modele_exp  = objects.resultats[MODELE_MESURE.nom]
    modele_act  = objects.resultats[MODELE_COMMANDE.nom]
    inte_spec = objects.inter_spec[INTE_SPEC.nom]
    resu_num.show()
    resu_num.show_cara_mod()
    resu_exp1.show()
    resu_exp1.show_cara_mod()
    resu_exp2.show()
    resu_exp2.show_cara_mod()    
    resu_exp3.show()
    resu_exp3.show_cara_mod()    

    
    ########################################
    # 1) Test des fonctions de correlation #
    ########################################
    meidee = meidee_correlation.MeideeCorrelation(macro,mess)

    norm_num = None
    norm_exp = None

    meidee.Setup(resu_num, resu_exp1, norm_num, norm_exp)
    # Lancement de la macro MACRO_MEIDEE_PROJ
    # Les concepts resultats sont srockes sous le nom RES_+type de resu
    # On realise l'expansion avec les quatre premiers modes de la base numerique
    # sur les trois premiers modes exp
    ind_mod_num = []
    ind_mod_exp = []

    # Selection des modes à étendre et des modes de la base d'expansion
    ind_mod_tmp  = take(resu_num.get_cara_mod(), (0,1,2,3,4,5,6,7,8,9,10))[:,0].tolist()
    for j in ind_mod_tmp:
        ind_mod_num.append(int(j))
    freq_mod_num = take(resu_num.get_cara_mod(),(0,1,2,3,4,5,6,7,8,9,10))[:,0].tolist()
    num_modes = (ind_mod_num,freq_mod_num)
    ind_mod_tmp  = take(resu_exp1.get_cara_mod(),(0,2,3,5,8))[:,0].tolist()
    for j in ind_mod_tmp:
        ind_mod_exp.append(int(j))
    freq_mod_exp  = take(resu_exp1.get_cara_mod(),(0,2,3,5,8))[:3,1].tolist()
    exp_modes = (ind_mod_exp,freq_mod_exp)
    
    meidee.calc_proj_resu(num_modes,exp_modes,meidee.resu_num, meidee.resu_exp,
                          "RESU")

    objects.recup_objects()
    resu_et  = objects.resultats["RESU_ET"]

    # Test des MAC_MODE (utilise la fonction calc_mac_mode)
    # NB : pour l'instant MAC_MODE ne marche pas avec les modes reduits
    tests = [("phi_et", ),("phi_et", "phi_num" ),("phi_exp", ),
             #("phi_exp", "phi_num_red"),
             ("phi_num",),
             #("phi_num_red",)
             ]
    for ind in range(len(tests)):
        meidee.get_mac(tests[ind],resu_num.obj,resu_exp1.obj,norm_num,norm_exp)

    # Test des IMPR_RESU (format GMSH) on affiche tous les resu de MACRO_MEIDEE_PROJ
    meidee.prep_fichier(1,1,1,1)


    #############################################
    # 2) Test des fonctions de l'onglet fludela #
    #############################################
    fludela = meidee_fludela.MeideeFludela(mess, out)

    fludela.set_results(resu_exp1, resu_exp2, resu_exp3)
    fludela.set_param_phy(rho_int=0, rho_ext=1000, diam_int=0.018, diam_ext=0.02)
    fludela.methode_globale = 'Granger'
    fludela.res_longeq = meidee.resu_et
##    fludela.res_longeq = meidee.resu_num
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
##    para = [0.2, 1E-5,"Efforts discrets localises" ]
    para = [0.2, 1E-5,"Efforts et moments discrets" ]

    calcturb = meidee_turbulent.CalculTurbulent(mess)
    calcturb.calculate_force(inte_spec,
                             modele_exp,
                             resu_et,
                             modele_act,
                             para)


##    calcturb.Syy.make_inte_spec(titre="Résultat Meidee turbulent", paras_out = out)
    calcturb.Sff.make_inte_spec(titre="Résultat Meidee turbulent", paras_out = out)
    calcturb.Syy_S.make_inte_spec(titre="Résultat Meidee turbulent", paras_out = out)





    









    
