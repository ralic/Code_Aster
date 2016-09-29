# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mathieu.courtois at edf.fr

import os

# Creation de la liste des coordonnees en Z d'un groupe de noeuds ou mailles :
def recu_coor_z(noma,group,typ_group,tole_r):

    import aster
    from Utilitai.Utmess import UTMESS

    collcnx = aster.getcolljev(noma.nom.ljust(8) + '.CONNEX')
    coord = aster.getvectjev(noma.nom.ljust(8) + '.COORDO    .VALE')
    cnom = aster.getvectjev(noma.nom.ljust(8) + '.NOMNOE')

    coor_z=[]

    if typ_group == 'group_ma':
        nomgrma = group
        collgrma = aster.getcolljev(noma.nom.ljust(8) + '.GROUPEMA')
        if nomgrma.ljust(24) not in collgrma:
            UTMESS("F", "MISS0_26", valk=group)
        else:
            numa = collgrma[nomgrma.ljust(24)]
            for ima in numa:
                n = collcnx[ima][0]
                uzmin = round(coord[3 * (n - 1) + 2],tole_r)
                uzmax = round(coord[3 * (n - 1) + 2],tole_r)
                for i in range(len(collcnx[ima])):
                    n = collcnx[ima][i]
                    uzmin = min(uzmin,round(coord[3 * (n - 1) + 2],tole_r))
                    uzmax = max(uzmax,round(coord[3 * (n - 1) + 2],tole_r))
                if uzmin not in coor_z:
                    coor_z.append(uzmin)
                if uzmax not in coor_z:
                    coor_z.append(uzmax)
    elif typ_group == 'group_no':
        collgrno = aster.getcolljev(noma.nom.ljust(8) + '.GROUPENO')
        nomgrno = group
        if nomgrno.ljust(24) not in collgrno:
            UTMESS("F", "MISS0_26", valk=group)
        else:
            grpn = collgrno[nomgrno.ljust(24)]
            l_coor_group = []
            i = 0
            for node in grpn:
                l_coor_group.append(
                    aster.getvectjev(noma.nom.ljust(8) + '.COORDO    .VALE', 3 * (node - 1), 3))
                uz = round(l_coor_group[i][2],tole_r)
                i += 1
                if uz not in coor_z:
                   coor_z.append(uz)
    else:
        assert 0, 'recu_coor_z : erreur argument typ_group'
    
    prov = sorted(coor_z)
    coor_z = prov[::-1]

    return coor_z



def defi_sol_miss_ops(self, MATERIAU, COUCHE, COUCHE_AUTO, TITRE, INFO, **args):
    """Macro DEFI_SOL_MISS :
    définir les caractéristiques du sol pour un calcul MISS3D
    """
    import aster

    from Accas import _F
    from Utilitai.Utmess import UTMESS
    from Utilitai.Table import Table
    CREA_TABLE = self.get_cmd("CREA_TABLE")

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Le concept sortant (de type table_sdaster) est tabout
    self.DeclareOut("tabout", self.sd)

    # 1. Création des dictionnaires des MATERIAUX
    l_mate = []
    for Mi in MATERIAU:
        dM = Mi.cree_dict_valeurs(Mi.mc_liste)
        l_mate.append(dM)
    nb_mate = len(l_mate)

    # 2. Création des dictionnaires des COUCHES
    l_couche = []
    n_substr = 0
    n_epais = 0
    # Mode manuel :
    if COUCHE != None:
        for Ci in COUCHE:
            dC = Ci.cree_dict_valeurs(Ci.mc_liste)
            if dC.get("SUBSTRATUM") == "OUI":
                n_substr += 1
            if dC.get("EPAIS") != None:
                n_epais += 1
            l_couche.append(dC)
        if n_substr != 1:
            UTMESS("F", "MISS0_3")
        if n_epais == 0:
            UTMESS("F", "MISS0_21")
        nb_couche = len(l_couche)
        print 'l_couche=',l_couche
   
    # Mode automatique pour les couches :
    grma_interf = None
    arg_grno = False
    arg_grma = False
    if COUCHE_AUTO != None:
        ll_mate = []
        l_epais = []
        enfonce = False
        homogene = False
        l_z0 = False
        Z0 = 0.
        max_z_input = 0.
        min_z_input = 0.
        # tole_r sert à arrondir les valeurs pour les tests
        tole_r = 5
        l_pt_ctrl = False
        coor_z_input = []
        decalage_auto = False            
        # Lecture des arguments :
        for Ci in COUCHE_AUTO:
            dC = Ci.cree_dict_valeurs(Ci.mc_liste)
            if dC.get("HOMOGENE") == "OUI":
                homogene = True
            if dC.get("MAILLAGE"):
                noma = dC.get("MAILLAGE")
            if dC.get("SURF") == "NON":
                enfonce = True
            if dC.get("GROUP_MA") or dC.get("GROUP_NO"):   
                collcnx = aster.getcolljev(noma.nom.ljust(8) + '.CONNEX')
                coord = aster.getvectjev(noma.nom.ljust(8) + '.COORDO    .VALE')
                cnom = aster.getvectjev(noma.nom.ljust(8) + '.NOMNOE')
                if dC.get("GROUP_MA"):
                    arg_grma = True
                    nomgrma = dC.get("GROUP_MA")
                    coor_z_input =  recu_coor_z(noma,nomgrma,'group_ma',tole_r)
                else:
                    arg_grno = True
                    nomgrno = dC.get("GROUP_NO")
                    coor_z_input =  recu_coor_z(noma,nomgrno,'group_no',tole_r)
                max_z_input = coor_z_input[0]
                min_z_input = coor_z_input[-1]
            if dC.get("NUME_MATE"):
                ll_mate = dC.get("NUME_MATE")
            if dC.get("EPAIS_PHYS"):
                if homogene:
                   l_epais.append(dC.get("EPAIS_PHYS")[0])         
                else:
                   l_epais = dC.get("EPAIS_PHYS")            
            if dC.get("NUME_MATE_SUBSTRATUM"):
                nume_substr = dC.get("NUME_MATE_SUBSTRATUM")
            if dC.get("NOMBRE_RECEPTEUR"):
                nb_recept = dC.get("NOMBRE_RECEPTEUR")
                if (nb_recept%2 <> 0):
                    UTMESS("F", "MISS0_27")
            if dC.get("GROUP_MA_INTERF"):
                grma_interf = dC.get("GROUP_MA_INTERF")
            if dC.get("Z0"):
                Z0 = dC.get("Z0")
                l_z0 = True
            if not l_z0 and enfonce:
                Z0 = max_z_input
            if not enfonce:
                max_z_input = Z0
            print 'La cote Z vaut : ',Z0
            if dC.get("TOLERANCE"):
               tole_verif = dC.get("TOLERANCE")
            if dC.get("DECALAGE_AUTO") == "OUI":               
               decalage_auto = True            
            if dC.get("GROUP_MA_CONTROL"):
                nomgrmactrl = dC.get("GROUP_MA_CONTROL")
                l_pt_ctrl = True
                coor_z_ctrl = recu_coor_z(noma,nomgrmactrl,'group_ma',tole_r)
                print 'Cotes verticales des points de controle=',coor_z_ctrl
                if coor_z_ctrl[0] > Z0:
                    UTMESS("F", "MISS0_28", valr=Z0)

        if homogene:
            max_z_input = Z0
            ll_mate.append(1)
            l_mate.append(l_mate[0])
            nb_mate += 1            
        coor_z_sol = [max_z_input,]
        for ep in l_epais:
            coor_z_sol.append(coor_z_sol[-1]-ep)
        # Typage des couches : 1 si couche de sol, 2 si point de controle
        type_couche=[1]*len(coor_z_sol)
        if enfonce:
            # Verification si il y a vraiment enfoncement
            if len(coor_z_sol) == 1:
                UTMESS("F", "MISS0_29")
            # Verification entre base de l'interface et couches de sol
            print 'coor_z_sol=',coor_z_sol
            if len(l_epais) <> len(ll_mate):
               UTMESS("F", "MISS0_30")
        
        if l_pt_ctrl:
            if coor_z_ctrl[-1] < coor_z_sol[-1]:
               UTMESS("F", "MISS0_32")
            # Liste regroupant les cotes des points de controles et des couches de sol
            coor_z_sol_ctrl = []
            for zz in coor_z_sol:
                coor_z_sol_ctrl.append(zz)
            for zz in coor_z_ctrl:
                if zz not in coor_z_sol_ctrl:
                    coor_z_sol_ctrl.append(zz)
            prov = sorted(coor_z_sol_ctrl)
            coor_z_sol_ctrl = prov[::-1]
            type_couche = [1]
            l_epais = []
            ll_mate2 = []
            zz_sup = Z0
            jj = 0
            for zz in coor_z_sol_ctrl[1:]:
                if zz in coor_z_ctrl:
                    if zz in coor_z_sol:
                        UTMESS("F", "MISS0_33")
                    else:
                        type_couche.append(2)
                else:
                    type_couche.append(1)
                pp = zz_sup-zz
                l_epais.append(pp)
                zz_sup = zz
                ll_mate2.append(ll_mate[jj])
                if zz in coor_z_sol:
                    jj += 1
                ll_mate = ll_mate2
               
    # Verification avec l'interface FEM-BEM
    nb_noeud = 0
    verif = False
    if ((grma_interf != None) and enfonce and (COUCHE_AUTO != None)):
        coor_z_interf = recu_coor_z(noma,grma_interf,'group_ma',tole_r)
        max_z_interf = coor_z_interf[0]
        min_z_interf = coor_z_interf[-1]
        l_z_sol=[max_z_interf,]
        for ep in l_epais:
            l_z_sol.append(l_z_sol[-1]-ep)
        nb_noeud = len(coor_z_input)
        if (max_z_input < max_z_interf) or (min_z_input > min_z_interf):
            UTMESS("F", "MISS0_34")
        verif = True
        denom = 1.
        for z_input in coor_z_input:
           verif1 = False
           for z_interf in coor_z_interf:
              if (abs(z_interf)>1.):
                 denom=abs(z_interf)
              if ((abs(z_input-z_interf)/denom) <= tole_verif):
                 verif1 = True
           verif = (verif and verif1)
        if (not verif):
            if arg_grma:
               UTMESS("F", "MISS0_34")
            else:
               UTMESS("A", "MISS0_35")
        if l_pt_ctrl:
            for zz in coor_z_ctrl:
                if zz > min_z_input:
                    UTMESS("F", "MISS0_36")
              
        
           
    #Generation table sol en mode auto
    if (COUCHE_AUTO != None):
        couche = {}
        nbc = 0
        idc = 1
        nbsscouche = 0
        if enfonce:
            l_noeud = coor_z_input
            # Liste des epaisseurs de sol
            l_z_ep = []
            for zz in l_z_sol:
                l_z_ep.append(Z0-zz)
            for pt in range(nb_noeud-1):
                idc += 1
                enfonc_sup = l_noeud[0] - l_noeud[pt]
                if enfonc_sup > l_z_ep[len(l_z_ep)-1]:
                    UTMESS("F", "MISS0_37")
                ep_ss_couche = (l_noeud[pt]-l_noeud[pt+1]) / nb_recept
                for nc in range(nb_recept):
                    couche = {}
                    enfonc_ss_c_haut = l_noeud[0]-l_noeud[pt] + (nc * ep_ss_couche)
                    enfonc_ss_c_bas  = enfonc_ss_c_haut + ep_ss_couche
                    i = 0
                    ind_mat_sup = 0
                    ind_mat_inf = 0
                    if not homogene:
                        while enfonc_ss_c_haut > l_z_ep[i]:
                            i += 1
                        while l_z_ep[ind_mat_sup] <= enfonc_ss_c_haut:
                            ind_mat_sup += 1
                        ind_mat_inf = ind_mat_sup    
                        ind_mat_sup = ind_mat_sup - 1
                        while enfonc_ss_c_bas >= l_z_ep[ind_mat_inf]:
                            ind_mat_inf += 1
                    nb_mat_couche = ind_mat_inf - ind_mat_sup
                    if homogene:
                        id_mate = ll_mate[0]
                    elif ind_mat_sup < (ind_mat_inf - 2):
                        #Cas plus de deux materiaux dans la sous-couche 
                        print '   Cas plus de deux materiaux dans la sous-couche'
                        ep_mat_h = []
                        ep_mat_b = []
                        ep_mat = [(l_z_ep[ind_mat_sup + 1] - enfonc_ss_c_haut),]
                        for ind_mat in range((ind_mat_sup + 1),(ind_mat_inf - 1)):
                           zz1 = l_z_ep[ind_mat] - enfonc_ss_c_haut
                           zz2 = enfonc_ss_c_bas - l_z_ep[ind_mat]
                           ep_mat_h.append(zz1)
                           ep_mat_b.append(zz2)
                        ep_mat.append(enfonc_ss_c_bas - l_z_ep[ind_mat_inf-1])
                        err_ep = abs(sum(ep_mat) - ep_ss_couche) / ep_ss_couche
                        if ( err_ep > tole_verif):
                           UTMESS("F", "MISS0_38")
                        ep1 = ep_mat[0]
                        ii = 1
                        id_mate = ll_mate[ind_mat_sup]
                        for ii in range(1,(len(ep_mat)-1)):
                           if (ep_mat[ii] > ep1):
                              ep1 = ep_mat[ii]
                              id_mate = ll_mate[ind_mat_sup+ii]
                    elif ind_mat_sup == (ind_mat_inf - 2):
                        #Cas deux materiaux dans la sous-couche 
                        zz1 = l_z_ep[ind_mat_sup+1] - enfonc_ss_c_haut
                        zz2 = enfonc_ss_c_bas - l_z_ep[ind_mat_sup+1]
                        if zz2 > zz1:
                            id_mate = ll_mate[ind_mat_sup+1]
                        else:
                            id_mate = ll_mate[ind_mat_sup]                     
                    elif ind_mat_sup == (ind_mat_inf - 1):
                        #Cas un seul materiau
                        id_mate = ll_mate[ind_mat_sup]
                    else:
                        assert False, "Erreur dans la contruction des sous-couches"
                    couche["SUBSTRATUM"] = None
                    couche["NUME_MATE"] = id_mate
                    couche["EPAIS"] = ep_ss_couche
                    couche["RECEPTEUR"] = "OUI"
                    couche["SOURCE"] = "NON"
                    if nc == nb_recept/2:
                       couche["SOURCE"] = "OUI"
                    l_couche.append(couche)
                    idc += 1                    
                    enfonc_ss_c_bas = l_noeud[0] - l_noeud[pt+1]       
            # Fin des sous-couches
            
            # Couche a la base de l'interface
            ii = 0
            epais = -1.
            if len(l_z_ep) > 1:
                while enfonc_ss_c_bas >= l_z_ep[ii]:
                    ii += 1
                epais = l_z_ep[ii]-enfonc_ss_c_bas
            else:
                if l_z_ep[ii] <= enfonc_ss_c_bas:
                    UTMESS("F", "MISS0_39")
                else:
                    epais = l_z_ep[ii] - enfonc_ss_c_bas
            couche = {}
            couche["EPAIS"] = epais
            couche["SUBSTRATUM"] = None
            if not homogene:
               couche["NUME_MATE"] = ll_mate[ii-1]
            else:
               couche["NUME_MATE"] = ll_mate[0]
            couche["EPAIS"] = epais
            couche["SOURCE"] = "OUI"
            couche["RECEPTEUR"] = "OUI"
            idc += 1            
            l_couche.append(couche)
            # Couches strictement sous l'interface
            while ii < len(l_epais):
                couche = {}
                couche["SUBSTRATUM"] = None
                couche["NUME_MATE"] = ll_mate[ii]
                couche["EPAIS"] = l_epais[ii]
                couche["SOURCE"] = "NON"
                couche["RECEPTEUR"] = "NON"
                if type_couche[ii]==2:
                    # Cas des points de controle
                    couche["RECEPTEUR"] = "OUI"
                    couche["SOURCE"] = "OUI"
                l_couche.append(couche)
                idc += 1
                ii += 1
            # Substratum
            couche = {}
            couche["SUBSTRATUM"] = "OUI"
            couche["SOURCE"] = "NON"
            couche["RECEPTEUR"] = "NON"
            if homogene:
               couche["NUME_MATE"] = ll_mate[0]+1
            else:
               couche["NUME_MATE"] = nume_substr
            couche["EPAIS"] = None
            l_couche.append(couche)
        else:
            # Cas superficiel
            couche = {}
            ii = 0
            couche["SOURCE"] = "OUI"
            couche["RECEPTEUR"] = "OUI"
            couche["NUME_MATE"] = ll_mate[ii]
            couche["EPAIS"] = l_epais[ii]
            couche["SUBSTRATUM"] = None
            l_couche.append(couche)
            ii = 1
            for nc in range(1,len(l_epais)):
                couche = {}
                couche["SUBSTRATUM"] = None
                couche["NUME_MATE"] = ll_mate[ii]
                couche["EPAIS"] = l_epais[ii]
                couche["RECEPTEUR"] = "NON"
                couche["SOURCE"] = "NON"
                if type_couche[ii] == 2:
                   # Cas des points de controle
                   couche["RECEPTEUR"] = "OUI"
                   couche["SOURCE"] = "OUI"
                l_couche.append(couche)
                idc += 1
                ii += 1
            couche = {}
            couche["SUBSTRATUM"] = "OUI"
            couche["SOURCE"] = "NON"
            couche["RECEPTEUR"] = "NON"
            if homogene:
               couche["NUME_MATE"] = ll_mate[0]+1
            else:
               couche["NUME_MATE"] = nume_substr
            couche["EPAIS"] = None
            l_couche.append(couche)

    if ((COUCHE_AUTO != None) and enfonce):
        # Verification entre base de l'interface et couches de sol
        min_z_input_r = round(min_z_input,tole_r)
        prof = Z0
        ii = 0
        for couche in l_couche[:-2]:
            prof = prof - couche["EPAIS"]
            prof = round(prof,tole_r)
            print 'ii=',ii
            print 'prof=',prof
            if prof == min_z_input_r:
                if decalage_auto:
                    UTMESS("A", "MISS0_40", valr=(prof,couche["EPAIS"]))
                    couche["NUME_MATE"] = l_couche[ii+1]["NUME_MATE"]
                else:
                    UTMESS("A", "MISS0_41")   
            ii += 1
        print 'l_couche mod=',l_couche


    # 3. définition de la table
    # para/typ pré-trie les colonnes
    tab = Table(
        para=["NUME_COUCHE", "EPAIS", "RHO", "E", "NU", "AMOR_HYST",
              "RECEPTEUR", "SOURCE", "NUME_MATE", "SUBSTRATUM"],
        typ=["I", "R", "R", "R", "R", "R", "K8", "K8", "I", "K8"])
    idc = 0
    for couche in l_couche:
        idc += 1
        id_mate = couche["NUME_MATE"]
        if id_mate > nb_mate:
            UTMESS("F", "MISS0_4", vali=(idc, nb_mate, id_mate))
        id_mate = id_mate - 1
        couche["NUME_COUCHE"] = idc
        couche.update(l_mate[id_mate])
        if couche.get("SUBSTRATUM") is None:
            del couche["SUBSTRATUM"]
        if couche["EPAIS"] is None:
            couche["EPAIS"] = 0.
        tab.append(couche)

    # 4. surcharge par le titre fourni
    if TITRE != None:
        if type(TITRE) not in (list, tuple):
            TITRE = [TITRE]
        tab.titr = os.linesep.join(TITRE)

    if INFO == 2:
        print tab

    # 5. création de la table
    dprod = tab.dict_CREA_TABLE()
    tabout = CREA_TABLE(**dprod)
