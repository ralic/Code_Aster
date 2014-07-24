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
# person_in_charge: aimery.assire at edf.fr

"""
    Macro-commande LIRE_EUROPLEXUS appelée par CALC_EUROPLEXUS.
"""


import aster
from Accas import _F
import os
from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
from Calc_epx.calc_epx_cata import cata_modelisa, cata_compor

def get_unite_libre():
    """
        Retoune une unité de fichier libre.
    """
    _UL = INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    unite = _UL['UNITE_LIBRE', 1]
    DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)
    return unite

def lire_europlexus_ops(self, FICHIER_MED, MAILLAGE, MODELE, CARA_ELEM,
                        CHAM_MATER, COMPORTEMENT, EXCIT, CONT_2_EFF, ARCHIVAGE,
                        INFO=1, **args):

    """
        Macro-commande LIRE_EUROPLEXUS.
        Lit le résultat EPX au format MED et construit un concept evol_noli
        à partir de celui-ci.
    """
    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # On importe les definitions des commandes a utiliser dans la macro
    # Le nom de la variable doit etre obligatoirement le nom de la commande

    global _F, DETRUIRE, DEFI_FICHIER, LIRE_RESU, INFO_EXEC_ASTER
    global LIRE_CHAMP, CREA_CHAMP, CREA_RESU, FORMULE, MODI_REPERE

    INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
    DETRUIRE = self.get_cmd('DETRUIRE')
    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')
    LIRE_RESU = self.get_cmd('LIRE_RESU')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    LIRE_CHAMP = self.get_cmd('LIRE_CHAMP')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CREA_RESU = self.get_cmd('CREA_RESU')
    FORMULE = self.get_cmd('FORMULE')
    MODI_REPERE = self.get_cmd('MODI_REPERE')

    import med_aster # lire tous les champs du fichier med

    self.DeclareOut('resu', self.sd)
    # Le concept sortant (de type evol_noli) est nomme 'resu'
    global resu


    # Format med des champs depl, vite et acce
    format_med = [
        {
        'NOM_CHAM_MED' : 'DEPL_001',
        'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
        'NOM_CMP_MED' : ('UX', 'UY', 'UZ', 'RX', 'RY', 'RZ'),
        'NOM_CHAM' :'DEPL'
         },
        {
        'NOM_CHAM_MED' : 'VITE_001',
        'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
        'NOM_CMP_MED' : ('VX', 'VY', 'VZ', 'RX', 'RY', 'RZ'),
        'NOM_CHAM' : 'VITE'
         },
        {
        'NOM_CHAM_MED' : 'ACCE_001',
        'NOM_CMP' : ('DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ'),
        'NOM_CMP_MED' : ('GX', 'GY', 'GZ', 'RX', 'RY', 'RZ'),
        'NOM_CHAM' : 'ACCE'
         },
                 ]

    # LECTURE DE COMPORTEMENT POUR RECUPERATION DES VARIABLES INTERNES
    dic_compo_gr = {}
    for dic in COMPORTEMENT:
        compo = dic['RELATION']
        comp_epx = cata_compor[compo]['NOM_EPX'][:4]
        if not dic_compo_gr.has_key(comp_epx):
            dic_compo_gr[comp_epx] = []
        dic_compo_gr[comp_epx].extend(dic['GROUP_MA'])

    # A PATIR DE CATA_MODELISA ON CONSTRUIT UN DICTIONNAIRE CONTENANT LES INFOS
    # NÉCESSAIRES SUR LES MODÉLISATIONS EPX.
    info_mode_epx = {}
    prexif_epx_cont = 'SIG'
    dic_mc_cara = {}
    for mode_aster in cata_modelisa.keys():
        dic_mode = cata_modelisa[mode_aster]
        if dic_mode.has_key('CONT_ASTER'):
            nom_cmp = dic_mode['CONT_ASTER']
            nom_cmp_med = []
            for i in range(1, len(dic_mode['CONT_ASTER'])+1):
                nom_cmp_med.append(prexif_epx_cont+"%s"%i)
        else:
            nom_cmp = None
            nom_cmp_med = None
        if dic_mode.has_key('MC_CARA'):
            mc_cara = dic_mode['MC_CARA']
            if not dic_mc_cara.has_key(mc_cara):
                dic_mc_cara[mc_cara] = {'INSTANCE' : [],
                                      'NOM_CMP' : nom_cmp,
                                      'CH_FONC' : None,
                                      'GROUP_MA' : []}
        else:
            mc_cara = None
        if dic_mode.has_key('MODI_REPERE'):
            type_modi = dic_mode['MODI_REPERE']
        else:
            type_modi = None
        for typ_ma in dic_mode['MODE_EPX']:
            for mode_epx in dic_mode['MODE_EPX'][typ_ma]:
                info_mode_epx[mode_epx] = {'NOM_CMP' : nom_cmp,
                                         'NOM_CMP_MED' : nom_cmp_med,
                                         'MC_CARA':mc_cara,
                                         'MODI_REPERE':type_modi}

    # A PATIR DE CATA_COMPOR ON CONSTRUIT UN DICTIONNAIRE CONTENANT LES INFOS
    # NÉCESSAIRES SUR LES LOIS EPX.
    info_comp_epx = {}
    prexif_epx_vari = 'VAR'
    prexif_aster_vari = 'V'
    for nom_comp in cata_compor.keys():
        comp_aster = cata_compor[nom_comp]
        if not comp_aster.has_key('NOM_EPX'):
            continue
        nom_epx = comp_aster['NOM_EPX'][:4]
        if not info_comp_epx.has_key(nom_epx):
            nb_var_aster = comp_aster['NB_VAR_ASTER']
            nb_var_epx = comp_aster['NB_VAR_EPX']
            var_aster = []
            var_epx = []
            for ivar in range(1, max([nb_var_aster, nb_var_epx])+1):
                var_aster.append(prexif_aster_vari+str(ivar))
                var_epx.append(prexif_epx_vari+str(ivar))

            if ((not comp_aster['TRANSFO']) and
                (nb_var_aster > nb_var_epx)):
                raise Exception("""
Pour la loi %s, Aster possède plus de variables internes que EPX,
or vous n'avez pas activer le mot-clé TRANSFO pour indiqué comment
retrouver les variables internes Aster"""%nom_comp)

            dic = {
                  'NB_VAR_ASTER' : nb_var_aster,
                  'NB_VAR_EPX'   : nb_var_epx,
                  'VAR_ASTER'     : var_aster,
                  'VAR_EPX'     : var_epx,
                  'TRANSFO'   : comp_aster['TRANSFO'],
                  }
            info_comp_epx[nom_epx] = dic
        else:
            raise Exception("""
Plusieurs comportements Aster correspondent à un même mot-clé EPX
sur 4 caractères""")

    # CONSTRUCTION DES CHAMPS POUR PASSAGE DES CONTRAINTES AUX EFFORTS
    cont_gener = (ARCHIVAGE['CONT_GENER'] == 'OUI')
    if cont_gener:
        cont_2_eff = CONT_2_EFF.List_F()
        for instance in cont_2_eff:
            gr = instance['GROUP_MA']
            mc_cara = instance['MC_CARA']
            if dic_mc_cara.has_key(mc_cara):
                dic_mc_cara[mc_cara]['GROUP_MA'].extend(gr)
            else:
                continue
            val_cle = instance['VAL_CLE']
            if instance.has_key('L_VALE'):
                l_vale = instance['L_VALE']
            if mc_cara == 'COQUE':
                epais = val_cle
                dic_mc_cara[mc_cara]['INSTANCE'].append({'VALE' : epais,
                                                         'GROUP_MA' : gr,
                                                         'NOM_CMP' : 'X1'})
                dic_mc_cara[mc_cara]['INSTANCE'].append({'VALE' : epais**2/6.,
                                                         'GROUP_MA' : gr,
                                                         'NOM_CMP' : 'X2'})
                if not dic_mc_cara[mc_cara]['CH_FONC']:
                    nbcomp = len(dic_mc_cara[mc_cara]['NOM_CMP'])
                    __FO_CO = [None]*nbcomp
                    nume_X = [1, 1, 1, 2, 2, 2, 1, 1]
                    vale_f = []
                    nom_cmp_f = []
                    for i, comp in enumerate(dic_mc_cara[mc_cara]['NOM_CMP']):
                        xnum_i = 'X%s'%(nume_X[i])
                        xi = 'X%s'%(i+1)
                        val_fonc = comp+'*'+xnum_i
                        nom_para = (comp, xnum_i)
                        __FO_CO[i] = FORMULE(VALE=val_fonc, NOM_PARA=nom_para)
                        vale_f.append(__FO_CO[i])
                        nom_cmp_f.append(xi)
                    dic_mc_cara[mc_cara]['VALE_F'] = vale_f
                    dic_mc_cara[mc_cara]['NOM_CMP_F'] = nom_cmp_f

            elif mc_cara == 'BARRE':
                aire_sect = val_cle
                dic_mc_cara[mc_cara]['INSTANCE'].append({'VALE' : aire_sect,
                                                         'GROUP_MA' : gr,
                                                         'NOM_CMP' : 'X1'})
                if not dic_mc_cara[mc_cara]['CH_FONC']:
                    nbcomp = len(dic_mc_cara[mc_cara]['NOM_CMP'])
                    __FO_BA = [None]*nbcomp
                    nume_X = [1,]
                    vale_f = []
                    nom_cmp_f = []
                    for i, comp in enumerate(dic_mc_cara[mc_cara]['NOM_CMP']):
                        xnum_i = 'X%s'%(nume_X[i])
                        xi = 'X%s'%(i+1)
                        val_fonc = comp+'*'+xnum_i
                        nom_para = (comp, xnum_i)
                        __FO_BA[i] = FORMULE(VALE=val_fonc, NOM_PARA=nom_para)
                        vale_f.append(__FO_BA[i])
                        nom_cmp_f.append(xi)
                    # creation du champ de fonction
                    dic_mc_cara[mc_cara]['VALE_F'] = vale_f
                    dic_mc_cara[mc_cara]['NOM_CMP_F'] = nom_cmp_f
            elif mc_cara in dic_mc_cara.keys():
                raise Exception("""
Le passage des contraintes aux efforts n'est pas programmé pour 
le mot-clé %s"""%mc_cara)

        # CREATION DES CHAMPS DE CARACTERISTIQUES ET DE FONCTIONS
        # POUR CONTRAINTES
        nb_cara = len(dic_mc_cara.keys())
        __CH_CAR = [None]*nb_cara
        __CH_FON = [None]*nb_cara
        for icar, mc_cara in enumerate(dic_mc_cara.keys()):
            if len(dic_mc_cara[mc_cara]['INSTANCE']) > 0:

                __CH_CAR[icar] = CREA_CHAMP(
                                    INFO=INFO,
                                    TYPE_CHAM='ELGA_NEUT_R',
                                    OPERATION='AFFE',
                                    MODELE=MODELE,
                                    PROL_ZERO='OUI',
                                    AFFE=dic_mc_cara[mc_cara]['INSTANCE'],
                                            )
                dic_mc_cara[mc_cara]['CH_CARA'] = __CH_CAR[icar]
                nom_cmp_f = dic_mc_cara[mc_cara]['NOM_CMP_F']
                vale_f = dic_mc_cara[mc_cara]['VALE_F']
                gr = dic_mc_cara[mc_cara]['GROUP_MA']
                __CH_FON[icar] = CREA_CHAMP(
                                            INFO=INFO,
                                            TYPE_CHAM='ELGA_NEUT_F',
                                            OPERATION='AFFE',
                                            MODELE=MODELE,
                                            PROL_ZERO='OUI',
                                            AFFE=_F(
                                                    GROUP_MA=gr,
                                                    NOM_CMP=nom_cmp_f,
                                                    VALE_F=vale_f),
                                                )
                dic_mc_cara[mc_cara]['CH_FONC'] = __CH_FON[icar]

    # RECUPERATION DES DEPL, VITE et ACCE DANS LE FICHIER MED
    unite = get_unite_libre()
    DEFI_FICHIER(UNITE=unite, ACTION='LIBERER')
    fort = 'fort.%i' %unite
    if os.path.isfile(fort):
        os.remove(fort)

    os.symlink(FICHIER_MED, fort)

    # Regeneration des mots-cles EXCIT passés en argument de la macro
    dExcit = []
    for j in EXCIT:
        dExcit.append(j.cree_dict_valeurs(j.mc_liste))
        for i in dExcit[-1].keys():
            if dExcit[-1][i] == None:
                del dExcit[-1][i]

    resu = LIRE_RESU(TYPE_RESU='EVOL_NOLI',
                FORMAT='MED',
                MODELE=MODELE,
                FORMAT_MED=format_med,
                UNITE=unite,
                CHAM_MATER=CHAM_MATER,
                CARA_ELEM=CARA_ELEM,
                TOUT_ORDRE='OUI',
                EXCIT=dExcit,
                INFO=INFO,
                )

#   RECUPERATION DES CONTRAITES ET VARIABLES INTERNES DANS LE FICHIER MED
#     on utilise le module med_aster pour obtenir les noms des champs
#     et les noms des composantes de ces champs
    dic_champ_med = med_aster.get_nom_champ_med(FICHIER_MED)

    dic_champ_cont = {'SANS':{}}
    dic_champ_var_int = {'SANS':{}}

    modi_repere = {'COQUE' : False}
    for nom_cham_med in dic_champ_med.keys():

        ch_split = nom_cham_med.split('_')
        if len(ch_split) != 3:
            continue
        type_cham = ch_split[0]
        mode_epx = ch_split[1]
        loi = ch_split[2]
        if type_cham == "CONT":

            if not info_mode_epx.has_key(mode_epx):
                raise Exception("""
La modélisation %s n'est pas encore programmée mais est présente
dans un champ."""%mode_epx)
            nbcomp = len(dic_champ_med[nom_cham_med])
            nbcomp_ref = len(info_mode_epx[mode_epx]['NOM_CMP'])
            if nbcomp != nbcomp_ref:
                raise Exception("""
Le champ de type %s sur les éléments %s ne comporte pas le bon nombre
de composantes"""%(type_cham, mode_epx))
            mc_cara = info_mode_epx[mode_epx]['MC_CARA']
            type_modi = info_mode_epx[mode_epx]['MODI_REPERE']
            if type_modi is not None:
                modi_repere[type_modi] = True
            if mc_cara and cont_gener:
                if not dic_champ_cont.has_key(mc_cara):
                    dic_champ_cont[mc_cara] = {}
                dic_champ_cont[mc_cara][nom_cham_med] = mode_epx
            else:
                mc_cara = 'SANS'
                dic_champ_cont[mc_cara][nom_cham_med] = mode_epx

        if type_cham == "ECRO":
            if not info_comp_epx.has_key(loi):
                raise Exception(
"La loi %s n'est pas encore programmée mais est présente dans un champ."%loi)
            nbcomp = len(dic_champ_med[nom_cham_med])
            nbcomp_ref = info_comp_epx[loi]['NB_VAR_EPX']
            if nbcomp != nbcomp_ref:
                raise Exception("""
Le champ de type %s pour la loi %s ne comporte pas le bon nombre de
composantes"""%(type_cham, loi))
            if info_comp_epx[loi]['TRANSFO']:
                if not dic_champ_var_int.has_key(loi):
                    dic_champ_var_int[loi] = {}
                dic_champ_var_int[loi][nom_cham_med] = loi
            else:
                dic_champ_var_int['SANS'][nom_cham_med] = loi

    # CREATION DES CHAMPS POUR TRANSFORMATION DES VARIABLES INTERNES
    dic_transfo = {}
    nb_compo = len(dic_champ_var_int.keys())-1
    __CH_CAV = [None]*nb_compo
    __CH_FOV = [None]*nb_compo
    ico = 0
    for compo in dic_champ_var_int.keys():
        if compo == 'SANS':
            continue

        dic_transfo[compo] = {}
        if compo == 'GLRC':
            __CH_CAV[ico] = CREA_CHAMP(
                                    INFO=INFO,
                                    TYPE_CHAM='ELGA_NEUT_R',
                                    OPERATION='AFFE',
                                    MODELE=MODELE,
                                    PROL_ZERO='OUI',
                                    AFFE=_F(VALE=(1., 1./2.), TOUT='OUI',
                                             NOM_CMP=('X1', 'X2')),
                                    )
            dic_transfo[compo]['CH_CARA'] = __CH_CAV[ico]
            nb_comp = info_comp_epx[compo]['NB_VAR_ASTER']
            __F_V_GL = [None]*nb_comp
            XI = ['X1']*nb_comp
            XI[2] = 'X2'
            XI[5] = 'X2'
            li_fonc = []
            nom_cmp_f = []
            for ivar, var in enumerate(info_comp_epx[compo]['VAR_ASTER']
                                                            [:nb_comp]):
                xi = XI[ivar]
                xi_f = 'X%s'%(ivar+1)
                nom_cmp_f.append(xi_f)
                __F_V_GL[ivar] = FORMULE(VALE=var+'*'+xi, NOM_PARA=(var, xi))
                li_fonc.append(__F_V_GL[ivar])

            __CH_FOV[ico] = CREA_CHAMP(
                                        INFO=INFO,
                                        TYPE_CHAM='ELGA_NEUT_F',
                                        OPERATION='AFFE',
                                        MODELE=MODELE,
                                        PROL_ZERO='OUI',
                                        AFFE=_F(
                                                GROUP_MA=dic_compo_gr[compo],
                                                NOM_CMP=nom_cmp_f,
                                                VALE_F=li_fonc
                                                )
                                            )
            dic_transfo[compo]['CH_FONC'] = __CH_FOV[ico]
            ico += 1
            dic_transfo[compo]['NOM_CMP_F'] = nom_cmp_f
            dic_transfo[compo]['NOM_CMP'] = (info_comp_epx[compo]['VAR_ASTER']
                                                                   [:nb_comp])
        else:
            raise Exception("""
Les transformations à apporter aux variables internes pour la loi ne sont pas
présentes%s"""%compo)

    ll = len(dic_champ_cont.keys())-1
    nb_SIG1 = len(dic_champ_cont['SANS'].keys())+ll
    ll = len(dic_champ_var_int.keys())-1
    nb_ECR1 = len(dic_champ_var_int['SANS'].keys())+ll
    itot = len(resu.LIST_PARA()['INST'])
    __EFFG = [None]*itot
    __ECRG = [None]*itot
    __SIG1 = [None]*nb_SIG1
    __ECR1 = [None]*nb_ECR1
    dicAffe = []
    dicAffe3 = []
    for i in xrange(itot):

        pas = ARCHIVAGE['PAS_NBRE']
        dicAsse = []
        dicAsse3 = []
        dicDetr = []

        # CONTRAINTES
        for mc_cara in dic_champ_cont.keys():
            j = 0
            if mc_cara == 'SANS':
                for champ in dic_champ_cont[mc_cara].keys():
                    mode_epx = dic_champ_cont[mc_cara][champ]
                    nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                    nom_cmp_med = info_mode_epx[mode_epx]['NOM_CMP_MED']
                    __SIG1[j] = LIRE_CHAMP(
                            INFO=INFO,
                            TYPE_CHAM='ELGA_SIEF_R',
                            UNITE=99,
                            NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                            MODELE=MODELE,
                            MAILLAGE=MAILLAGE,
                            PROL_ZERO='OUI',
                            NOM_MED=champ,
                            NOM_CMP=nom_cmp,
                            NOM_CMP_MED=nom_cmp_med,
                                )

                    dicDetr.append({'NOM' : __SIG1[j]})
                    dicAsse.append({'TOUT' : 'OUI', 'CHAM_GD' : __SIG1[j],
                                    'NOM_CMP' : nom_cmp,
                                    'CUMUL' : 'OUI', 'COEF_R':1.})
                    j += 1
            else:
                nb_champ_cara = len(dic_champ_cont[mc_cara].keys())
                dicDetr_cara = []
                if nb_champ_cara == 1:
                    champ = dic_champ_cont[mc_cara].keys()[0]
                    mode_epx = dic_champ_cont[mc_cara][champ]
                    nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                    nom_cmp_med = info_mode_epx[mode_epx]['NOM_CMP_MED']
                    __SIG_AS = LIRE_CHAMP(
                            INFO=INFO,
                            TYPE_CHAM='ELGA_SIEF_R',
                            UNITE=99,
                            NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                            MODELE=MODELE,
                            MAILLAGE=MAILLAGE,
                            #PROL_ZERO='OUI',
                            NOM_MED=champ,
                            NOM_CMP=nom_cmp,
                            NOM_CMP_MED=nom_cmp_med,
                                )
                else:
                    __SIG = [None]*nb_champ_cara
                    dicAsse_cara = []
                    for k, champ in enumerate(dic_champ_cont[mc_cara].keys()):
                        mode_epx = dic_champ_cont[mc_cara][champ]
                        nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                        nom_cmp_med = info_mode_epx[mode_epx]['NOM_CMP_MED']
                        __SIG[k] = LIRE_CHAMP(
                                INFO=INFO,
                                TYPE_CHAM='ELGA_SIEF_R',
                                UNITE=99,
                                NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                                MODELE=MODELE,
                                MAILLAGE=MAILLAGE,
                                PROL_ZERO='OUI',
                                NOM_MED=champ,
                                NOM_CMP=nom_cmp,
                                NOM_CMP_MED=nom_cmp_med,
                                    )
                        dicAsse_cara.append({'TOUT' : 'OUI',
                                             'CHAM_GD' : __SIG[k],
                                             'NOM_CMP' : nom_cmp,
                                             'CUMUL' : 'OUI',
                                             'COEF_R':1.})
                        dicDetr_cara.append({'NOM' : __SIG[k]})
                    # assemblage
                    __SIG_AS = CREA_CHAMP(
                                    INFO=INFO,
                                    TYPE_CHAM='ELGA_SIEF_R',
                                    OPERATION='ASSE',
                                    PROL_ZERO='OUI',
                                    MODELE=MODELE,
                                    ASSE=dicAsse_cara,
                                    )
                dicDetr_cara.append({'NOM' : __SIG_AS})
                cham_para = (dic_mc_cara[mc_cara]['CH_CARA'], __SIG_AS)
                cham_fonc = dic_mc_cara[mc_cara]['CH_FONC']
                # EVAL : passage des contraintes aux efforts
                __SIG1[j] = CREA_CHAMP(OPERATION='EVAL',
                                        TYPE_CHAM='ELGA_NEUT_R',
                                        CHAM_F=cham_fonc,
                                        CHAM_PARA=cham_para,
                                        )
                dicDetr.append({'NOM' : __SIG1[j]})
                nom_cmp = dic_mc_cara[mc_cara]['NOM_CMP']
                nom_cmp_f = dic_mc_cara[mc_cara]['NOM_CMP_F']
                dicAsse.append({'TOUT' : 'OUI', 'CHAM_GD' : __SIG1[j],
                                'NOM_CMP' : nom_cmp_f,
                                'NOM_CMP_RESU' : nom_cmp,
                                'CUMUL' : 'OUI', 'COEF_R':1.})
                DETRUIRE(CONCEPT=dicDetr_cara, INFO=1)
                j += 1
        # VARIABLES INTERNES
        for compo in dic_champ_var_int.keys():
            j = 0
            if compo == 'SANS':
                for champ in dic_champ_var_int[compo].keys():
                    loi = dic_champ_var_int[compo][champ]
                    nb_var_aster = info_comp_epx[loi]['NB_VAR_ASTER']
                    nom_cmp = info_comp_epx[loi]['VAR_ASTER'][:nb_var_aster]
                    nom_cmp_med = info_comp_epx[loi]['VAR_EPX'][:nb_var_aster]
                    __ECR1[j] = LIRE_CHAMP(
                        INFO=INFO,
                        TYPE_CHAM='ELGA_VARI_R',
                        UNITE=99,
                        NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                        MODELE=MODELE,
                        MAILLAGE=MAILLAGE,
                        PROL_ZERO='OUI',
                        NOM_MED=champ,
                        NOM_CMP=nom_cmp,
                        NOM_CMP_MED=nom_cmp_med,
                        )
                    dicAsse3.append({'TOUT' : 'OUI', 'CHAM_GD' : __ECR1[j],
                                     'NOM_CMP': nom_cmp,
                                     'CUMUL' : 'OUI', 'COEF_R':1.})
                    dicDetr.append({'NOM' : __ECR1[j]})
                    j += 1
            else:
                nb_champ_transfo = len(dic_champ_var_int[compo].keys())
                dicDetr_transfo = []
                if nb_champ_transfo == 1:
                    champ = dic_champ_var_int[compo].keys()[0]
                    loi = dic_champ_var_int[compo][champ]
                    nb_var_epx = info_comp_epx[loi]['NB_VAR_EPX']
                    nom_cmp = info_comp_epx[loi]['VAR_ASTER'][:nb_var_epx]
                    nom_cmp_med = info_comp_epx[loi]['VAR_EPX'][:nb_var_epx]
                    __ECR_AS = LIRE_CHAMP(
                            INFO=INFO,
                            TYPE_CHAM='ELGA_VARI_R',
                            UNITE=99,
                            NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                            MODELE=MODELE,
                            MAILLAGE=MAILLAGE,
                            PROL_ZERO='OUI',
                            NOM_MED=champ,
                            NOM_CMP=nom_cmp,
                            NOM_CMP_MED=nom_cmp_med,)
                else:
                    __ECR = [None]*nb_champ_transfo
                    dicAsse_transfo = []
                    for k, champ in enumerate(dic_champ_var_int[compo].keys()):
                        loi = dic_champ_var_int[compo][champ]
                        nb_var_epx = info_comp_epx[loi]['NB_VAR_EPX']
                        nom_cmp = info_comp_epx[loi]['VAR_ASTER'][:nb_var_epx]
                        nom_cmp_med = info_comp_epx[loi]['VAR_EPX'][:nb_var_epx]
                        __ECR[k] = LIRE_CHAMP(
                            INFO=INFO,
                            TYPE_CHAM='ELGA_VARI_R',
                            UNITE=99,
                            NUME_PT=resu.LIST_PARA()['NUME_ORDRE'][i],
                            MODELE=MODELE,
                            MAILLAGE=MAILLAGE,
                            PROL_ZERO='OUI',
                            NOM_MED=champ,
                            NOM_CMP=nom_cmp,
                            NOM_CMP_MED=nom_cmp_med,)

                        dicAsse_transfo.append({'TOUT' : 'OUI',
                                                'CHAM_GD' : __ECR[k],
                                                'NOM_CMP' : nom_cmp,
                                                'CUMUL' : 'OUI',
                                                'COEF_R':1.})
                        dicDetr_transfo.append({'NOM' : __ECR[k]})
                    # assemblage
                    __ECR_AS = CREA_CHAMP(
                                    INFO=INFO,
                                    TYPE_CHAM='ELGA_VARI_R',
                                    OPERATION='ASSE',
                                    PROL_ZERO='OUI',
                                    MODELE=MODELE,
                                    ASSE=dicAsse_transfo,
                                    )
                dicDetr_transfo.append({'NOM' : __ECR_AS})
                cham_para = (dic_transfo[compo]['CH_CARA'], __ECR_AS)
                cham_fonc = dic_transfo[compo]['CH_FONC']
                # EVAL : passage des contraintes aux efforts
                __ECR1[j] = CREA_CHAMP(OPERATION='EVAL',
                                        TYPE_CHAM='ELGA_NEUT_R',
                                        CHAM_F=cham_fonc,
                                        CHAM_PARA=cham_para,
                                        )
                dicDetr.append({'NOM' : __ECR1[j]})
                nom_cmp = dic_transfo[compo]['NOM_CMP']
                nom_cmp_f = dic_transfo[compo]['NOM_CMP_F']
                dicAsse3.append({'TOUT' : 'OUI', 'CHAM_GD' : __ECR1[j],
                                 'NOM_CMP' : nom_cmp_f,
                                'NOM_CMP_RESU' : nom_cmp,
                                'CUMUL' : 'OUI',
                                'COEF_R':1.})
                DETRUIRE(CONCEPT=dicDetr_transfo, INFO=1)
                j += 1

        __EFFG[i] = CREA_CHAMP(
            INFO=INFO,
            TYPE_CHAM='ELGA_SIEF_R',
            OPERATION='ASSE',
            PROL_ZERO='OUI',
            MODELE=MODELE,
            ASSE=dicAsse,
            )
        dicAffe.append({'CHAM_GD' : __EFFG[i], 'MODELE' : MODELE,
                        'CHAM_MATER' : CHAM_MATER,
                        'CARA_ELEM' : CARA_ELEM,
                        'INST': resu.LIST_PARA()['INST'][i]})

        __ECRG[i] = CREA_CHAMP(
            INFO=INFO,
            TYPE_CHAM='ELGA_VARI_R',
            OPERATION='ASSE',
            PROL_ZERO='OUI',
            MODELE=MODELE,
            ASSE=dicAsse3)
        dicAffe3.append({'CHAM_GD' : __ECRG[i], 'MODELE' : MODELE,
                         'CHAM_MATER' : CHAM_MATER,
                         'CARA_ELEM' : CARA_ELEM,
                         'INST': resu.LIST_PARA()['INST'][i]})
        DETRUIRE(CONCEPT=dicDetr, INFO=1)

    MasquerAlarme('COMPOR2_26')
    MasquerAlarme('COMPOR2_23')
    resu = CREA_RESU(reuse=resu,
        OPERATION='AFFE',
        TYPE_RESU='EVOL_NOLI',
        NOM_CHAM='SIEF_ELGA',
        AFFE=dicAffe,
        )

    resu = CREA_RESU(reuse=resu,
        OPERATION='AFFE',
        TYPE_RESU='EVOL_NOLI',
        NOM_CHAM='VARI_ELGA',
        AFFE=dicAffe3,
        )
    RetablirAlarme('COMPOR2_26')
    RetablirAlarme('COMPOR2_23')

    if modi_repere['COQUE']:
        MODI_REPERE(RESULTAT=resu, reuse=resu,
                REPERE='COQUE_INTR_UTIL',
                MODI_CHAM=_F(TYPE_CHAM='COQUE_GENE',
                                NOM_CHAM='SIEF_ELGA',
                                NOM_CMP=('NXX', 'NYY', 'NXY',
                                            'MXX', 'MYY', 'MXY',
                                            'QX', 'QY')))

    DEFI_FICHIER(UNITE=unite, ACTION='LIBERER')

    os.remove(fort)
