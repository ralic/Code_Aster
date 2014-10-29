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
    Macro-commande LIRE_EUROPLEXUS principalement appelée par CALC_EUROPLEXUS.
    Lit le résultat EPX au format MED et construit un concept evol_noli
    à partir de celui-ci.
"""

import aster
from Accas import _F
import os
from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
from Calc_epx.calc_epx_cata import cata_modelisa, cata_compor


def lire_europlexus_ops(self, UNITE_MED, MODELE, COMPORTEMENT,
                        INFO=1, CHAM_MATER=None, CARA_ELEM=None, EXCIT=None,
                        **args):
    """Fonction d'appel de la macro LIRE_EUROPLEXUS"""
    self.set_icmd(1)

    # Le concept sortant (de type evol_noli) est nomme 'resu'
    self.DeclareOut('resu', self.sd)
    resu = None
    # global resu

    analysis = LireEPX(UNITE_MED, MODELE, CARA_ELEM,
                       CHAM_MATER, COMPORTEMENT, EXCIT,
                       INFO)
    analysis.read_compor()
    analysis.info_mode_epx, analysis.dic_mc_cara = build_info_mode_epx()
    analysis.info_comp_epx = build_info_comp_epx()
    analysis.prep_cont2effo()
    resu = analysis.lire_champs_noeud(resu)
    analysis.traite_champs_gauss(resu)


class LireEPX():

    """
        Classe pour la lecture d'un ficher résultat EPX en MED
        et sa transformation en objet evol_noli
    """

    def __init__(self, UNITE_MED, MODELE, CARA_ELEM,
                 CHAM_MATER, COMPORTEMENT, EXCIT,
                 INFO):
        """
            Initialisation
        """
        self.UNITE_MED = UNITE_MED
        self.MODELE = MODELE
        self.CARA_ELEM = CARA_ELEM
        self.CHAM_MATER = CHAM_MATER
        self.COMPORTEMENT = COMPORTEMENT
        self.EXCIT = EXCIT
        self.INFO = INFO
        self.fichier_med = 'fort.%s' % UNITE_MED

        # Récuperation des concepts de la base
        macro = CONTEXT.get_current_step()

        # récuperation du maillage
        nom_MODELE = self.MODELE.get_name()
        iret, ibid, nomsd = aster.dismoi('NOM_MAILLA', nom_MODELE, 'MODELE',
                                         'F')
        nomsd = nomsd.strip()
        self.MAILLAGE = macro.get_concept(nomsd)
    # -------------------------------------------------------------------------

    def read_compor(self):
        """
            Lecture des comportements associés aux groupes de maille.
            Stockage des infos dans un dictionnaire dont les clés sont les
            matériaux Europlexus et dont les valeurs sont des listes de groupes
            de maille.
            Ce dictionnaire est utilisé lors de la récupécation
            des variables internes.
        """
        from Calc_epx.calc_epx_cata import cata_compor
        dic_compo_gr = {}
        for dic in self.COMPORTEMENT:
            compo = dic['RELATION']
            if cata_compor[compo].has_key('NOM_EPX_CH_MED'):
                comp_epx = cata_compor[compo]['NOM_EPX_CH_MED'][:4]
            else:
                comp_epx = cata_compor[compo]['NOM_EPX'][:4]
            if not dic_compo_gr.has_key(comp_epx):
                dic_compo_gr[comp_epx] = []
            dic_compo_gr[comp_epx].extend(dic['GROUP_MA'])
        self.compor = dic_compo_gr
    # -------------------------------------------------------------------------

    def prep_cont2effo(self,):
        """
            Construction des champs pour le passage des contraintes aux efforts
            Complète dic_mc_cara avec ces infos.
        """

        from Cata.cata import FORMULE, CREA_CHAMP
        from Calc_epx.calc_epx_utils import recupere_structure, tolist
        from Calc_epx.calc_epx_utils import get_group_ma
        from Calc_epx.calc_epx_cara import export_cara
        from Calc_epx.calc_epx_struc import DIRECTIVE
        #  1- RECUPERATION DES INFOS
        dic_mc_cara = self.dic_mc_cara

        if self.CARA_ELEM is None:
            return
        cara_elem_struc = recupere_structure(self.CARA_ELEM)

        for mc_cara in dic_mc_cara.keys():
            if not cara_elem_struc.has_key(mc_cara):
                continue
            donnees_cara = tolist(cara_elem_struc[mc_cara])

            epx = {}
            epx['COMPLEMENT'] = DIRECTIVE('COMPLEMENT', ' ', 2)
            mode_from_cara = {}
            dic_gr_cara_supp = {}
            # analyse du cara_elem
            [epx, mode_from_cara] = export_cara(mc_cara, epx,
                                                cara_elem_struc[mc_cara],
                                                None, None,
                                                dic_gr_cara_supp, mode_from_cara)

            # COQUE -------------------------------------------------------
            if mc_cara == 'COQUE':

                if len(dic_mc_cara[mc_cara]['INSTANCE']) == 0:
                    nbcomp = len(dic_mc_cara[mc_cara]['NOM_CMP'])
                    __FO_CO = [None] * nbcomp
                    nume_X = [1, 1, 1, 2, 2, 2, 1, 1]
                    vale_f = []
                    nom_cmp_f = []
                    for i, comp in enumerate(dic_mc_cara[mc_cara]['NOM_CMP']):
                        xnum_i = 'X%s' % (nume_X[i])
                        xi = 'X%s' % (i + 1)
                        val_fonc = comp + '*' + xnum_i
                        nom_para = (comp, xnum_i)
                        __FO_CO[i] = FORMULE(VALE=val_fonc, NOM_PARA=nom_para)
                        vale_f.append(__FO_CO[i])
                        nom_cmp_f.append(xi)
                    dic_mc_cara[mc_cara]['VALE_F'] = vale_f
                    dic_mc_cara[mc_cara]['NOM_CMP_F'] = nom_cmp_f

                for instance in donnees_cara:
                    epais = instance['EPAIS']
                    gr = get_group_ma(instance)
                    dic_mc_cara[mc_cara]['GROUP_MA'].extend(gr)
                    dic_mc_cara[mc_cara]['INSTANCE'].append({'VALE': epais,
                                                             'GROUP_MA': gr,
                                                             'NOM_CMP': 'X1'})
                    dic_mc_cara[
                        mc_cara]['INSTANCE'].append({'VALE': epais ** 2 / 6.,
                                                     'GROUP_MA': gr,
                                                     'NOM_CMP': 'X2'})
            # BARRE -------------------------------------------------------
            elif mc_cara == 'BARRE':

                if len(dic_mc_cara[mc_cara]['INSTANCE']) == 0:
                    nbcomp = len(dic_mc_cara[mc_cara]['NOM_CMP'])
                    __FO_BA = [None] * nbcomp
                    nume_X = [1, ]
                    vale_f = []
                    nom_cmp_f = []
                    for i, comp in enumerate(dic_mc_cara[mc_cara]['NOM_CMP']):
                        xnum_i = 'X%s' % (nume_X[i])
                        xi = 'X%s' % (i + 1)
                        val_fonc = comp + '*' + xnum_i
                        nom_para = (comp, xnum_i)
                        __FO_BA[i] = FORMULE(VALE=val_fonc, NOM_PARA=nom_para)
                        vale_f.append(__FO_BA[i])
                        nom_cmp_f.append(xi)
                    # creation du champ de fonction
                    dic_mc_cara[mc_cara]['VALE_F'] = vale_f
                    dic_mc_cara[mc_cara]['NOM_CMP_F'] = nom_cmp_f

                for instance in donnees_cara:
                    cara = instance['CARA']
                    cara = tolist(cara)
                    if len(cara) != 1 or cara[0] != 'A':
                        raise Exception("""si on tombe la on utilise LIRE_EPX
                        en dehors de CALC_EPX.
                        Il faut ajouter une analyse du CARA_ELEM.
                        """)
                    aire_sect = tolist(instance['VALE'])[0]
                    gr = get_group_ma(instance)
                    dic_mc_cara[mc_cara]['GROUP_MA'].extend(gr)

                    dic_mc_cara[
                        mc_cara]['INSTANCE'].append({'VALE': aire_sect,
                                                     'GROUP_MA': gr,
                                                     'NOM_CMP': 'X1'})

            # CAS NON DEVELOPPES ------------------------------------------
            elif mc_cara in dic_mc_cara.keys():
                raise Exception("""
Le passage des contraintes aux efforts n'est pas programmé pour
le mot-clé %s""" % mc_cara)

        # 2- CREATION DES CHAMPS DE CARACTERISTIQUES ET DE FONCTIONS
        # POUR CONTRAINTES
        nb_cara = len(dic_mc_cara.keys())
        __CH_CAR = [None] * nb_cara
        __CH_FON = [None] * nb_cara
        for icar, mc_cara in enumerate(dic_mc_cara.keys()):
            if len(dic_mc_cara[mc_cara]['INSTANCE']) > 0:

                __CH_CAR[icar] = CREA_CHAMP(
                    INFO=self.INFO,
                    TYPE_CHAM='ELGA_NEUT_R',
                    OPERATION='AFFE',
                    MODELE=self.MODELE,
                    PROL_ZERO='OUI',
                    AFFE=dic_mc_cara[mc_cara]['INSTANCE'],
                )
                dic_mc_cara[mc_cara]['CH_CARA'] = __CH_CAR[icar]
                nom_cmp_f = dic_mc_cara[mc_cara]['NOM_CMP_F']
                vale_f = dic_mc_cara[mc_cara]['VALE_F']
                gr = dic_mc_cara[mc_cara]['GROUP_MA']
                __CH_FON[icar] = CREA_CHAMP(
                    INFO=self.INFO,
                    TYPE_CHAM='ELGA_NEUT_F',
                    OPERATION='AFFE',
                    MODELE=self.MODELE,
                    PROL_ZERO='OUI',
                    AFFE=_F(
                        GROUP_MA=gr,
                        NOM_CMP=nom_cmp_f,
                        VALE_F=vale_f),
                )
                dic_mc_cara[mc_cara]['CH_FONC'] = __CH_FON[icar]
    # -------------------------------------------------------------------------

    def lire_champs_noeud(self, resu):
        """
            Lecture des champs aux noeuds dans le fichier MED.
            Création d'un résultat ASTER avec ces champs.
        """
        from Cata.cata import LIRE_RESU
        from Calc_epx.calc_epx_cata import format_med_6ddl, format_med_3ddl
        import med_aster

        # RECUPERATION DES DEPL, VITE et ACCE DANS LE FICHIER MED
        dic_champ_med = med_aster.get_nom_champ_med(self.fichier_med)
        nb_ddl = len(dic_champ_med['DEPL_001'])
        if nb_ddl == 3:
            format_med = format_med_3ddl
        elif nb_ddl == 6:
            format_med = format_med_6ddl
        else:
            raise Exception(
                '%s ddls pour les noeuds du fichier med EPX non prévu')

        lire_resu = {
            'TYPE_RESU': 'EVOL_NOLI',
            'FORMAT': 'MED',
            'MODELE':  self.MODELE,
            'FORMAT_MED': format_med,
            'UNITE': self.UNITE_MED,
            'TOUT_ORDRE': 'OUI',
            'INFO': self.INFO,
        }
        if self.CARA_ELEM is not None:
            lire_resu['CARA_ELEM'] = self.CARA_ELEM
        if self.CHAM_MATER is not None:
            lire_resu['CHAM_MATER'] = self.CHAM_MATER
        if self.EXCIT is not None:
            # Regeneration des mots-cles EXCIT passés en argument de la macro
            dExcit = []
            for j in self.EXCIT:
                dExcit.append(j.cree_dict_valeurs(j.mc_liste))
                for i in dExcit[-1].keys():
                    if dExcit[-1][i] == None:
                        del dExcit[-1][i]

        resu = LIRE_RESU(**lire_resu)
        return resu
    # -------------------------------------------------------------------------

    def traite_champs_gauss(self, resu):
        """
            Lit et met en forme dans un résultat ASTER les champs aux points de
            de Gauss présents dans le fichier MED.
        """

        dic_champ_cont, dic_champ_var_int = self.lire_champs_gauss()
        dic_transfo = self.transfo_var_int(dic_champ_var_int)
        self.asse_champs_gauss(dic_champ_cont, dic_champ_var_int, dic_transfo,
                               resu)
    # -------------------------------------------------------------------------

    def lire_champs_gauss(self):
        """
            Récupération des champs
        """

        import med_aster
        dic_champ_med = med_aster.get_nom_champ_med(self.fichier_med)
        info_mode_epx = self.info_mode_epx
        info_comp_epx = self.info_comp_epx

        dic_champ_cont = {'SANS': {}}
        dic_champ_var_int = {'SANS': {}}

        modi_repere = {'COQUE': False}
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
    dans un champ.""" % mode_epx)
                nbcomp = len(dic_champ_med[nom_cham_med])
                nbcomp_ref = len(info_mode_epx[mode_epx]['NOM_CMP'])
                if nbcomp != nbcomp_ref:
                    raise Exception("""
    Le champ de type %s sur les éléments %s ne comporte pas le bon nombre
    de composantes""" % (type_cham, mode_epx))
                mc_cara = info_mode_epx[mode_epx]['MC_CARA']
                type_modi = info_mode_epx[mode_epx]['MODI_REPERE']
                if type_modi is not None:
                    modi_repere[type_modi] = True
                if mc_cara:
                    if not dic_champ_cont.has_key(mc_cara):
                        dic_champ_cont[mc_cara] = {}
                    dic_champ_cont[mc_cara][nom_cham_med] = mode_epx
                else:
                    mc_cara = 'SANS'
                    dic_champ_cont[mc_cara][nom_cham_med] = mode_epx

            if type_cham == "ECRO":
                if not info_comp_epx.has_key(loi):
                    raise Exception(
                        "La loi %s n'est pas encore programmée mais est présente dans un champ." % loi)
                nbcomp = len(dic_champ_med[nom_cham_med])
                nbcomp_ref = info_comp_epx[loi]['NB_VAR_EPX']
                if nbcomp != nbcomp_ref:
                    raise Exception("""
    Le champ de type %s pour la loi %s ne comporte pas le bon nombre de
    composantes.
    Nombre de composantes trouvées  : %s
    Nombre de composantes attendues : %s
    """ % (type_cham, loi, nbcomp, nbcomp_ref))
                if info_comp_epx[loi]['TRANSFO']:
                    if not dic_champ_var_int.has_key(loi):
                        dic_champ_var_int[loi] = {}
                    dic_champ_var_int[loi][nom_cham_med] = loi
                else:
                    dic_champ_var_int['SANS'][nom_cham_med] = loi
        self.modi_repere = modi_repere
        return dic_champ_cont, dic_champ_var_int
    # -------------------------------------------------------------------------

    def transfo_var_int(self, dic_champ_var_int):
        """
            Création des champs pour transformation des variables internes
        """
        from Calc_epx.trans_var_int import *

        dic_transfo = {}
        nb_compo = len(dic_champ_var_int.keys()) - 1
        __CH_FOV = [None] * nb_compo
        ico = 0

        cc = {
            'INFO': self.INFO,
            'OPERATION': 'AFFE',
            'MODELE': self.MODELE,
            'PROL_ZERO': 'OUI',
        }

        for compo in dic_champ_var_int.keys():
            if compo == 'SANS':
                continue

            nb_comp = self.info_comp_epx[compo]['NB_VAR_ASTER']
            var_aster = self.info_comp_epx[compo]['VAR_ASTER']
            gr_ma = self.compor[compo]
            # glrc_damage
            if compo == 'GLRC':
                dic_transfo[compo] = tr_e2a_glrc_damage(__CH_FOV, ico, cc,
                                                        nb_comp, var_aster, gr_ma)
            # vmis_isot_trac
            elif compo == 'ISOT':
                dic_transfo[compo] = tr_e2a_vmis_isot_trac(__CH_FOV, ico, cc,
                                                           nb_comp, var_aster, gr_ma)
            else:
                raise Exception("""
Les transformations à apporter aux variables internes pour la loi ne sont pas
présentes%s""" % compo)

            ico += 1

        return dic_transfo
    # -------------------------------------------------------------------------

    def asse_champs_gauss(self, dic_champ_cont, dic_champ_var_int, dic_transfo,
                          resu):
        """
            Transformation et assemblage des champs aux points de Gauss.
        """
        from Cata.cata import LIRE_CHAMP, CREA_CHAMP, DETRUIRE
        from Cata.cata import CREA_RESU, MODI_REPERE

        info_mode_epx = self.info_mode_epx
        info_comp_epx = self.info_comp_epx
        dic_mc_cara = self.dic_mc_cara

        ll = len(dic_champ_cont.keys()) - 1
        nb_SIG1 = len(dic_champ_cont['SANS'].keys()) + ll
        ll = len(dic_champ_var_int.keys()) - 1
        nb_ECR1 = len(dic_champ_var_int['SANS'].keys()) + ll
        itot = len(resu.LIST_PARA()['INST'])
        __EFFG = [None] * itot
        __ECRG = [None] * itot
        __SIG1 = [None] * nb_SIG1
        __ECR1 = [None] * nb_ECR1
        dicAffe = []
        dicAffe3 = []

        lc = {
            'INFO': self.INFO,
            'UNITE': self.UNITE_MED,
            'MODELE': self.MODELE,
            'MAILLAGE': self.MAILLAGE,
            'PROL_ZERO': 'OUI',
        }
        cc = {
            'INFO': self.INFO,
            'MODELE': self.MODELE,
            'PROL_ZERO': 'OUI',
            'OPERATION': 'ASSE',
        }
        MasquerAlarme('MED_4')
        for i in xrange(itot):

            lc['NUME_PT'] = resu.LIST_PARA()['NUME_ORDRE'][i]

            dicAsse = []
            dicAsse3 = []
            dicDetr = []

            # CONTRAINTES

            lc['TYPE_CHAM'] = 'ELGA_SIEF_R'
            for mc_cara in dic_champ_cont.keys():
                j = 0
                if mc_cara == 'SANS':
                    for champ in dic_champ_cont[mc_cara].keys():
                        mode_epx = dic_champ_cont[mc_cara][champ]
                        nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                        nom_cmp_med = info_mode_epx[mode_epx]['NOM_CMP_MED']
                        lcc = lc.copy()
                        lcc.update(NOM_MED=champ,
                                   NOM_CMP=nom_cmp,
                                   NOM_CMP_MED=nom_cmp_med,)
                        __SIG1[j] = LIRE_CHAMP(**lcc)

                        dicDetr.append({'NOM': __SIG1[j]})
                        dicAsse.append({'TOUT': 'OUI', 'CHAM_GD': __SIG1[j],
                                        'NOM_CMP': nom_cmp,
                                        'CUMUL': 'OUI', 'COEF_R': 1.})
                        j += 1
                else:
                    nb_champ_cara = len(dic_champ_cont[mc_cara].keys())
                    dicDetr_cara = []
                    if nb_champ_cara == 1:
                        champ = dic_champ_cont[mc_cara].keys()[0]
                        mode_epx = dic_champ_cont[mc_cara][champ]
                        nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                        nom_cmp_med = info_mode_epx[mode_epx]['NOM_CMP_MED']
                        lcc = lc.copy()
                        lcc.update(NOM_MED=champ,
                                   NOM_CMP=nom_cmp,
                                   NOM_CMP_MED=nom_cmp_med,)
                        __SIG_AS = LIRE_CHAMP(**lcc)
                    else:
                        __SIG = [None] * nb_champ_cara
                        dicAsse_cara = []
                        for k, champ in enumerate(dic_champ_cont[mc_cara].keys()):
                            mode_epx = dic_champ_cont[mc_cara][champ]
                            nom_cmp = info_mode_epx[mode_epx]['NOM_CMP']
                            nom_cmp_med = info_mode_epx[
                                mode_epx]['NOM_CMP_MED']
                            lcc = lc.copy()
                            lcc.update(NOM_MED=champ,
                                       NOM_CMP=nom_cmp,
                                       NOM_CMP_MED=nom_cmp_med,
                                       )
                            __SIG[k] = LIRE_CHAMP(**lcc)
                            dicAsse_cara.append({'TOUT': 'OUI',
                                                 'CHAM_GD': __SIG[k],
                                                 'NOM_CMP': nom_cmp,
                                                 'CUMUL': 'OUI',
                                                 'COEF_R': 1.})
                            dicDetr_cara.append({'NOM': __SIG[k]})
                        # assemblage
                        ccc = cc.copy()
                        ccc.update(TYPE_CHAM='ELGA_SIEF_R',
                                   ASSE=dicAsse_cara,)
                        __SIG_AS = CREA_CHAMP(**ccc)
                    dicDetr_cara.append({'NOM': __SIG_AS})
                    cham_para = (dic_mc_cara[mc_cara]['CH_CARA'], __SIG_AS)
                    cham_fonc = dic_mc_cara[mc_cara]['CH_FONC']
                    # EVAL : passage des contraintes aux efforts
                    __SIG1[j] = CREA_CHAMP(OPERATION='EVAL',
                                           TYPE_CHAM='ELGA_NEUT_R',
                                           CHAM_F=cham_fonc,
                                           CHAM_PARA=cham_para,
                                           )
                    dicDetr.append({'NOM': __SIG1[j]})
                    nom_cmp = dic_mc_cara[mc_cara]['NOM_CMP']
                    nom_cmp_f = dic_mc_cara[mc_cara]['NOM_CMP_F']
                    dicAsse.append({'TOUT': 'OUI', 'CHAM_GD': __SIG1[j],
                                    'NOM_CMP': nom_cmp_f,
                                    'NOM_CMP_RESU': nom_cmp,
                                    'CUMUL': 'OUI', 'COEF_R': 1.})
                    DETRUIRE(CONCEPT=dicDetr_cara, INFO=1)
                    j += 1
            # VARIABLES INTERNES
            lc['TYPE_CHAM'] = 'ELGA_VARI_R'

            for compo in dic_champ_var_int.keys():
                j = 0
                if compo == 'SANS':
                    for champ in dic_champ_var_int[compo].keys():
                        loi = dic_champ_var_int[compo][champ]
                        nb_var_aster = info_comp_epx[loi]['NB_VAR_ASTER']
                        nom_cmp = info_comp_epx[loi][
                            'VAR_ASTER'][:nb_var_aster]
                        nom_cmp_med = info_comp_epx[
                            loi]['VAR_EPX'][:nb_var_aster]
                        lcc = lc.copy()
                        lcc.update(NOM_MED=champ,
                                   NOM_CMP=nom_cmp,
                                   NOM_CMP_MED=nom_cmp_med,)
                        __ECR1[j] = LIRE_CHAMP(**lcc)
                        dicAsse3.append({'TOUT': 'OUI', 'CHAM_GD': __ECR1[j],
                                         'NOM_CMP': nom_cmp,
                                         'CUMUL': 'OUI', 'COEF_R': 1.})
                        dicDetr.append({'NOM': __ECR1[j]})
                        j += 1
                else:
                    nb_champ_transfo = len(dic_champ_var_int[compo].keys())
                    dicDetr_transfo = []
                    if nb_champ_transfo == 1:
                        champ = dic_champ_var_int[compo].keys()[0]
                        loi = dic_champ_var_int[compo][champ]
                        nb_var_epx = info_comp_epx[loi]['NB_VAR_EPX']
                        nom_cmp = info_comp_epx[loi]['VAR_ASTER'][:nb_var_epx]
                        nom_cmp_med = info_comp_epx[
                            loi]['VAR_EPX'][:nb_var_epx]
                        lcc = lc.copy()
                        lcc.update(NOM_MED=champ,
                                   NOM_CMP=nom_cmp,
                                   NOM_CMP_MED=nom_cmp_med,)
                        __ECR_AS = LIRE_CHAMP(**lcc)
                    else:
                        __ECR = [None] * nb_champ_transfo
                        dicAsse_transfo = []
                        for k, champ in enumerate(dic_champ_var_int[compo].keys()):
                            loi = dic_champ_var_int[compo][champ]
                            nb_var_epx = info_comp_epx[loi]['NB_VAR_EPX']
                            nom_cmp = info_comp_epx[loi][
                                'VAR_ASTER'][:nb_var_epx]
                            nom_cmp_med = info_comp_epx[
                                loi]['VAR_EPX'][:nb_var_epx]
                            lcc = lc.copy()
                            lcc.update(NOM_MED=champ,
                                       NOM_CMP=nom_cmp,
                                       NOM_CMP_MED=nom_cmp_med,)
                            __ECR[k] = LIRE_CHAMP(**lcc)

                            dicAsse_transfo.append({'TOUT': 'OUI',
                                                    'CHAM_GD': __ECR[k],
                                                    'NOM_CMP': nom_cmp,
                                                    'CUMUL': 'OUI',
                                                    'COEF_R': 1.})
                            dicDetr_transfo.append({'NOM': __ECR[k]})
                        # assemblage
                        ccc = cc.copy()
                        ccc.update(TYPE_CHAM='ELGA_VARI_R',
                                   ASSE=dicAsse_transfo,)
                        __ECR_AS = CREA_CHAMP(**ccc)
                    dicDetr_transfo.append({'NOM': __ECR_AS})
                    cham_para = __ECR_AS
                    cham_fonc = dic_transfo[compo]['CH_FONC']
                    # EVAL : passage des contraintes aux efforts
                    __ECR1[j] = CREA_CHAMP(OPERATION='EVAL',
                                           TYPE_CHAM='ELGA_NEUT_R',
                                           CHAM_F=cham_fonc,
                                           CHAM_PARA=cham_para,
                                           )
                    dicDetr.append({'NOM': __ECR1[j]})
                    nom_cmp = dic_transfo[compo]['NOM_CMP']
                    nom_cmp_f = dic_transfo[compo]['NOM_CMP_F']
                    dicAsse3.append({'TOUT': 'OUI', 'CHAM_GD': __ECR1[j],
                                     'NOM_CMP': nom_cmp_f,
                                    'NOM_CMP_RESU': nom_cmp,
                                    'CUMUL': 'OUI',
                                    'COEF_R': 1.})
                    DETRUIRE(CONCEPT=dicDetr_transfo, INFO=1)
                    j += 1

            ccc = cc.copy()
            ccc.update(TYPE_CHAM='ELGA_SIEF_R',
                       ASSE=dicAsse,)
            __EFFG[i] = CREA_CHAMP(**ccc)

            dic = {
                'MODELE': self.MODELE,
                'INST': resu.LIST_PARA()['INST'][i],
            }
            if self.CHAM_MATER is not None:
                dic['CHAM_MATER'] = self.CHAM_MATER
            if self.CARA_ELEM is not None:
                dic['CARA_ELEM'] = self.CARA_ELEM

            dic['CHAM_GD'] = __EFFG[i]
            dicAffe.append(dic)

            ccc = cc.copy()
            ccc.update(TYPE_CHAM='ELGA_VARI_R',
                       ASSE=dicAsse3)
            __ECRG[i] = CREA_CHAMP(**ccc)

            dic2 = dic.copy()
            dic2['CHAM_GD'] = __ECRG[i]
            dicAffe3.append(dic2)

            DETRUIRE(CONCEPT=dicDetr, INFO=1)
        RetablirAlarme('MED_4')

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

        if self.modi_repere['COQUE']:
            from Cata.cata import MODI_REPERE
            MODI_REPERE(RESULTAT=resu, reuse=resu,
                        REPERE='COQUE_INTR_UTIL',
                        MODI_CHAM=_F(TYPE_CHAM='COQUE_GENE',
                                     NOM_CHAM='SIEF_ELGA',
                                     NOM_CMP=('NXX', 'NYY', 'NXY',
                                              'MXX', 'MYY', 'MXY',
                                              'QX', 'QY')))

    # -------------------------------------------------------------------------
# fin classe LireEPX


def build_info_mode_epx():
    """
    Mise en forme des informations de modelisation dans le dictionnaire
    info_mode_epx et initialisation du dictionnaire dic_mc_cara.
    """
    info_mode_epx = {}
    prexif_epx_cont = 'SIG'
    dic_mc_cara = {}
    for mode_aster in cata_modelisa.keys():
        dic_mode = cata_modelisa[mode_aster]
        if dic_mode.has_key('CONT_ASTER'):
            nom_cmp = dic_mode['CONT_ASTER']
            nom_cmp_med = []
            for i in range(1, len(dic_mode['CONT_ASTER']) + 1):
                nom_cmp_med.append(prexif_epx_cont + "%s" % i)
        else:
            nom_cmp = None
            nom_cmp_med = None
        if dic_mode.has_key('MC_CARA'):
            mc_cara = dic_mode['MC_CARA']
            if not dic_mc_cara.has_key(mc_cara):
                dic_mc_cara[mc_cara] = {'INSTANCE': [],
                                        'NOM_CMP': nom_cmp,
                                        'CH_FONC': None,
                                        'GROUP_MA': []}
        else:
            mc_cara = None
        if dic_mode.has_key('MODI_REPERE'):
            type_modi = dic_mode['MODI_REPERE']
        else:
            type_modi = None

        for typ_ma in dic_mode['MODE_EPX']:
            for mode_epx in dic_mode['MODE_EPX'][typ_ma]:
                info_mode_epx[mode_epx] = {'NOM_CMP': nom_cmp,
                                           'NOM_CMP_MED': nom_cmp_med,
                                           'MC_CARA': mc_cara,
                                           'MODI_REPERE': type_modi,
                                           }
    return info_mode_epx, dic_mc_cara


def build_info_comp_epx():
    """
    Mise en forme des informations sur les lois de comportement dans le
    dictionnaire info_comp_epx et initialisation du dictionnaire dic_mc_cara.
    """
    info_comp_epx = {}
    prexif_epx_vari = 'VAR'
    prexif_aster_vari = 'V'
    for nom_comp in cata_compor.keys():
        comp_aster = cata_compor[nom_comp]
        if not comp_aster.has_key('NOM_EPX'):
            continue
        if comp_aster.has_key('NOM_EPX_CH_MED'):
            nom_epx = comp_aster['NOM_EPX_CH_MED'][:4]
        else:
            nom_epx = comp_aster['NOM_EPX'][:4]
        if not info_comp_epx.has_key(nom_epx):
            nb_var_aster = comp_aster['NB_VAR_ASTER']
            nb_var_epx = comp_aster['NB_VAR_EPX']
            var_aster = []
            var_epx = []
            for ivar in range(1, max([nb_var_aster, nb_var_epx]) + 1):
                var_aster.append(prexif_aster_vari + str(ivar))
                var_epx.append(prexif_epx_vari + str(ivar))

            if ((not comp_aster['TRANSFO']) and
                    (nb_var_aster > nb_var_epx)):
                raise Exception("""
Pour la loi %s, Aster possède plus de variables internes que EPX,
or vous n'avez pas activer le mot-clé TRANSFO pour indiqué comment
retrouver les variables internes Aster""" % nom_comp)

            # attention la longueur de var_aster peut ne pas être égale
            # a nb_var_aster . Idem pour epx
            dic = {
                'NB_VAR_ASTER': nb_var_aster,
                'NB_VAR_EPX': nb_var_epx,
                'VAR_ASTER': var_aster,
                'VAR_EPX': var_epx,
                'TRANSFO': comp_aster['TRANSFO'],
            }
            info_comp_epx[nom_epx] = dic
        else:
            raise Exception("""
Plusieurs comportements Aster correspondent à un même mot-clé EPX
sur 4 caractères""")
    return info_comp_epx
