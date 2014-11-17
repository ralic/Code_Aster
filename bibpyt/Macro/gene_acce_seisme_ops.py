# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: irmela.zentner at edf.fr


"""Commande GENE_ACCE_SEISME"""

import sys
import traceback
from math import pi, ceil, exp, sqrt, log
import numpy as NP
from Accas import _F
from Utilitai.Utmess import UTMESS
from Cata_Utils.t_fonction import t_fonction
from Utilitai.Table import Table
from Utilitai.optimize import fmin

from Utilitai.random_signal_utils import (
    DSP2ACCE1D, itersim_SRO, gene_traj_gauss_evol1D, Rice2,
    peak, SRO2DSP, DSP2FR, corrcoefmodel, RAND_DSP, RAND_VEC,
    calc_dsp_KT, f_ARIAS, f_ARIAS_TSM, fonctm_gam, dsp_filtre_CP,
    fonctm_JetH, acce_filtre_CP, f_opta, f_opt1, f_opt2
)


def gene_acce_seisme_ops(self, **kwargs):
    """Corps de la macro GENE_ACCE_SEIMSE"""
    self.set_icmd(1)
    ier = 0
    # conteneur des paramètres du calcul
    params = GeneAcceParameters(**kwargs)
    if params.seed:
        NP.random.seed(params.seed)
    # création de l'objet generator
    generator = Generator.factory(self, params)
    try:
        generator.run()
    except Exception, err:
        trace = ''.join(traceback.format_tb(sys.exc_traceback))
        UTMESS('F', 'SUPERVIS2_5', valk=('GENE_ACCE_SEISME', trace, str(err)))


class GeneAcceParameters(object):

    def __init__(self, **kwargs):
        """Enregistrement des valeurs des mots-clés dans un dictionnaire.
        - On cree deux dictionnaires de parametres:
                      modulation_keys et method_keys
        """
        # GeneralKeys
        self.args = kwargs
        self.seed = kwargs.get('INIT_ALEA')
        self.norme = kwargs.get('PESANTEUR')
        # ModulationKeys
        modkeys = kwargs.get('MODULATION')[0]
        keys = self.modulation_keys = modkeys.cree_dict_valeurs(
            modkeys.mc_liste)
        keys.update({'DUREE_PHASE_FORTE': kwargs.get('DUREE_PHASE_FORTE'), })
        keys.update({'NORME': kwargs.get('PESANTEUR'), })
        keys.update({'INFO': kwargs.get('INFO'), })
        if keys.has_key('ECART_TYPE'):
            if keys['ECART_TYPE']:
                keys['ECART_TYPE'] = keys['ECART_TYPE'] * self.norme
                del keys['ACCE_MAX'],
                del keys['INTE_ARIAS']
            elif keys['ACCE_MAX']:
                keys['ACCE_MAX'] = keys['ACCE_MAX'] * self.norme
                del keys['ECART_TYPE'],
                del keys['INTE_ARIAS']
            elif keys['INTE_ARIAS']:
                del keys['ECART_TYPE'],
                del keys['ACCE_MAX']
        # OtherKeys
        others = kwargs.keys()
        others.remove('MODULATION')
        # GeneratorKeys
        if kwargs.get('DSP'):
            self.cas = 'DSP'
            self.specmethode = None
            GeneratorKeys = kwargs.get('DSP')[0]
            method_keys = GeneratorKeys.cree_dict_valeurs(
                GeneratorKeys.mc_liste)
            others.remove('DSP')
        else:
            self.cas = 'SPECTRE'
            if kwargs.get('SPEC_FRACTILE'):
                self.specmethode = 'SPEC_FRACTILE'
            elif kwargs.get('SPEC_MEDIANE'):
                self.specmethode = 'SPEC_MEDIANE'
                if kwargs.get('NB_TIRAGE') == 1:
                    UTMESS('F', 'SEISME_38')
            elif kwargs.get('SPEC_UNIQUE'):
                self.specmethode = 'SPEC_UNIQUE'

            GeneratorKeys = kwargs.get(self.specmethode)[0]
            method_keys = GeneratorKeys.cree_dict_valeurs(
                GeneratorKeys.mc_liste)
            others.remove(self.specmethode)
        self.method_keys = {}
        for key in method_keys:
            if method_keys[key] != None:
                self.method_keys[key] = method_keys[key]
        # OtherKeys remplissage
        other_keys = {}
        for key in others:
            other_keys[key] = kwargs.get(key)
        other_keys['FREQ_FILTRE'] = kwargs.get('FREQ_FILTRE')
        other_keys['FREQ_CORNER'] = kwargs.get('FREQ_CORNER')
        self.method_keys.update(other_keys)


class Generator(object):

    """Base class Generator"""

    @staticmethod
    def factory(macro, params):
        """create an instance of the appropriated type of Generator"""
        if params.cas == 'DSP':
            return GeneratorDSP(macro, params)
        elif params.cas == 'SPECTRE':
            return GeneratorSpectrum(macro, params)
        else:
            raise ValueError('unknown configuration')

    def __init__(self, macro, params):
        """Constructor Base class"""
        self.name = macro.sd.nom
        self.macro = macro
        self.norme = params.norme
        self.INFO = params.method_keys['INFO']
        self.modul_params = params.modulation_keys
        self.method_params = params.method_keys
        self.specmethode = params.specmethode
        self.FREQ_FILTRE = params.method_keys['FREQ_FILTRE']
        self.FREQ_CORNER = params.method_keys['FREQ_CORNER']
        self.DSP_args = {}
        self.SRO_args = {'NORME': self.norme}
        self.ntir = 0
        self.sampler = Sampler(params.modulation_keys, params.method_keys)
        # modulation indépendant de DSP/SPECTRE mais dépend de sampler:
        self.modulator = Modulator.factory(params.modulation_keys)
        # parametres des t_fonctions  a creer
        self.para_fonc_traj = {
            'NOM_PARA': 'INST', 'NOM_RESU': 'ACCE', 'PROL_DROITE': 'EXCLU',
            'PROL_GAUCHE': 'EXCLU', 'TITRE': params.method_keys['TITRE'], }
        self.para_dsp = {
            'INTERPOL': ['LIN', 'LIN'], 'NOM_PARA': 'FREQ',
            'PROL_DROITE': 'CONSTANT', 'PROL_GAUCHE': 'EXCLU',
            'NOM_RESU': 'ACCE'}
        self.para_sro = self.para_dsp

    def sampling(self):
        """run sampling"""
        self.sampler.run()

    def modulation(self):
        """run modulation"""
        self.modulator.run(self.sampler.liste_temps, self.sampler.DUREE_SIGNAL)

    def compute_TimeHistory(self, Xt):
        """apply modulation and low pass filter if requested"""
        Xm = Xt * self.modulator.fonc_modul.vale_y
        if self.FREQ_FILTRE > 0.0:
            Xm = acce_filtre_CP(Xm, self.sampler.DT, self.FREQ_FILTRE)
        return Xm

    def prepare_data(self):
        """specific to each method"""
        raise NotImplementedError('must be implemented in a subclass')

    def build_DSP(self):
        """specific to each method"""
        raise NotImplementedError('must be implemented in a subclass')

    def build_result(self):
        """specific to each method"""
        raise NotImplementedError('must be implemented in a subclass')

    def run(self):
        """Generate the signal"""
        self.sampling()
        self.modulation()
        self.prepare_data()
        self.build_DSP()
        self.build_result()


class GeneratorDSP(Generator):

    """DSP class"""

    def prepare_data(self):
        """prepare data for DSP class"""
        self.DSP_args.update({
            'FREQ_FOND': self.method_params['FREQ_FOND'],
            'AMORT': self.method_params['AMOR_REDUIT']})
        if self.FREQ_CORNER == None:
            self.FREQ_CORNER = 0.05 * self.DSP_args['FREQ_FOND']
        # Il faut calculer le facteur de pic si la donnee = PGA
        # pour obtenir sigma et multiplier la modulation avec cette valeur
        if self.modul_params.has_key('ACCE_MAX'):
            PeakFactor = self.calc_PeakFactor()
            sigma = self.modul_params['ACCE_MAX'] / PeakFactor
            self.modulator.sigma = sigma
            f_mod = t_fonction(self.sampler.liste_temps,
                               self.modulator.fonc_modul.vale_y * sigma,
                               para=self.modulator.para_fonc_modul)
            self.modulator.fonc_modul = f_mod
            if self.INFO == 2:
                UTMESS('I', 'PROBA0_12', valr=(PeakFactor))

    def calc_PeakFactor(self):
        """calculate peak factor"""
        spec = calc_dsp_KT(self, self.DSP_args['FREQ_FOND'],
                           self.DSP_args['AMORT'])
        m0, m1, m2, vop, delta = Rice2(self.sampler.liste_w2, spec)
        nup = peak(0.5, self.sampler.DUREE_PHASE_FORTE, vop, delta)
        return nup

    def build_DSP(self):
        """build DSP for DSP class"""
        if 'FREQ_PENTE' in self.method_params:
            self.DSP_args.update({'FREQ_CORNER': self.FREQ_CORNER,
                                  'FREQ_PENTE': self.method_params['FREQ_PENTE'],
                                  'TYPE_DSP': 'KT'})
        else:
            # calcul du facteur de normalisation
            dsp = calc_dsp_KT(self, self.DSP_args['FREQ_FOND'],
                              self.DSP_args['AMORT'])
            # constante de normalisation pour que ecart_type=1:
            S_cst = 1. / (NP.trapz(dsp, self.sampler.liste_w2) * 2.)
            # calcul de la DSP KT
            vale_dsp_KT = calc_dsp_KT(self, self.DSP_args['FREQ_FOND'],
                                      self.DSP_args['AMORT'], S_cst)
            fonc_dsp = t_fonction(self.sampler.liste_w2,
                                  vale_dsp_KT, para=self.para_dsp,)
            self.DSP_args.update({'FONC_DSP': fonc_dsp,
                                  'TYPE_DSP': 'KT'})

    def build_TimeHistory(self):
        """build Time History for DSP class"""
        if self.INFO == 2:
            UTMESS('I', 'PROBA0_13', vali=self.ntir + 1)
        if 'FREQ_PENTE' in self.DSP_args:
            Xt = gene_traj_gauss_evol1D(self, **self.DSP_args)
        else:
            Xt = DSP2ACCE1D(self.DSP_args['FONC_DSP'])
        Xt = self.compute_TimeHistory(NP.array(Xt))
        return Xt

    def build_result(self):
        """Create the result function"""
       # Le concept sortant (de type table_fonction) est tab
        macr = self.macro
        DEFI_FONCTION = macr.get_cmd('DEFI_FONCTION')
        CREA_TABLE = macr.get_cmd('CREA_TABLE')
        macr.DeclareOut('tab_out', macr.sd)
        #--- construction des fonctions sortie
        tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.name)
        for iii in range(self.method_params['NB_TIRAGE']):
            Xt = self.build_TimeHistory()
            _f_out = DEFI_FONCTION(ABSCISSE=tuple(self.sampler.liste_temps),
                                   ORDONNEE=tuple(Xt), **self.para_fonc_traj)
            tab.append({'NUME_ORDRE': self.ntir + 1,  'FONCTION': _f_out.nom})
            self.ntir = self.ntir + 1
        #--- Creation du concept (table) en sortie
        dict_keywords = tab.dict_CREA_TABLE()
        tab_out = CREA_TABLE(TYPE_TABLE='TABLE_FONCTION', **dict_keywords)


class GeneratorSpectrum(Generator):

    """Response Spectra class"""

    def prepare_data(self):
        """prepare data for Spectrum class"""
        if self.FREQ_CORNER == None:
            self.FREQ_CORNER = 0.0
        if 'NB_ITER' in self.method_params:
            dico_err = {'ERRE_ZPA': list(self.method_params['ERRE_ZPA']),
                        'ERRE_MAX':  list(self.method_params['ERRE_MAX']),
                        'ERRE_RMS': list(self.method_params['ERRE_RMS'])}
            err_def = 0.2
            for keys in dico_err:
                if len(dico_err[keys]) < 2:
                    dico_err[keys].append(err_def)
            self.SRO_args.update({'DICO_ERR': dico_err,
                                  'NB_ITER': self.method_params['NB_ITER']})
        spec_osci = self.method_params['SPEC_OSCI']
        l_freq_sro, sro_ref = spec_osci.Valeurs()
        ZPA = sro_ref[-1]
        F_MIN = l_freq_sro[0]
        if self.sampler.FREQ_COUP > l_freq_sro[-1]:
            sro_ref.append(ZPA)
            l_freq_sro.append(FREQ_COUP)
        f_spec = t_fonction(l_freq_sro, sro_ref, para=self.para_sro)
        self.SRO_args.update({'FONC_SPEC': f_spec,
                              'FMIN': F_MIN,
                              'AMORT': self.method_params['AMOR_REDUIT']})
        if 'METHODE' in self.method_params:
            self.SRO_args.update(
                {'METHODE_SRO': self.method_params['METHODE']})
        if self.method_params.has_key('SPEC_1_SIGMA'):
            spec_sigma = self.method_params['SPEC_1_SIGMA']
            f_spec_sigma = t_fonction(spec_sigma.Absc(), spec_sigma.Ordo(),
                                      para=self.para_sro)
            f_spec_sigma = f_spec_sigma.evalfonc(l_freq_sro)
            sro_beta = NP.log(f_spec_sigma.vale_y / sro_ref)
            f_beta = t_fonction(l_freq_sro, sro_beta, para=self.para_sro)
            self.SRO_args.update({'FONC_BETA': f_beta})
        if 'FREQ_PAS' in self.method_params:
            self.SRO_args.update({'PAS': self.method_params['FREQ_PAS']})
        elif 'LIST_FREQ' in self.method_params:
            L_FREQ = self.method_params['LIST_FREQ'].Valeurs()
            assert L_FREQ[0] > 0.0, "LIST_FREQ: il faut des valeurs >0.0"
            self.SRO_args.update({'LIST_FREQ': L_FREQ})
        else:
            self.SRO_args.update({'PAS': self.sampler.DW / 2. / pi})

    def build_DSP(self):
        """build DSP for Spectrum class"""
        #  CALCUL DE LA DSP SPECTRUM-COMPATIBLE
        f_dsp, f_spec_ref = SRO2DSP(self.sampler.FREQ_COUP,
                                    self.sampler.DUREE_PHASE_FORTE,
                                    **self.SRO_args)
        if self.FREQ_CORNER > 0.0:
            f_dsp = dsp_filtre_CP(f_dsp, self.FREQ_CORNER)
        fonc_dsp = f_dsp.evalfonc(self.sampler.liste_w2)
        self.DSP_args.update({'FONC_DSP': fonc_dsp,
                              'TYPE_DSP': 'SC', 'FC': 0.05})
        self.SRO_args['FONC_SPEC'] = f_spec_ref
        if 'FREQ_PENTE' in self.method_params:
            if self.method_params['FREQ_PENTE'] != None:
                self.DSP_args['TYPE_DSP'] = 'FR'
                vop, amo, R0, R2, f_FIT = DSP2FR(self.DSP_args['FONC_DSP'],
                                                 self.DSP_args['FC'])
                self.DSP_args.update({
                                     'FREQ_PENTE': self.method_params['FREQ_PENTE'],
                                     'FREQ_FOND': vop, 'AMORT': amo,
                                     'para_R0': R0, 'para_R2': R2,
                                     'fonc_FIT': f_FIT, 'TYPE_DSP': 'FR'})
        if self.specmethode == 'SPEC_FRACTILE':
            Periods = 1. / (self.sampler.liste_w2 / (2. * pi))
            Periods, MAT_COVC = corrcoefmodel(Periods,
                                              self.SRO_args['FONC_BETA'])
            self.DSP_args.update({'PERIODS': Periods, 'MAT_COVC': MAT_COVC})

    def build_TimeHistory(self):
        """build Time History for Spectrum class"""
        if self.INFO == 2:
            UTMESS('I', 'PROBA0_13', vali=self.ntir + 1)
        if self.specmethode == 'SPEC_UNIQUE':
            if 'NB_ITER' in self.method_params:
                fonc_dsp_opt, rv = itersim_SRO(self, self.DSP_args['FONC_DSP'],
                                               NB_TIRAGE=1, **self.SRO_args)
                Xt = DSP2ACCE1D(fonc_dsp_opt, rv[0])
            else:
                Xt = DSP2ACCE1D(self.DSP_args['FONC_DSP'])
            Xt = self.compute_TimeHistory(NP.array(Xt))
            return Xt
        if self.specmethode == 'SPEC_FRACTILE':
            if 'FREQ_PENTE' in self.method_params:
                alpha2 = RAND_VEC(self.DSP_args['MAT_COVC'],
                                  len(self.sampler.liste_w2), para=2.0)
                self.DSP_args.update({'ALEA_DSP': alpha2})
                Xt = gene_traj_gauss_evol1D(self, **self.DSP_args)
            else:
                fonc_dsp_rv = RAND_DSP(self.DSP_args['MAT_COVC'],
                                       len(self.sampler.liste_w2),
                                       self.DSP_args['FONC_DSP'])
                Xt = DSP2ACCE1D(fonc_dsp_rv)
            Xt = self.compute_TimeHistory(NP.array(Xt))
            return Xt
        if self.specmethode == 'SPEC_MEDIANE':
            if 'NB_ITER' not in self.method_params:
                if 'FREQ_PENTE' in self.method_params:
                    Xt = gene_traj_gauss_evol1D(self, **self.DSP_args)
                else:
                    Xt = DSP2ACCE1D(self.DSP_args['FONC_DSP'])
                Xt = self.compute_TimeHistory(NP.array(Xt))
                return Xt
            else:  # 'NB_ITER' in self.method_params:
                tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.name)
                DEFI_FONCTION = self.macro.get_cmd('DEFI_FONCTION')
                if 'FREQ_PENTE' in self.method_params:
                    fonc_dsp_opt, liste_rv = itersim_SRO(
                        self, self.DSP_args['FONC_DSP'],
                        NB_TIRAGE=self.method_params['NB_TIRAGE'],
                        **self.SRO_args)
                    vop, amo, R0, R2, f_FIT = DSP2FR(fonc_dsp_opt,
                                                     self.DSP_args['FC'])
                    self.DSP_args.update({'FREQ_FOND': vop, 'AMORT': amo,
                                          'para_R0': R0, 'para_R2': R2,
                                          'fonc_FIT': f_FIT})
                    for (ntir, rvtir) in enumerate(liste_rv):
                        Xt = gene_traj_gauss_evol1D(self, rv=rvtir,
                                                    **self.DSP_args)
                        Xt = self.compute_TimeHistory(Xt)
                        _f_out = DEFI_FONCTION(
                            ABSCISSE=tuple(self.sampler.liste_temps),
                            ORDONNEE=tuple(Xt), **self.para_fonc_traj)
                        tab.append({'NUME_ORDRE': self.ntir + 1,
                                    'FONCTION': _f_out.nom})
                        self.ntir = self.ntir + 1
                else:
                    fonc_dsp_opt, liste_rv = itersim_SRO(self,
                                                         self.DSP_args[
                                                             'FONC_DSP'],
                                                         NB_TIRAGE=self.method_params[
                                                             'NB_TIRAGE'],
                                                         **self.SRO_args)
                    for (ntir, rvtir) in enumerate(liste_rv):
                        Xt = DSP2ACCE1D(fonc_dsp_opt, rv=rvtir)
                        Xt = self.compute_TimeHistory(Xt)
                        _f_out = DEFI_FONCTION(
                            ABSCISSE=tuple(self.sampler.liste_temps),
                            RDONNEE=tuple(Xt), **self.para_fonc_traj)
                        tab.append({'NUME_ORDRE': self.ntir + 1,
                                    'FONCTION': _f_out.nom})
                        self.ntir = self.ntir + 1
                return tab

    def build_result(self):
        """Create the result function"""
       # Le concept sortant (de type table_fonction) est tab
        macr = self.macro
        DEFI_FONCTION = macr.get_cmd('DEFI_FONCTION')
        CREA_TABLE = macr.get_cmd('CREA_TABLE')
        macr.DeclareOut('tab_out', macr.sd)
        # construction fonctions sortie
        #     avec iterations sur l'ensemble (SPEC_MEDIANE)
        if 'NB_ITER' in self.method_params and self.specmethode == 'SPEC_MEDIANE':
            tab = self.build_TimeHistory()
        #--- construction des fonctions sortie sans iterations sur l'ensemble
        else:
            tab = Table(titr='GENE_ACCE_SEISME concept : %s' % self.name)
            for iii in range(self.method_params['NB_TIRAGE']):
                listv = self.build_TimeHistory()
                _f_out = DEFI_FONCTION(
                    ORDONNEE=tuple(listv),
                    ABSCISSE=tuple(self.sampler.liste_temps),
                    **self.para_fonc_traj)
                tab.append({'NUME_ORDRE': self.ntir + 1,
                            'FONCTION': _f_out.nom})
                self.ntir = self.ntir + 1
        #--- construction de la table produite: Creation du concept en sortie
        dict_keywords = tab.dict_CREA_TABLE()
        tab_out = CREA_TABLE(TYPE_TABLE='TABLE_FONCTION',
                             **dict_keywords)


#  ------------------------------------------------------------------
#  ECHANTILLONNAGE
#  ------------------------------------------------------------------

class Sampler(object):

    """class Sampling: common task for all cases"""

    def __init__(self, modul_params, method_params):
        self.FREQ_FILTRE = method_params['FREQ_FILTRE']
        self.INFO = modul_params['INFO']
        self.DT = method_params['PAS_INST']
        self.NB_POIN = method_params['NB_POIN']
        self.modulation_type = modul_params['TYPE']
        self.DUREE_PHASE_FORTE = modul_params['DUREE_PHASE_FORTE']
        self.INST_INI = 0.0
        if modul_params.has_key('INST_INI'):
            self.INST_INI = modul_params['INST_INI']
        self.FREQ_COUP = 1. / (2. * self.DT)
        self.liste_temps = None
        self.DUREE_SIGNAL = None
        self.liste_w = None
        self.liste_w2 = None
        self.DW = None

    def run(self):
        """ compute sampling"""
       # discretisation temps et freq
        OM = pi / self.DT
        if self.modulation_type == 'CONSTANT':
            # on simule uniquement la phase forte si CONSTANT
            TTS = self.DUREE_PHASE_FORTE
            # on prend NB_POIN pair uniquement
            self.NB_POIN = int(ceil((TTS / self.DT + 1) / 2.) * 2.)
            DW = 2. * OM / self.NB_POIN
            TT = (self.NB_POIN - 1) * self.DT
       # on calcule la duree de simulation si NB_POIN donne
        elif self.NB_POIN != None:
            if self.NB_POIN % 2 != 0:
                self.NB_POIN = self.NB_POIN + 1
            TT = (self.NB_POIN - 1) * self.DT
            DW = 2. * OM / self.NB_POIN
            if TT < self.DUREE_PHASE_FORTE * 1.5:
                UTMESS('A', 'SEISME_39', valk=(str(TT)))

        else:     # on prend 3* phase forte comme duree de simulation
            TTS = self.INST_INI + 3. * self.DUREE_PHASE_FORTE
            # on prend NB_POIN pair uniquement
            self.NB_POIN = int(ceil((TTS / self.DT + 1) / 2.) * 2.)
            DW = 2. * OM / self.NB_POIN
            TT = (self.NB_POIN - 1) * self.DT

        liste_temps = NP.arange(0., self.NB_POIN * self.DT, self.DT)
        l_w = NP.arange(-OM + DW / 2., OM + DW / 2., DW)
        l_w2 = NP.arange(DW / 2., OM + DW / 2., DW)
        # parfois les listes ne sont pas bien construites
        # pour cause d'erreur num si valeurs reeles
        liste_temps = liste_temps[0: self.NB_POIN]
        l_w = l_w[0: self.NB_POIN]
        l_w2 = l_w2[0: self.NB_POIN / 2]
        nbfreq = 2 * len(l_w2)
        assert self.NB_POIN == nbfreq
        assert len(liste_temps) == self.NB_POIN
        assert len(l_w) == self.NB_POIN
        self.liste_temps = liste_temps
        self.liste_w = l_w
        self.liste_w2 = l_w2
        self.DUREE_SIGNAL = TT
        self.DW = DW
        if self.INFO == 2:
            if self.FREQ_FILTRE > 0.0:
                vale_filtre = str(self.FREQ_FILTRE) + ' Hz'
            else:
                vale_filtre = 'None'
            UTMESS('I', 'SEISME_9', vali=self.NB_POIN,
                   valr=(
                       self.FREQ_COUP, self.DW / 2. / pi, self.DT, self.DUREE_SIGNAL),
                   valk=vale_filtre)

#     -----------------------------------------------------------------
#          MODULATION   Gamma, JH, Constant
#     -----------------------------------------------------------------


class Modulator(object):

    """class Modulator: common task for all cases"""

    @staticmethod
    def factory(modul_params):
        """create an instance of the Modulator"""
        if modul_params['TYPE'] == 'GAMMA':
            return ModulatorGamma(modul_params)
        elif modul_params['TYPE'] == 'JENNINGS_HOUSNER':
            return ModulatorJH(modul_params)
        elif modul_params['TYPE'] == 'CONSTANT':
            return ModulatorConstant(modul_params)
        else:
            raise ValueError('unknown configuration')

    def __init__(self, modul_params):
        self.para_fonc_modul = {'NOM_PARA': 'INST',
                                'NOM_RESU': 'ACCE',
                                'INTERPOL': ['LIN', 'LIN'],
                                'PROL_DROITE': 'EXCLU',
                                'PROL_GAUCHE': 'EXCLU', }
        self.modul_params = modul_params
        self.DUREE_PHASE_FORTE = modul_params['DUREE_PHASE_FORTE']
        self.norme = modul_params['NORME']
        self.sigma = 1.  # > il faut le calculer plus tard (GeneratorDSP) avec le facteur de pic.
        self.fonc_modul = None
        self.T1 = 0.0
        self.T2 = self.DUREE_PHASE_FORTE

    def run(self, sample_time, DUREE_SIGNAL):
        """ Generate Modulating function: specific to each method"""
        raise NotImplementedError('must be implemented in a subclass')

    def calc_fonc_modul(self, sample_time, N1, N2, fqt):
        """determine amplitude of modulating function fqt"""
        if self.modul_params.has_key('INTE_ARIAS'):
            vale_arias = f_ARIAS(sample_time, fqt, self.norme)
            fqt = fqt * sqrt(self.modul_params['INTE_ARIAS'] / vale_arias)
        elif self.modul_params.has_key('ECART_TYPE'):
            int12 = NP.trapz((fqt[N1:N2]) ** 2, sample_time[N1:N2])
            fqt = fqt * \
                self.modul_params['ECART_TYPE'] * sqrt(
                    self.DUREE_PHASE_FORTE / int12)
        elif self.modul_params.has_key('ACCE_MAX'):
            int12 = NP.trapz(fqt[N1:N2] ** 2, sample_time[N1:N2])
            fqt = fqt * self.sigma * sqrt(self.DUREE_PHASE_FORTE / int12)
        else:
            # equivalence energie totale avec signal module par CONSTANT sur
            # DUREE
            int12 = NP.trapz(fqt ** 2, sample_time)
            fqt = fqt * sqrt(self.DUREE_PHASE_FORTE / int12)
        f_mod = t_fonction(sample_time, fqt, para=self.para_fonc_modul)
        self.fonc_modul = f_mod


class ModulatorGamma(Modulator):

    """Modulator type Gamma"""

    def run(self, sample_time, DUREE_SIGNAL):
        T1 = self.modul_params['INST_INI']
        T2 = T1 + self.DUREE_PHASE_FORTE
        x0 = [1.3, 0.25]
        liste_t = NP.arange(0., DUREE_SIGNAL,  0.01)
        N1 = NP.searchsorted(liste_t, T1)
        N2 = NP.searchsorted(liste_t, T2)
        fqt_ini = fonctm_gam(liste_t, 1.0, x0[0], x0[1])
        aria, TSM, t1, t2 = f_ARIAS_TSM(liste_t, fqt_ini, self.norme)
        x_opt = fmin(f_opta, x0, args=(liste_t, N1, N2))
        a2 = x_opt[0]
        a3 = x_opt[1]
        fqt = fonctm_gam(sample_time, 1.0, a2, a3)
        aria, TSM, self.T1, self.T2 = f_ARIAS_TSM(sample_time, fqt, self.norme)
        self.calc_fonc_modul(sample_time, N1, N2, fqt)
        if self.modul_params['INFO'] == 2:
            UTMESS('I', 'SEISME_44', valk=('GAMMA', str(a2) + ' ' + str(a3)),
                   valr=(TSM, self.T1, self.T2))


class ModulatorJH(Modulator):

    """Modulator type JH"""

    def run(self, sample_time, DUREE_SIGNAL):
        x0 = [0.5, 1.0]
        t1_ini = [2.0]
        #      identify T1 such that Inta1=0.05*Arias
        liste_t = NP.arange(0., DUREE_SIGNAL, 0.01)
        t_opt = fmin(f_opt1, t1_ini,
                     args=(liste_t, self.DUREE_PHASE_FORTE, 0.5, 1.0))
        T1 = t_opt[0]
        x_opt = fmin(f_opt2, x0, args=(liste_t, T1, self.DUREE_PHASE_FORTE))
        alpha = x_opt[0]
        beta = x_opt[1]
        T2 = T1 + self.DUREE_PHASE_FORTE
        N1 = NP.searchsorted(liste_t, T1)
        N2 = NP.searchsorted(liste_t, T2)
        fqt = fonctm_JetH(sample_time, T1, T2, alpha, beta)
        aria, TSM, self.T1, self.T2 = f_ARIAS_TSM(sample_time, fqt, self.norme)
        self.calc_fonc_modul(sample_time, N1, N2, fqt)
        if self.modul_params['INFO'] == 2:
            UTMESS('I', 'SEISME_44',
                   valk=('JENNINGS & HOUSNER', str(alpha) + ' ' + str(beta)),
                   valr=(TSM, self.T1, self.T2))


class ModulatorConstant(Modulator):

    """Modulator type Constant"""

    def run(self, sample_time, DUREE_SIGNAL):
        if self.modul_params.has_key('INTE_ARIAS'):
            vale_arias = self.DUREE_PHASE_FORTE * pi / (2. * self.norme)
            fq = sqrt(self.modul_params['INTE_ARIAS'] / vale_arias)
        elif self.modul_params.has_key('ECART_TYPE'):
            fq = self.modul_params['ECART_TYPE']
        elif self.modul_params.has_key('ACCE_MAX'):
            fq = self.sigma
        else:
            fq = 1.0
        fqt = NP.array([fq] * len(sample_time))
        f_mod = t_fonction(sample_time, fqt, para=self.para_fonc_modul)
        self.fonc_modul = f_mod
        if self.modul_params['INFO'] == 2:
            UTMESS('I', 'SEISME_44', valk=('CONSTANTE', 'None'),
                   valr=(self.DUREE_PHASE_FORTE, self.T1, self.T2))
