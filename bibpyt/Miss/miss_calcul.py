# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""Module permettant le lancement d'un calcul MISS3D

Les classes définies sont :
    CalculMiss      : classe générique d'un appel à MISS3D
    CalculMissImpe  : spécialisation au calcul d'impédance
    CalculMissFichierTemps : calcul dans le domained de Laplace
    CalculMissIssf  : calcul avec prise en compte de l'interaction
                      sol-structure-fluide
    CalculMissPost  : étapes limitées au post-traitement
"""

import os
import re
import shutil
import traceback
import os.path as osp

from math import cos, sin, pi

import aster_core
import aster
from Cata.cata import _F, MACR_ELEM_DYNA, IMPR_MACR_ELEM

from Utilitai.Utmess          import UTMESS
from Utilitai.System          import ExecCommand
from Utilitai.UniteAster      import UniteAster
from Utilitai.utils           import set_debug, _print, _printDBG
from Miss.miss_fichier_sol    import fichier_sol
from Miss.miss_fichier_option import fichier_option
from Miss.miss_resu_aster     import ResuAsterReader
from Miss.miss_fichier_interf import (
    fichier_mvol, fichier_chp, fichier_ext, fichier_sign,
)
from Miss.miss_fichier_cmde   import MissCmdeGen
from Miss.miss_post           import PostMissFactory, info_freq


class CalculMiss(object):
    """Définition d'un calcul MISS3D.
    """
    option_calcul = None
    
    @staticmethod
    def factory(parent, param):
        """Factory that returns the CalculMiss object"""
        if param['TYPE_RESU'] == 'FICHIER_TEMPS':
            return CalculMissFichierTemps(parent, param)
        elif param['TYPE_RESU'] != 'FICHIER' \
            and not param['_exec_Miss']:
            return CalculMissPost(parent, param)
        elif param['ISSF'] == 'OUI':
            return CalculMissIssf(parent, param)
        else:
            return CalculMissImpe(parent, param)

    def __init__(self, parent, parameters):
        """Initialisations"""
        if not self.option_calcul:
            raise NotImplementedError, "option_calcul non défini"
        self.parent = parent
        self.param = parameters
        self.data = None
        self.verbose = parameters['INFO'] >= 2
        self.debug = self.verbose
        self.resu_aster_reader = None
        if self.verbose:
            _print('Paramètres du calcul', self.param)
        if self.debug:
            from Utilitai.as_timer import ASTER_TIMER
            self.timer = ASTER_TIMER()
            set_debug(True)

    def run(self):
        """Enchaine les tâches élémentaires"""
        self.prepare_donnees()
        self.execute()
        self.post_traitement()

    def init_reader(self):
        """Initialise le lecteur de fichier Aster"""
        mcgrp = []
        for grp in self.param:
            if grp.startswith('GROUP_MA_') and self.param[grp] is not None:
                mcgrp.append(grp)
        self.resu_aster_reader = ResuAsterReader(len(mcgrp))

    def prepare_donnees(self):
        """Préparation des données"""
        self.cree_reptrav()
        self.init_reader()
        self.cree_resultat_aster()
        self.cree_fichier_mvol()
        self.cree_fichier_pc()
        self.cree_fichier_chp()
        self.cree_fichier_sol()
        self.cree_commande_miss()
        self.cree_fichier_option()
        # libérer la structure contenant les données numériques
        self.init_data()

    def init_data(self):
        """Libérer la structure contenant les données numériques
        """
        self.data = None


    def execute(self):
        """Exécute MISS3D.
        """
        self._dbg_trace("Start")
        
        copie_fichier(self._fichier_tmp("in"), osp.join(self.param['_WRKDIR'], "MISS.IN"))
        cmd = osp.join(aster_core.get_option('repout'), "run_miss3d") + " " + self.param['VERSION']
        iret = 4
        try:
            os.chdir(self.param['_WRKDIR'])
            if self.verbose:
                _print("Exécution de MISS3D dans :", self.param['_WRKDIR'])
                os.system('ls -la')
            comment = "Lancement de la commande :\n%s" % cmd
            if self.verbose:
                aster.affiche("MESSAGE", comment)
            iret, output, error = ExecCommand(cmd, alt_comment=comment,
                                              verbose=False, separated_stderr=True)
            if self.verbose:
                _print("Contenu du répertoire après l'exécution de MISS3D :")
                os.system('ls -la')
        finally:
            os.chdir(self.param['_INIDIR'])
        UTMESS('I', 'EXECLOGICIEL0_9',  valk=output)
        miss_out = ""
        if osp.exists(self._fichier_tmp("OUT")):
            miss_out = open(self._fichier_tmp("OUT"), "r").read()
        is_ok = iret == 0 and \
            (miss_out.find("INSUFFISAN") < 0 and miss_out.find("*** STOP") < 0)
        if not is_ok or self.verbose:
            aster.affiche("MESSAGE", miss_out)
        if not is_ok:
            UTMESS('I', 'EXECLOGICIEL0_10', valk=error, print_as='E')
            raise aster.error('EXECLOGICIEL0_3', vali=[0, iret])
        self._dbg_trace("Stop")


    def post_traitement(self):
        """Opérations de post-traitement.
        """
        self._dbg_trace("Start")
        self.fichier_resultat()

        post = PostMissFactory(self.param['TYPE_RESU'], self.parent, self.param)
        post.set_filename_callback(self._fichier_tmp)
        post.run()
        # nécessaire s'il y a deux exécutions Miss dans le même calcul Aster
        self.menage()
        self._dbg_trace("Stop")
        if self.debug:
            print self.timer


    def fichier_resultat(self):
        """Copie les fichiers résultats dans les unités logiques."""
        if self.param['UNITE_IMPR_ASTER'] and osp.exists(self._fichier_tmp("aster")):
            copie_fichier(self._fichier_tmp("aster"),
                          self._fichier_aster(self.param['UNITE_IMPR_ASTER']))
        if osp.exists(self._fichier_tmp("resu_impe")):
            copie_fichier(self._fichier_tmp("resu_impe"),
                          self._fichier_aster(self.param['UNITE_RESU_IMPE']))
        if osp.exists(self._fichier_tmp("resu_forc")):
            copie_fichier(self._fichier_tmp("resu_forc"),
                          self._fichier_aster(self.param['UNITE_RESU_FORC']))


    def cree_reptrav(self):
        """Création du répertoire d'exécution de MISS.
        """
        if not osp.exists(self.param['_WRKDIR']):
            os.makedirs(self.param['_WRKDIR'])


    def menage(self):
        """Suppression des fichiers/répertoires de travail.
        """
        if osp.exists(self.param['_WRKDIR']) and self.param['REPERTOIRE'] is None:
            shutil.rmtree(self.param['_WRKDIR'])


    def cree_resultat_aster(self):
        """Produit le(s) fichier(s) issu(s) d'Aster."""
        self._dbg_trace("Start")
        UL = UniteAster()
        ulaster = UL.Libre(action='ASSOCIER')
        mael = self.param['MACR_ELEM_DYNA']
        if mael is None:
            opts = {}
            if self.param['MATR_RIGI']:
                opts['MATR_RIGI'] = self.param['MATR_RIGI']
            if self.param['MATR_MASS']:
                opts['MATR_MASS'] = self.param['MATR_MASS']
            __mael = MACR_ELEM_DYNA(BASE_MODALE=self.param['BASE_MODALE'],
                                    **opts)
            mael = __mael
        if self.param['_hasPC']:
            grma = self.param['GROUP_MA_CONTROL']
            self.param.set('_nbPC', get_number_PC(self.parent, mael, grma))
        other_groups = {}
        if self.param['ISSF'] == 'OUI':
            other_groups = _F(
                GROUP_MA_FLU_STR=self.param['GROUP_MA_FLU_STR'],
                GROUP_MA_FLU_SOL=self.param['GROUP_MA_FLU_SOL'],
                GROUP_MA_SOL_SOL=self.param['GROUP_MA_SOL_SOL'],)
        IMPR_MACR_ELEM(MACR_ELEM_DYNA=mael,
                       FORMAT='MISS_3D',
                       GROUP_MA_INTERF=self.param['GROUP_MA_INTERF'],
                       GROUP_MA_CONTROL=self.param.get('GROUP_MA_CONTROL'),
                       FORMAT_R='1PE16.9',
                       SOUS_TITRE='PRODUIT PAR CALC_MISS',
                       UNITE=ulaster,
                       **other_groups)
        UL.EtatInit()
        copie_fichier(self.param.UL.Nom(ulaster), self._fichier_tmp("aster"))
        self.data = self.resu_aster_reader.read(self._fichier_tmp("aster"))
        self._dbg_trace("Stop")

    def cree_fichier_mvol(self):
        """Produit le fichier de maillage.
        """
        self._dbg_trace("Start")
        content = fichier_mvol(self.data)
        open(self._fichier_tmp("mvol"), "w").write(content)
        self._dbg_trace("Stop")

    def cree_fichier_pc(self):
        """Produit les fichiers pour les points de contrôle"""
        if not self.param['_hasPC']:
            return
        self._dbg_trace("Start")
        content = fichier_ext(self.data)
        open(self._fichier_tmp("ext"), "w").write(content)
        content = fichier_sign(self.param)
        open(self._fichier_tmp("01.sign"), "w").write(content)
        self._dbg_trace("Stop")

    def cree_fichier_chp(self):
        """Produit le fichier chp (modes statiques, dynamiques...).
        """
        self._dbg_trace("Start")
        content = fichier_chp(self.param, self.data)
        open(self._fichier_tmp("chp"), "w").write(content)
        self._dbg_trace("Stop")


    def cree_fichier_sol(self):
        """Ecrit le fichier de sol.
        """
        self._dbg_trace("Stop")
        if self.param['TABLE_SOL'] is not None:
            tabsol = self.param['TABLE_SOL'].EXTR_TABLE()
            sol_content = fichier_sol(tabsol, self.param)
            if self.verbose:
                _print('Fichier de sol', sol_content)
            open(self._fichier_tmp("sol"), 'w').write(sol_content)
        self._dbg_trace("Stop")

    def cree_commande_miss(self, ext='in', lapl_temps=False):
        """Produit le fichier de commandes Miss"""
        self._dbg_trace("Start")
        # Execute méthode classique
        generator = MissCmdeGen(self.param, self.data, self._fichier_tmp_local,
                                lapl_temps=lapl_temps)
        content = generator.build()
        open(self._fichier_tmp(ext), "w").write(content)
        self._dbg_trace("Stop")

    def cree_fichier_option(self):
        """Ecrit le fichier OPTMIS."""
        self._dbg_trace("Start")
        option_content = fichier_option(self.param)
        if self.verbose:
            _print("Fichier d'option", option_content)
        open(self._fichier_tmp("optmis"), 'w').write(option_content)
        self._dbg_trace("Stop")


    # --- utilitaires internes
    def _fichier_tmp(self, ext):
        """Retourne le nom d'un fichier MISS dans WRKDIR.
        """
        fich = '%s.%s' % (self.param['PROJET'], ext)
        return osp.join(self.param['_WRKDIR'], fich)

    def _fichier_tmp_local(self, ext):
        """Retourne le nom d'un fichier MISS en local.
        """
        fich = '%s.%s' % (self.param['PROJET'], ext)
        return osp.join('./', fich)


    def _fichier_aster(self, unite):
        """Nom du fichier d'unité logique unite dans le répertoire d'exécution de Code_Aster.
        """
        return osp.join(self.param['_INIDIR'], "fort.%d" % unite)


    def _dbg_trace(self, on_off):
        if not self.debug:
            return
        stack = traceback.format_stack(limit=5)[-2]
        mat = re.search('File [\'\"]*(.*?)[\'\"]*, *line ([0-9]+), *in (.*)', stack)
        getattr(self.timer, on_off)("CALC_MISS." + mat.group(3))



class CalculMissImpe(CalculMiss):
    """Définition d'un calcul MISS3D de type MISS_IMPE."""
    option_calcul = 'MISS_IMPE'


class CalculMissIssf(CalculMiss):
    """Définition d'un calcul MISS3D de type MISS_IMPE avec ISSF."""
    option_calcul = 'MISS_IMPE'


class CalculMissPost(CalculMiss):
    """Définition d'une exécution de CALC_MISS où seul le
    post-traitement est demandé."""
    option_calcul = 'POST-TRAITEMENT'

    def prepare_donnees(self):
        """Préparation des données."""


    def execute(self):
        """Exécute MISS3D."""


    def fichier_resultat(self):
        """Copie les fichiers résultats dans les unités logiques."""


class CalculMissFichierTemps(CalculMiss):
    """Définition d'une exécution de CALC_MISS dans le domaine de Laplace"""
    option_calcul = 'IMPE_LAPL'

    def init_attr(self):
        """Initialisations"""
        self.dt = self.param['PAS_INST']
        N_inst = int(self.param['INST_FIN']/self.param['PAS_INST'])
        if N_inst % 2 != 0 :
            UTMESS('F','MISS0_18')
        eps = self.param['PRECISION']
        self.rho = eps**(1./(2.*N_inst))
        factor = self.param['COEF_SURECH']
        self.L_points = factor*N_inst
        self.nbr_freq = self.L_points/2 + 1

        # Noms des fichiers à utiliser
        
        lfich = ( "impe_Laplace", "forc_Laplace" )
        lfich = map(self._fichier_tmp, lfich)
        # chemins relatifs au _WRKDIR sinon trop longs pour Miss
        lfich = map(osp.basename, lfich)
        
        self._fname = lfich[0]
        self._fname2 = lfich[1]
        
        lfich = ("resu_impe", "resu_forc")
        lfich = map(self._fichier_tmp, lfich)
        self._fichier_impe = lfich[0]
        self._fichier_forc = lfich[1]

        # Variables à rajouter dans 'param'
        self.param.set('LIST_FREQ', None)
        self.param.set('FREQ_IMAG', None)
        # Nombre de modes (dynamiques) d'interface
        modes_nb = self.data.mode_stat_nb 
        self.param.set('NB_MODE', modes_nb)
         
        # Creation du fichier sol 
        self.param.set('FICHIER_SOL_INCI', self._fichier_tmp_local("sol.inci"))
         
    
    def execute(self):
        """Exécute MISS3D : calcul du champ incident + boucle sur les fréquences complexes"""
        self.init_attr()

        if self.param['EXCIT_SOL']:
            copie_fichier(self._fichier_tmp("inci"), self._fichier_tmp("in"))
            aster.affiche("MESSAGE",'LANCEMENT DU CALCUL DE LA FORCE SISMIQUE EN FREQUENCE')
            CalculMiss.execute(self)
            copie_fichier(self._fichier_tmp("OUT"), self._fichier_tmp("OUT.inci"))
            copie_fichier(self._fichier_tmp("sol"), self._fichier_tmp("sol.inci"))
            fd = open(self._fname2, 'w')
            text = open(self._fichier_forc, 'r').read()
            fd.write(text)
            fd.close()
            copie_fichier(self._fichier_forc, self._fichier_aster(self.param['EXCIT_SOL']['UNITE_RESU_FORC']))
        
        fd = open(self._fname, 'w')
        CalculMiss.cree_fichier_sol(self)
        aster.affiche("MESSAGE",'BOUCLE SUR LES FREQUENCES COMPLEXES')
        self._exec_boucle_lapl(fd)
        fd.close()                     

   
    def _exec_boucle_lapl(self,fd): 
        """Exécute MISS3D dans une boucle sur toutes les fréquences complexes"""
        L_points = self.L_points
        dt = self.dt
        rho = self.rho
        #TODO la règle UN_PARMI(FREQ_MIN, LIST_FREQ, FREQ_IMAG) ne convient pas!
        # au moins mettre FREQ_MIN à None
        self.param.set('FREQ_MIN', None)

        for k in range(0, self.nbr_freq):
            if (k == 0) or (k == self.nbr_freq - 1):
                self.param.set('LIST_FREQ', (0.1E-4,) )
                self.param.set('FREQ_IMAG',(1.5-2.0*rho*cos(2*pi*k/L_points)+0.5*rho*rho*cos(4*pi*k/L_points) )/dt )         
            else:
                self.param.set('LIST_FREQ', (-(-2.0*rho*sin(2*pi*k/L_points)+0.5*rho*rho*sin(4*pi*k/L_points))/dt/(2*pi),) )
                self.param.set('FREQ_IMAG',(1.5-2.0*rho*cos(2*pi*k/L_points)+0.5*rho*rho*cos(4*pi*k/L_points) )/dt )

            self.param.set('_calc_impe', True)
            CalculMiss.cree_commande_miss(self)
            str00 = str(self.param['FREQ_IMAG'])+' + i . '+str(self.param['LIST_FREQ'][0])+' ('+str(k+1)+'/'+str(self.nbr_freq)+')'
            aster.affiche("MESSAGE",'FREQUENCE COMPLEXE COURANTE =  '+str00)
            CalculMiss.execute(self)
            
            text = open(self._fichier_impe, 'r').read()
            fd.write(text)
            
        # libérer la structure contenant les données numériques
        CalculMiss.init_data(self)


    def cree_commande_miss(self):
        """Produit le fichier de commandes Miss du champ incident (.inci)"""
        CalculMiss.cree_commande_miss(self, ext='inci', lapl_temps=True)

    def fichier_resultat(self):
        """Libérer la structure contenant les données numériques
        """
        if self.param['UNITE_IMPR_ASTER'] and osp.exists(self._fichier_tmp("aster")):
            copie_fichier(self._fichier_tmp("aster"),
                          self._fichier_aster(self.param['UNITE_IMPR_ASTER']))

    def init_data(self):
        """Libérer la structure contenant les données numériques
        """
        pass


def get_number_PC(parent, macr_elem, lgrpc):
    """Retourne le nombre de points de contrôle"""
    nomail = macr_elem.sdj.REFM.get()[1]
    mail = parent.get_concept(nomail)
    assert mail is not None, \
        'impossible de récupérer le maillage du macro-élément'
    lgrpma = mail.LIST_GROUP_MA()
    result = sum([nbel for name, nbel, dim in lgrpma if name in lgrpc])
    return result

def copie_fichier(src, dst):
    """Copie d'un fichier.
    """
    if src and dst:
        try:
            shutil.copyfile(src, dst)
        except:
            raise aster.error('MISS0_6', valk=(src, dst))
