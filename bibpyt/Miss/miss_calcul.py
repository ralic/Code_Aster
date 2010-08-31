#@ MODIF miss_calcul Miss  DATE 30/08/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE COURTOIS M.COURTOIS

"""Module permettant le lancement d'un calcul MISS3D

Les classes définies sont :
    CALCUL_MISS      : classe générique d'un appel à MISS3D
    CALCUL_MISS_IMPE : spécialisation au calcul d'impédance
"""

import os
import re
import shutil
import traceback
import os.path as osp

import aster
from Utilitai.Utmess          import UTMESS
from Utilitai.System          import ExecCommand
from Miss.miss_utils          import MISS_PARAMETER, _print
from Miss.miss_fichier_sol    import fichier_sol
from Miss.miss_fichier_option import fichier_option
from Miss.miss_resu_aster     import lire_resultat_aster
from Miss.miss_fichier_interf import fichier_mvol, fichier_chp, fichier_cmde


class CALCUL_MISS(object):
    """Définition d'un calcul MISS3D.
    """
    option_calcul = None

    def __init__(self, parent, parameters):
        """Initialisations"""
        if not self.option_calcul:
            raise NotImplementedError, "option_calcul non défini"
        self.parent = parent
        self.param = parameters
        self.data = None
        self.verbose = parameters['INFO'] >= 2
        self.debug = self.verbose
        if self.verbose:
            _print('Paramètres du calcul', self.param)
        if self.debug:
            from Utilitai.as_timer import ASTER_TIMER
            self.timer = ASTER_TIMER()


    def prepare_donnees(self):
        """Préparation des données
        """
        self.cree_reptrav()
        self.cree_resultat_aster()
        self.cree_fichier_mvol()
        self.cree_fichier_chp()
        self.cree_fichier_sol()
        self.cree_commande_miss()
        self.cree_fichier_option()
        # libérer la structure contenant les données numériques
        self.data = None


    def execute(self):
        """Exécute MISS3D.
        """
        self._dbg_trace("Start")
        copie_fichier(self._fichier_tmp("in"), os.path.join(self.param['_WRKDIR'], "MISS.IN"))

        cmd = os.path.join(aster.repout(), "run_miss3d") + " " + self.param['VERSION']
        try:
            os.chdir(self.param['_WRKDIR'])
            if self.verbose:
                _print("Exécution de MISS3D dans :", self.param['_WRKDIR'])
                os.system('ls -la')
            comment = "Lancement de la commande :\n%s" % cmd
            if self.verbose:
                aster.affiche("MESSAGE", comment)
            iret, output, error = ExecCommand(cmd, alt_comment=comment, verbose=False, separated_stderr=True)
            if self.verbose:
                _print("Contenu du répertoire après l'exécution de MISS3D :")
                os.system('ls -la')
        finally:
            os.chdir(self.param['_INIDIR'])
        UTMESS('I', 'EXECLOGICIEL0_9',  valk=output)
        miss_out = ""
        if osp.exists(self._fichier_tmp("OUT")):
            miss_out = open(self._fichier_tmp("OUT"), "r").read()
        is_ok = iret == 0 and miss_out.find("INSUFFISAN") < 0
        if not is_ok:
            aster.affiche("MESSAGE", miss_out)
        if not is_ok:
            UTMESS('I', 'EXECLOGICIEL0_10', valk=error, print_as='E')
            raise aster.error('EXECLOGICIEL0_3', vali=[0, iret])
        self._dbg_trace("Stop")


    def post_traitement(self):
        """Opérations de post-traitement.
        """
        self._dbg_trace("Start")
        copie_fichier(self._fichier_tmp("OUT"), self._fichier_repe("OUT"))
        if os.path.exists(self._fichier_tmp("resu_impe")):
            copie_fichier(self._fichier_tmp("resu_impe"), self._fichier_aster(self.param['UNITE_RESU_IMPE']))
        if os.path.exists(self._fichier_tmp("resu_forc")):
            copie_fichier(self._fichier_tmp("resu_forc"), self._fichier_aster(self.param['UNITE_RESU_FORC']))

        # nécessaire s'il y a deux exécutions Miss dans le même calcul Aster
        self.menage()
        self._dbg_trace("Stop")
        if self.debug:
            print self.timer


    def cree_reptrav(self):
        """Création du répertoire d'exécution de MISS.
        """
        os.makedirs(self.param['_WRKDIR'])
        if self.param['REPERTOIRE'] and not os.path.exists(self.param['REPERTOIRE']):
            os.makedirs(self.param['REPERTOIRE'])


    def menage(self):
        """Suppression des fichiers/répertoires de travail.
        """
        if os.path.exists(self.param['_WRKDIR']):
            shutil.rmtree(self.param['_WRKDIR'])

    
    def cree_resultat_aster(self):
        """Produit le(s) fichier(s) issu(s) d'Aster."""
        self._dbg_trace("Start")
        if self.param['UNITE_IMPR_ASTER'] is not None:
            DEFI_FICHIER = self.parent.get_cmd("DEFI_FICHIER")
            DEFI_FICHIER(ACTION='LIBERER', UNITE=self.param['UNITE_IMPR_ASTER'],)
            copie_fichier(self._fichier_aster(self.param['UNITE_IMPR_ASTER']), self._fichier_tmp("aster"))
        else:
            pass
            #XXX appeler ici IMPR_MACR_ELEM/IMPR_MISS (inverser le test !)
            #    UNITE -> self._fichier_tmp("aster")
        self.data = lire_resultat_aster(self._fichier_tmp("aster"))
        self._dbg_trace("Stop")


    def cree_fichier_mvol(self):
        """Produit le fichier de maillage.
        """
        self._dbg_trace("Start")
        content = fichier_mvol(self.data)
        open(self._fichier_tmp("mvol"), "w").write(content)
        copie_fichier(self._fichier_tmp("mvol"), self._fichier_repe("mvol"))
        self._dbg_trace("Stop")


    def cree_fichier_chp(self):
        """Produit le fichier chp (modes statiques, dynamiques...).
        """
        self._dbg_trace("Start")
        content = fichier_chp(self.data)
        open(self._fichier_tmp("chp"), "w").write(content)
        copie_fichier(self._fichier_tmp("chp"), self._fichier_repe("chp"))
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
            copie_fichier(self._fichier_tmp("sol"), self._fichier_repe("sol"))
        self._dbg_trace("Stop")


    def cree_commande_miss(self):
        """Produit le fichier de commandes Miss.
        """
        self._dbg_trace("Start")
        lfich = ("mvol", "chp", "sol", "resu_impe", "resu_forc")
        lfich = map(self._fichier_tmp, lfich)
        # chemins relatifs au _WRKDIR sinon trop longs pour Miss
        lfich = map(os.path.basename, lfich)
        content = fichier_cmde(self.param, self.data, *lfich)
        open(self._fichier_tmp("in"), "w").write(content)
        copie_fichier(self._fichier_tmp("in"), self._fichier_repe("in"))
        self._dbg_trace("Stop")


    def cree_fichier_option(self):
        """Ecrit le fichier OPTMIS."""
        self._dbg_trace("Start")
        option_content = fichier_option(self.param)
        if self.verbose:
            _print("Fichier d'option", option_content)
        open(self._fichier_tmp("optmis"), 'w').write(option_content)
        copie_fichier(self._fichier_tmp("optmis"), self._fichier_repe("optmis"))
        self._dbg_trace("Stop")


    # --- utilitaires internes
    def _fichier_repe(self, ext):
        """Retourne un nom d'un fichier MISS dans REPERTOIRE.
        """
        if not self.param['REPERTOIRE']:
            return ''
        fich = '%s.%s' % (self.param['PROJET'], ext)
        return os.path.join(self.param['REPERTOIRE'], fich)


    def _fichier_tmp(self, ext):
        """Retourne le nom d'un fichier MISS dans WRKDIR.
        """
        fich = '%s.%s' % (self.param['PROJET'], ext)
        return os.path.join(self.param['_WRKDIR'], fich)


    def _fichier_aster(self, unite):
        """Nom du fichier d'unité logique unite dans le répertoire d'exécution de Code_Aster.
        """
        return os.path.join(self.param['_INIDIR'], "fort.%d" % unite)


    def _dbg_trace(self, on_off):
        if not self.debug:
            return
        stack = traceback.format_stack(limit=5)[-2]
        mat = re.search('File [\'\"]*(.*?)[\'\"]*, *line ([0-9]+), *in (.*)', stack)
        getattr(self.timer, on_off)("CALCUL_MISS." + mat.group(3))




class CALCUL_MISS_IMPE(CALCUL_MISS):
    """Définition d'un calcul MISS3D de type MISS_IMPE
    """
    option_calcul = 'MISS_IMPE'

#   def __init__(self, parent, parameters):
#      """Initializations"""
#      super(CALCUL_MISS_IMPE, self).__init__(parent, parameters)



def CalculMissFactory(option_calcul, *args, **kwargs):
    """Crée l'objet CALCUL_MISS pour résoudre l'option demandée.
    """
    if option_calcul == 'MISS_IMPE':
        return CALCUL_MISS_IMPE(*args, **kwargs)
    else:
        raise NotImplementedError, option_calcul



def copie_fichier(src, dst):
    """Copie d'un fichier.
    """
    if src and dst:
        try:
            shutil.copyfile(src, dst)
        except:
            raise aster.error('MISS0_6', valk=(src, dst))



