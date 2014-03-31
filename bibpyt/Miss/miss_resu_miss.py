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

"""Module permettant de lire un fichier produit par Miss"""

import os
import os.path as osp
import re
import unittest

import numpy as NP

from Miss.miss_utils import lire_nb_valeurs, double

# to allow unittest without aster module
try:
    import aster
    AsterError = aster.error
except ImportError:
    from Execution.E_Exception import error as AsterError

class MissCsolReader(object):
    """Lit un fichier csol"""

    def __init__(self, nbpc, freq_nb):
        """Initialisation"""
        self.fobj = None
        self.ln = 0
        self.nbpc = nbpc
        self.freq_nb = freq_nb
        self.listfreq = None
        self.values = [ResultatPC() for i in range(nbpc)]

    def read(self, fname):
        """Read the file line per line."""
        try:
            self.fobj = open(fname, "r")
            self._read_all()
            self.fobj.close()
        except (ValueError, IOError, AssertionError), err:
            raise AsterError('MISS0_7', vali=self.ln, valk=str(err))
        return self.listfreq, self.values

    def _read_all(self):
        """Read the file line per line."""
        re_lab = re.compile('POINT *([0-9]+) *CHAMP NO *([0-9]+) *'
                            'DDL NO *([0-9]+)')
        nbf = self.freq_nb
        for ich in range(3):
            for iddl in range(3):
                if ich != iddl:
                    for ifrq in range(self.nbpc * (nbf + 2)):
                        self.fobj.readline()
                        self.ln += 1
                    continue
                for ipc in range(self.nbpc):
                    self.ln += 2
                    self.fobj.readline()
                    mat = re_lab.search(self.fobj.readline())
                    assert mat is not None, 'unexpected line'
                    nums = map(int, mat.groups())
                    assert nums == [ipc + 1, ich + 1, iddl + 1], \
                        '(%d, %d, %d) expected' % (ipc + 1, ich + 1, iddl + 1)
                    val = []
                    self.ln += lire_nb_valeurs(self.fobj, 4 * nbf, val, double)
                    array = NP.array(val).reshape((nbf, 4))
                    self.listfreq = array[:, 0]
                    real = array[:, 1]
                    imag = array[:, 2]
                    self.values[ipc].set(iddl, real, imag)

class ResultatPC:
    """Simple conteneur des valeurs relus en un point de contrôle"""

    def __init__(self):
        """Initialisation"""
        self.comp = [None] * 3

    def set(self, component, real, imag):
        """register the value for a component"""
        self.comp[component] = (real, imag)

class TestMissCsolReader(unittest.TestCase):
    """test the reader of csol files"""
    fcsol = 'ZZZZ108B.01.csol.a'
    
    #unittest.skipIf(not osp.isfile(faster),   # decorator requires python 2.7
                     #"requires %s" % faster)
    def test01_ext(self):
        """test creation of the .ext file"""
        from Utilitai.Table import Table
        if not osp.isfile(self.fcsol):
            return
        reader = MissCsolReader(3, 201)
        lfreq, values = reader.read(self.fcsol)
        self.tab = tab = Table()
        tab['FREQ'] = lfreq
        for ipc, respc in enumerate(values):
            for iddl, comp in enumerate(('X', 'Y', 'Z')):
                lab = 'PC_{}_{}_REEL'.format(ipc + 1, comp)
                tab[lab] = respc.comp[iddl][0]
                lab = 'PC_{}_{}_IMAG'.format(ipc + 1, comp)
                tab[lab] = respc.comp[iddl][1]

if __name__ == '__main__':
    unittest.main()
