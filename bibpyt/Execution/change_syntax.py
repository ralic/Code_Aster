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
# person_in_charge: mathieu.courtois at edf.fr

"""
Working module - not intended to be committed !
"""

import os
import os.path as osp
import re

from Noyau.N_ASSD import ASSD
from Noyau.N_types import force_list
from Noyau.N_info import message, Category, LEVEL

from E_utils import repr_float
from command_text import CommandTextVisitor

ALL = False
COMMANDS = ('TEST_RESU', 'TEST_FONCTION')
FILENAME = 'change_syntax'
DEST = '/tmp/essai'
STX = Category()
STX.set_header(LEVEL.DEBUG, '#DEBUG')
message.add(STX)
message.set_level(STX, LEVEL.DEBUG)

class SyntaxVisitor(CommandTextVisitor):
    """Visitor to mass-convert command files.
    """
    def __init__(self, with_default=False):
        """Initialization.
        with_default : if True, visit the default values of undefined keywords
        """
        CommandTextVisitor.__init__(self, with_default)
        self.cmdname = None
        self.command = None
        self._pass = 0
        self.text = {}

    def _visit_etape(self, step, reuse):
        """Visit generic ETAPE objects."""
        if not (ALL or step.definition.nom in COMMANDS):
            return
        self.cmdname = step.definition.nom
        self.command = ChangeFactory(self.cmdname)
        # first pass to store keywords values
        message.debug(STX, "visit ETAPE - pass 1: %s", self.cmdname)
        self._pass = 1
        self._visitMCCOMPO(step)
        # second pass to change some keywords and eventually their values
        message.debug(STX, "visit ETAPE - pass 2: %s", self.cmdname)
        CommandTextVisitor.__init__(self, self.with_default, indent=0)
        self._pass = 2
        CommandTextVisitor._visit_etape(self, step, reuse)
        self.store_changes(step)

    def visitMCFACT(self, fact):
        """Visit the MCFACT object."""
        #print "visit MCFACT", fact.nom
        self.command.set_current_mcfact(fact)
        if self._pass == 2:
            self.command.init_pass2()
        CommandTextVisitor.visitMCFACT(self, fact)
        self.command.reset_mcfact()

    def visitMCSIMP(self, mcsimp):
        """Visit the MCSIMP object."""
        if mcsimp.definition.statut == 'c':
            return
        if self._pass == 1:
            svalues = self._repr_MCSIMP(mcsimp)
            if len(svalues) == 1:
                svalues = svalues[0]
            self.command.set_value(mcsimp.nom, svalues)
        if self._pass == 2:
            #print "visit MCSIMP", mcsimp.nom
            newvalues = self.command.get_value(mcsimp.nom)
            for i, couple in enumerate(newvalues):
                key, val = couple
                self.curline.extend([key, "=", val, ","])
                self._count += 1
                if i + 1 < len(newvalues):
                    self._newline()

    def store_changes(self, step):
        """Store new command text"""
        inf = step.frame_info
        assert inf, 'ERROR: command:%s, can not get line numbers' % self.cmdname
        start, end, indent, fname = inf
        values = (start, end, indent, self.get_text(), )
        self.text[fname] = self.text.get(fname, [])
        self.text[fname].append(values)
        message.debug(STX, "stored: %s : %r", fname, values)

    def finalize(self, code=None):
        """End"""
        import sys
        import shutil
        from glob import glob
        sys.path.append('/opt/aster/lib/python2.7/site-packages')
        from asrun.profil import AsterProfil
        exp = glob('*.export')
        if len(exp) == 0:
            return
        assert len(exp) == 1, exp
        prof = AsterProfil(exp[0])
        comm = prof.get_type('comm')
        num = len(glob(FILENAME + '.*')) + 1
        lfn = [fname for fname in self.text.keys() if fname.startswith('fort.')]
        assert len(lfn) <= 1, lfn
        if len(lfn) == 0:
            fname = 'fort.1'
        else:
            fname = lfn[0]
        changes = self.text.get(fname, [])
        modified = '%s.%d' % (FILENAME, num)
        final = osp.basename(comm[num - 1].path)
        orig = open(fname, 'r').read().splitlines()
        new = orig[:]
        changes.reverse()
        for chg in changes:
            start, end, indent, txt = chg
            offset = ' ' * indent
            nlin = [offset + lin for lin in txt.splitlines()]
            new = new[:start] + nlin + new[end+1:]
            txt = ["### filename : %s ###" % fname,
                   "### lines range: %d-%d (indent: %d)" % tuple(chg[:3]),
                   chg[-1], ]
        if new[-1].strip():
            new.append('')
        open(modified, 'w').write(os.linesep.join(new))
        shutil.copy(modified, osp.join(DEST, final))
        #shutil.copy(fname, osp.join(DEST, final + '.orig'))

class ChangeCommand(object):
    """Functions to change a command."""
    def __init__(self):
        """Initialization"""
        self.kwval = {}
        self.mcfact_id = None
        self.cur_vale_calc = None

    def _cur_dict(self):
        """Return the current storage."""
        dico = self.kwval[self.mcfact_id] = self.kwval.get(self.mcfact_id, {})
        return dico

    def set_current_mcfact(self, fact):
        """Register the current MCFACT."""
        self.mcfact_id = id(fact)

    def reset_mcfact(self):
        """Register the current MCFACT."""
        self.mcfact_id = None

    def set_value(self, keyword, value):
        """Store the value of a keyword"""
        message.debug(STX, "store: %r = %r", keyword, value)
        self._cur_dict()[keyword] = value

    def get_value(self, keyword):
        """Return the couples (new keyword, new value)."""
        svalues = self._cur_dict()[keyword]
        if type(svalues) in (list, tuple):
            svalues = "(" + ", ".join(svalues) + ")"
        res = [(keyword, svalues), ]
        return res

    def init_pass2(self):
        """Start the pass 2.
        Extract the 'vale_calc' values for the current MCFACT."""
        self.vale_calc = vale_calc.get()
        message.debug(STX, "vale_calc = %s", self.vale_calc)
        self.cur_vale_calc = self.vale_calc and self.vale_calc.pop(0) or 0.123456

class NoChange(ChangeCommand):
    """Does nothing"""
    def init_pass2(self):
        pass

class ChangeTestFonction(ChangeCommand):
    """Change TEST_FONCTION syntax"""
    def get_value(self, keyword):
        """Return the couples (new keyword, new value)."""
        dico = self._cur_dict()
        value = dico[keyword]
        reference = dico.get('REFERENCE', repr('NON_REGRESSION'))
        precision = dico.get('PRECISION', 1.e-3)
        really_nonreg = reference == repr('NON_REGRESSION') and float(precision) <= 1.e-6
        if really_nonreg:
            dconv = {
                'REFERENCE' : [], # NON_REGRESSION is implicit
                'PRECISION' : [],
                'VALE_REFE' : [('VALE_CALC', dico['VALE_REFE']), ],
            }
            if float(precision) < 1.e-6:  # more severe than by default
                dconv['PRECISION'] = [('TOLE_MACHINE', value), ]
        else:
            dconv = {
                'VALE_REFE' : [('VALE_REFE', dico['VALE_REFE']),
                               ('VALE_CALC', repr_float(self.cur_vale_calc))],
            }
            if reference == repr('NON_REGRESSION'):
                dconv['REFERENCE'] = [('REFERENCE', repr('NON_DEFINI')), ]
        res = dconv.get(keyword, [(keyword, value), ])
        #print "  return:", res
        return res

class ChangeTestResu(ChangeCommand):
    """Change TEST_RESU syntax"""
    def get_value(self, keyword):
        """Return the couples (new keyword, new value)."""
        dico = self._cur_dict()
        value = dico[keyword]
        reference = dico.get('REFERENCE', repr('NON_REGRESSION'))
        precision = dico.get('PRECISION', 1.e-3)
        really_nonreg = reference == repr('NON_REGRESSION') and float(precision) <= 1.e-6
        message.info(STX, 'NON_REGRESSION ? %s', really_nonreg)
        val = dico.get('VALE') or dico.get('VALE_I') or dico.get('VALE_R')
        if really_nonreg:
            dconv = {
                'REFERENCE' : [], # NON_REGRESSION is implicit
                'PRECISION' : [],
                'VALE' : [('VALE_CALC', dico['VALE']), ],
            }
            if float(precision) < 1.e-6:  # more severe than by default
                dconv['PRECISION'] = [('TOLE_MACHINE', value), ]
        else:
            dconv = {
                'VALE' : [('VALE_REFE', dico['VALE']),
                          ('VALE_CALC', repr_float(self.cur_vale_calc))],
            }
            if reference == repr('NON_REGRESSION'):
                dconv['REFERENCE'] = [('REFERENCE', repr('NON_DEFINI')), ]
        res = dconv.get(keyword, [(keyword, value), ])
        #print "  return:", res
        return res

def ChangeFactory(cmdname):
    if cmdname == 'TEST_FONCTION':
        return ChangeTestFonction()
    elif cmdname == 'TEST_RESU':
        return ChangeTestResu()
    else:
        return NoChange()

class StoreValeCalc(object):
    def __init__(self):
        self._vale = None

    def reset(self):
        self._vale = []

    def append(self, value):
        self._vale.append(value)

    def get(self):
        return self._vale

vale_calc = StoreValeCalc()
