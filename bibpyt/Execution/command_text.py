# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
This module defines the visitor used to print the text of the commands
with their keywords.
"""

import os

from Noyau.N_ASSD import ASSD
from Noyau.N_types import force_list, is_float

from E_Visitor import JDCVisitor
from E_utils import repr_float


class CommandTextVisitor(JDCVisitor):
    """Visitor to print the text of the commands.
    """
    def __init__(self, with_default=True, indent=2):
        """Initialization.
        with_default : if True, visit the default values of undefined keywords
        lines : list of the lines.
        curline : buffer for the current line.
        indent : number of spaces of indentation at each level.
        sdname : name of the current result.
        """
        self.with_default = with_default
        self.lines = []
        self.curline = []
        self.indent = [indent,]
        self.sdname = None
        self._saved = {}
        self._count = 0

    def save_buffer(self, obj):
        """Save the current state."""
        self._saved[id(obj)] = {
            'lines' : self.lines[:],
            'curline' : self.curline[:],
            'indent' : self.indent[:],
            'sdname' : self.sdname,
        }

    def restore_buffer(self, obj):
        """Restore the previous saved state."""
        ido = id(obj)
        dsav = self._saved[ido]
        self.lines = dsav['lines']
        self.curline = dsav['curline']
        self.indent = dsav['indent']
        self.sdname = dsav['sdname']
        del self._saved[ido]

    def _newline(self):
        """Initialize a new line."""
        self._endline()
        self.curline.append(" " * self.indent[-1])

    def _endline(self):
        """Add the current line."""
        line = ''.join(self.curline)
        if line.strip() != '':
            self.lines.append(''.join(self.curline))
        self.curline = []

    def _add_indent(self):
        """Set the next indent spacing."""
        self.indent.append(len(''.join(self.curline)))

    def _reset_indent(self):
        """Revert indent spacing to its previous level."""
        self.indent.pop(-1)

    def get_text(self):
        """Return the text"""
        return os.linesep.join(self.lines)

    def _visit_etape(self, step, reuse):
        """Visit generic ETAPE objects."""
        self._newline()
        if step.sd is not None:
            step.sd.accept(self)
            self.curline.extend([self.sdname, " = "])
        self.curline.extend([step.definition.nom, "("])
        self._add_indent()
        if reuse:
            self.curline.extend(['reuse=', self.sdname, ','])
            self._newline()
        self._visitMCCOMPO(step)
        self.curline.append(")")
        self._reset_indent()
        self._endline()
        self.lines.append("")

    def visitMCFACT(self, fact):
        """Visit the MCFACT object."""
        #print "visit MCFACT", fact.nom
        self.save_buffer(fact)
        self._count = 0
        self.curline.append("_F(")
        self._add_indent()
        self._visitMCCOMPO(fact)
        self.curline.append("),")
        self._reset_indent()
        if self._count == 0:
            # do not print _F() without any keyword
            self.restore_buffer(fact)
        self._newline()

    def visitMCList(self, mclist):
        """Visit the MCList object."""
        #print 'visit MCList', mclist
        self.save_buffer(mclist)
        numb = len(mclist.data)
        if numb < 1:
            return
        self.curline.extend([mclist[0].nom, "="])
        if numb > 1:
            self.curline.append("(")
        self._add_indent()
        for i, data in enumerate(mclist.data):
            data.accept(self)
            if i + 1 < numb:
                self._newline()
        if numb > 1:
            self.curline.append("),")
        self._reset_indent()
        if self._count == 0:
            # do not print _F() without any keyword
            self.restore_buffer(mclist)
        self._newline()

    def _visit_default_keywords(self, node, seen, icount):
        """Visit the default values of 'node' not already seen."""
        if not self.with_default:
            return
        to_add = {}
        for key, obj in node.definition.entites.items():
            has_default = getattr(obj, 'defaut', None) is not None \
                        or getattr(obj, 'statut', None) == 'd'
            if not key in seen and has_default:
                to_add[key] = obj
        numb = len(to_add)
        if numb > 0 and icount > 0:
            self._newline()
        i = 0
        for key, obj in to_add.items():
            mc = obj(obj.defaut, key, parent=node)
            mc.accept(self)
            if i + 1 < numb:
                self._newline()
            i += 1

    def _visitMCCOMPO(self, compo):
        """Visit generic MCCOMPO objects (ETAPE, MCFACT, MCBLOC)
        (*private*, no 'accept' method in MCCOMPO class)."""
        #print "visit MCCOMPO", compo.nom
        seen = set()
        numb = len(compo.mc_liste)
        for i, obj in enumerate(compo.mc_liste):
            obj.accept(self)
            seen.add(obj.nom)
            if i + 1 < numb:
                self._newline()
        self._visit_default_keywords(compo, seen, numb)

    def visitMCSIMP(self, mcsimp):
        """Visit the MCSIMP object."""
        if mcsimp.definition.statut == 'c':
            return
        svalues = self._repr_MCSIMP(mcsimp)
        as_list = len(svalues) > 1 \
            or (mcsimp.definition.max == '**' or mcsimp.definition.max > 1)
        if as_list and len(svalues) == 1:
            svalues.append("")
        svalues = ", ".join(svalues)
        if as_list:
            svalues = "(%s)" % svalues
        self.curline.extend([mcsimp.nom, "=", svalues, ","])
        self._count += 1

    def _repr_MCSIMP(self, mcsimp):
        """Return the representation of the value of a MCSIMP."""
        lval = force_list(mcsimp.valeur)
        svalues = []
        for i, value in enumerate(lval):
            if isinstance(value, ASSD):
                value.accept(self)
                repr_value = self.sdname
                if hasattr(mcsimp.etape, 'sdprods') and value in mcsimp.etape.sdprods:
                    repr_value = "CO(%s)" % repr_value
            elif is_float(value):
                repr_value = repr_float(value)
            elif mcsimp.definition.type[0] == 'shell':
                # the initial text of the formula is store in mcsimp.val
                repr_value = '"""%s"""' % mcsimp.val
            else:
                repr_value = repr(value)
            svalues.append(repr_value)
        return svalues

    def visitASSD(self, sd):
        """Visit the ASSD object."""
        try:
            self.sdname = sd.get_name()
        except:
            self.sdname = "sansnom"
