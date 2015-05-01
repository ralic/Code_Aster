# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
import re
from pprint import pformat

try:
    import aster
    error = aster.error
except ImportError:
    error = StandardError

from Utilitai.Table import Table
from Utilitai.string_utils import cut_long_lines, maximize_lines
from Utilitai.utils import set_debug, _printDBG

# Aster type : regular expression
FMT = {
    'I': '([0-9\-\+]+)',
    'R': '([0-9\.,\-\+eEdD]+)',
    'K': '(.{%(len)s})'
}


def msplit(chaine, separ):
    """Similar to str.split(separ) with one or several occurrences
    of the separator.
    """
    return re.split('%s+' % re.escape(separ), chaine.strip(separ))


def convert(valk):
    """Try to convert a string to an integer or float... or a string."""
    try:
        try:
            try:
                val = int(valk)
            except ValueError:
                val = float(valk)
        except ValueError:
            # try with ',' as decimal separator
            val = float(valk.replace(',', '.'))
    except ValueError:
        val = valk
    return val


class TableReader(object):

    """Standard reader of a Table."""
    id_vide = ''

    def __init__(self, text, separator=' ', debug=False):
        """Initialization"""
        self.text = text
        self.sep = separator
        self.tab = self.title = self.lines = None
        self.debug = debug
        set_debug(debug)

    def _build_row(self, values):
        """Make a dict line."""
        dico = {}
        for para, valk in zip(self.tab.para, values):
            valk = valk.strip()
            if valk != self.id_vide:
                dico[para] = convert(valk)
        return dico

    def is_comment(self, line):
        """Tell if 'line' is a comment"""
        return False

    def read_line_i(self, i, line):
        """Read a line."""
        raise NotImplementedError('must be defined in a derivated class')

    def read_all_lines(self):
        """Loop on the lines and add values to the Table."""
        for i, line in enumerate(self.lines):
            values = self.read_line_i(i, line)
            self.tab.append(self._build_row(values))

    def read(self, nblock=None, check_para=None):
        """Read and create the Table.
        'check_para' is a function to check the list of the parameters."""
        raise NotImplementedError('must be defined in a derivated class')


class TableReaderFree(TableReader):

    """Table reader in a free format."""

    def split_tab(self, nblock):
        """Split the text to extract the 'nblock'-th block."""
        all_lines = [
            line for line in self.text.splitlines() if line.strip() != '']
        stat = [[], ]
        nbcol = 0
        curblock = 0
        _printDBG("NB: Les lignes sont numérotées après la suppression "
                  "des lignes vides.")
        _printDBG("Début de la table {} à la ligne {}".format(curblock+1, 1) )
        curblock_hasvalue = False
        for i, line in enumerate(all_lines):
            cur = len(msplit(line, self.sep))
            if self.is_comment(line) and not curblock_hasvalue:
                # only comments in this block
                cur = 0
            elif nbcol > 1 and cur < nbcol:
                # less fields = new block
                _printDBG("Début de la table {} à la ligne {}".format(curblock+2, i+1) )
                if curblock >= nblock and not self.debug:
                    break
                curblock += 1
                curblock_hasvalue = False
                stat.append([])
            nbcol = cur
            stat[curblock].append((nbcol, line))
            if not self.is_comment(line):
                curblock_hasvalue = True
        nbtab = len(stat)
        _printDBG("Nombre de blocs lus :", nbtab, pformat(stat))
        if nblock > nbtab:
            raise error('TABLE0_10', None, (nblock, nbtab))
        return stat[nblock - 1]

    def extract_lines(self, stat):
        """First global parsing to separate lines and title."""
        nbcol = max([i[0] for i in stat])
        self.lines = []
        ltit = []
        for i, line in stat:
            if i == nbcol and not self.is_comment(line):
                self.lines.append(line)
            else:
                ltit.append(line)
        ltit = cut_long_lines(os.linesep.join(ltit), 80)
        self.title = os.linesep.join(
            maximize_lines(ltit.splitlines(), 80, ' '))
        _printDBG("TITLE:", self.title)
        _printDBG("LINES:", '\n', self.lines)
        return nbcol

    def read_line_i(self, i, line):
        """Read a line."""
        return msplit(line, self.sep)

    def read(self, nblock=None, check_para=None):
        """Read and create the Table.
        'check_para' is a function to check the list of the parameters."""
        block_stat = self.split_tab(nblock)
        nbcol = self.extract_lines(block_stat)
        line_para = self.lines.pop(0)
        para = msplit(line_para, self.sep)
        if len(para) != nbcol:
            raise error('TABLE0_43', line_para, nbcol)
        # if sep != ' ', parameter may contain a space (not valid in Table)
        para = [p.replace(' ', '_') for p in para]
        if callable(check_para):
            para = check_para(para)
        _printDBG("PARAMS:", para)
        self.tab = Table(para=para, titr=self.title)
        self.read_all_lines()
        return self.tab


class TableReaderTableau(TableReaderFree):

    """Table reader in TABLEAU format."""
    id_vide = '-'

    def is_comment(self, line):
        """Tell if 'line' is a comment"""
        # _printDBG('is_comment : %s : %s' % (line.startswith('#'), line))
        return line.startswith('#')


class TableReaderAster(TableReader):

    """Table reader in ASTER format."""
    idt_deb = '#DEBUT_TABLE\n'
    idt_fin = '#FIN_TABLE\n'
    idt_tit = '#TITRE'
    id_vide = '-'

    def split_tab(self, nblock):
        """Split the text to extract the 'nblock'-th block."""
        expr = re.escape(self.idt_deb) + '(.*?)' + re.escape(self.idt_fin)
        re_split_tab = re.compile(expr, re.MULTILINE | re.DOTALL)
        l_txttab = re_split_tab.findall(self.text)
        nbtab = len(l_txttab)
        if nblock > nbtab:
            raise error('TABLE0_10', None, (nblock, nbtab))
        self.text = l_txttab[nblock - 1]
        _printDBG("TEXT:", self.text)

    def set_title(self):
        """Extract the title"""
        exp = re.compile(re.escape(self.idt_tit) + '(.*)$', re.M)
        self.title = os.linesep.join(
            [s.strip(self.sep) for s in exp.findall(self.text)])
        _printDBG("TITLE:", self.title)

    def extract_lines(self):
        """Extract the text of the lines"""
        self.lines = [line for line in self.text.splitlines()
                      if line.strip(self.sep) != '' and not line.startswith(self.idt_tit)]
        _printDBG("LINES:", '\n', self.lines)

    def read_line_i(self, i, line):
        """Read a line."""
        para = self.tab.para
        mat = re.search(self.re_line, line)
        _printDBG(line, len(para), mat)
        if mat is None or len(para) != len(mat.groups()):
            lerr = [error('TABLE0_11', vali=i + 1),
                    error('TABLE0_13', vali=len(para))]
            if mat is not None:
                lerr.append(error('TABLE0_12', vali=len(mat.groups())))
            else:
                lerr.append(error('TABLE0_15', valk=(line, self.re_line)))
            raise error(lerr)
        return mat.groups()

    def read(self, nblock, check_para=None):
        """Read and create the Table.
        'check_para' is a function to check the list of the parameters."""
        self.split_tab(nblock)
        self.set_title()
        self.extract_lines()

        # ligne des paramètres et des types
        para = msplit(self.lines.pop(0), self.sep)
        types = msplit(self.lines.pop(0), self.sep)
        if callable(check_para):
            para = check_para(para)
        _printDBG("PARAMS:", para)
        _printDBG("TYPES:", types)
        self.tab = Table(para=para, typ=types, titr=self.title)

        lfmt = [FMT[typ[0]] % {'len': typ[1:]} for typ in types]
        self.re_line = ('%s+' % re.escape(self.sep)).join(lfmt)
        _printDBG("REGEXP:", self.re_line)

        self.read_all_lines()
        return self.tab


def TableReaderFactory(text, fmt, separator, debug=False):
    """Return the appropriate reader."""
    if fmt == 'ASTER':
        return TableReaderAster(text, separator, debug)
    elif fmt == 'TABLEAU':
        return TableReaderTableau(text, separator, debug)
    else:
        return TableReaderFree(text, separator, debug)


def unique_parameters(l_para):
    """Rename parameters to be unique."""
    res = []
    for par in l_para:
        newp = par
        i = 0
        while newp in res:
            i += 1
            newp = '%s_%d' % (par, i)
        res.append(newp)
    return res


if __name__ == '__main__':
    txt = """
A B C
1. 2. nom
3. 4. valeur
"""
    reader = TableReaderFree(txt)
    tab = reader.read(1)
    print tab

    taster = """
#DEBUT_TABLE
#TITRE table aster
A B C
R R K8
1. 2. nom     \n
3. 4. valeur  \n
#FIN_TABLE
"""
    reader = TableReaderAster(taster)
    tab = reader.read(1)
    print tab
