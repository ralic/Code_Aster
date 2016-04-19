# -*- coding: utf-8 -*-
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

"""
This module defines objects for the testing feature.
"""

import re
from functools import partial
from string import maketrans
from glob import glob


_trans = maketrans('e', 'E')


def _fortran(srepr):
    """for fortran look"""
    return srepr.translate(_trans, '()')


class TestResult(object):

    """This class provides the feature to print the testcase results.
    A singleton object is created to avoid to repeat some global tasks.
    """

    def __init__(self):
        """Initialization"""
        try:
            import aster
            from Utilitai.Utmess import UTMESS
            self._printLine = partial(aster.affiche, 'RESULTAT')
            self._utmess = UTMESS
        except ImportError:
            self._printLine = _internal_print
            self._utmess = _internal_mess
        self._isVerif = self._checkVerif()
        if not self._isVerif:
            self._utmess('I', 'TEST0_19')

    def isVerif(self):
        """Tell if the testcase is a verification one"""
        return self._isVerif

    def write(self, width, *args):
        """shortcut to print in the RESULTAT file"""
        fmtval = '%%-%ds' % width
        fmtcols = ['%-4s ', '%-16s', '%-16s', fmtval, fmtval, '%-16s', '%-16s']
        assert len(args) <= 7, args
        fmt = ' '.join(fmtcols[:len(args)])
        line = fmt % args
        self._printLine(line)
        return line

    def showResult(self, type_ref, legend, label, skip, relative,
                   tole, ref, val, compare=1.):
        """Print a table for TEST_RESU

        type_ref : ANALYTIQUE, NON_REGRESSION, AUTRE_ASTER...
        legend : component name or XXXX
        label : boolean to print or not the labels
        skip : boolean to skip the test and print an empty line
        relative : boolean, True if for relative, False for absolute comparison
        tole : maximum error tolerated
        ref : reference value (integer, real or complex)
        val : computed value (same type as ref)
        compare : order of magnitude
        """
        # ignore NON_REGRESSION tests for validation testcases
        isNonRegr = type_ref.strip() == "NON_REGRESSION"
        isValidIgn = isNonRegr and not self._isVerif
        lines = ['pass in showResult']
        # compute
        diag = 'SKIP'
        error = '-'
        if not skip:
            error = abs(1. * ref - val)
            tole = 1. * tole
            if relative:
                ok = error <= abs((tole * ref))
                tole = tole * 100.
                if ref != 0.:
                    error = error / abs(ref) * 100.
                elif ok:
                    error = 0.
                else:
                    error = 999.999999
            else:
                tole = abs(tole * compare)
                ok = error <= tole
            diag = ' OK ' if ok else 'NOOK'
        else:
            # do not warn if validation testcase
            if not isValidIgn:
                self._utmess('A', 'TEST0_12')
        # formatting
        sref = '%s' % ref
        sval = '%s' % val
        width = max([16, len(sref), len(sval)]) + 2
        serr = '%s' % error
        if len(serr) > 15:
            serr = '%13.6e' % error
        stol = '%s' % tole
        if relative:
            serr += '%'
            stol += '%'
        sref, sval, serr, stol = [_fortran(i) for i in [sref, sval, serr, stol]]
        if diag == 'SKIP':
            legend = sref = sval = serr = stol = '-'
        # printing
        if compare != 1.:
            lines.append(self.write(width, ' ', 'ORDRE DE GRANDEUR :', compare))
        if label:
            lines.append(self.write(width, ' ', 'REFERENCE', 'LEGENDE',
                                    'VALE_REFE', 'VALE_CALC', 'ERREUR', 'TOLE'))
        if isValidIgn:
            lines.append(self.write(width, "-", type_ref, legend, sref,
                                    sval, serr, "-"))
        else:
            lines.append(self.write(width, diag, type_ref, legend, sref,
                                    sval, serr, stol))
        return lines

    def _checkVerif(self):
        """Check if the current execution is for a verification testcase
        (and not a validation one)."""
        exports = glob("*.export")
        if not exports:
            # export file not found, return "verification" that is more strict!
            return True
        text = open(exports[0], "rb").read()
        expr = re.compile("^P +testlist.*validation", re.M)
        isVerif = expr.search(text) is None
        return isVerif


def testresu_print(type_ref, legend, label, skip, relative,
                   tole, ref, val, compare=1.):
    """Print a table for TEST_RESU

    type_ref : ANALYTIQUE, NON_REGRESSION, AUTRE_ASTER...
    legend : component name or XXXX
    label : boolean to print or not the labels
    skip : boolean to skip the test and print an empty line
    relative : boolean, True if for relative, False for absolute comparison
    tole : maximum error tolerated
    ref : reference value (integer, real or complex)
    val : computed value (same type as ref)
    compare : order of magnitude
    """
    lines = testPrinter.showResult(type_ref, legend, label, skip, relative,
                                   tole, ref, val, compare)
    return lines


def _internal_print(text):
    """Define a basic print function for unittest"""
    print(text)


def _internal_mess(a, b):
    """UTMESS replacement for unittest"""
    print('<{0}> message {1}').format(a, b)


# Creation of the singleton instance
testPrinter = TestResult()


if __name__ == '__main__':
    testresu_print('NON_REGRESSION', 'DX', True, False, False,
                   1.e-6, 1.123e-6, 0.0, compare=275.0)
    testresu_print('AUTRE_ASTER', 'DX', False, False, False,
                   1.e-6, 1.123e-6, 0.0)
    print

    testresu_print('NON_REGRESSION', 'DX', True, True, False,
                   1.e-6, 1.123e-6, 0.0)
    testresu_print('NON_REGRESSION', 'XXXXX', True, False, False,
                   1.e-6, 1.123e-3, 0.0, compare=275.0)
    print

    testresu_print('NON_REGRESSION', 'XXXXX', True, False, True,
                   1.e-6, 1.123e-2, 0.0)
    print

    testresu_print('NON_REGRESSION', 'XXXXX', True, False, True,
                   0.02, 456, 458)
    print

    testresu_print('ANALYTIQUE', 'DEPL_C', True, False, True,
                   1.e-4, 1. + 1.j, -0.5 + 0.99j)
    print
