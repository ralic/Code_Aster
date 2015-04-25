# -*- coding: utf-8 -*-
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

'''
This module manages core settings of aster.
'''

# .. note:: Some of these functions are binded into C through
#   ``_aster_core`` (see ``bibc/aster_core_module.c``). They have been
#   written in python for convenience.

# .. note:: the _aster_core module contains globals that is set for a
#   particular computation. The actual implementation does not allow
#   to manage more than one computation.

import sys
from string import maketrans
from functools import partial
import time
from datetime import datetime

import aster_pkginfo
import _aster_core
# methods and attributes of C implementation of the module
from _aster_core import (
    register,
    matfpe,
    get_mem_stat,
    set_mem_stat,
    MPI_CommRankSize,
    MPI_Warn,
    MPI_Barrier,
    MPI_Bcast,
    MPI_GatherStr,
    _USE_MPI,
    _USE_OPENMP,
    _USE_64_BITS,
    _POSIX,
    _NO_EXPIR,
    ASTER_INT_SIZE,
)
# prefer use get_version()
__version__ = '.'.join(str(i) for i in aster_pkginfo.version_info.version)


def _is_initialized():
    return getattr(_aster_core, 'get_option', None) is not None


def get_option(option, default=None):
    '''return the setting parameter value.

    :option: a string containing the option name
    :default: the value to be returned if the option is not present
    '''
    if not _is_initialized():
        raise EnvironmentError(
            'aster_core must be initialized before (see aster_core.register) ')
    return _aster_core.get_option(option, default)


def get_version():
    '''Return the version number as string'''
    return __version__


def set_info(option, value):
    '''modify the setting parameter value.

    :option: a string containing the option name
    :value: the value to be affected
    '''
    if not _is_initialized():
        raise EnvironmentError(
            'aster_core must be initialized before (see aster_core.register) ')
    _aster_core.set_option(option, value)


def register(catalog, settings, logger=None):
    '''Register the catalog and the settings in order to share them with
    every aster component.

    This function must be called in order to initialize aster.

    :catalog: a JDC instance
    :settings: aster settings object (as given by aster_settings.CoreOptions)
    :logger: the message logger (default: Utilitai.Utmess.MessageLog)
    '''
    if logger is None:
        from Utilitai.Utmess import MessageLog  # prevent cycling import
        logger = MessageLog
    _aster_core.register(catalog, settings, logger, sys.modules[__name__])


def _print_alarm():
    import aster_core
    from Utilitai.Utmess import UTMESS
    changes = aster_pkginfo.version_info.changes
    uncommitted = aster_pkginfo.version_info.uncommitted
    if changes:
        UTMESS('A+', 'SUPERVIS_41',
               valk=__version__, vali=changes)
    if uncommitted and type(uncommitted) is list:
        fnames = ', '.join(uncommitted)
        UTMESS('A+', 'SUPERVIS_42',
               valk=(aster_pkginfo.version_info.parentid, fnames),)
    UTMESS('I', 'VIDE_1')


def _print_header():
    """Appelé par entete.F90 pour afficher des informations sur
    la machine."""
    from Execution.i18n import localization
    from Utilitai.Utmess import UTMESS
    import numpy
    lang_settings = '%s (%s)' % localization.get_current_settings()
    date_build = aster_pkginfo.version_info.date
    UTMESS('I', 'SUPERVIS2_4',
           valk=aster_pkginfo.get_version_desc())
    UTMESS('I', 'SUPERVIS2_23',
           valk=(__version__,
                 date_build,
                 aster_pkginfo.version_info.parentid[:12],
                 aster_pkginfo.version_info.branch),)
    UTMESS('I', 'SUPERVIS2_10',
           valk=("1991", time.strftime('%Y'),
                 time.strftime('%c'),
                 get_option('hostname'),
                 get_option('architecture'),
                 get_option('processor'),
                 get_option('system') + ' ' + get_option('osname')
                 + ' ' + get_option('osrelease'),
                 lang_settings,),)
    pyvers = '%s.%s.%s' % tuple(sys.version_info[:3])
    UTMESS('I', 'SUPERVIS2_9', valk=(pyvers, numpy.__version__))
    # avertissement si la version a plus de 15 mois
    if _aster_core._NO_EXPIR == 0:
        try:
            d0, m0, y0 = map(int, date_build.split('/'))
            tbuild = datetime(y0, m0, d0)
            tnow = datetime.today()
            delta = (tnow - tbuild).days
            if delta > 550:
                UTMESS('A', 'SUPERVIS2_2')
        except ValueError:
            pass


def print_header(part):
    """Appelé par entete.F90 pour afficher des informations sur la machine.
    Certaines informations étant obtenues en fortran, une partie des messages
    est imprimée par le fortran. On a donc découpé en plusieurs morceaux.
    part = 1 : entête principal : ici
    part = 2 : informations librairies : dans entete.F90
    part = 3 : message d'alarme en cas de modification du code source : ici
    """
    if part == 1:
        _print_header()
    elif part == 3:
        _print_alarm()
    else:
        raise ValueError("unknown value for 'part'")


def checksd(nomsd, typesd):
    """
    Vérifie la validité de la SD `nom_sd` (nom jeveux) de type `typesd`.
    Exemple : typesd = sd_maillage
    C'est le pendant de la "SD.checksd.check" à partir d'objets nommés.
    Code retour :
      0 : tout est ok
      1 : erreurs lors du checksd
      4 : on n'a meme pas pu tester
    """
    from Utilitai.Utmess import UTMESS
    import aster
    nomsd = nomsd.strip()
    typesd = typesd.strip().lower()
    # import
    iret = 4
    try:
        sd_module = __import__('SD.%s' % typesd, globals(), locals(), [typesd])
    except ImportError, msg:
        UTMESS('F', 'SDVERI_1', valk=typesd)
        return iret
    # on récupère la classe typesd
    clas = getattr(sd_module, typesd, None)
    if not clas:
        return iret

    objsd = clas(nomj=nomsd)
    chk = objsd.check()
    ichk = min([1, ] + [level for level, obj, msg in chk.msg])
    if ichk == 0:
        iret = 1
    else:
        iret = 0
    # on imprime les messages d'erreur (level=0):
    for level, obj, msg in chk.msg:
        if level == 0:
            aster.affiche('MESSAGE', repr(obj) + msg)
    return iret

_trans = maketrans('e', 'E')


def _fortran(srepr):
    """for fortran look"""
    return srepr.translate(_trans, '()')


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
    try:
        import aster
        from Utilitai.Utmess import UTMESS
        prresu = partial(aster.affiche, 'RESULTAT')
    except ImportError:
        def prresu(x):
            print x

        def UTMESS(a, b):
            print '<%s> message %s' % (a, b)
    lines = []

    def _print(wid, *args):
        """shortcut to print in the RESULTAT file"""
        fmtval = '%%-%ds' % wid
        fmtcols = ['%-4s ', '%-16s', '%-16s', fmtval, fmtval, '%-16s', '%-16s']
        assert len(args) <= 7, args
        fmt = ' '.join(fmtcols[:len(args)])
        line = fmt % args
        lines.append(line)
        prresu(line)
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
        UTMESS('A', 'TEST0_12')
    # formatting
    sref = '%s' % ref
    sval = '%s' % val
    wid = max([16, len(sref), len(sval)]) + 2
    serr = '%s' % error
    if len(serr) > 15:
        serr = '%13.6e' % error
    stol = '%s' % tole
    if relative:
        serr += '%'
        stol += '%'
    sref, sval, serr, stol = map(_fortran, [sref, sval, serr, stol])
    if diag == 'SKIP':
        legend = sref = sval = serr = stol = '-'
    # printing
    if compare != 1.:
        _print(wid, ' ', 'ORDRE DE GRANDEUR :', compare)
    if label:
        _print(wid, ' ', 'REFERENCE', 'LEGENDE',
                    'VALE_REFE', 'VALE_CALC', 'ERREUR', 'TOLE')
    _print(wid, diag, type_ref, legend, sref, sval, serr, stol)
    return lines


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
