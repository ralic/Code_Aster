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

"""Module rassemblant des fonctions utilitaires de manipulations
de chaines de caractères
"""
# module identique à Execution/strfunc.py pour usage dans Eficas

import locale

_encoding = None
def get_encoding():
    """Return local encoding
    """
    global _encoding
    if _encoding is None:
        try:
            _encoding = locale.getpreferredencoding() or 'ascii'
        except locale.Error:
            _encoding = 'ascii'
    return _encoding


def to_unicode(string):
    """Try to convert string into a unicode string."""
    if type(string) is unicode:
        return string
    elif type(string) is dict:
        new = {}
        for k, v in string.items():
            new[k] = to_unicode(v)
        return new
    elif type(string) is list:
        return [to_unicode(elt) for elt in string]
    elif type(string) is tuple:
        return tuple(to_unicode(list(string)))
    elif type(string) is not str:
        return string
    assert type(string) is str, u"unsupported object: %s" % string
    for encoding in ('utf-8', 'iso-8859-15', 'cp1252'):
        try:
            s = unicode(string, encoding)
            return s
        except UnicodeDecodeError:
            pass
    return unicode(string, 'utf-8', 'replace')

def from_unicode(ustring, encoding, errors='replace'):
    """Try to encode a unicode string using encoding."""
    try:
        return ustring.encode(encoding)
    except UnicodeError:
        pass
    return ustring.encode(encoding, errors)

def convert(content, encoding=None, errors='replace'):
    """Convert content using encoding or default encoding if None."""
    if type(content) not in (str, unicode):
        content = unicode(content)
    if type(content) == str:
        content = to_unicode(content)
    return from_unicode(content, encoding or get_encoding(), errors)

def ufmt(uformat, *args):
    """Helper function to format a string by converting all its arguments to unicode"""
    if type(uformat) is not unicode:
        uformat = to_unicode(uformat)
    if len(args) == 1 and type(args[0]) is dict:
        arguments = to_unicode(args[0])
    else:
        nargs = []
        for arg in args:
            if type(arg) in (str, unicode, list, tuple, dict):
                nargs.append(to_unicode(arg))
            elif type(arg) not in (int, long, float):
                nargs.append(to_unicode(str(arg)))
            else:
                nargs.append(arg)
        arguments = tuple(nargs)
    try:
        formatted_string = uformat % arguments
    except UnicodeDecodeError, err:
        print type(uformat), uformat
        print type(arguments), arguments
        raise
    return formatted_string
