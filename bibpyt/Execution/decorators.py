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

"""Collection of decorators"""

import sys
import traceback
from functools import wraps


def jdc_required(method):
    """decorator to check that the jdc attribute has been initialized"""
    @wraps(method)
    def wrapper(inst, *args, **kwds):
        """wrapper"""
        assert inst.jdc is not None, 'jdc must be initialized (call InitJDC(...) before)'
        return method(inst, *args, **kwds)
    return wrapper


def stop_on_returncode(method):
    """decorator that calls the interrupt method if the returncode
    is not null"""
    @wraps(method)
    def wrapper(inst, *args, **kwds):
        """wrapper"""
        errcode = method(inst, *args, **kwds)
        if errcode:
            inst.interrupt(errcode)
        return errcode
    return wrapper


def never_fail(func):
    """decorator to wrap functions that must never fail"""
    @wraps(func)
    def wrapper(*args, **kwds):
        """wrapper"""
        try:
            ret = func(*args, **kwds)
        except Exception, exc:
            traceback.print_exc(file=sys.stdout)
            print 'continue...'
            ret = None
        return ret
    return wrapper
