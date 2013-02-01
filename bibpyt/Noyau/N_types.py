#@ MODIF N_types Noyau  DATE 28/01/2013   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
# RESPONSABLE COURTOIS M.COURTOIS

"""
   Ce module contient des fonctions utilitaires pour tester les types
"""

# eficas sentinel
try:
    import numpy as NP
    _np_arr = NP.ndarray
except ImportError:
    _np_arr = None

# use isinstance() instead of type() because objects returned from numpy arrays
# inherit from python scalars but are numpy.float64 or numpy.int32...
def is_int(obj):
    return isinstance(obj, int) or type(obj) is long

def is_float(obj):
    return isinstance(obj, float)

def is_complex(obj):
    return isinstance(obj, complex)

from decimal import Decimal
def is_float_or_int(obj):
    return is_float(obj) or is_int(obj) or isinstance(obj, Decimal)

def is_number(obj):
    return is_float_or_int(obj) or is_complex(obj)

def is_str(obj):
    return isinstance(obj, (str, unicode))

def is_list(obj):
    return type(obj) is list

def is_tuple(obj):
    return type(obj) is tuple

def is_array(obj):
    """a numpy array ?"""
    return type(obj) is _np_arr

def is_sequence(obj):
    """a sequence (allow iteration, not a string) ?"""
    return is_list(obj) or is_tuple(obj) or is_array(obj)

def is_assd(obj):
    from N_ASSD import ASSD
    return isinstance(obj, ASSD)


def force_list(obj):
    """Retourne `obj` si c'est une liste ou un tuple,
    sinon retourne [obj,] (en tant que list).
    """
    if not is_sequence(obj):
        obj = [obj,]
    return list(obj)

def force_tuple(obj):
    """Return `obj` as a tuple."""
    return tuple(force_list(obj))

# backward compatibility
from warnings import warn
def is_enum(obj):
    """same as is_sequence"""
    warn("'is_enum' is deprecated, use 'is_sequence'",
         DeprecationWarning, stacklevel=2)
    return is_sequence(obj)
