# coding=utf-8

from waflib import Configure

###############################################################################
# Add OPTLIB_FLAGS support
from waflib.Tools import fc, c, ccroot
# original run_str command line is store as hcode
for feature in ('c', 'cxx', 'cprogram', 'cshlib', 'fc', 'fcprogram', 'fcshlib'):
    ccroot.USELIB_VARS[feature].add('OPTLIB_FLAGS')

class fcprogram(fc.fcprogram):
    """Link object files into a fortran program, add optional OPTLIB_FLAGS at the end"""
    run_str = fc.fcprogram.hcode + ' ${OPTLIB_FLAGS}'

class cprogram(c.cprogram):
    """Link object files into a C program, add optional OPTLIB_FLAGS at the end"""
    run_str = c.cprogram.hcode + ' ${OPTLIB_FLAGS}'

###############################################################################
# Force static libs
CHECK = '_check'

@Configure.conf
def _force_stlib(self, *args, **kwargs):
    """Always use 'stlib' keyword argument"""
    kwargs = kwargs.copy()
    stlib = kwargs.get('stlib') or kwargs.get('lib')
    if stlib:
        kwargs['stlib'] = stlib
    try:
        del kwargs['lib']
    except KeyError:
        pass
    return getattr(self, CHECK)(*args, **kwargs)

@Configure.conf
def static_lib_pref(self):
    """Change temporarly the 'check' method"""
    if not self.options.embed_all:
        return
    if getattr(self, CHECK, None) is None:
        setattr(self, CHECK, self.check)
    self.check = self._force_stlib

@Configure.conf
def revert_lib_pref(self):
    """Restore original method"""
    if not self.options.embed_all:
        return
    self._force_stlib = getattr(self, CHECK)
