# coding=utf-8

import os
import os.path as osp
from functools import partial
from waflib import Options, Configure, Errors, Logs

def options(self):
    #self.load('python')    # only --nopyc/--nopyo, always disabled below
    pass

def configure(self):
    self.check_python()
    self.check_numpy()

###############################################################################

@Configure.conf
def check_python(self):
    self.load('python')
    self.check_tool('python')
    self.check_python_version((2, 6, 0))
    self.check_python_headers()

@Configure.conf
def check_numpy(self):
    if not self.env['PYTHON']:
        self.fatal('load python tool first')
    self.start_msg('Checking for numpy')
    # getting python module
    self.check_python_module('numpy')
    # retrieve includes dir from numpy module
    numpy_includes = self.get_python_variables(
        ['"\\n".join(misc_util.get_numpy_include_dirs())'],
        ['from numpy.distutils import misc_util'])
    # check the given includes dirs
    self.check(
                     feature = 'c',
                 header_name = 'Python.h numpy/arrayobject.h',
                    includes = numpy_includes,
                     defines = 'NPY_NO_PREFIX',
                         use = ['PYEMBED'],
                uselib_store = 'NUMPY',
        errmsg='Could not find the numpy development headers'
    )
    self.end_msg(numpy_includes)

@Configure.conf
def check_optimization_python(self):
    self.setenv('debug')
    self.env['PYC'] = self.env['PYO'] = 0
    self.setenv('release')
    self.env['PYC'] = self.env['PYO'] = 0
