# coding=utf-8

import os
import os.path as osp
from functools import partial
from waflib import Options, Configure, Errors, Logs, Utils


def options(self):
    self.load('compiler_c')
    self.load('compiler_cxx')
    self.load('compiler_fc')

    group = self.get_option_group("Code_Aster options")
    group.add_option('--enable-mpi', dest='parallel', action='store_true',
                    help='Build a parallel version with mpi')

def configure(self):
    if self.options.parallel:
        default = ['mpicc', 'mpicxx', 'mpif90']
    else:
        default = [''] * 3
    for var in ('CC', 'CXX', 'FC'):
        if not self.env[var]:
            self.add_os_flags(var)
        val = Utils.to_list(self.env[var])
        os.environ[var] = (val and val[0]) or default.pop(0)
    self.load_compilers()
    self.check_openmp()
    self.check_fortran_clib()

###############################################################################

@Configure.conf
def load_compilers(self):
    self.env.stash()                    # Store a snapshot of the environment
    self.load_compilers_mpi()           #   |
    if not self.get_define('HAVE_MPI'): #   |
        self.env.revert()               # <-'
        self.load('compiler_cc')
        self.load('compiler_cxx')
        self.load('compiler_fc')

@Configure.conf
def load_compilers_mpi(self):
    check = partial(self.check_cfg, args='--showme:compile --showme:link -show',
                    package='', uselib_store='MPI', mandatory=False)
    cc = os.environ.get('CC')
    cxx = os.environ.get('CXX')
    fc = os.environ.get('FC')
    if (cc and check(path=cc)) and (fc and check(path=fc)):
        self.check_mpi()
    elif self.options.parallel:
        self.fatal("Unable to configure the parallel environment")

@Configure.conf
def check_mpi(self):
    self.load('compiler_cc')
    self.load('compiler_cxx')
    self.load('compiler_fc')
    self.check_cc(header_name='mpi.h', use='MPI', define_name='_USE_MPI')

@Configure.conf
def check_openmp(self):
    try:
        self.check_fortran_verbose_flag()
        self.detect_openmp()
    except (Errors.ConfigurationError, Errors.BuildError):
        self.env.append_value('FCFLAGS_OPENMP', ['-fopenmp'])
        self.env.append_value('FCLINKFLAGS_OPENMP', ['-fopenmp'])
    if self.env.FCFLAGS_OPENMP:
        self.define('_USE_OPENMP', 1)

@Configure.conf
def check_sizeof_mpi_int(self):
    """Check size of MPI_Fint"""
    if self.get_define('HAVE_MPI'):
        fragment = '\n'.join([
            '#include <stdio.h>',
            '#include "mpi.h"',
            'int main(void){',
            '    MPI_Fint var;',
            '    printf("%d", (int)sizeof(var));',
            '    return 0;',
            '}',
            ''])
        self.code_checker('MPI_INT_SIZE', self.check_cc, fragment,
                          'Checking size of MPI_Fint integers',
                          'unexpected value for sizeof(MPI_Fint): %(size)s',
                          into=(4, 8))
