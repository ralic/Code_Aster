# coding=utf-8

import os
import os.path as osp
from functools import partial
from waflib import Options, Configure, Errors, Logs


def options(self):
    self.load('compiler_c')
    self.load('compiler_cxx')
    self.load('compiler_fc')

    group = self.get_option_group("Aster options")
    group.add_option('--enable-mpi', dest='parallel', action='store_true',
                    help='Build a parallel version with mpi')

def configure(self):
    from Options import options as opts
    if opts.parallel:
        default = ['mpicc', 'mpicxx', 'mpif90']
    else:
        default = [''] * 3
    os.environ['CC'] = self.env.CC or default.pop(0)
    os.environ['CXX'] = self.env.CXX or default.pop(0)
    os.environ['FC'] = self.env.FC or default.pop(0)
    self.load_compilers()
    self.check_openmp()

    self.check_fortran_clib()
    if self.env.FC_NAME == 'IFORT':
        defines = ['_USE_INTEL_IFORT', '_DISABLE_MATHLIB_FPE']
        self.env.append_unique('DEFINES', defines)

###############################################################################

@Configure.conf
def load_compilers(self):
    self.env.stash()          # Store a snapshot of the environment
    self.load_compilers_mpi() #   |
    if not self.env.HAVE_MPI: #   |
        self.env.revert()     # <-'
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
        self.env.append_unique('CCNAME', osp.basename(cc))
        self.env.append_unique('CXXNAME', osp.basename(cxx))
        self.env.append_unique('FCNAME', osp.basename(fc))
        self.check_mpi()

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
        self.env.append_unique('DEFINES', ['_USE_OPENMP'])
