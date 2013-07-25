# coding=utf-8

import os
import os.path as osp
from itertools import product, takewhile
from functools import partial
from subprocess import Popen, PIPE
from waflib import Options, Configure, Errors, Logs, Utils

BLAS = ('openblas', 'blacs', 'cblas', 'blas')
LAPACK = ('scalapack', 'lapack')

def options(self):
    group = self.add_option_group("Mathematics  libraries options")
    group.add_option('--maths-libs', type='string',
                    dest='maths_libs', default=None,
                    help='Math librairies to link with like blas and lapack')
    group.add_option('--embed-maths', dest='embed_math',
                    default=False, action='store_true',
                    help='Embed math libraries as static library')

def configure(self):
    from Options import options as opts

    # always check for libm
    self.check_cc(uselib_store='MATH', lib='m')
    if opts.maths_libs is None:
        self.detect_math_lib()
    elif opts.maths_libs:
        self.check_opts_math_lib()
    self.check_libm_after_files()

###############################################################################

@Configure.conf
def check_opts_math_lib(self):
    from Options import options as opts
    embed = opts.embed_math or opts.embed_all
    check_lib = lambda lib: self.check_cc(**{
        'mandatory':True, 'uselib_store':'MATH', 'use':'MPI',
        ('st' * embed + 'lib'):lib})

    for lib in Utils.to_list(opts.maths_libs):
        check_lib(lib)

@Configure.conf
def check_libm_after_files(self):
    """Avoid warning #10315: specifying -lm before files may supercede the
    Intel(R) math library and affect performance"""
    self.start_msg('Setting libm after files')
    flags = self.env.LINKFLAGS_CLIB
    if '-lm' in flags:
        while True:
            try:
                flags.remove('-lm')
            except ValueError:
                break
        self.end_msg('ok ("-lm" moved from LINKFLAGS_CLIB)')
        self.env.LINKFLAGS_CLIB = flags
    else:
        self.end_msg('nothing done')

@Configure.conf
def detect_math_lib(self):
    from Options import options as opts
    embed = opts.embed_math or opts.embed_all

    self.start_msg('Detecting math libraries')
    check_maths = partial(self.check_cc, uselib_store='MATH', use='MPI',
                          mandatory=False)
    if embed:
        check_lib = lambda lib: check_maths(stlib=lib)
    else:
        check_lib = lambda lib: check_maths(lib=lib)
    blaslibs, lapacklibs = self.get_mathlib_from_numpy()
    libs =  list(BLAS) + blaslibs
    if self.env.HAVE_MPI:
        libs = ['-'.join(n) for n in product(libs, ['mpi', 'openmpi'])] + libs
    for lib in libs:
        if check_lib(lib):
            break
    else:
        self.fatal('Missing the Blas library')

    libs = lapacklibs + list(LAPACK)
    if self.env.HAVE_MPI:
        libs = ['-'.join(n) for n in product(libs, ['mpi', 'openmpi'])] + libs
    for lib in libs:
        if check_lib(lib=lib):
            break
    else:
        self.fatal('Missing the LAPACK library')

    self.end_msg(self.env.STLIB_MATH if embed else self.env.LIB_MATH )


@Configure.conf
def get_mathlib_from_numpy(self):
    '''The idea is that numpy shall be linked to blas and lapack.
    So we will try to get then using ldd if available'''
    libblas = []
    pathblas = []
    liblapack = []
    pathlapack = []

    self.load('python')

    self.check_python_module('numpy')
    pymodule_path = self.get_python_variables(
        ['lapack_lite.__file__'],
        ['from numpy.linalg import lapack_lite'])[0]

    self.find_program('ldd')
    ldd_env = {'LD_LIBRARY_PATH': ':'.join(self.env.LIBPATH)}
    cmd = [self.env.LDD, pymodule_path]
    out = Popen(cmd, stdout=PIPE, env=ldd_env).communicate()[0]

    for line in out.split('\n'):
        lib = _detect_libnames_in_ldd_line(line, LAPACK)
        if lib:
            liblapack.append(lib)
            continue
        lib = _detect_libnames_in_ldd_line(line, BLAS)
        if lib:
            libblas.append(lib)
    return libblas, liblapack

def _detect_libnames_in_ldd_line(line, libnames):
    if not filter(line.__contains__, libnames):
        return None
    lib = line.split()[0].split('.', 1)[0]
    return lib[3:]
