# coding=utf-8

import os
import os.path as osp
from itertools import product, takewhile
from functools import partial
from subprocess import Popen, PIPE
from waflib import Options, Configure, Errors, Logs, Utils

BLAS = ('openblas', 'blas')
BLACS = ('blacs', )
LAPACK = ('lapack', )
SCALAPACK = ('scalapack', )
OPTIONAL_DEPS = ('cblas', )

###############################################################################
from waflib.Tools import fc, c
# original run_str command line is store as hcode
class fcprogram(fc.fcprogram):
    """Link object files into a fortran program, add optional OPTLIB_FLAGS at the end"""
    run_str = fc.fcprogram.hcode + ' ${OPTLIB_FLAGS}'

class cprogram(c.cprogram):
    """Link object files into a C program, add optional OPTLIB_FLAGS at the end"""
    run_str = c.cprogram.hcode + ' ${OPTLIB_FLAGS}'

###############################################################################
def options(self):
    group = self.add_option_group("Mathematics  libraries options")
    group.add_option('--maths-libs', type='string',
                    dest='maths_libs', default=None,
                    help='Math librairies to link with like blas and lapack. '
                         'Use None or "auto" to search them automatically.')
    group.add_option('--embed-maths', dest='embed_math',
                    default=False, action='store_true',
                    help='Embed math libraries as static library')

def configure(self):
    from Options import options as opts

    # always check for libpthread, libm
    self.check_cc(uselib_store='MATH', lib='pthread')
    self.check_cc(uselib_store='MATH', lib='m')
    if opts.maths_libs in (None, 'auto'):
        # try MKL first, then automatic blas/lapack
        if not self.detect_mkl():
            self.detect_math_lib()
    elif opts.maths_libs:
        self.check_opts_math_lib()
    self.check_libm_after_files()
    self.check_math_libs_call()

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
def check_sizeof_blas_int(self):
    """Check size of blas integers"""
    self.set_define_from_env('BLAS_INT_SIZE',
                             'Setting size of blas/lapack integers',
                             'unexpected value for blas int: %(size)s',
                             into=(4, 8), default=4)

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
        self.end_msg('ok ("-lm" moved to LINKFLAGS_CLIB)')
        self.env.LINKFLAGS_CLIB = flags
    else:
        self.end_msg('nothing done')

@Configure.conf
def detect_mkl(self):
    """Try to use MKL if ifort was detected"""
    from Options import options as opts
    embed = opts.embed_math or opts.embed_all
    if self.env.FC_NAME != 'IFORT':
        return
    self.start_msg('Detecting MKL libraries')
    suffix = '_lp64' if self.env.DEST_CPU.endswith('64') else ''
    # first: out of the box (OPTLIB_FLAGS as provided)
    totest = ['']
    # http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
    if self.env.HAVE_MPI:
        totest.append('-mkl=cluster')
        scalapack = ['-lmkl_scalapack' + suffix or '_core']   # ia32: mkl_scalapack_core
        blacs = ['-lblacs_intelmpi' + suffix]
    else:
        scalapack = []
        blacs = []
    interf = 'mkl_intel' + suffix
    for typ in ('sequential', 'parallel'):
        totest.append('-mkl=' + typ)
        thread = 'mkl_sequential' if typ == 'sequential' else 'mkl_intel_thread'
        core = 'mkl_core'
        libs = ['-l%s' % name for name in (interf, thread, core)]
        totest.append(scalapack + libs + blacs)
        libs = ['-Wl,--start-group'] + libs + ['-Wl,--end-group']
        totest.append(libs)
    Logs.debug("\ntest: %r" % totest)
    while totest:
        self.env.stash()
        opts = totest.pop(0)
        if opts:
            self.env.append_value('OPTLIB_FLAGS', opts)
        try:
            self.check_math_libs_call()
        except:
            self.env.revert()
            continue
        else:
            self.end_msg(self.env['OPTLIB_FLAGS'])
            return True
    self.end_msg('no', color='YELLOW')
    return False

@Configure.conf
def detect_math_lib(self):
    from Options import options as opts
    embed = opts.embed_math or (opts.embed_all and not self.env.HAVE_MPI)
    varlib = ('ST' if embed else '') + 'LIB_MATH'
    self.start_msg('Detecting math libraries')

    # blas
    blaslibs, lapacklibs = self.get_mathlib_from_numpy()
    self.check_math_libs('blas', list(BLAS) + blaslibs, embed)
    # lapack
    if 'openblas' not in self.env.get_flat(varlib):
        self.check_math_libs('lapack', list(LAPACK) + lapacklibs, embed)

    # parallel
    if self.env.HAVE_MPI:
        # scalapack
        libs = list(SCALAPACK)
        libs = ['-'.join(n) for n in product(libs, ['mpi', 'openmpi'])] + libs
        self.check_math_libs('scalapack', libs, embed)
        # blacs
        libs = list(BLACS)
        libs = ['-'.join(n) for n in product(libs, ['mpi', 'openmpi'])] + libs
        self.check_math_libs('blacs', libs, embed)

    # optional dependencies
    self.check_math_libs('optional', OPTIONAL_DEPS, embed, optional=True)

    self.end_msg(self.env[varlib])
    if self.env.HAVE_MPI and embed:
        msg = "WARNING:\n"\
              "    Static link with MPI libraries is not recommended.\n"\
              "    Remove the option --embed-maths in case of link error.\n"\
              "    See http://www.open-mpi.org/faq/?category=mpi-apps#static-mpi-apps"
        Logs.warn(msg)

@Configure.conf
def check_math_libs(self, name, libs, embed, optional=False):
    """Check for library 'name', stop on first found"""
    check_maths = partial(self.check_cc, uselib_store='MATH', use='MPI',
                          mandatory=False)
    if embed:
        check_lib = lambda lib: check_maths(stlib=lib)
    else:
        check_lib = lambda lib: check_maths(lib=lib)
    for lib in libs:
        if check_lib(lib=lib):
            break
    else:
        if not optional:
            self.fatal('Missing the %s library' % name)

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

@Configure.conf
def check_math_libs_call(self):
    """Compile and run a small blas/lapack program"""
    self.start_msg('Checking for a program using blas/lapack')
    try:
        ret = self.check_fc(fragment=blas_lapack_fragment, use='MATH MPI',
                            mandatory=False, execute=True, define_ret=True)
        values = map(float, ret and ret.split() or [])
        ref = [10.0, 5.0]
        if list(values) != ref:
            raise Errors.ConfigurationError('invalid result: %r (expecting %r)' % (values, ref))
    except:
        self.end_msg('no', 'YELLOW')
        raise
    else:
        self.end_msg('ok')

# program testing a blas and a lapack call, output is 10.0 and 5.0
blas_lapack_fragment = r"""
subroutine test(res, res2)
    implicit none
    real(kind=8), intent(out) :: res, res2
!
    real(kind=8) :: ddot, dlapy2
    real(kind=8) :: a1(2), a2(2)
    integer  i
!
    do i = 1, 2
        a1(i) = 1.d0 * i
        a2(i) = 2.d0 * i
    end do
    res = ddot(2, a1, 1, a2,1)
    res2 = dlapy2(3.d0, 4.d0)
end subroutine test

program main
    real(kind=8) :: a, b
    call test(a, b)
    print *, a
    print *, b
end program main
"""