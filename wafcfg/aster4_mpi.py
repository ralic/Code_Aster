# coding: utf-8

"""
Configuration for aster4_mpi

. $HOME/dev/codeaster/devtools/etc/env_unstable_mpi.sh

waf configure --use-config=aster4_mpi --prefix=../install/mpi
waf install -p
"""

import aster4
YAMMROOT = aster4.YAMMROOT

def configure(self):
    from Options import options as opts
    
    aster4.configure(self)
    self.env['ADDMEM'] = 400
    self.env['OPTLIB_FLAGS'] = [
        '-Wl,-Bstatic', '-Wl,--start-group',
        '-lmkl_intel_lp64', '-lmkl_intel_thread', '-lmkl_core',
        '-lmkl_scalapack_lp64', '-lmkl_blacs_intelmpi_lp64',
        '-Wl,--end-group']
    
    self.env.append_value('OPT_ENV', [
        '. /aster/etc/codeaster/profile_intel_mpi.sh'])

    self.env['FC'] = 'mpiifort'
    self.env['CC'] = 'mpiicc'
    self.env['CXX'] = 'mpiicpc'

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + 'prerequisites/Mumps_mpi_for_aster/lib',
        YAMMROOT + 'prerequisites/Petsc_mpi_petsc_aster/lib'])
    self.env.prepend_value('INCLUDES', [
        YAMMROOT + 'prerequisites/Petsc_mpi_petsc_aster/include'])

    opts.parallel = True
    
    opts.enable_petsc = True
    opts.embed_petsc = True
    opts.petsc_libs='petsc ml HYPRE'
