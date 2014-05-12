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
    opts = self.options
    
    # parallel must be set before calling intel.configure() to use MPI wrappers
    opts.parallel = True
    aster4.configure(self)
    self.env['ADDMEM'] = 400
    self.env['OPTLIB_FLAGS_MATH'] = [
        '-Wl,--start-group', '-Wl,-Bstatic',
        '-lmkl_intel_lp64', '-lmkl_intel_thread', '-lmkl_core',
        '-lmkl_scalapack_lp64', '-lmkl_blacs_intelmpi_lp64', '-liomp5',
        '-Wl,--end-group']
    
    self.env.append_value('OPT_ENV', [
        '. ' + ASTER_ROOT + '/etc/codeaster/profile_intel_mpi.sh'])

    self.env.prepend_value('LIBPATH', [
        YAMMROOT + '/prerequisites/Mumps_mpi_20141/lib',
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/lib'])
    self.env.prepend_value('INCLUDES', [
        YAMMROOT + '/prerequisites/Petsc_mpi_petsc_aster/include'])

    opts.enable_petsc = True
