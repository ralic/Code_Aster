# coding=utf-8

"""
Configuration using Intel compilers

Use automatically MPI wrappers if opt.parallel was previously set.
"""

def configure(self):
    opts = self.options
    mpi = 'mpi' if opts.parallel else ''
    # Configure.find_program uses first self.environ, then os.environ
    self.environ['FC'] = mpi + 'ifort'
    self.environ['CC'] = mpi + 'icc'
    self.environ['CXX'] = mpi + 'icpc'
