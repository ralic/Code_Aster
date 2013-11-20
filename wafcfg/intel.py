# coding=utf-8

"""
Example of configuration using Intel compilers
"""

def configure(self):
    from Options import options as opts
    mpi = 'mpi' if opts.parallel else ''
    self.env['FC'] = mpi + 'ifort'
    self.env['CC'] = mpi + 'icc'
    self.env['CXX'] = mpi + 'icpc'
