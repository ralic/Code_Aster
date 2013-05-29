# coding=utf-8

"""
Example of configuration using Intel compilers
"""

def configure(self):
    from Options import options as opts
    self.env['FC'] = 'ifort'
    self.env['CC'] = 'icc'
    self.env['CXX'] = 'icpc'
