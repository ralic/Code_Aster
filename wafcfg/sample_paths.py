# encoding: utf-8

"""
Example of configuration to use non-standard paths
"""

def configure(self):
    from Options import options as opts
    self.env.append_value('LIBPATH', [
        '/opt/aster/public/hdf5-1.8.8/lib',
        '/opt/aster/public/med-3.0.5/lib',
        '/opt/aster/public/mumps-4.9.2/lib',
        '/opt/aster/public/metis-4.0.3/lib',
        '/opt/aster/public/scotch_5.1.11_esmumps/lib'])

    self.env.append_value('INCLUDES', [
        '/opt/aster/public/hdf5-1.8.8/include',
        '/opt/aster/public/med-3.0.5/include',
        '/opt/aster/public/scotch_5.1.11_esmumps/include'])

    opts.enable_med = True

    opts.enable_mumps = True
    opts.mumps_version = '4.9.2'
    opts.mumps_libs = 'dmumps zmumps smumps cmumps mumps_common pord metis'
    opts.embed_mumps = True

    opts.enable_scotch = True
    opts.embed_scotch = True

    opts.embed_aster = True
    opts.embed_fermetur = True
