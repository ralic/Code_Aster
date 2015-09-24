# coding=utf-8

"""
Example of configuration to use non-standard paths
"""

def configure(self):
    self.env.append_value('CFLAGS_ASTER_DEBUG', ['-D__DEBUG_ALL__'])
    self.env.append_value('FCFLAGS_ASTER_DEBUG', ['-D__DEBUG_ALL__'])

    opts = self.options
    self.env.append_value('LIBPATH', [
        '/opt/aster/public/hdf5-1.8.10/lib',
        '/opt/aster/public/med-3.0.7/lib',
        '/opt/aster/public/mumps-5.0.1/lib',
        '/opt/aster/public/metis-4.0.3/lib',
        '/opt/aster/public/scotch_5.1.11_esmumps/lib'])

    self.env.append_value('INCLUDES', [
        '/opt/aster/public/hdf5-1.8.10/include',
        '/opt/aster/public/med-3.0.7/include',
        '/opt/aster/public/mumps-5.0.1/include',
        '/opt/aster/public/mumps-5.0.1/include_seq',
        '/opt/aster/public/scotch_5.1.11_esmumps/include'])

    opts.enable_med = True

    opts.enable_mumps = True
    opts.mumps_libs = 'dmumps zmumps smumps cmumps mumps_common pord metis'
