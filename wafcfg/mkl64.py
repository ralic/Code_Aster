# coding=utf-8

"""
Example of configuration using sequential MKL librairies

To adjust the librairies names for your platform, try :
http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
"""

def configure(self):
    from Options import options as opts
    self.env.append_value('OPTLIB_FLAGS', [
        '-Wl,-Bstatic', '-Wl,--start-group',
        '-lmkl_intel_lp64', '-lmkl_sequential', '-lmkl_core',
        '-Wl,--end-group'])
    opts.maths_libs = ''
