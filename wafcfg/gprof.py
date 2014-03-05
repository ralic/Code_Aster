# coding=utf-8

"""
Configuration with profiling enabled

This configuration just adds option for gprof.
It must be used with another configuration.

Example for calibre7 + gprof:

cd $HOME/dev/codeaster/src
cp waf_variant waf_prof

./waf_prof configure --use-config=calibre7,gprof --prefix=../install/prof
./waf_prof install -p
"""


def configure(self):
    """Add flags for gprof"""
    self.env.append_value('FCFLAGS', ['-pg'])
    self.env.append_value('CFLAGS', ['-pg'])
    self.env.append_value('LINKFLAGS', ['-pg'])
