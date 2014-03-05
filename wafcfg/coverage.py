# coding=utf-8

"""
Configuration for code coverage with Intel compilers

This configuration just adds Intel option for code coverage.
It must be used with another configuration.

Example for calibre7 + coverage:

cd $HOME/dev/codeaster/src
cp waf_variant waf_cov

./waf_cov configure --use-config=calibre7,coverage --prefix=../install/cov
./waf_cov install -p
"""


def configure(self):
    """Add flags for code coverage"""
    self.env['ADDMEM'] = 500

    self.env.append_value('FCFLAGS', ['-prof-gen=srcpos'])
    self.env.append_value('CFLAGS', ['-prof-gen=srcpos'])
    self.env.append_value('LINKFLAGS', ['-prof-gen=srcpos'])
