# coding=utf-8

"""
Configuration to disable MFront support even if a upper configuration enabled it.

It is usually used with another configuration.

Example for calibre7 + no_mfront:

./waf configure --use-config=calibre7,no_mfront
./waf install
"""


def configure(self):
    """Add flags for gprof"""
    self.options.enable_mfront = False
