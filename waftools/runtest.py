#coding: utf-8

import os
import os.path as osp
import tempfile
from subprocess import Popen, PIPE

from waflib import TaskGen, Logs

def options(self):
    """To get the names of the testcases"""
    group = self.get_option_group("Aster options")
    group.add_option('-n', '--name', dest='testname',
                    action='append', default=None,
                    help='Name of testcases to run, comma separated')

@TaskGen.feature('test')
def runtest(self):
    """Run a testcase by calling as_run"""
    from Options import options as opts
    versdir = osp.join(self.env['PREFIX'], 'share', 'aster')
    for test in opts.testname:
        cmd = ['as_run', '--vers=%s' % versdir, '--test', test]
        Logs.info("running %s" % test)
        output = []
        proc = Popen(cmd, stdout=PIPE, bufsize=1)
        for line in iter(proc.stdout.readline, ''):
            output.append(line)
        proc.stdout.close()
        retcode = proc.wait()
        if retcode == 0:
            func = Logs.info
        else:
            func = Logs.error
            fd, fname = tempfile.mkstemp(prefix=test + '_')
            fobj = os.fdopen(fd, 'wb')
            fobj.write(''.join(output))
            fobj.close()
            Logs.warn("`- output in %s" % fname)
        func('`- exit %s' % retcode)
