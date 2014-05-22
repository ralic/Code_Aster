# coding=utf-8

import os
import os.path as osp
import tempfile
from subprocess import Popen, PIPE

from waflib import TaskGen, Logs, Errors

def options(self):
    """To get the names of the testcases"""
    group = self.get_option_group("Code_Aster options")
    group.add_option('-n', '--name', dest='testname',
                    action='append', default=None,
                    help='name of testcases to run (as_run must be in PATH)')
    group.add_option('--outputdir', action='store', default=None, metavar='DIR',
                    help='directory to store the output files')
    group.add_option('--exectool', dest='exectool',
                    action='store', default=None,
                    help='run a testcase by passing additional arguments '
                         '(possible values are "debugger", "env" + those '
                         'defined in the as_run configuration)')

@TaskGen.feature('test')
def runtest(self):
    """Run a testcase by calling as_run"""
    opts = self.options
    if not _has_asrun():
        Logs.error("'as_run' not found, please check your $PATH")
        return
    toolargs = []
    if opts.exectool == 'debugger':
        toolargs.append('--debugger')
    elif opts.exectool == 'env':
        toolargs.append('--run_params=actions=make_env')
    elif opts.exectool is not None:
        toolargs.append('--exectool=%s' % opts.exectool)
    dtmp = opts.outputdir or tempfile.mkdtemp(prefix='runtest_')
    try:
        os.makedirs(dtmp)
    except (OSError, IOError):
        pass
    Logs.info("destination of output files: %s" % dtmp)
    status = 0
    if not opts.testname:
        raise Errors.WafError('no testcase name provided, use the -n option')
    for test in opts.testname:
        cmd = ['as_run', '--vers=%s' % self.env['ASTERDATADIR'], '--test', test]
        if self.variant == 'debug':
            cmd.extend(['-g', '--nodebug_stderr'])
        cmd.extend(toolargs)
        Logs.info("running %s in '%s'" % (test, self.variant))
        fname = osp.join(dtmp, osp.basename(test) + '.output')
        fobj = open(fname, 'wb')
        Logs.info("`- output in %s" % fname)
        nook = False
        proc = Popen(cmd, stdout=PIPE, bufsize=1)
        for line in iter(proc.stdout.readline, ''):
            fobj.write(line)
            nook = nook or 'NOOK_TEST_RESU' in line
            fobj.flush()
        proc.stdout.close()
        fobj.close()
        retcode = proc.wait()
        if nook and retcode == 0:
            retcode = 'nook'
        if retcode == 0:
            func = Logs.info
        else:
            func = Logs.error
            status += 1
        func('`- exit %s' % retcode)
        notify('testcase %s ended - exit %s' % (test, retcode), errlevel=retcode)
    if status != 0:
        raise Errors.WafError('testcase failed')


def _has_asrun():
    """check that as_run is available"""
    try:
        iret = Popen(['as_run', '--version'], stdout=PIPE, stderr=PIPE).wait()
    except OSError:
        iret = 127
    return iret == 0

def notify(message, errlevel=0):
    """Send a message as a notification bubble"""
    title = 'codeaster waf'
    d_icon = {
        0 : 'weather-clear',
        'nook' : 'weather-overcast',
        1 : 'weather-storm',
    }
    icon = d_icon.get(errlevel, d_icon[1])
    try:
        Popen(['notify-send', '-i', icon, title, message])
    except OSError:
        pass
