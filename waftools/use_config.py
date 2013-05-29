# coding=utf-8

import sys
import os.path as osp

from waflib import Configure, Errors, Context

DEFAULT_DIR = 'wafcfg'
sys.path.append(osp.abspath(DEFAULT_DIR))

def options(self):
    group = self.add_option_group('Aster options')
    group.add_option('--use-config', action='store', default=None,
                     metavar='CFG', dest='use_config',
                     help='force the configuration parameters by importing '
                          'CFG.py. Several modules may be provided (comma '
                          'separated).')
    group.add_option('--use-config-dir', action='store', default=DEFAULT_DIR,
                     metavar='CFG_DIR', dest='use_config_dir',
                     help='path where to find the configuration file, ' \
                          'may be an url')

def configure(self):
    from Options import options as opts
    try:
        self.check_use_config()
    except Errors.ConfigurationError:
        if opts.use_config is not None:
            raise

@Configure.conf
def check_use_config(self):
    from Options import options as opts
    self.start_msg('Checking for custom configuration')
    use_cfg = opts.use_config
    if use_cfg is None:
        self.end_msg('no')
        return
    use_dir = opts.use_config_dir
    # http:// or ftp://
    #XXX is there any methods to backup this properly ?
    _saved = Context.remote_repo, Context.remote_locs, opts.download
    if 'tp://' in use_dir:
        Context.remote_repo, Context.remote_locs, opts.download = (
            use_dir, ['', DEFAULT_DIR], True
        )
    tooldir = use_dir + ' ' + DEFAULT_DIR
    for cfg in use_cfg.split(','):
        self.load_mixing_local_remote(cfg, tooldir=tooldir)
    # revert
    Context.remote_repo, Context.remote_locs, opts.download = _saved
    self.end_msg(use_cfg, 'YELLOW')



# because 'load' method fills 'tooldir' dict entry even if the tool was
# just downloaded in waflib/extras/
from waflib import Options, Utils
from waflib.Configure import download_tool

@Configure.conf
def load_mixing_local_remote(self, input, tooldir=None, funs=None, download=True):
    """Same as `load` but works with mixed remote/local tools.
    `tooldir` must not be set to make `Context.load_tool` works.
    """

    tools = Utils.to_list(input)
    if tooldir: tooldir = Utils.to_list(tooldir)
    for tool in tools:
        # avoid loading the same tool more than once with the same functions
        # used by composite projects

        mag = (tool, id(self.env), funs)
        if mag in self.tool_cache:
            self.to_log('(tool %s is already loaded, skipping)' % tool)
            continue
        self.tool_cache.append(mag)

        module = None
        try:
            module = Context.load_tool(tool, tooldir)
        except ImportError as e:
            if Options.options.download:
                module = download_tool(tool, ctx=self)
                if not module:
                    self.fatal('Could not load the Waf tool %r or download a suitable replacement from the repository (sys.path %r)\n%s' % (tool, sys.path, e))
                tooldir = None  # the only change
            else:
                self.fatal('Could not load the Waf tool %r from %r (try the --download option?):\n%s' % (tool, sys.path, e))
        except Exception as e:
            self.to_log('imp %r (%r & %r)' % (tool, tooldir, funs))
            self.to_log(Utils.ex_stack())
            raise

        if funs is not None:
            self.eval_rules(funs)
        else:
            func = getattr(module, 'configure', None)
            if func:
                if type(func) is type(Utils.readf): func(self)
                else: self.eval_rules(func)

        self.tools.append({'tool':tool, 'tooldir':tooldir, 'funs':funs})
