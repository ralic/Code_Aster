# encoding: utf-8

import os
import os.path as osp
import re

from waflib import Configure

def configure(self):
    self.check_aster_version()

@Configure.conf
def check_aster_version(self):
    # ASTER_VERSION = (version_as_tuple, revision_id, branch)
    self.start_msg('Getting Code_Aster version')
    pkginfo = osp.join('bibpyt', 'Accas', 'properties.py')
    d = {}
    vers, ids, br = [None] * 3
    try:
        execfile(pkginfo, d)
        vers = self.version2tuple(d['version'])
    except (IOError, KeyError), exc:
        pass
    # add revision id if hg is available
    try:
        from mercurial import hg, ui as UI
        ui = UI.ui()
        repo = hg.repository(ui, os.getcwd())
        wctx = repo[None]
        ids = '+'.join([parent.hex()[:12] for parent in wctx.parents()])
        br = wctx.branch()
        tag = get_parent_tag(wctx)
        if tag:
            vers = self.version2tuple(tag)
    except ImportError:
        pass
    self.env.append_value('ASTER_VERSION', [vers, ids, br])
    self.end_msg(self.env.ASTER_VERSION)

@Configure.conf
def version2tuple(self, vers_string):
    """1.7.9alpha --> (1, 7, 9, 'alpha')"""
    tupl0 = vers_string.split('.')
    val = []
    for v in tupl0:
        m = re.search('(^[ 0-9]+)(.*)', v)
        if m:
            val.append(int(m.group(1)))
            if m.group(2):
                val.append(m.group(2).replace('-', '').replace('_', '').strip())
        else:
            val.append(v)
    val.extend([0]*(3-len(val)))
    return tuple(val)


def get_parent_tag(ctx):
    """Return the first tag found the parents of 'ctx'."""
    tag = None
    try:
        tag = _get_parent_tag(ctx, init=0)[0]
    except RuntimeError:
        pass
    return tag

def _get_parent_tag(ctx, init=0):
    """Recursive function to walk along the parents of 'ctx'."""
    init += 1
    if init > 1500:
        raise RuntimeError
    tag = None
    for ctxi in ctx.parents():
        tag = ctxi.tags()
        if tag:
            return tag[0], init
    for ctxi in ctx.parents():
        tag, init = _get_parent_tag(ctxi, init+1)
        if tag:
            return tag, init
    return tag, init

