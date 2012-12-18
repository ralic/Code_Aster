# encoding: utf-8

import os
import os.path as osp
import re
from time import localtime, strftime

from waflib import TaskGen, Utils


def configure(self):
    self.start_msg('Getting Code_Aster version')
    values = check_aster_version()
    self.env.append_value('ASTER_VERSION', values)
    self.end_msg(self.env.ASTER_VERSION[:-1])

def build(self):
    env = self.all_envs[self.variant]
    
    self(
       features = 'pkginfo py',
           name = 'pkginfo',
         target = 'pkginfo.py',
   install_path = osp.join(env.PYTHONDIR, 'Accas'),
    )


@TaskGen.feature('pkginfo')
def build_pkginfo(self):
    """Create 'pkginfo.py' containing the parent revision."""
    target = self.bld.bldnode.make_node(self.target)
    txt = "#@ MODIF pkginfo Accas\npkginfo = %s\n" % check_aster_version()
    with open(target.abspath(), 'w') as fid:
        fid.write(txt)
    target.sig = Utils.h_file(target.abspath())
    self.process_py(target)


def check_aster_version():
    """Set ASTER_VERSION to the tuple = (
        version_as_tuple : last tag found in parents,
        revision_id      : hex identifier of the parent (join by '+' if
                           more than one),
        branch           : branch name,
        date             : date of the revision,
        from_branch      : 'official' parent branch,
        changes          : number of changes after the tagged revision (0 means
                           no change since the tag was set)
    )
    """
    properties = osp.join('bibpyt', 'Accas', 'properties.py')
    d = {}
    vers, ids, br, date, frbr, chg, loc = [None] * 7
    try:
        execfile(properties, d)
        vers = version2tuple(d['version'])
    except (IOError, KeyError), exc:
        pass
    def revid(ctx):
        """how to show the revision id"""
        return '%s:%s' % (ctx.rev(), ctx.hex()[:8])
    # add revision id if hg is available
    try:
        from mercurial import hg, ui as UI
        ui = UI.ui()
        repo = hg.repository(ui, os.getcwd())
        wctx = repo[None]
        parent = wctx.parents()[0]
        ids = '+'.join([revid(p) for p in wctx.parents()])
        br = parent.branch()
        date = strftime('%d/%m/%Y', localtime(parent.date()[0]))
        tag, chg = get_parent_info(parent, get_tag_in_branch)
        frbr, lvl = get_parent_info(parent, get_branch)
        loc = local_changes(ui, repo)
        if tag and version2tuple(tag) > vers:
            vers = version2tuple(tag)
    except ImportError:
        pass
    return [vers, ids, br, date, frbr, chg, loc]

def version2tuple(vers_string):
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

def local_changes(ui, repo):
    """Return the output of `hg status`."""
    from mercurial import commands
    cmd = commands.status
    # count added, modified and removed files
    ui.pushbuffer()
    cmd(ui, repo, no_status=True, added=True, modified=True, removed=True, deleted=True)
    fchg = ui.popbuffer().split()
    ui.pushbuffer()
    # count unknown files if not waf files
    cmd(ui, repo, unknown=True, no_status=True)
    unknown = ui.popbuffer().split()
    unknown = [p for p in unknown if not p.startswith('waf')]
    fchg.extend(unknown)
    return fchg

def get_tag(ctx):
    """Return a valid tag version (digits)."""
    tag = ctx.tags()
    if tag and tag[0].split('.')[0].isdigit():
        return tag[0]
    return None

def get_branch(ctx):
    """Return an official branch name"""
    BR = ('unstable', 'testing', 'stable-updates', 'stable')
    return ctx.branch() in BR and ctx.branch() or None

def get_tag_in_branch(ctx):
    """Return the tag in an official branch"""
    branch = get_branch(ctx)
    tag = get_tag(ctx)
    if branch and tag:
        return tag
    return None

def get_parent_info(ctx, get_info):
    """Return the first not None info found in 'ctx' or its parents."""
    info = get_info(ctx)
    if info:
        return info, 0
    try:
        info, level = _get_parent_info(ctx, get_info, init=0)
    except RuntimeError:
        level = -1
    return info, level

def _get_parent_info(ctx, get_info, init=0):
    """Recursive function to walk along the parents of 'ctx'."""
    if init > 1500:
        raise RuntimeError
    info = None
    for ctxi in ctx.parents():
        info = get_info(ctxi)
        if info:
            #print 'found %s in parents' % branch[0], init, ctxi, ctxi.rev()
            return info, init
    init += 1
    level = 9e9
    for ctxi in ctx.parents():
        try:
            info_i, lvi = _get_parent_info(ctxi, get_info, init)
        except RuntimeError:
            info_i, lvi = None, 9e9
        #print 'parent return', info_i, lvi, level
        if info_i and lvi < level:
            info = info_i
            level = lvi
    return info, level

