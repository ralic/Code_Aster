# coding=utf-8

# Copyright (C) 1991 - 2017  EDF R&D                www.code-aster.org
#
# This file is part of Code_Aster.
#
# Code_Aster is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Code_Aster is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Code_Aster.  If not, see <http://www.gnu.org/licenses/>.

import os.path as osp
from glob import glob

from code_aster.Cata.SyntaxObjects import Command


def _init_command(ctx, debug):
    """Import de toutes les commandes et les copie dans le contexte"""
    pkgdir = osp.dirname(__file__)
    pkg = osp.basename(pkgdir)
    l_mod = [osp.splitext(osp.basename(modname))[0]
             for modname in glob(osp.join(pkgdir, '*.py'))]
    curDict = {}
    for modname in l_mod:
        if modname == '__init__':
            continue
        wrkctx = {}
        mod = __import__('code_aster.Cata.{}.{}'.format(pkg, modname),
                         wrkctx, wrkctx, [modname])
        # liste des commandes définies dans le module
        for objname in dir(mod):
            if curDict.get(objname) is not None:
                if debug:
                    print("DEBUG: Module {0}: {1} already seen, "
                          "ignored!".format(modname, objname))
                continue
            obj = getattr(mod, objname)
            if isinstance(obj, Command):
                if debug:
                    print("DEBUG: Module {0}: add {1}".format(modname, objname))
                curDict[objname] = obj
    ctx.update(curDict)
    return curDict

commandStore = _init_command(ctx=globals(), debug=False)
del _init_command
