# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: mathieu.courtois at edf.fr

"""
Ce package contient la définition des comportements.
"""

import os.path as osp
from glob import glob

from cata_comportement import CataComportementError, LoiComportement, KIT, catalc


def _init_cata(debug):
    """Import de tous les comportements"""
    from Execution.strfunc import ufmt
    from cata_vari import DICT_NOM_VARI
    pkgdir = osp.dirname(__file__)
    pkg = osp.basename(pkgdir)
    l_mod = [osp.splitext(osp.basename(modname))[0]
             for modname in glob(osp.join(pkgdir, '*.py'))]
    l_mod = [modname for modname in l_mod
             if modname not in ('__init__', 'cata_comportement')]
    all_vari = set()
    for modname in l_mod:
        try:
            mod = __import__('%s.%s' %
                             (pkg, modname), globals(), locals(), [modname])
            # liste des lois de comportements définies dans le module
            for objname in dir(mod):
                obj = getattr(mod, objname)
                if isinstance(obj, LoiComportement):
                    if debug:
                        print '<Comportement> Module "%s" - ajout objet "%s"' % (modname, objname)
                    catalc.add(obj)
                    all_vari.update(obj.nom_vari)
        except Exception, msg:
            err = ufmt(u"Erreur import de '%s' : %s", modname, str(msg))
            raise CataComportementError(err)
    if debug:
        print catalc
    # vérification que toutes les variables déclarées sont utilisées
    unused = list(set(DICT_NOM_VARI.keys()).difference(all_vari))
    if unused:
        unused.sort()
        msg = u"Variables déclarées dans cata_vari mais non utilisées: %s" \
            % ', '.join(unused)
        raise CataComportementError(msg)

_init_cata(debug=False)
del _init_cata
