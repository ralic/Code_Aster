# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
   Utilitaire sur le catalogue des structures de données.
"""

__revision__ = "$Id: $"

import sys
import os
from glob import glob
from optparse import OptionParser

# ----- get bibpyt location
main = sys.argv[0]
if os.path.islink(main):
    main = os.path.realpath(main)
bibpyt = os.path.normpath(os.path.join(
                          os.path.dirname(os.path.abspath(main)), os.pardir))
sys.path.append(bibpyt)

# -----------------------------------------------------------------------------


def import_sd(nomsd):
    """Import une SD.
    """
    try:
        mod = __import__('SD.%s' % nomsd, globals(), locals(), [nomsd])
        klass = getattr(mod, nomsd)
    except (ImportError, AttributeError), msg:
        print msg
        raise ImportError, "impossible d'importer la SD '%s'" % nomsd
    return klass

# -----------------------------------------------------------------------------


def tree(nom, *args):
    """Retourne l'arbre des sd en arguments
    """
    l = []
    for i, sd in enumerate(args):
        if len(args) > 1 and i > 0:
            l.append('-' * 80)
        sd_class = import_sd(sd)
        tmpobj = sd_class(nomj=nom)
        l.append(tmpobj.dump())
    return os.linesep.join(l)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    # command arguments parser
    parser = OptionParser(usage=__doc__)
    parser.add_option('-t', '--tree', dest='tree',
                      action='store_true', default=False,
                      help="affiche une SD sous forme d'arbre")
    parser.add_option('--nom', dest='nom',
                      action='store', default='^' * 8,
                      help="nom du concept dans les représentations")
    parser.add_option('-a', '--all', dest='all',
                      action='store_true', default=False,
                      help="construit la liste des SD à partir des fichiers 'sd_*.py' trouvés")

    opts, l_sd = parser.parse_args()
    if opts.all:
        l_fich = glob(os.path.join(bibpyt, 'SD', 'sd_*.py'))
        l_sd = [os.path.splitext(os.path.basename(f))[0] for f in l_fich]

    if len(l_sd) == 0:
        parser.error('quelle(s) structure(s) de données ?')

    if opts.tree:
        print tree(opts.nom, *l_sd)
