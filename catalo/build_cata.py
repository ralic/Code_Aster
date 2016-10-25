# coding=utf-8

# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: mathieu.courtois at edf.fr

"""%prog --output=cata_elem.ojb

This script build an .ojb file that contains the overall information
included in all catalogs.
"""

import os
import os.path as osp
import shutil


def build(target, debug, *args):
    """Create the jeveux object of the catalog"""
    from Execution.i18n import localization
    from Utilitai.as_timer import ASTER_TIMER
    from cataelem.elem import CataElem
    from cataelem.Tools.build_jeveux import impr_cata
    if args:
        from cataelem import __DEBUG_ELEMENTS__
        __DEBUG_ELEMENTS__.extend(args)
    timer = ASTER_TIMER()
    timer.Start('T0')
    cel = CataElem()
    cel.build()
    timer.Stop('T0')
    if args:
        return
    debugdir = None
    if debug:
        debugdir = osp.join(osp.dirname(target), 'debug')
        if osp.exists(debugdir):
            shutil.rmtree(debugdir)
        os.makedirs(debugdir)
    impr_cata(cel, target, timer, debugdir)


if __name__ == '__main__':
    import optparse
    parser = optparse.OptionParser(usage=__doc__)
    parser.add_option('-g', '--debug', action='store_true',
                      help='enable debugging')
    parser.add_option('-o', '--output', dest='ojb', metavar='FILE',
                      help='output object file')
    opts, args = parser.parse_args()
    if not opts.ojb:
        parser.error('You must provide the destination file')
    build(opts.ojb, opts.debug, *args)
