# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

from N_utils import import_object

class OPS:
    """Wrapper to ops functions.
    This allows to import them only when they are needed."""

    def __init__(self, uri):
        """Initialization"""
        self.uri = uri

    def __call__(self, *args, **kwargs):
        """Import the real function and call it."""
        func = import_object(self.uri)
        return func(*args, **kwargs)


# utilisé par exemple par des macros où tout est fait dans l'init.
class NOTHING(OPS):
    """OPS which does nothing."""

    def __call__(self, macro, *args, **kwargs):
        macro.set_icmd(1)
        return 0

EMPTY_OPS = NOTHING(None)
