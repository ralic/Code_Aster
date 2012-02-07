#@ MODIF i18n Execution  DATE 07/02/2012   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

"""
Internationalization support for Code_Aster.
"""

import os
import os.path as osp
import gettext
import locale

from Noyau.N_types import force_list
from E_Core import version_shortname
from strfunc import get_encoding

def get_language():
    """Return default language (2 letters)"""
    lang = locale.getdefaultlocale()[0]
    if type(lang) is str:
        lang = lang.split('_')[0]
    else:
        lang = ""
    return lang

class Language():
    """Simple class to switch between languages."""
    def __init__(self):
        """Initialization"""
        self.localedir = os.environ.get('ASTER_LOCALEDIR') or \
            osp.join(os.environ.get('ASTER_ROOT'), 'share', 'locale')
        self.domain = 'aster_%s' % version_shortname()
        self.current_lang = self.default_lang = get_language()

    def get_current_settings(self):
        """Return the current language."""
        return self.current_lang, get_encoding()

    def install(self, lang=None):
        """Install the translation object for the given 'lang'."""
        self.current_lang = (lang or self.default_lang).lower()
        if lang:
            lang = force_list(lang.lower())
        tr = gettext.translation(self.domain, self.localedir, languages=lang, fallback=True)
        tr.install(unicode=True)
        return tr

localization = Language()
