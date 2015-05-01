# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
Internationalization support for Code_Aster.
"""

import os
import os.path as osp
import gettext
import locale

from Noyau.N_utils import Singleton
from Noyau.N_types import force_list
from strfunc import get_encoding


def get_language():
    """Return default language (2 letters)"""
    lang = locale.getdefaultlocale()[0]
    if type(lang) is str:
        # support en-US or en_US
        lang = lang.split('_')[0].split('-')[0]
    else:
        lang = ""
    return lang


class Language(Singleton):

    """Simple class to switch between languages."""
    _singleton_id = 'i18n.Language'

    def __init__(self):
        """Initialization"""
        self.localedir = os.environ.get('ASTER_LOCALEDIR') or \
            osp.join(os.environ.get('ASTER_ROOT', ''), 'share', 'locale')
        self.domain = None
        self.current_lang = self.default_lang = get_language()

    def set_localedir(self, path):
        """Change the locale directory"""
        self.localedir = path

    def set_domain(self):
        """set the current domain"""
        self.domain = 'aster_messages'

    def get_current_settings(self):
        """Return the current language."""
        return self.current_lang, get_encoding()

    def install(self, lang=None):
        """Install the translation object for the given 'lang'."""
        if not self.domain:
            self.set_domain()
        self.current_lang = (lang or self.default_lang).lower()
        if lang:
            lang = force_list(lang)
            low = lang[0].lower()
            lang.append(low)
            # add variants lang* (ex. en-UK, en-US...)
            try:
                variants = [i for i in os.listdir(self.localedir) \
                            if i.startswith(low)]
            except OSError:
                variants = []
            lang.extend(variants)
        tr = gettext.translation(
            self.domain, self.localedir, languages=lang, fallback=True)
        tr.install(unicode=True)
        return tr

localization = Language()
