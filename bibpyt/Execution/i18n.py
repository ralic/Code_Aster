#@ MODIF i18n Execution  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

"""
Internationalization support for Code_Aster.
"""

import os
import os.path as osp
import gettext
import locale
from E_Global import version_shortname

LOCALEDIR = os.environ.get('ASTER_LOCALEDIR') or \
            osp.join(os.environ.get('ASTER_ROOT'), 'share', 'locale')

DOMAIN = 'aster_%s' % version_shortname()

def get_language():
    """Return default language (2 letters)"""
    lang = locale.getdefaultlocale()[0]
    if type(lang) is str:
        lang = lang.split('_')[0]
    else:
        lang = "en"
    return lang

_lang = {}
_lang['default'] = gettext.translation(DOMAIN, LOCALEDIR, fallback=True)
_lang['fr'] = gettext.translation(DOMAIN, LOCALEDIR, languages=['fr'], fallback=True)
_lang['en'] = gettext.translation(DOMAIN, LOCALEDIR, languages=['en'], fallback=True)

def get_translation(lang=''):
    """Return the translation object for 'lang'."""
    lang = lang.lower()
    return _lang.get(lang, _lang['default'])

