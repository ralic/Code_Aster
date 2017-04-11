# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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

import os
import os.path as osp


# translation of the docstrings of the commands and keywords is
# used within Eficas, that's why we are using PyQt i18n functions here
try:
    from PyQt5 import QtCore, Qt
    app = Qt.QApplication
    # define the tr function
    try:
        _encoding = app.UnicodeUTF8
        def tr(text):
            return app.translate("@default", text, None, _encoding)
    except AttributeError:
        def tr(text):
            return app.translate("@default", text, None)
    # install the translator
    translator = QtCore.QTranslator()
    # ask Eficas for the selected language
    try:
        from Extensions.localisation import get_locale
        locale = get_locale()
    except ImportError:
        locale = QtCore.QLocale.system().name()
    # TODO change i18n/wscript for the installation dir (prefer use share/aster)
    localedir = osp.normpath(osp.dirname(__file__))
    if translator.load("catapy_" + locale, localedir):
        app.installTranslator(translator)
except ImportError:
    try:
        # let gettext function in place
        tr = _
    except NameError:
        def tr(string):
            return string

import Accas
from Accas import *
from Accas import _F
from . import ops


JdC = JDC_CATA(code='ASTER',
               execmodul=None,
               regles=(UN_PARMI('DEBUT', 'POURSUITE'),
                       AU_MOINS_UN('FIN'),
                       A_CLASSER(('DEBUT', 'POURSUITE'), 'FIN')))


class FIN_ETAPE(PROC_ETAPE):
    """Particularisation pour FIN"""
    def Build_sd(self):
        """Fonction Build_sd pour FIN"""
        PROC_ETAPE.Build_sd(self)
        if self.nom == 'FIN':
            try:
                from Noyau.N_Exception import InterruptParsingError
                raise InterruptParsingError
            except ImportError:
                # eficas does not known this exception
                pass
        return None


class FIN_PROC(PROC):
    """Procédure FIN"""
    class_instance = FIN_ETAPE
