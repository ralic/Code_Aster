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
# person_in_charge: mathieu.courtois at edf.fr

import os.path as osp


def lire_table_ops(self, UNITE, FORMAT, SEPARATEUR, NUME_TABLE, RENOMME_PARA,
                   INFO, TITRE, **args):
    """Méthode corps de la macro LIRE_TABLE
    """
    import aster
    from Utilitai.Utmess     import UTMESS, raise_UTMESS
    from Utilitai.UniteAster import UniteAster
    from Utilitai.TableReader import TableReaderFactory, unique_parameters

    ier = 0
    # On importe les definitions des commandes a utiliser dans la macro
    CREA_TABLE = self.get_cmd('CREA_TABLE')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Lecture de la table dans un fichier d unité logique UNITE
    UL = UniteAster()
    nomfich = UL.Nom(UNITE)
    if not osp.isfile(nomfich):
        UTMESS('F', 'FONCT0_41', valk=nomfich)

    texte = open(nomfich,'r').read()
    # remet UNITE dans son état initial
    UL.EtatInit()

    check_para = None
    if RENOMME_PARA == "UNIQUE":
        check_para = unique_parameters

    reader = TableReaderFactory(texte, FORMAT, SEPARATEUR, debug=(INFO == 2))
    try:
        tab = reader.read(NUME_TABLE, check_para=check_para)
    except TypeError, exc:
        UTMESS('F', 'TABLE0_45', valk=str(exc))
    except aster.error, exc:
        raise_UTMESS(exc)

    UTMESS('I', 'TABLE0_44', valk=(self.sd.nom, tab.titr),
                             vali=(len(tab.rows), len(tab.para)))

    # création de la table ASTER :
    self.DeclareOut('ut_tab', self.sd)
    motscles = tab.dict_CREA_TABLE()
    if TITRE:
        motscles['TITRE'] = TITRE
    ut_tab = CREA_TABLE(**motscles)

    return ier
