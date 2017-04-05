# -*- coding: utf-8 -*-
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
# person_in_charge: nicolas.sellenet at edf.fr

def crea_lib_mfront_ops(self, UNITE_MFRONT, UNITE_LIBRAIRIE, **args):
    """Compiler une loi de comportement MFront"""

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    import os
    fichierMFront = 'fort.%s' % UNITE_MFRONT
    if not os.path.exists(fichierMFront):
        raise

    os.system("mfront --obuild "+fichierMFront+" --interface=aster")
    if not os.path.exists("src/libAsterBehaviour.so"):
        raise

    fichierLib = 'fort.%s' % UNITE_LIBRAIRIE
    os.system("cp src/libAsterBehaviour.so ./"+fichierLib)
    os.system("ls -ltr")
    os.system("rm -fr src include")

    return ier
