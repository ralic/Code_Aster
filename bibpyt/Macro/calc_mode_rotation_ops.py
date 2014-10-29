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
#

from Accas import _F
from types import ListType, TupleType


def calc_mode_rotation_ops(self, MATR_RIGI, MATR_MASS, MATR_AMOR, MATR_GYRO,
                           VITE_ROTA, METHODE, CALC_FREQ, VERI_MODE, **args):
# Macro pour calculer les frequences et modes en fonction des vitesses de rotation
# MATR_RIGI, matrice de raideur
# MATR_MASS, matrice de masse
# MATR_AMOR, matrice d'amortissement
# MATR_GYRO, matrice de gyroscopie
# VITE_ROTA, liste de vitesses de rotation
# METHODE, methode de calcul, QZ par defaut ou SORENSEN
# CALC_FREQ
# VERI_MODE
    from Utilitai.Table import Table
    ier = 0

    # On importe les definitions des commandes a utiliser dans la macro
    MODE_ITER_SIMULT = self.get_cmd('MODE_ITER_SIMULT')
    COMB_MATR_ASSE = self.get_cmd('COMB_MATR_ASSE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    motscit = {}
    if METHODE == 'QZ':
        motscit['CALC_FREQ'] = _F(OPTION='PLUS_PETITE',
                                  SEUIL_FREQ=CALC_FREQ['SEUIL_FREQ'],
                                  NMAX_FREQ=CALC_FREQ['NMAX_FREQ'])

    if METHODE == 'SORENSEN':
        motscit['CALC_FREQ'] = _F(OPTION='CENTRE',
                                  SEUIL_FREQ=CALC_FREQ['SEUIL_FREQ'],
                                  NMAX_FREQ=CALC_FREQ['NMAX_FREQ'],
                                  FREQ=CALC_FREQ['FREQ'])

    motscit['VERI_MODE'] = _F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                              SEUIL=VERI_MODE['SEUIL'],
                              STURM=VERI_MODE['STURM'],
                              PREC_SHIFT=VERI_MODE['PREC_SHIFT'])

    self.DeclareOut('tab_out', self.sd)

    NBV = len(VITE_ROTA)

    _mod = [None] * NBV

    tab = Table()
    for ii in range(0, NBV):
        OM = VITE_ROTA[ii]

        # ----------------------------------
        # Ajout des effets gyroscopiques w*G
        # dans la matrice d amortissement C
        # ----------------------------------

        __gyom = COMB_MATR_ASSE(COMB_R=(_F(MATR_ASSE=MATR_GYRO, COEF_R=OM,),
                                        _F(MATR_ASSE=MATR_AMOR, COEF_R=1.,),))

        _mod[ii] = MODE_ITER_SIMULT(MATR_RIGI=MATR_RIGI,
                                    MATR_MASS=MATR_MASS,
                                    MATR_AMOR=__gyom,
                                    METHODE=METHODE,
                                    **motscit)

        tab.append({'NUME_VITE': ii, 'VITE_ROTA': OM, 'NOM_OBJET':
                   'MODE_MECA', 'TYPE_OBJET': 'MODE_MECA', 'NOM_SD': _mod[ii].nom})

    motcles = tab.dict_CREA_TABLE()
    tab_out = CREA_TABLE(TYPE_TABLE='TABLE_CONTENEUR', **motcles)
    return ier
