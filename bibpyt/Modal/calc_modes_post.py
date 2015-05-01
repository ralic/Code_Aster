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
# person_in_charge: nicolas.brie at edf.fr


def calc_modes_post(self, modes, lmatphys, norme_mode, filtre_mode, impression):
    """
       Macro-command CALC_MODES, post-treatment
    """

    from Accas import _F
    from Noyau.N_utils import AsType

    # import the definitions of the commands to use in the macro-command
    # The name of the variable has to be the name of the command
    NORM_MODE = self.get_cmd('NORM_MODE')
    IMPR_RESU = self.get_cmd('IMPR_RESU')
    EXTR_MODE = self.get_cmd('EXTR_MODE')
    IMPR_RESU = self.get_cmd('IMPR_RESU')
    DETRUIRE = self.get_cmd('DETRUIRE')

    motscles = {}

    if (lmatphys) & (norme_mode != None):
        modes = NORM_MODE(reuse=modes,
                          MODE=modes,
                          NORME=norme_mode['NORME'],
                          INFO=norme_mode['INFO'],
                          )

    # copy the modes concept in a temporary concept, in order to free its name
    __modes_temp = EXTR_MODE(FILTRE_MODE=_F(MODE=modes,
                                            TOUT_ORDRE='OUI'))
    DETRUIRE(CONCEPT=_F(NOM=modes),
             INFO=1)

    impr_tout = False

    lfiltre = False  # False if not specified by the user, or if matr_asse_gene_r,
                     # True otherwise
    if lmatphys:
        if filtre_mode != None:
            lfiltre = True
        else:
            lfiltre = False
    else:
        lfiltre = False
    if lfiltre:
        motscles['FILTRE_MODE'] = _F(MODE=__modes_temp,
                                     CRIT_EXTR=filtre_mode['CRIT_EXTR'],
                                     SEUIL=filtre_mode['SEUIL'])
    else:
        motscles['FILTRE_MODE'] = _F(MODE=__modes_temp,
                                     TOUT_ORDRE='OUI')

    if (lmatphys) & (impression != None):
        motscles['IMPRESSION'] = _F(CUMUL=impression['CUMUL'],
                                    CRIT_EXTR=impression['CRIT_EXTR'])

    modes = EXTR_MODE(**motscles)

    # print all the modes parameters
    if (lmatphys) & (impression != None):
        if impression['TOUT_PARA'] == 'OUI':
            impr_tout = True
    if not lmatphys:
        impr_tout = True
    if impr_tout:
        IMPR_RESU(RESU=_F(RESULTAT=modes,
                          TOUT_ORDRE='OUI',
                          TOUT_CHAM='NON',
                          TOUT_PARA='OUI',))

    return modes
