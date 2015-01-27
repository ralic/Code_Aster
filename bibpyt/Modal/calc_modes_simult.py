# coding=utf-8
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
# person_in_charge: nicolas.brie at edf.fr


def calc_modes_simult(self, TYPE_RESU, OPTION, SOLVEUR_MODAL,
                      SOLVEUR, VERI_MODE, INFO, TITRE, **args):
    """
       Macro-command CALC_MODES, case of the simultaneous iterations method
    """

    from Accas import _F
    from Modal.mode_iter_simult import MODE_ITER_SIMULT


    motcles = {}
    matrices = {}

    # read the input matrices
    if TYPE_RESU == 'DYNAMIQUE':
        type_vp = 'FREQ'
        matrices['MATR_RIGI'] = args['MATR_RIGI']
        matrices['MATR_MASS'] = args['MATR_MASS']
        if args['MATR_AMOR'] != None:
            matrices['MATR_AMOR'] = args['MATR_AMOR']

    elif TYPE_RESU == 'MODE_FLAMB':
        type_vp = 'CHAR_CRIT'
        matrices['MATR_RIGI'] = args['MATR_RIGI']
        matrices['MATR_RIGI_GEOM'] = args['MATR_RIGI_GEOM']

    elif TYPE_RESU == 'GENERAL':
        type_vp = 'CHAR_CRIT'
        matrices['MATR_A'] = args['MATR_A']
        matrices['MATR_B'] = args['MATR_B']

    motcles.update(matrices)

    #
    # read the keyword CALC_FREQ or CALC_CHAR_CRIT
    motcles_calc_vp = {}

    calc_vp = args['CALC_' + type_vp]
    nmax_vp = 'NMAX_' + type_vp

    if OPTION in ('PLUS_PETITE', 'PLUS_GRANDE'):
        motcles_calc_vp[nmax_vp] = calc_vp[nmax_vp]

    if OPTION == 'CENTRE':
        motcles_calc_vp[type_vp] = calc_vp[type_vp]
        if type_vp == 'FREQ':
            if calc_vp['AMOR_REDUIT'] != None:
                motcles_calc_vp['AMOR_REDUIT'] = calc_vp['AMOR_REDUIT']
        motcles_calc_vp[nmax_vp] = calc_vp[nmax_vp]

    if OPTION == 'BANDE':
        motcles_calc_vp[type_vp] = calc_vp[type_vp]

    motcles_calc_vp['SEUIL_' + type_vp] = calc_vp['SEUIL_' + type_vp]

    if SOLVEUR_MODAL['DIM_SOUS_ESPACE'] != None:
        motcles_calc_vp['DIM_SOUS_ESPACE'] = SOLVEUR_MODAL['DIM_SOUS_ESPACE']
    if SOLVEUR_MODAL['COEF_DIM_ESPACE'] != None:
        motcles_calc_vp['COEF_DIM_ESPACE'] = SOLVEUR_MODAL['COEF_DIM_ESPACE']
    if SOLVEUR_MODAL['APPROCHE'] != None:
        motcles_calc_vp['APPROCHE'] = SOLVEUR_MODAL['APPROCHE']

    motcles['CALC_' + type_vp] = _F(OPTION=OPTION,
                                    NMAX_ITER_SHIFT=calc_vp['NMAX_ITER_SHIFT'],
                                    PREC_SHIFT=calc_vp['PREC_SHIFT'],
                                    **motcles_calc_vp)

    #
    # read the modal solver parameters
    motcles_solveur_modal = {}

    methode = SOLVEUR_MODAL['METHODE']
    motcles_solveur_modal['METHODE'] = methode

    if methode == 'TRI_DIAG':
        if SOLVEUR_MODAL['NMAX_ITER_ORTHO'] != None:
            motcles_solveur_modal[
                'NMAX_ITER_ORTHO'] = SOLVEUR_MODAL['NMAX_ITER_ORTHO']
        if SOLVEUR_MODAL['PREC_ORTHO'] != None:
            motcles_solveur_modal[
                'PREC_ORTHO'] = SOLVEUR_MODAL['PREC_ORTHO']
        if SOLVEUR_MODAL['PREC_LANCZOS'] != None:
            motcles_solveur_modal[
                'PREC_LANCZOS'] = SOLVEUR_MODAL['PREC_LANCZOS']
        if SOLVEUR_MODAL['NMAX_ITER_QR'] != None:
            motcles_solveur_modal[
                'NMAX_ITER_QR'] = SOLVEUR_MODAL['NMAX_ITER_QR']
        if SOLVEUR_MODAL['MODE_RIGIDE'] != None:
            if SOLVEUR_MODAL['MODE_RIGIDE'] == 'OUI':
                motcles['OPTION'] = 'MODE_RIGIDE'
            else:
                motcles['OPTION'] = 'SANS'
    elif methode == 'JACOBI':
        if SOLVEUR_MODAL['NMAX_ITER_BATHE'] != None:
            motcles_solveur_modal[
                'NMAX_ITER_BATHE'] = SOLVEUR_MODAL['NMAX_ITER_BATHE']
        if SOLVEUR_MODAL['PREC_BATHE'] != None:
            motcles_solveur_modal[
                'PREC_BATHE'] = SOLVEUR_MODAL['PREC_BATHE']
        if SOLVEUR_MODAL['NMAX_ITER_JACOBI'] != None:
            motcles_solveur_modal[
                'NMAX_ITER_JACOBI'] = SOLVEUR_MODAL['NMAX_ITER_JACOBI']
        if SOLVEUR_MODAL['PREC_JACOBI'] != None:
            motcles_solveur_modal[
                'PREC_JACOBI'] = SOLVEUR_MODAL['PREC_JACOBI']
    elif methode == 'SORENSEN':
        if SOLVEUR_MODAL['NMAX_ITER_SOREN'] != None:
            motcles_solveur_modal[
                'NMAX_ITER_SOREN'] = SOLVEUR_MODAL['NMAX_ITER_SOREN']
        if SOLVEUR_MODAL['PARA_ORTHO_SOREN'] != None:
            motcles_solveur_modal[
                'PARA_ORTHO_SOREN'] = SOLVEUR_MODAL['PARA_ORTHO_SOREN']
        if SOLVEUR_MODAL['PREC_SOREN'] != None:
            motcles_solveur_modal[
                'PREC_SOREN'] = SOLVEUR_MODAL['PREC_SOREN']
    elif methode == 'QZ':
        if SOLVEUR_MODAL['TYPE_QZ'] != None:
            motcles_solveur_modal['TYPE_QZ'] = SOLVEUR_MODAL['TYPE_QZ']

    motcles.update(motcles_solveur_modal)

    #
    # read the keyword SOLVEUR (linear solver)
    solveur = SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
    if solveur.has_key('TYPE_RESU'):  # because TYPE_RESU is a keyword with a 'global' position
        solveur.pop('TYPE_RESU')
    if solveur.has_key('OPTION'):    # because OPTION is a keyword with a 'global' position
        solveur.pop('OPTION')
    if solveur.has_key('FREQ'):      # because FREQ can be a keyword with a 'global' position
        solveur.pop('FREQ')
    motcles['SOLVEUR'] = _F(**solveur)

    #
    # read the keyword VERI_MODE
    sturm = VERI_MODE['STURM']
    if sturm in ('GLOBAL', 'LOCAL'):
        # for MODE_ITER_SIMULT, value for STURM can be only OUI or NON. Other
        # values are equivalent to OUI
        sturm = 'OUI'
    motcles['VERI_MODE'] = _F(STOP_ERREUR=VERI_MODE['STOP_ERREUR'],
                              SEUIL=VERI_MODE['SEUIL'],
                              STURM=sturm,
                              PREC_SHIFT=VERI_MODE['PREC_SHIFT']
                              )

    #

    if args['STOP_BANDE_VIDE'] != None:
        motcles['STOP_BANDE_VIDE'] = args['STOP_BANDE_VIDE']

    if TITRE != None:
        motcles['TITRE'] = TITRE

    modes = MODE_ITER_SIMULT(TYPE_RESU=TYPE_RESU,
                             INFO=INFO,
                             **motcles
                             )

    return modes
