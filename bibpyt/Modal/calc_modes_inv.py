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


def calc_modes_inv(self, TYPE_RESU, OPTION, SOLVEUR_MODAL,
                         SOLVEUR, VERI_MODE, INFO, TITRE, **args):
    """
       Macro-command CALC_MODES, case of the inverse iterations method
    """

    from Accas import _F

    # import the definitions of the commands to use in the macro-command
    # The name of the variable has to be the name of the command
    MODE_ITER_INV  =self.get_cmd('MODE_ITER_INV')


    motcles     = {}
    matrices = {}


    # read the input matrices
    if TYPE_RESU == 'DYNAMIQUE':
        type_vp = 'FREQ'
        matrices['MATR_RIGI']   = args['MATR_RIGI']
        matrices['MATR_MASS']   = args['MATR_MASS']
        if args.has_key('MATR_AMOR') :
            matrices['MATR_AMOR']= args['MATR_AMOR']

    elif TYPE_RESU == 'MODE_FLAMB':
        type_vp = 'CHAR_CRIT'
        matrices['MATR_RIGI']      = args['MATR_RIGI']
        matrices['MATR_RIGI_GEOM'] = args['MATR_RIGI_GEOM']

    elif TYPE_RESU == 'GENERAL':
        type_vp = 'CHAR_CRIT'
        matrices['MATR_A'] = args['MATR_A']
        matrices['MATR_B'] = args['MATR_B']

    motcles.update(matrices)


    #################################################################
    # read the keyword CALC_FREQ or CALC_CHAR_CRIT
    motcles_calc_vp = {}

    calc_vp = args['CALC_'+type_vp]
    nmax_vp = 'NMAX_'+type_vp

    motcles_calc_vp[type_vp] = calc_vp[type_vp]
    motcles_calc_vp[nmax_vp] = calc_vp[nmax_vp]
    motcles_calc_vp['SEUIL_'+type_vp] = calc_vp['SEUIL_'+type_vp]


    motcles['CALC_'+type_vp] = _F(OPTION           = OPTION,
                                  NMAX_ITER_SHIFT  = calc_vp['NMAX_ITER_SHIFT'],
                                  PREC_SHIFT       = calc_vp['PREC_SHIFT'],
                                  NMAX_ITER_SEPARE = SOLVEUR_MODAL['NMAX_ITER_SEPARE'],
                                  PREC_SEPARE      = SOLVEUR_MODAL['PREC_SEPARE'],
                                  NMAX_ITER_AJUSTE = SOLVEUR_MODAL['NMAX_ITER_AJUSTE'],
                                  PREC_AJUSTE      = SOLVEUR_MODAL['PREC_AJUSTE'],
                                  **motcles_calc_vp
                                  )

    #################################################################
    # read the keyword CALC_MODE
    motcles['CALC_MODE'] = _F(OPTION    = SOLVEUR_MODAL['OPTION_INV'],
                              PREC      = SOLVEUR_MODAL['PREC_INV'],
                              NMAX_ITER = SOLVEUR_MODAL['NMAX_ITER_INV'],
                              )


    #################################################################
    # read the keyword SOLVEUR (linear solver)
    solveur = SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)
    if solveur.has_key('TYPE_RESU'): # because TYPE_RESU is a keyword with a 'global' position
        solveur.pop('TYPE_RESU')
    if solveur.has_key('OPTION'):    # because OPTION is a keyword with a 'global' position
        solveur.pop('OPTION')
    if solveur.has_key('FREQ'):      # because FREQ can be a keyword with a 'global' position
        solveur.pop('FREQ')
    motcles['SOLVEUR']=_F(**solveur)


    #################################################################
    # read the keyword VERI_MODE
    motcles['VERI_MODE'] = _F(STOP_ERREUR = VERI_MODE['STOP_ERREUR'],
                              SEUIL       = VERI_MODE['SEUIL'],
                              )

    #################################################################

    if TITRE != None:
        motcles['TITRE'] = TITRE


    modes=MODE_ITER_INV( TYPE_RESU = TYPE_RESU,
                         INFO      = INFO,
                         **motcles
                        )

    return modes
