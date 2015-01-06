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


def calc_modes_amelioration(self, modes, TYPE_RESU,
                             SOLVEUR_MODAL, SOLVEUR, VERI_MODE,
                             INFO, TITRE, **args):
    """
       Macro-command CALC_MODES, file for improving the quality of the eigenmodes
    """


    import aster
    from Accas import _F
    from Noyau.N_utils import AsType
    from Utilitai.Utmess import UTMESS

    # import the definitions of the commands to use in the macro-command
    # The name of the variable has to be the name of the command
    MODE_ITER_INV = self.get_cmd('MODE_ITER_INV')
    DETRUIRE = self.get_cmd('DETRUIRE')


    ##############################################################################
    # 1. CHECK IF THE COMPUTATION WITH MODE_ITER_INV CAN BE PERFORMED

    if TYPE_RESU == 'DYNAMIQUE':
        type_vp = 'FREQ'
        matr_A  = 'MATR_RIGI'
        matr_B  = 'MATR_MASS'
        if args.has_key('MATR_AMOR'):
            matr_C = 'MATR_AMOR'
    elif TYPE_RESU == 'MODE_FLAMB':
        type_vp = 'CHAR_CRIT'
        matr_A  = 'MATR_RIGI'
        matr_B  = 'MATR_RIGI_GEOM'
    elif TYPE_RESU == 'GENERAL':
        type_vp = 'CHAR_CRIT'
        matr_A  = 'MATR_A'
        matr_B  = 'MATR_B'

    # 1.1 check if the input matrices are symetric and real
    lsym = True
    lreel= True
    iret, ibid, type_matr_A = aster.dismoi('TYPE_MATRICE', args[matr_A].nom, 'MATR_ASSE', 'F')
    if not type_matr_A == 'SYMETRI':
        lsym = False
    if AsType(args[matr_A]).__name__ in ('matr_asse_depl_c', 'matr_asse_gene_c'):
        lreel = False
    iret, ibid, type_matr_B = aster.dismoi('TYPE_MATRICE', args[matr_B].nom, 'MATR_ASSE', 'F')
    if not type_matr_B == 'SYMETRI':
        lsym = False
    if TYPE_RESU == 'DYNAMIQUE':
        if args['MATR_AMOR']!=None:
            iret, ibid, type_matr_C = aster.dismoi('TYPE_MATRICE', args['MATR_AMOR'].nom, 'MATR_ASSE', 'F')
            if not type_matr_C == 'SYMETRI':
                lsym = False
    if not lsym:
        UTMESS('I','MODAL_15')
        return modes
    if not lreel:
        UTMESS('I','MODAL_16',valk=matr_A)
        return modes


    # 1.2 detect too closed eigen values (gap < CALC_* / SEUIL_*)
    list_vp = modes.LIST_PARA()[type_vp]  # list of the eigen values
    seuil_vp = args['CALC_'+type_vp]['SEUIL_'+type_vp]
    # One reproduces here the detection performed in the routine vpgsmm.F90
    seuilr = 100.*seuil_vp
    seuilp = seuil_vp
    ltest = False
    for k in range(0, len(list_vp)-1):
        if (abs(list_vp[k]) < seuilr):
            ltest = abs(list_vp[k+1])<seuilr
        else:
            if (abs(list_vp[k+1]) >= seuilr):
                ltest = abs(2.*(list_vp[k]-list_vp[k+1])/(list_vp[k]+list_vp[k+1])) < seuilp
    if ltest:
        UTMESS('I','MODAL_17',valk=modes.nom)
        return modes


    ##############################################################################
    # 2. PERFORM THE IMPROVEMENT OF THE MODES WITH MODE_ITER_INV / OPTION='PROCHE'        
    DETRUIRE( CONCEPT=_F(NOM = modes),
              INFO=1 )

    motcles  = {}
    matrices = {}
    
    # read the input matrices
    if TYPE_RESU == 'DYNAMIQUE':
        type_vp = 'FREQ'
        matrices['MATR_RIGI']   = args['MATR_RIGI']
        matrices['MATR_MASS']   = args['MATR_MASS']
        if args['MATR_AMOR']!=None:
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
    
    motcles_calc_vp = {}
    
    motcles_calc_vp[type_vp] = list_vp
    
    motcles['CALC_'+type_vp] = _F(OPTION = 'PROCHE',
                                  **motcles_calc_vp
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
                              )
    #################################################################
    if TITRE != None:
        motcles['TITRE'] = TITRE
    
    #################################################################
    
    modes=MODE_ITER_INV( TYPE_RESU = TYPE_RESU,
                         INFO      = INFO,
                         **motcles
                        )
    
    return modes
