# coding=utf-8
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
# ======================================================================

def crea_elem_ssd_ops(self,  NUME_DDL, INTERFACE, BASE_MODALE,
                             CALC_FREQ, SOLVEUR, **args):
    """
     Enchainement des commandes :
        CALC_MATR_ELEM + ASSE_MATRICE + MODE_ITER_SIMULT + MODE_STATIQUE
        DEFI_INTERF_DYNA + DEFI_BASE_MODALE + MACR_ELEM_DYNA
    """

    from Accas import _F

    # On met le mot cle NUME_DDL dans une variable locale pour le proteger
    numeddl=NUME_DDL

    if numeddl:
        self.DeclareOut('_nume_ddl', numeddl)

    # On importe les definitions des commandes a utiliser dans la macro
    CALC_MATR_ELEM = self.get_cmd('CALC_MATR_ELEM')
    NUME_DDL = self.get_cmd('NUME_DDL')
    ASSE_MATRICE = self.get_cmd('ASSE_MATRICE')
    EXTR_MODE = self.get_cmd('EXTR_MODE')
    MODE_ITER_SIMULT = self.get_cmd('MODE_ITER_SIMULT')
    MODE_STATIQUE = self.get_cmd('MODE_STATIQUE')
    DEFI_INTERF_DYNA = self.get_cmd('DEFI_INTERF_DYNA')
    DEFI_BASE_MODALE = self.get_cmd('DEFI_BASE_MODALE')
    MACR_ELEM_DYNA = self.get_cmd('MACR_ELEM_DYNA')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    mSolveur = SOLVEUR[0].cree_dict_valeurs(SOLVEUR[0].mc_liste)

    _kelem = CALC_MATR_ELEM(CHARGE = args['CHARGE'],
                            OPTION = 'RIGI_MECA',
                            CARA_ELEM = args['CARA_ELEM'],
                            MODELE = args['MODELE'],
                            CHAM_MATER = args['CHAM_MATER'],)

    __melem = CALC_MATR_ELEM(CHARGE = args['CHARGE'],
                             OPTION = 'MASS_MECA',
                             CARA_ELEM = args['CARA_ELEM'],
                             MODELE = args['MODELE'],
                             CHAM_MATER = args['CHAM_MATER'],)

    _nume_ddl = NUME_DDL(MATR_RIGI = _kelem,
                         METHODE = SOLVEUR[0]['METHODE'],
                         RENUM = SOLVEUR[0]['RENUM'],)

    _matrigi = ASSE_MATRICE(NUME_DDL = _nume_ddl,
                            MATR_ELEM = _kelem,)

    _mmass = ASSE_MATRICE(NUME_DDL = _nume_ddl,
                           MATR_ELEM = __melem,)

    # recuperation des options de MODE_ITER_SIMULT (equivalent CALC_MODAL)
    motscit = {}
    motscfa = {}

    if CALC_FREQ['OPTION'] == 'PLUS_PETITE':
        nbande = 1

    if CALC_FREQ['OPTION'] == 'CENTRE':
        nbande = 1

    if CALC_FREQ['OPTION'] == 'BANDE':
        nbande = len(CALC_FREQ['FREQ'])-1
    
    if CALC_FREQ['OPTION'] == 'SANS':
        nbande = 0

    if CALC_FREQ['DIM_SOUS_ESPACE']:
        motscfa['DIM_SOUS_ESPACE'] = CALC_FREQ['DIM_SOUS_ESPACE']

    motscit['VERI_MODE'] = _F(STOP_ERREUR = CALC_FREQ['STOP_ERREUR'],)

    motfilt = {}
    motfilt['FILTRE_MODE'] = []
    for i in range(nbande):
        if CALC_FREQ['OPTION'] == 'PLUS_PETITE':
            motscfa['NMAX_FREQ'] = CALC_FREQ['NMAX_FREQ']

        if CALC_FREQ['OPTION'] == 'CENTRE':
            motscfa['FREQ'] = CALC_FREQ['FREQ']
            if CALC_FREQ['AMOR_REDUIT']:
                motscfa['AMOR_REDUIT'] = CALC_FREQ['AMOR_REDUIT']
            motscfa['NMAX_FREQ'] = CALC_FREQ['NMAX_FREQ']

        if CALC_FREQ['OPTION'] == 'BANDE':
            motscfa['FREQ'] = (CALC_FREQ['FREQ'][i], CALC_FREQ['FREQ'][i+1])

        motscit['CALC_FREQ'] = _F(OPTION =CALC_FREQ['OPTION'],
                            APPROCHE =CALC_FREQ['APPROCHE'],
                            **motscfa)

        __modes = MODE_ITER_SIMULT(MATR_RIGI = _matrigi,
                                   MATR_MASS = _mmass,
                                   INFO = args['INFO'],
                                   **motscit)

        motfilt['FILTRE_MODE'].append(_F(MODE=__modes,
                                             TOUT_ORDRE='OUI',),)

    if nbande:
        _mode_meca = EXTR_MODE(**motfilt)

    if BASE_MODALE[0]['TYPE'] == 'RITZ':
        mcfactc = []
        mcfactm = []
        mcfacti = []
        arg_no = []
        arg_grno = []
        for i in range(len(INTERFACE)):
            if BASE_MODALE[0]['TYPE_MODE'] == 'INTERFACE':
              if INTERFACE[i]['TYPE'] == 'CRAIGB':
                if INTERFACE[i]['NOEUD'] :
                  if isinstance(INTERFACE[i]['NOEUD'],(list,tuple)):
                    for noeu in INTERFACE[i]['NOEUD']:
                      arg_no.append(noeu)
                  else:
                    arg_no.append(INTERFACE[i]['NOEUD'])
                if INTERFACE[i]['GROUP_NO'] :
                  if isinstance(INTERFACE[i]['GROUP_NO'],(list,tuple)):
                    for grno in INTERFACE[i]['GROUP_NO']:
                      arg_grno.append(grno)
                  else:
                    arg_grno.append(INTERFACE[i]['GROUP_NO'])
            else:
                arg_int = {}
                if INTERFACE[i]['NOEUD'] :
                    arg_int['NOEUD'] = INTERFACE[i]['NOEUD']
                if INTERFACE[i]['GROUP_NO'] :
                    arg_int['GROUP_NO'] = INTERFACE[i]['GROUP_NO']
                arg_int['TOUT_CMP'] = 'OUI'
                if INTERFACE[i]['TYPE'] == 'CRAIGB':
                    mcfactc.append( _F(**arg_int))
                elif INTERFACE[i]['TYPE'] == 'MNEAL':
                    mcfactm.append( _F(**arg_int))
        modstatc = {}
        modstatm = {}
        modstati = {}
        lmodint = []
        if mcfactc:
            modstatc['MODE_STAT'] = mcfactc
            _mode_intf = MODE_STATIQUE(MATR_RIGI = _matrigi,
                                           SOLVEUR = mSolveur,
                                           **modstatc)
            lmodint.append(_mode_intf)
        if mcfactm:
            modstatm['FORCE_NODALE'] = mcfactm
            _mode_intf = MODE_STATIQUE(MATR_RIGI = _matrigi,
                                           SOLVEUR = mSolveur,
                                           **modstatm)
            lmodint.append(_mode_intf)
        if BASE_MODALE[0]['TYPE_MODE'] == 'INTERFACE':
            arg_int = {}
            if arg_no:
                arg_int['NOEUD'] = arg_no
            if arg_grno:
                arg_int['GROUP_NO'] = arg_grno
            arg_int['NBMOD'] = BASE_MODALE[0]['NMAX_MODE_INTF']
            arg_int['TOUT_CMP'] = 'OUI'
            mcfacti.append( _F(**arg_int))
            modstati['MODE_INTERF'] = mcfacti
            _mode_intf = MODE_STATIQUE(MATR_RIGI = _matrigi,
                                           MATR_MASS = _mmass,
                                           SOLVEUR = mSolveur,
                                           **modstati)
            lmodint.append(_mode_intf)

    interface = {}
    mcfact = []
    freqnew = None
    ifreq = INTERFACE[0]['FREQ']
    for i in range(len(INTERFACE)):
        arg_int = {}
        if INTERFACE[i]['NOEUD'] :
            arg_int['NOEUD'] = INTERFACE[i]['NOEUD']
        if INTERFACE[i]['GROUP_NO'] :
            arg_int['GROUP_NO'] = INTERFACE[i]['GROUP_NO']
        mcfact.append( _F(NOM = INTERFACE[i]['NOM'],
                          TYPE = INTERFACE[i]['TYPE'],
                           **arg_int))
        ifreq_i = INTERFACE[i]['FREQ']
        if ifreq != ifreq_i:
            freqnew = ifreq_i
    if freqnew:
        UTMESS('A','SOUSTRUC2_12',valr=freqnew)
        ifreq = freqnew
    interface['INTERFACE'] = mcfact

    if args['INFO']:
        interface['INFO'] = args['INFO']
    if ifreq:
        interface['FREQ'] = ifreq

    _interf = DEFI_INTERF_DYNA(NUME_DDL = _nume_ddl,
                               **interface)

    base = {}
    if args['INFO']:
        base['INFO'] = args['INFO']
    mcfact = []

    if BASE_MODALE[0]['TYPE'] == 'CLASSIQUE':
        arg_base = {}
        type_base = 'CLASSIQUE'
        arg_base['NMAX_MODE'] = CALC_FREQ[0]['NMAX_FREQ']
        mcfact.append(_F(INTERF_DYNA = _interf,
                         MODE_MECA = _mode_meca,
                         **arg_base))

    if BASE_MODALE[0]['TYPE'] == 'RITZ':
        type_base = 'RITZ'
        arg_base = {}
        if BASE_MODALE[0]['TYPE_MODE'] == 'STATIQUE':
            mcfact.append(_F(MODE_MECA = _mode_intf,))
        else:
          if nbande:
            if CALC_FREQ[0]['OPTION'] == 'PLUS_PETITE' or \
               CALC_FREQ[0]['OPTION'] == 'CENTRE':
                arg_base['NMAX_MODE'] = CALC_FREQ[0]['NMAX_FREQ']
            mcfact.append(_F(MODE_MECA = _mode_meca,
                             **arg_base))
          else:
            arg_base['NMAX_MODE'] = 0
            mcfact.append(_F(MODE_MECA = _mode_intf,
                             **arg_base))

        # il faut deux occurrences du mot cle facteur RITZ
        arg_base = {}
        if BASE_MODALE[0]['TYPE_MODE'] == 'INTERFACE':
            if BASE_MODALE[0]['NMAX_MODE_INTF']:
                arg_base['NMAX_MODE'] = BASE_MODALE[0]['NMAX_MODE_INTF']
            mcfact.append(_F(MODE_INTF = _mode_intf,
                             **arg_base))

    if type_base == 'CLASSIQUE':
        base['CLASSIQUE'] = mcfact
    elif type_base == 'RITZ':
        base['RITZ'] = mcfact
        base['INTERF_DYNA'] = _interf
        base['NUME_REF'] = _nume_ddl

    _base_modale = DEFI_BASE_MODALE( **base)

    elem = {}
    elem['MATR_RIGI'] = _matrigi
    elem['MATR_MASS'] = _mmass
  
    self.DeclareOut('macr_elem', self.sd)
    macr_elem = MACR_ELEM_DYNA(BASE_MODALE = _base_modale,
                               **elem)

    return
