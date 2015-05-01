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

def dyna_visco_modes(self, TYPE_RESU, TYPE_MODE, list_FREQ, fmax, RESI_RELA,
                           MATER_ELAS_FO, __asseKg, __asseKgr, __asseMg, trKg, __listKv, e0, eta0, ltrv, **args):

    """
       Macro-command DYNA_VISCO,
       function to compute the eigenmodes of the structure
    """
    
    from Macro.dyna_visco_modes_calc import dyna_visco_modes_calc
    from Accas import _F
    from Utilitai.Utmess import UTMESS

    EXTR_MODE       = self.get_cmd('EXTR_MODE')
    DEFI_BASE_MODALE= self.get_cmd('DEFI_BASE_MODALE')
    DETRUIRE        = self.get_cmd('DETRUIRE')


    if TYPE_RESU=='MODE' :
        self.DeclareOut('_modes',self.sd)
    if (TYPE_RESU=='HARM' and args['MODE_MECA']!=None):
        self.DeclareOut('_modes',args['MODE_MECA'])

  
    i=0
    nmode=5
  
# EIGENMODES COMPUTATION

    freq1=list_FREQ[0]

    # search for the 1st eigenfrequency
    j=0
    [_modes, freq1, nmode] = dyna_visco_modes_calc( self, TYPE_MODE, freq1, nmode, RESI_RELA, i, j,
                                                          MATER_ELAS_FO, e0, eta0, __asseMg, __asseKgr, __asseKg,
                                                          __listKv, trKg, ltrv, TYPE_RESU, **args )

    # search for following eigenfrequencies
    while freq1<=fmax:
        j=j+1

        [_modes, freq1, nmode] = dyna_visco_modes_calc( self, TYPE_MODE, freq1, nmode, RESI_RELA, i, j,
                                                              MATER_ELAS_FO, e0, eta0, __asseMg, __asseKgr, __asseKg,
                                                              __listKv, trKg, ltrv, TYPE_RESU,
                                                              reuse='oui', co_reuse=_modes, **args )


    if TYPE_MODE in ['REEL','BETA_REEL']:
        __modes_temp = EXTR_MODE( FILTRE_MODE=_F( MODE=_modes,
                                                  TOUT_ORDRE='OUI'), );
        DETRUIRE( CONCEPT=_F(NOM = _modes) )
        _modes = DEFI_BASE_MODALE( ORTHO_BASE=_F( BASE=__modes_temp,
                                                  MATRICE=__asseMg,), );



    #########################################################
    # PRINTING OF THE EIGENMODES
    UTMESS('I', 'DYNAVISCO_3')
    eigenfreq = _modes.LIST_VARI_ACCES()['FREQ']
    if TYPE_MODE in ['REEL','BETA_REEL']:
        UTMESS('I', 'DYNAVISCO_4')
    if TYPE_MODE=='COMPLEXE':
        eigendamping = _modes.LIST_PARA()['AMOR_REDUIT']
        UTMESS('I', 'DYNAVISCO_6')
    for k in range(0,len(eigenfreq)):
        if TYPE_MODE in ['REEL','BETA_REEL']:
            UTMESS('I', 'DYNAVISCO_5', vali=k+1, valr=eigenfreq[k])
        if TYPE_MODE=='COMPLEXE':
            UTMESS('I', 'DYNAVISCO_7', vali=k+1, valr=(eigenfreq[k], eigendamping[k]))


    return _modes
