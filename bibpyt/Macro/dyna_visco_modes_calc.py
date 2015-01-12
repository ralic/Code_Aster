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

def dyna_visco_modes_calc( self, TYPE_MODE, freq1, nmode, RESI_RELA, i, j,
                                 MATER_ELAS_FO, e0, eta0, __asseMg, __asseKgr, __asseKg,
                                 __listKv, trKg, ltrv, TYPE_RESU,
                                 reuse='non', **args ):

    """
       Macro-command DYNA_VISCO,
       function to compute with iterations one eigenmode,
       and store it
    """

    from Accas import _F
    import numpy as NP

    COMB_MATR_ASSE = self.get_cmd('COMB_MATR_ASSE')
    CALC_MODES     = self.get_cmd('CALC_MODES')
    CREA_CHAMP     = self.get_cmd('CREA_CHAMP')
    CREA_RESU      = self.get_cmd('CREA_RESU')
    DETRUIRE       = self.get_cmd('DETRUIRE')


    dfreq=freq1

    while abs(dfreq)>=RESI_RELA*freq1:

        if i>10:
            nmode=nmode+5
            i=0

        if TYPE_MODE=='REEL':
            __asseKw=__asseKgr
        elif TYPE_MODE=='BETA_REEL':
            __asseKw=__asseKg
            betab=NP.real(trKg)
            betah=NP.imag(trKg)
        elif TYPE_MODE=='COMPLEXE':
            __asseKw=__asseKg
        else:
            assert False


        ny=0
        for y in MATER_ELAS_FO:
            e=y['E'](freq1)
            eta=y['AMOR_HYST'](freq1)

            if TYPE_MODE=='REEL':
                __asseKw=COMB_MATR_ASSE(COMB_R=(_F(MATR_ASSE=__asseKw,
                                                   COEF_R=1.),
                                                 _F(MATR_ASSE=__listKv[ny],
                                                    COEF_R=e/e0[ny]-1.),
                                                 ),);

            if TYPE_MODE in ['BETA_REEL','COMPLEXE']:
                __asseKw=COMB_MATR_ASSE(COMB_C=(_F(MATR_ASSE=__asseKw,
                                                   COEF_R=1.),
                                                _F(MATR_ASSE=__listKv[ny],
                                                   COEF_C=('RI',e/e0[ny]-1.,eta*e/e0[ny]-eta0[ny]),),
                                                ),);

                if TYPE_MODE=='BETA_REEL':
                    betab=betab+(e/e0[ny]-1.)*ltrv[ny]
                    betah=betah+(eta*e/e0[ny]-eta0[ny])*ltrv[ny]

            ny=ny+1


        if TYPE_MODE=='BETA_REEL':
            __asseKw=COMB_MATR_ASSE(COMB_R=(_F(MATR_ASSE=__asseKw,
                                               PARTIE='REEL',
                                               COEF_R=1.),
                                            _F(MATR_ASSE=__asseKw,
                                               PARTIE='IMAG',
                                               COEF_R=betah/betab),
                                            ),);

        # IMPR_CO        = self.get_cmd('IMPR_CO')
        # IMPR_CO(CONCEPT=_F(NOM=__asseKw))

        __modtmp=CALC_MODES(MATR_RIGI=__asseKw,
                            MATR_MASS=__asseMg,
                            OPTION='CENTRE',
                            CALC_FREQ=_F(FREQ=freq1,
                                         NMAX_FREQ=nmode,),
                            VERI_MODE=_F(STOP_ERREUR='OUI',
                                         SEUIL=1.e-3,
                                         STURM='NON',),
                            );


        freq2=__modtmp.LIST_VARI_ACCES()['FREQ']
        dfreq=abs(freq1-freq2[0])
        __numod=0


        for ii in range(1,nmode):
            __new_dfreq=abs(freq1-freq2[ii])
            if __new_dfreq<dfreq:
                dfreq=__new_dfreq
                __numod=ii

        freq1=freq2[__numod]
        if TYPE_MODE=='COMPLEXE':
            amor_red1=__modtmp.LIST_PARA()['AMOR_REDUIT'][__numod]

        if __numod+1==nmode:
            nmode=nmode+5
            dfreq=freq1

        i=i+1



    if TYPE_MODE in ['REEL','BETA_REEL']:
        type_cham = 'NOEU_DEPL_R'
    elif TYPE_MODE=='COMPLEXE':
        type_cham = 'NOEU_DEPL_C'
    else:
        assert False

    # extract the modal shape
    __unmod=CREA_CHAMP(OPERATION='EXTR',
                       NOM_CHAM='DEPL',
                       TYPE_CHAM=type_cham,
                       RESULTAT=__modtmp,
                       NUME_ORDRE=__numod+1,
                       );

    motcles = {}

    if TYPE_MODE in ['REEL','BETA_REEL']:
        type_resu = 'MODE_MECA'
        motcles['AFFE'] = _F(CHAM_GD=__unmod,
                             NUME_MODE=j+1,
                             FREQ=freq1)
    elif TYPE_MODE=='COMPLEXE':
        type_resu = 'MODE_MECA_C'
        motcles['AFFE'] = _F(CHAM_GD=__unmod,
                             NUME_MODE=j+1,
                             FREQ=freq1,
                             AMOR_REDUIT=amor_red1)
    else:
        assert False

    if reuse=='oui':
        motcles['reuse']=args['co_reuse']

    if (TYPE_RESU=='HARM' and args['MODE_MECA']!=None):
        self.DeclareOut('_modes',args['MODE_MECA'])

    # fill the concept containing the eigenmodes
    _modes=CREA_RESU(OPERATION='AFFE',
                     TYPE_RESU=type_resu,
                     NOM_CHAM='DEPL',
                     MATR_MASS=__asseMg,
                     **motcles
                     );


    freq1=freq2[__numod+1]

    DETRUIRE(CONCEPT = _F (NOM = __modtmp,) , )

    return _modes, freq1, nmode

