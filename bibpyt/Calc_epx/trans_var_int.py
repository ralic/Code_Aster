# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: serguei.potapov at edf.fr

"""
Transformation des variables internes
"""

from Cata.cata import CREA_CHAMP, FORMULE, CALC_CHAMP, IMPR_RESU
from Calc_epx.calc_epx_cata import cata_compor
from Accas import _F
from Utilitai.Utmess import MasquerAlarme, RetablirAlarme
# =====================     PASSAGE ASTER -> EPX    ============================

def var_int_a2e(compor_gr, resu, mod, nume_ordre):
    """
        Transforme le champ VARI_ELGA pour correspondre aux attentes d'EPX
        Renvoie un résultat contenant les champs DEPL, SIEF_ELGA et VARI_ELGA
        transformé du NUME_ORDRE nume_ordre du résultat resu.
    """
    from Cata.cata import CREA_RESU
    
    # extraction des champs :
    
    __DEPL = CREA_CHAMP(OPERATION = 'EXTR',
                   TYPE_CHAM='NOEU_DEPL_R',
                   RESULTAT=resu,
                   NOM_CHAM='DEPL',
                   INST=nume_ordre,
                   )
    __SIEF = CREA_CHAMP(OPERATION = 'EXTR',
                   TYPE_CHAM='ELGA_SIEF_R',
                   RESULTAT=resu,
                   NOM_CHAM='SIEF_ELGA',
                   INST=nume_ordre,
                   )
    __VARI = CREA_CHAMP(OPERATION = 'EXTR',
                   TYPE_CHAM='ELGA_VARI_R',
                   RESULTAT=resu,
                   NOM_CHAM='VARI_ELGA',
                   INST=nume_ordre,
                   )
    # transformation
    
    nb_compor = len(compor_gr)
    __VARICO = [None]*nb_compor
    asse = []
    for ico, compor in enumerate(compor_gr.keys()):
        gr_ma = compor_gr[compor]
        nb_var_epx = cata_compor[compor]['NB_VAR_EPX']
        nom_cmp = ['V%i' % ii for ii in range(1,nb_var_epx+1)] 
        if compor == 'VMIS_JOHN_COOK':
            tr_a2e_vmis_john_cook(__VARICO, ico, gr_ma, mod, __VARI,
                                  nume_ordre, resu)
        else:
            vale = [0.]*nb_var_epx
            __VARICO[ico] = CREA_CHAMP(OPERATION='AFFE',
                                 TYPE_CHAM='ELGA_VARI_R',
                                 MODELE=MODELE,
                                 PROL_ZERO='NON',
                                 AFFE=(_F(GROUP_MA='CUBE',
                                 NOM_CMP=nom_cmp,
                                 VALE=vale),
                      )
                 )
            UTMESS('A','PLEXUS_47', valk = compor)

        asse.append({'GROUP_MA': gr_ma, 
                         'CHAM_GD': __VARICO[ico],
                         'NOM_CMP': nom_cmp,
                         'CUMUL'  : 'NON', 
                         'COEF_R': 1.})
    if len(asse) == 0:
        UTMESS('A','PLEXUS_48')

    __VARITR=CREA_CHAMP(OPERATION = 'ASSE',
                 MODELE = mod,
                 TYPE_CHAM='ELGA_VARI_R',
                 PROL_ZERO='OUI',
                 ASSE=asse,
                )
    
    # construction du concept resultat
    MasquerAlarme('COMPOR2_23')
    __res = CREA_RESU(
                  OPERATION = 'AFFE',
                  TYPE_RESU = 'EVOL_NOLI',
                  NOM_CHAM  = 'DEPL',
                  AFFE =(_F (CHAM_GD = __DEPL,
                            MODELE=mod,
                            INST = 1.0,
                           ),)
            )

    __res = CREA_RESU(reuse=__res,
                  OPERATION = 'AFFE',
                  TYPE_RESU = 'EVOL_NOLI',
                  NOM_CHAM  = 'SIEF_ELGA',
                  AFFE =_F (CHAM_GD = __SIEF,
                            MODELE=mod,
                            INST =1.0,
                           ),
            )

    __res = CREA_RESU(reuse=__res,
                  OPERATION = 'AFFE',
                  TYPE_RESU = 'EVOL_NOLI',
                  NOM_CHAM  = 'VARI_ELGA',
                  AFFE =_F (CHAM_GD = __VARITR,
                            MODELE=mod,
                            INST =1.0,
                           ),
            )
    RetablirAlarme('COMPOR2_23')
    return __res
#-----------------------------------------------------------------------
def tr_a2e_vmis_john_cook(__VARICO, ico, gr_ma, mod, __VARI, nume_ordre, resu):
    """
        Transformation pour VMIS_JOHN_COOK ASTER vers EPX
    """
    
    CALC_CHAMP(reuse=resu,RESULTAT=resu,CRITERES='SIEQ_ELGA', 
               NUME_ORDRE = nume_ordre)
    CALC_CHAMP(reuse=resu,CHAM_UTIL=_F(NOM_CHAM='SIEF_ELGA',
                                       CRITERE='TRACE',
                                       NUME_CHAM_RESU=1),
                                       NUME_ORDRE = nume_ordre,
                                       RESULTAT=resu)

    __TRACE = CREA_CHAMP(OPERATION = 'EXTR',
                   TYPE_CHAM='ELGA_NEUT_R',
                   RESULTAT=resu,
                   NOM_CHAM='UT01_ELGA',
                   PROL_ZERO='OUI',
                   NUME_ORDRE = nume_ordre,
                   )

    __VMIS = CREA_CHAMP(OPERATION = 'EXTR',
                   TYPE_CHAM='ELGA_SIEF_R',
                   RESULTAT=resu,
                   NOM_CHAM='SIEQ_ELGA',
                   NUME_ORDRE = nume_ordre,
                   )

#cel =numpy.sqrt(E_b*(1-Nu_b)/(rho_b*(1+Nu_b)*(1-2*Nu_b)))

    __AFFE = CREA_CHAMP(OPERATION='AFFE',
                 TYPE_CHAM='ELGA_NEUT_R',
                 MODELE=mod,
                 PROL_ZERO='OUI',
                 AFFE=(_F(GROUP_MA=gr_ma,
                 NOM_CMP=('X1','X2','X3','X4','X5'),
                 #VALE=(1.0E-6, cel, 0.,0.,0.)),
                 VALE=(1.0E-6, 0., 0.,0.,0.)),
                      )
                 )


    __VARICO[ico]=CREA_CHAMP(OPERATION = 'ASSE',
                 MODELE = mod,
                 TYPE_CHAM='ELGA_VARI_R',
                 PROL_ZERO='OUI',
                 ASSE=(_F(CHAM_GD=__TRACE,
                          GROUP_MA=gr_ma,
                          NOM_CMP='X1',
                          NOM_CMP_RESU='V1',
                          COEF_R=1.0/3,
                         ),
                       _F(CHAM_GD=__VMIS,
                          GROUP_MA=gr_ma,
                          NOM_CMP='VMIS',
                          NOM_CMP_RESU='V2',
                         ),
                       _F(CHAM_GD=__VARI,
                          GROUP_MA=gr_ma,
                          NOM_CMP='V1',
                          NOM_CMP_RESU='V3',
                         ),
                       _F(CHAM_GD=__VMIS,
                          GROUP_MA=gr_ma,
                          NOM_CMP='VMIS',
                          NOM_CMP_RESU='V4',
                         ),
                       _F(CHAM_GD=__AFFE,
                          GROUP_MA=gr_ma,
                          NOM_CMP=('X1','X2','X3','X4','X5'),
                          NOM_CMP_RESU=('V4','V5','V6','V7','V8'),
                          CUMUL='OUI',
                         ),
                      ),
                )
#-----------------------------------------------------------------------


# =====================     PASSAGE EPX -> ASTER    ============================

# Attention : lors du passage dans ces routines les composantes des champs
#             récupérés d'EPX ne sont plus nommées VAR1, VAR2,... mais
#             déjà V1, V2 ...
#             Mais le nombre de variables internes présentes est bien celui
#             d'EPX.

#-----------------------------------------------------------------------


def tr_e2a_glrc_damage(__CH_FOV, ico, cc, nb_comp, var_aster, gr_ma):
    """
        Transformation pour GLRC_DAMAGE
    """

    dic_transfo = {}

    __F_V_GL = [None] * nb_comp
    coef = [1.] * nb_comp
    coef[2] = 0.5
    coef[5] = 0.5
    li_fonc = []
    nom_cmp_f = []
    for ivar, var in enumerate(var_aster[:nb_comp]):
        co = coef[ivar]
        xi_f = 'X%s' % (ivar + 1)
        nom_cmp_f.append(xi_f)
        __F_V_GL[ivar] = FORMULE(VALE=var + '*' + str(co), NOM_PARA=(var))
        li_fonc.append(__F_V_GL[ivar])

    ccc = cc.copy()
    ccc.update(TYPE_CHAM='ELGA_NEUT_F',
               AFFE={'GROUP_MA': gr_ma,
                     'NOM_CMP': nom_cmp_f,
                     'VALE_F': li_fonc, })
    __CH_FOV[ico] = CREA_CHAMP(**ccc)
    dic_transfo['CH_FONC'] = __CH_FOV[ico]
    dic_transfo['NOM_CMP_F'] = nom_cmp_f
    dic_transfo['NOM_CMP'] = var_aster[:nb_comp]

    return dic_transfo
#-----------------------------------------------------------------------
def tr_e2a_vmis_isot_trac(__CH_FOV, ico, cc, nb_comp, var_aster, gr_ma):
    """
        Transformation pour VMIS_ISOT_TRAC
    """

    dic_transfo = {}

    __F_V_GL = [None] * nb_comp
    li_fonc = []
    nom_cmp_f = []

    # V1 = VAR3
    var = 'V3'
    xi_f = 'X%s' % (1)
    nom_cmp_f.append(xi_f)
    __F_V_GL[0] = FORMULE(VALE=var, NOM_PARA=(var))
    li_fonc.append(__F_V_GL[0])
    # V2 on met 0 tout le temps
    xi_f = 'X%s' % (2)
    nom_cmp_f.append(xi_f)
    __F_V_GL[1] = FORMULE(VALE='0.', NOM_PARA=(var))
    li_fonc.append(__F_V_GL[1])

    ccc = cc.copy()
    ccc.update(TYPE_CHAM='ELGA_NEUT_F',
               AFFE={'GROUP_MA': gr_ma,
                     'NOM_CMP': nom_cmp_f,
                     'VALE_F': li_fonc, })
    __CH_FOV[ico] = CREA_CHAMP(**ccc)
    dic_transfo['CH_FONC'] = __CH_FOV[ico]
    dic_transfo['NOM_CMP_F'] = nom_cmp_f
    dic_transfo['NOM_CMP'] = var_aster[:nb_comp]

    return dic_transfo
#-----------------------------------------------------------------------
def tr_e2a_vmis_john_cook(__CH_FOV, ico, cc, nb_comp, var_aster, gr_ma):
    """
        Transformation pour VMIS_JOHN_COOK
    """

    dic_transfo = {}

    __F_V_GL = [None] * nb_comp
    li_fonc = []
    nom_cmp_f = []

    # V1 = VAR3
    var = 'V3'
    xi_f = 'X%s' % (1)
    nom_cmp_f.append(xi_f)
    __F_V_GL[0] = FORMULE(VALE=var, NOM_PARA=(var))
    li_fonc.append(__F_V_GL[0])
    # on met les autres à 0 car pas de correspondance
    for ii in range(2,6):
        xi_f = 'X%s' % (ii)
        nom_cmp_f.append(xi_f)
        __F_V_GL[ii-1] = FORMULE(VALE='0.', NOM_PARA=(var))
        li_fonc.append(__F_V_GL[ii-1])

    ccc = cc.copy()
    ccc.update(TYPE_CHAM='ELGA_NEUT_F',
               AFFE={'GROUP_MA': gr_ma,
                     'NOM_CMP': nom_cmp_f,
                     'VALE_F': li_fonc, })
    __CH_FOV[ico] = CREA_CHAMP(**ccc)
    dic_transfo['CH_FONC'] = __CH_FOV[ico]
    dic_transfo['NOM_CMP_F'] = nom_cmp_f
    dic_transfo['NOM_CMP'] = var_aster[:nb_comp]

    return dic_transfo
