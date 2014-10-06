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
Transformation des variables internes de EPX vers Code_Aster
"""

from Cata.cata import CREA_CHAMP, FORMULE

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

    __F_V_GL = [None]*nb_comp
    coef = [1.]*nb_comp
    coef[2] = 0.5
    coef[5] = 0.5
    li_fonc = []
    nom_cmp_f = []
    for ivar, var in enumerate(var_aster[:nb_comp]):
        co = coef[ivar]
        xi_f = 'X%s'%(ivar+1)
        nom_cmp_f.append(xi_f)
        __F_V_GL[ivar] = FORMULE(VALE=var+'*'+str(co), NOM_PARA=(var))
        li_fonc.append(__F_V_GL[ivar])

    ccc = cc.copy()
    ccc.update(TYPE_CHAM='ELGA_NEUT_F',
               AFFE={'GROUP_MA' : gr_ma,
                     'NOM_CMP'  : nom_cmp_f,
                     'VALE_F'   : li_fonc,})
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

    __F_V_GL = [None]*nb_comp
    li_fonc = []
    nom_cmp_f = []

    # V1 = VAR3
    var = 'V3'
    xi_f = 'X%s'%(1)
    nom_cmp_f.append(xi_f)
    __F_V_GL[0] = FORMULE(VALE=var, NOM_PARA=(var))
    li_fonc.append(__F_V_GL[0])
    # V2 on met 0 tout le temps
    xi_f = 'X%s'%(2)
    nom_cmp_f.append(xi_f)
    __F_V_GL[1] = FORMULE(VALE='0.', NOM_PARA=(var))
    li_fonc.append(__F_V_GL[1])
    

    ccc = cc.copy()
    ccc.update(TYPE_CHAM='ELGA_NEUT_F',
               AFFE={'GROUP_MA' : gr_ma,
                     'NOM_CMP'  : nom_cmp_f,
                     'VALE_F'   : li_fonc,})
    __CH_FOV[ico] = CREA_CHAMP(**ccc)
    dic_transfo['CH_FONC'] = __CH_FOV[ico]
    dic_transfo['NOM_CMP_F'] = nom_cmp_f
    dic_transfo['NOM_CMP'] = var_aster[:nb_comp]

    return dic_transfo
