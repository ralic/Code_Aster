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


def post_coque_ops(self, RESULTAT, COOR_POINT, CHAM, NUME_ORDRE, INST,
                   **args):
    """
    macro post_coque
    """
    import aster
    import os
    import string
    import Accas
    from Accas import _F
    from Utilitai.Utmess import UTMESS, MasquerAlarme, RetablirAlarme
    from Utilitai.Table import Table
    from Noyau.N_utils import AsType
    ier = 0

    # On importe les definitions des commandes a utiliser dans la macro
    MACR_LIGN_COUPE = self.get_cmd('MACR_LIGN_COUPE')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    CALC_TABLE = self.get_cmd('CALC_TABLE')
    CALC_CHAMP = self.get_cmd('CALC_CHAMP')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    MasquerAlarme('MODELISA4_9')

  # Le concept sortant (de type table) est nomme
  # 'tabout' dans le contexte de la macro

    self.DeclareOut('tabout', self.sd)
    assert AsType(RESULTAT).__name__ in ('evol_elas', 'evol_noli',)
    dico = RESULTAT.LIST_CHAMPS()
    dico2 = RESULTAT.LIST_VARI_ACCES()
  # si ni INST ni NUME_ORDRE ne sont presents, on prend le premier
  # instant calcule
    if not INST and not NUME_ORDRE:
        INST = dico2['INST'][0]
    iret, ibid, n_modele = aster.dismoi(
        'MODELE', RESULTAT.nom, 'RESULTAT', 'F')
    MODEL = self.get_concept(n_modele)
    if NUME_ORDRE:
        if not NUME_ORDRE in dico2['NUME_ORDRE']:
            UTMESS('F', 'POST0_25', vali=NUME_ORDRE)
    else:
        if not INST in dico2['INST']:
            UTMESS('F', 'POST0_26', valr=INST)
#
    if NUME_ORDRE:
        if CHAM == 'EFFORT':
            if not NUME_ORDRE in dico['EFGE_ELNO']:
                if NUME_ORDRE in dico['DEPL']:
                    CALC_CHAMP(
                        RESULTAT=RESULTAT, reuse=RESULTAT, CONTRAINTE='EFGE_ELNO',
                        NUME_ORDRE=NUME_ORDRE)
                else:
                    UTMESS('F', 'POST0_19', vali=NUME_ORDRE)
        elif CHAM == 'DEFORMATION':
            if not NUME_ORDRE in dico['DEGE_ELNO']:
                if NUME_ORDRE in dico['DEPL']:
                    CALC_CHAMP(
                        RESULTAT=RESULTAT, reuse=RESULTAT, DEFORMATION='DEGE_ELNO',
                        NUME_ORDRE=NUME_ORDRE)
                else:
                    UTMESS('F', 'POST0_19', vali=NUME_ORDRE)

    dico = RESULTAT.LIST_CHAMPS()

    # Appel MACR_LIGN_COUPE :
    motscles = {}
    if CHAM == 'EFFORT':
        motscles['NOM_CHAM'] = 'EFGE_ELNO'
    if CHAM == 'DEFORMATION':
        motscles['NOM_CHAM'] = 'DEGE_ELNO'

    if CHAM == 'EFFORT':
        motscles['LIGN_COUPE'] = []
        iocc = 0
        for m in COOR_POINT:
            iocc = iocc + 1
            lst = m['COOR']
            if len(lst) == 4 and lst[3] != 0.:
                UTMESS('A', 'POST0_21', vali=iocc, valr=lst[3])
            lst = lst[0:3]
            motscles['LIGN_COUPE'].append(_F(TYPE='SEGMENT',
                                             NB_POINTS=2,
                                             COOR_ORIG=lst,
                                             COOR_EXTR=lst,
                                             DISTANCE_MAX=10.0,),)
        __tabl = MACR_LIGN_COUPE(RESULTAT=RESULTAT, **motscles)

    if CHAM == 'DEFORMATION':
        motscles['LIGN_COUPE'] = []
        iocc = 0
        for m in COOR_POINT:
            iocc = iocc + 1
            lst = m['COOR']
            if len(lst) != 4:
                UTMESS('F', 'POST0_22', vali=iocc)
            else:
                lst = lst[0:3]
                motscles['LIGN_COUPE'].append(_F(TYPE='SEGMENT',
                                                 NB_POINTS=2,
                                                 COOR_ORIG=lst,
                                                 COOR_EXTR=lst,
                                                 DISTANCE_MAX=10.0,),)
        __tabl = MACR_LIGN_COUPE(RESULTAT=RESULTAT, **motscles)

    tab2 = __tabl.EXTR_TABLE()
    if NUME_ORDRE:
        tab3 = (tab2.NUME_ORDRE == NUME_ORDRE)
    else:
        tab3 = (tab2.INST == INST)
    tab2 = tab3

    tab4 = Table()
    ilig = 0
    for ligne in tab2:
        ilig = ilig + 1
        if(ilig % 2) == 0:
            tab4.append(ligne)
    tab4 = tab4[tab2.para]
#
#  on cree une table(dege) bidon qu'on va surcharger
#
    if CHAM == 'DEFORMATION':
        motscles['NOM_CHAM'] = 'DEGE_ELNO'
        motscles['LIGN_COUPE'] = []
        tabz = []
        iocc = 0
        for m in COOR_POINT:
            iocc = iocc + 1
            lst = m['COOR']
            z = lst[3]
            tabz.append(z)
            lst = lst[0:3]
            motscles['LIGN_COUPE'].append(_F(TYPE='SEGMENT',
                                             NB_POINTS=2,
                                             COOR_ORIG=lst,
                                             COOR_EXTR=lst,
                                             DISTANCE_MAX=10.0,),)
        __tabeps = MACR_LIGN_COUPE(RESULTAT=RESULTAT, **motscles)
        __teps = CALC_TABLE(TABLE=__tabeps,
                            ACTION=(
                            _F(OPERATION='RENOMME',
                               NOM_PARA=('EXX', 'EPXX')),
                            _F(OPERATION='RENOMME',
                               NOM_PARA=('EYY', 'EPYY')),
                                _F(OPERATION='RENOMME',
                                   NOM_PARA=('EXY', 'EPZZ')),
                                _F(OPERATION='RENOMME',
                                   NOM_PARA=('KXX', 'EPXY')),
                                _F(OPERATION='RENOMME',
                                   NOM_PARA=('KYY', 'EPXZ')),
                                _F(OPERATION='RENOMME',
                                   NOM_PARA=('KXY', 'EPYZ')),
                                _F(OPERATION='EXTR',
                                   NOM_PARA=(
                                       'INTITULE', 'NOM_CHAM', 'NUME_ORDRE', 'INST', 'ABSC_CURV',
                                   'COOR_X', 'COOR_Y', 'COOR_Z',
                                   'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ',)),
                            ),)

        tabep2 = __teps.EXTR_TABLE()
        if NUME_ORDRE:
            tabep3 = (tabep2.NUME_ORDRE == NUME_ORDRE)
        else:
            tabep3 = (tabep2.INST == INST)
        tabep2 = tabep3

        tabep4 = Table()
        ilig = 0
        for ligne in tabep2:
            ilig = ilig + 1
            if(ilig % 2) == 0:
                tabep4.append(ligne)
        tabep4 = tabep4[tabep2.para]

        iligout = 0
        for ligout in tabep4:
            iligout = iligout + 1
            iligin = 0
            for ligin in tab4:
                iligin = iligin + 1
                if(iligout == iligin):
                    ligout['EPXX'] = ligin[
                        'EXX'] + ligin['KXX'] * tabz[iligout - 1]
                    ligout['EPYY'] = ligin[
                        'EYY'] + ligin['KYY'] * tabz[iligout - 1]
                    ligout['EPXY'] = ligin[
                        'EXY'] + ligin['KXY'] * tabz[iligout - 1]
                    ligout['EPZZ'] = 0.0
                    ligout['EPXZ'] = ligin['GAX'] * 0.5
                    ligout['EPYZ'] = ligin['GAY'] * 0.5

    if CHAM == 'EFFORT':
        dprod = tab4.dict_CREA_TABLE()
    elif CHAM == 'DEFORMATION':
        dprod = tabep4.dict_CREA_TABLE()

    tabout = CREA_TABLE(TYPE_TABLE='TABLE',
                        **dprod)
    RetablirAlarme('MODELISA4_9')
    return ier
