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

from Accas import _F
import string

try:
    import aster
    from Utilitai.Utmess import UTMESS
except:
    pass


def macr_spectre_ops(
    self, MAILLAGE, PLANCHER, NOM_CHAM, CALCUL, RESU, IMPRESSION=None,
        FREQ=None, LIST_FREQ=None, LIST_INST=None, AMOR_SPEC=None, **args):
    """
        Ecriture de la macro MACR_SPECTRE
    """
    ier = 0
    import string
    from types import ListType, TupleType, StringType
    EnumType = (ListType, TupleType)

    # On importe les definitions des commandes a utiliser dans la macro
    RECU_FONCTION = self.get_cmd('RECU_FONCTION')
    CALC_FONCTION = self.get_cmd('CALC_FONCTION')
    IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
    CREA_TABLE = self.get_cmd('CREA_TABLE')

    # Comptage commandes + déclaration concept sortant
    self.set_icmd(1)
    self.DeclareOut('tab', self.sd)
    macro = 'MACR_SPECTRE'

    # construction de la liste des noeuds à traiter
    planch_nodes = {}
    if (MAILLAGE):
        dic_gpno = aster.getcolljev(MAILLAGE.nom.ljust(8) + ".GROUPENO")
        l_nodes = aster.getvectjev(MAILLAGE.nom.ljust(8) + ".NOMNOE")
    l_plancher = []
    #
    dplancher = []
    for j in PLANCHER:
        dplancher.append(j.cree_dict_valeurs(j.mc_liste))
    #
    for plancher in dplancher:
        liste_no = []
        clefs = plancher.keys()
        if ('NOEUD' in clefs):
            if plancher['NOEUD'] != None:
                if type(plancher['NOEUD']) == StringType:
                    liste_no.append(plancher['NOEUD'])
                else:
                    for noeud in plancher['NOEUD']:
                        liste_no.append(noeud)
        if ('GROUP_NO' in clefs):
            if plancher['GROUP_NO'] != None:
                assert (MAILLAGE != None)
                if type(plancher['GROUP_NO']) == StringType:
                    noms_no = [string.strip(l_nodes[n - 1])
                               for n in dic_gpno[plancher['GROUP_NO'].ljust(24)]]
                    liste_no = liste_no + noms_no
                else:
                    for group_no in plancher['GROUP_NO']:
                        noms_no = [string.strip(l_nodes[n - 1])
                                   for n in dic_gpno[group_no.ljust(24)]]
                        liste_no = liste_no + noms_no
        planch_nodes[plancher['NOM']] = liste_no
        l_plancher.append(plancher['NOM'])

    if AMOR_SPEC != None and type(AMOR_SPEC) not in EnumType:
        AMOR_SPEC = (AMOR_SPEC,)
    #
    if NOM_CHAM == 'ACCE':
        dico_glob = {}
    if NOM_CHAM == 'DEPL':
        dico_glob = {'DX_max': [], 'DY_max': [], 'DZ_max': [], 'DH_max': [], }
    #
    # ---------------------------------------------------------------------------------------------
    # boucle 1 sur les planchers
    for plancher in l_plancher:
        #
        if NOM_CHAM == 'ACCE':
            __moy_x = [None] * len(planch_nodes[plancher])
            __moy_y = [None] * len(planch_nodes[plancher])
            __moy_z = [None] * len(planch_nodes[plancher])
        if NOM_CHAM == 'DEPL':
            dicDmax = {}
        # -----------------------------------------------------------------------------------------
        # boucle 2 sur les noeuds du plancher
        indexn = 0
        for node in planch_nodes[plancher]:
            # -------------------------------------------------------------------------------------
            # boucle 3 sur les directions (X,Y,Z)
            for dd in ('X', 'Y', 'Z'):
                # ---------------------------------------------------------------------------------
                # boucle 4 sur les résultats
                l_fonc = []
                for resu in RESU:
                    # Etape 1: Récupération des fonctions
                    motscles = {}
                    if resu['RESU_GENE'] != None:
                        if CALCUL == 'ABSOLU':
                            motscles['MULT_APPUI'] = args['MULT_APPUI']
                        #
                        motscles['RESU_GENE'] = resu['RESU_GENE']
                        __spo = RECU_FONCTION(NOM_CHAM=NOM_CHAM,
                                              TOUT_ORDRE='OUI',
                                              NOM_CMP='D' + dd,
                                              INTERPOL='LIN',
                                              PROL_GAUCHE='CONSTANT',
                                              PROL_DROITE='CONSTANT',
                                              NOEUD=node,
                                              **motscles
                                              )
                    elif args.get('MULT_APPUI') != None:
                        UTMESS('F', 'SPECTRAL0_13')
                    #
                    if resu['RESULTAT'] != None:
                        motscles['RESULTAT'] = resu['RESULTAT']
                        __spo = RECU_FONCTION(
                            NOM_CHAM=NOM_CHAM,
                            TOUT_ORDRE='OUI',
                            NOM_CMP='D' + dd,
                            INTERPOL='LIN',
                            PROL_GAUCHE='CONSTANT',
                            PROL_DROITE='CONSTANT',
                            NOEUD=node,
                            **motscles
                        )
                    #
                    if resu['TABLE'] != None:
                        # 2 formats de table possible. Avec les colonnes :
                        #   INST NOEUD NOM_CHAM NOM_CMP VALE
                        #   INST NOEUD NOM_CHAM DX DY DZ
                        # récupération du nom des colonnes de la table
                        nomcol = resu['TABLE'].get_nom_para()
                        #
                        lst1 = ('INST', 'NOEUD', 'NOM_CHAM', 'NOM_CMP', 'VALE')
                        ok1 = True
                        for para in lst1:
                            ok1 = ok1 and (para in nomcol)
                        #
                        lst2 = ('INST', 'NOEUD', 'NOM_CHAM', 'D' + dd,)
                        ok2 = True
                        for para in lst2:
                            ok2 = ok2 and (para in nomcol)
                        #
                        if (not ok1 ^ ok2):
                            print nomcol
                            assert (ok1 ^ ok2)
                        #
                        if (ok1):
                            __spo = RECU_FONCTION(
                                TABLE=resu['TABLE'],
                                PARA_X='INST', PARA_Y='VALE', INTERPOL='LIN',
                                FILTRE=(
                                    _F(NOM_PARA='NOEUD',    VALE_K=node),
                                    _F(NOM_PARA='NOM_CHAM',
                                       VALE_K=NOM_CHAM),
                                    _F(NOM_PARA='NOM_CMP',  VALE_K='D' + dd),),
                            )
                        #
                        if (ok2):
                            __spo = RECU_FONCTION(
                                TABLE=resu['TABLE'], PARA_X='INST', PARA_Y='D' + dd, INTERPOL='LIN',
                                FILTRE=(
                                    _F(NOM_PARA='NOEUD',    VALE_K=node),
                                    _F(NOM_PARA='NOM_CHAM', VALE_K=NOM_CHAM),),
                            )
                    #
                    # Etape 2: Combinaisons
                    if NOM_CHAM == 'ACCE':
                        # Accelerations relatives
                        if CALCUL == 'RELATIF':
                            # Combinaison avec fonction d'accélération
                            motscles = {}
                            if LIST_INST != None:
                                motscles['LIST_PARA'] = LIST_INST
                            __spo = CALC_FONCTION(
                                COMB=(_F(FONCTION=__spo, COEF=1.0),
                                      _F(FONCTION=resu[
                                         'ACCE_' + dd], COEF=1.0),),
                                **motscles
                            )

                        # Etape 3: Calcul des spectres d'oscillateur
                        motscles = {}
                        if FREQ != None:
                            motscles['FREQ'] = FREQ
                        if LIST_FREQ != None:
                            motscles['LIST_FREQ'] = LIST_FREQ
                        __spo = CALC_FONCTION(
                            SPEC_OSCI=_F(FONCTION=__spo,
                                         AMOR_REDUIT=AMOR_SPEC,
                                         NORME=args['NORME'],
                                         **motscles)
                        )
                        l_fonc.append(__spo)

                    elif NOM_CHAM == 'DEPL':
                        if CALCUL == 'ABSOLU':
                            # On retranche les deplacements d entrainement
                            motscles = {}
                            if LIST_INST != None:
                                motscles['LIST_PARA'] = LIST_INST
                            __spo = CALC_FONCTION(
                                COMB=(_F(FONCTION=__spo, COEF=1.0),
                                      _F(FONCTION=resu[
                                         'DEPL_' + dd], COEF=-1.0),),
                                **motscles
                            )
                        l_fonc.append(__spo)

                # fin boucle 4 sur les résultats
                # ---------------------------------------------------------------------------------
                #
                # calcul de la moyenne sur les resultats à noeud et direction
                # fixes
                nbresu = len(RESU)
                if NOM_CHAM == 'ACCE':
                    mcfCMBx = []
                    mcfCMBy = []
                    mcfCMBz = []
                    for spo in l_fonc:
                        mcfCMBx.append(
                            _F(FONCTION=spo, COEF=1. / float(nbresu),))
                        mcfCMBy.append(
                            _F(FONCTION=spo, COEF=1. / float(nbresu),))
                        mcfCMBz.append(
                            _F(FONCTION=spo, COEF=1. / float(nbresu),))
                    motscles = {}
                    if LIST_FREQ != None:
                        motscles['LIST_PARA'] = LIST_FREQ
                    if dd == 'X':
                        __moy_x[indexn] = CALC_FONCTION(
                            COMB=mcfCMBx, **motscles)
                    if dd == 'Y':
                        __moy_y[indexn] = CALC_FONCTION(
                            COMB=mcfCMBy, **motscles)
                    if dd == 'Z':
                        __moy_z[indexn] = CALC_FONCTION(
                            COMB=mcfCMBz, **motscles)
                #
                elif NOM_CHAM == 'DEPL':
                    moy = 0.
                    for spo in l_fonc:
                        fspo = spo.convert()
                        aspo = fspo.abs()
                        vmax = aspo.extreme()['max']
                        moy = moy + vmax[-1][-1]
                    dicDmax[(node, dd)] = moy / nbresu
            # fin boucle 3 sur les directions
            # -------------------------------------------------------------------------------------
            #
            # impressions en chaque noeud
            if NOM_CHAM == 'ACCE' and IMPRESSION != None:
                if IMPRESSION['TOUT'] == 'OUI':
                    __moyxa = [None] * len(AMOR_SPEC)
                    __moyya = [None] * len(AMOR_SPEC)
                    __moyza = [None] * len(AMOR_SPEC)
                    for i in range(len(AMOR_SPEC)):
                        amor = AMOR_SPEC[i]
                        __moyxa[i] = RECU_FONCTION(
                            NAPPE=__moy_x[indexn], VALE_PARA_FONC=AMOR_SPEC[i])
                        __moyya[i] = RECU_FONCTION(
                            NAPPE=__moy_y[indexn], VALE_PARA_FONC=AMOR_SPEC[i])
                        __moyza[i] = RECU_FONCTION(
                            NAPPE=__moy_z[indexn], VALE_PARA_FONC=AMOR_SPEC[i])
                    motscles = {}
                    dI = IMPRESSION[0].cree_dict_valeurs(
                        IMPRESSION[0].mc_liste)
                    if dI.has_key('PILOTE'):
                        motscles['PILOTE'] = IMPRESSION['PILOTE']
                    if IMPRESSION['FORMAT'] != 'TABLEAU':
                        motscles['ECHELLE_X'] = 'LOG'
                    if IMPRESSION['TRI'] == 'AMOR_SPEC':
                        for i in range(len(AMOR_SPEC)):
                            TITRE   = 'Spectres / Plancher = ' + plancher + \
                                ' / amor=' + \
                                str(AMOR_SPEC[i]) + ' / noeud=' + node
                            IMPR_FONCTION(
                                FORMAT=IMPRESSION['FORMAT'],
                                UNITE=IMPRESSION['UNITE'],
                                COURBE=(_F(FONCTION=__moyxa[i], LEGENDE='X',),
                                        _F(FONCTION=__moyya[i], LEGENDE='Y',),
                                        _F(FONCTION=__moyza[
                                            i], LEGENDE='Z',),),
                                TITRE =TITRE,
                                **motscles
                            )
                    elif IMPRESSION['TRI'] == 'DIRECTION':
                        lfonc = []
                        for dd in ('X', 'Y', 'Z'):
                            TITRE = 'Spectres / Plancher = ' + plancher + \
                                ' / direction = ' + dd + ' / noeud = ' + node
                            legende = 'amor=' + str(AMOR_SPEC[i])
                            if dd == 'X':
                                l_fonc = [_F(FONCTION=__moyxa[i], LEGENDE=legende,)
                                          for i in range(len(AMOR_SPEC))]
                            if dd == 'Y':
                                l_fonc = [_F(FONCTION=__moyya[i], LEGENDE=legende,)
                                          for i in range(len(AMOR_SPEC))]
                            if dd == 'Z':
                                l_fonc = [_F(FONCTION=__moyza[i], LEGENDE=legende,)
                                          for i in range(len(AMOR_SPEC))]
                            IMPR_FONCTION(
                                FORMAT=IMPRESSION['FORMAT'],
                                UNITE=IMPRESSION['UNITE'],
                                COURBE=l_fonc,
                                TITRE=TITRE,
                                **motscles
                            )

            # increment de l'indice de noeud
            indexn = indexn + 1
        # fin boucle 2 sur les noeuds du plancher
        # -----------------------------------------------------------------------------------------
        #
        # Etape 4: Calcul des enveloppes des spectres ou des deplacements max
        if NOM_CHAM == 'ACCE':
            mcslx = []
            mcsly = []
            mcslz = []
            indexn = 0
            for node in planch_nodes[plancher]:
                mcslx.append(__moy_x[indexn])
                mcsly.append(__moy_y[indexn])
                mcslz.append(__moy_z[indexn])
                indexn = indexn + 1
            __snx = CALC_FONCTION(ENVELOPPE=_F(FONCTION=mcslx))
            __sny = CALC_FONCTION(ENVELOPPE=_F(FONCTION=mcsly))
            __snz = CALC_FONCTION(ENVELOPPE=_F(FONCTION=mcslz))
            __snh = CALC_FONCTION(ENVELOPPE=_F(FONCTION=(__snx, __sny)))
        elif NOM_CHAM == 'DEPL':
            DRmX = max([dicDmax[(node, 'X')]
                       for node in planch_nodes[plancher]])
            DRmY = max([dicDmax[(node, 'Y')]
                       for node in planch_nodes[plancher]])
            DRmZ = max([dicDmax[(node, 'Z')]
                       for node in planch_nodes[plancher]])
            DRmH = max([DRmX, DRmY])
        #
        # Renseignement de la table finale des résultats
        if NOM_CHAM == 'ACCE':
            nbind = len(AMOR_SPEC)
            for i in range(nbind):
                dico_glob[
                    'FREQ'] = __snx.Valeurs()[1][i][0]
                dico_glob['eX_%d_%s' %
                          (i, plancher)] = __snx.Valeurs()[1][i][1]
                dico_glob['eY_%d_%s' %
                          (i, plancher)] = __sny.Valeurs()[1][i][1]
                dico_glob['eZ_%d_%s' %
                          (i, plancher)] = __snz.Valeurs()[1][i][1]
                dico_glob['eH_%d_%s' %
                          (i, plancher)] = __snh.Valeurs()[1][i][1]
        elif NOM_CHAM == 'DEPL':
            dico_glob['DX_max'].append(DRmX)
            dico_glob['DY_max'].append(DRmY)
            dico_glob['DZ_max'].append(DRmZ)
            dico_glob['DH_max'].append(DRmH)
        #
        # Etape 5: Impression des courbes
        if NOM_CHAM == 'ACCE' and IMPRESSION != None:
            motscles = {}
            dI = IMPRESSION[0].cree_dict_valeurs(IMPRESSION[0].mc_liste)
            if dI.has_key('PILOTE'):
                motscles['PILOTE'] = IMPRESSION['PILOTE']
            if IMPRESSION['FORMAT'] != 'TABLEAU':
                motscles['ECHELLE_X'] = 'LOG'
            __snxa = [None] * len(AMOR_SPEC)
            __snya = [None] * len(AMOR_SPEC)
            __snza = [None] * len(AMOR_SPEC)
            __snha = [None] * len(AMOR_SPEC)
            for i in range(nbind):
                __snxa[i] = RECU_FONCTION(
                    NAPPE=__snx, VALE_PARA_FONC=AMOR_SPEC[i], )
                __snya[i] = RECU_FONCTION(
                    NAPPE=__sny, VALE_PARA_FONC=AMOR_SPEC[i], )
                __snza[i] = RECU_FONCTION(
                    NAPPE=__snz, VALE_PARA_FONC=AMOR_SPEC[i], )
                __snha[i] = RECU_FONCTION(
                    NAPPE=__snh, VALE_PARA_FONC=AMOR_SPEC[i], )
            if IMPRESSION['TRI'] == 'AMOR_SPEC':
                for i in range(nbind):
                    TITRE   = 'Spectres moyens / Plancher = ' + \
                        plancher + ' / amor=' + str(AMOR_SPEC[i])
                    IMPR_FONCTION(
                        FORMAT=IMPRESSION['FORMAT'],
                        UNITE=IMPRESSION['UNITE'],
                        COURBE=(_F(FONCTION=__snxa[i], LEGENDE='X',),
                                _F(FONCTION=__snya[i], LEGENDE='Y',),
                                _F(FONCTION=__snza[i], LEGENDE='Z',),
                                _F(FONCTION=__snha[i], LEGENDE='H',),),
                        TITRE =TITRE,
                        **motscles
                    )
            elif IMPRESSION['TRI'] == 'DIRECTION':
                for dd in ('X', 'Y', 'Z', 'H'):
                    TITRE   = 'Spectres moyens / Plancher = ' + \
                        plancher + ' / direction = ' + dd
                    legende = 'amor=' + str(AMOR_SPEC[i])
                    l_fonc = []
                    if dd == 'X':
                        l_fonc = [_F(FONCTION=__snxa[i], LEGENDE=legende,)
                                  for i in range(len(AMOR_SPEC))]
                    if dd == 'Y':
                        l_fonc = [_F(FONCTION=__snya[i], LEGENDE=legende,)
                                  for i in range(len(AMOR_SPEC))]
                    if dd == 'Z':
                        l_fonc = [_F(FONCTION=__snza[i], LEGENDE=legende,)
                                  for i in range(len(AMOR_SPEC))]
                    if dd == 'H':
                        l_fonc = [_F(FONCTION=__snha[i], LEGENDE=legende,)
                                  for i in range(len(AMOR_SPEC))]
                    IMPR_FONCTION(
                        FORMAT=IMPRESSION['FORMAT'],
                        UNITE=IMPRESSION['UNITE'],
                        COURBE=l_fonc,
                        TITRE=TITRE,
                        **motscles
                    )
    # fin boucle 1 sur les planchers
    # ---------------------------------------------------------------------------------------------
    #
    # Etape6 : Renseignement de la table finale des résultats
    lListe = []
    if NOM_CHAM == 'DEPL':
        lListe.append(_F(LISTE_K=l_plancher, PARA='PLANCHER'))
        titre = 'Calcul des spectres enveloppes'
    elif NOM_CHAM == 'ACCE':
        titre = [
            'Calcul des spectres enveloppes par planchers pour les amortissements numérotés :', ]
        b = [' %d : %g ' % (i, AMOR_SPEC[i]) for i in range(len(AMOR_SPEC))]
        titre.append('/'.join(b))
    lkeys = dico_glob.keys()
    lkeys.sort()
    for key in lkeys:
        lListe.append(_F(LISTE_R=dico_glob[key], PARA=key))
    tab = CREA_TABLE(LISTE=lListe, TITRE=titre)
    return ier
