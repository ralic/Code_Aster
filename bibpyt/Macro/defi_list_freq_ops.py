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


def defi_list_freq_ops(self, RAFFINEMENT, INFO, TITRE, **args):

    from Accas import MCList
    from Utilitai.Utmess import UTMESS
    from math import fmod, sqrt

    self.set_icmd(1)
    ier = 0

    self.DeclareOut('co_l_freq', self.sd)

    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')

    # 1. Construction de la liste des fréquences "de base"
    motscle = {}
    for key in args:
        if args[key] != None:
            motscle[key] = args[key]

    __co_l_freq0 = DEFI_LIST_REEL(**motscle)
    l_freq0 = __co_l_freq0.Valeurs()

    # 2. Récuperation des données liées au raffinement
    if RAFFINEMENT['CRITERE'] in ('RELATIF', 'ABSOLU'):
        dispersion = RAFFINEMENT['DISPERSION']

    haveAmor = False
    if RAFFINEMENT['CRITERE'] == 'LARGEUR_3DB':
        haveAmor = True
        if self['RAFFINEMENT']['AMOR_REDUIT']:
            l_amor = list(RAFFINEMENT['AMOR_REDUIT'])
        else:
            l_amor = RAFFINEMENT['LIST_AMOR'].Valeurs()

    dfMin = RAFFINEMENT['PAS_MINI']
    nbPtsRaf = RAFFINEMENT['NB_POINTS']
    l_freq = RAFFINEMENT['LIST_RAFFINE']

    # Si le nombre d'amortissements donnés est inférieur au nombre de fréquences données
    # dans LIST_RAFFINE, les amortissements des fréquences supplémentaires sont
    # pris égaux au dernier amortissement de la liste.
    if haveAmor:
        if len(l_amor) < len(l_freq):
            for i in range(len(l_freq) - len(l_amor)):
                l_amor.append(l_amor[-1])

    # 3. Elimination des modes multiples dans la liste des fréquences l_freq
    l_freq_clean = [l_freq[0]]
    if haveAmor:
        lamor_clean = [l_amor[0]]

    for i in range(1, len(l_freq)):
        if (l_freq[i] - l_freq[i - 1]) > dfMin:
            l_freq_clean.append(l_freq[i])
            if haveAmor:
                lamor_clean.append(l_amor[i])

    l_freq = l_freq_clean[:]
    if haveAmor:
        l_amor = lamor_clean[:]

    # 4. Raffinement de la liste des fréquences

    # On stocke toutes les fréquences des intervalles raffinés, dans la liste l_raf.
    # Celle-ci contient automatiquement toutes les valeurs de la liste de base.
    l_raf = []

    for i in range(0, len(l_freq)):
        if haveAmor:
            if l_amor[i] > 1e-12:
                # on décale la fréquence l_freq[i] pour se centrer sur la
                # résonance d'amplitude
                l_freq[i] = l_freq[i] * sqrt(1 - 2 * l_amor[i] ** 2)
                # largeur de l'intervalle i à raffiner
                df = 2 * l_amor[i] * l_freq[i]
            else:
                df = 0.01 * l_freq[i]
        elif RAFFINEMENT['CRITERE'] == 'RELATIF':
            df = dispersion * l_freq[i]
        elif RAFFINEMENT['CRITERE'] == 'ABSOLU':
            df = dispersion

        ltemp = [l_freq[i] - df / 2. + j * df / (nbPtsRaf - 1)
                 for j in range(0, nbPtsRaf)]

        if i > 1:
            # on vérifie s'il y a un recouvrement d'intervalle
            if ltemp[0] < max(l_raf):
                freq_i = l_freq[i]
                freq_imoins1 = l_freq[i - 1]
                if haveAmor:
                    freq_i = l_freq[i] / sqrt(1 - 2 * l_amor[i] ** 2)
                    freq_imoins1 = l_freq[i - 1] / sqrt(
                        1 - 2 * l_amor[i - 1] ** 2)
                UTMESS('I', 'DYNAMIQUE_26', valr=(freq_imoins1, freq_i))

        if haveAmor:
            # si le nombre de points à ajouter est pair :
            if not fmod(nbPtsRaf, 2):
                # on crée un point au milieu (résonance d'amplitude)
                ltemp.append(l_freq[i])
                # résonance de phase
                ltemp.append(l_freq[i] / sqrt(1 - 2 * l_amor[i] ** 2))

        l_raf += ltemp

    l_raf += l_freq0
    l_raf.sort()

    # 5. Elimination des fréquences trop proches (écart < dfMin)

    l_freq_ok = l_freq + l_freq0

    i = 1
    while i < len(l_raf):
        if (l_raf[i] - l_raf[i - 1]) < dfMin:
            if (l_raf[i - 1] in l_freq_ok and l_raf[i] not in l_freq_ok):
                l_raf.remove(l_raf[i])
                i -= 1
            elif (l_raf[i] in l_freq_ok and l_raf[i - 1] not in l_freq_ok):
                l_raf.remove(l_raf[i - 1])
                i -= 1
            elif (l_raf[i] in l_freq_ok and l_raf[i - 1] in l_freq_ok):
                UTMESS('I', 'DYNAMIQUE_27', valr=(l_raf[i - 1], l_raf[i]))
            else:
                l_raf.remove(l_raf[i])
                i -= 1
        i += 1

    # 6. Création de la sd_listr8 de sortie
    co_l_freq = DEFI_LIST_REEL(VALE=l_raf)

    return ier
