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
#


def impr_diag_campbell_ops(
    self, MAILLAGE, MODES, NFREQ_CAMP, TYP_PREC, TYP_TRI,
        UNIT_FLE, UNIT_TOR, UNIT_LON, UNIT_TOT, UNIT_INT, L_S, **args):
# Macro permettant de tracer le diagramme de Campbell suivant
# le type de suivi des modes et le type de calcul de la precession
# Type de suivi, 0 SANS_TRI, 1 TRI_PREC, 2 TRI_FORM_MOD
# Type de precession, 1 somme, 2 grande orbite

    import numpy
    from math import pi
    import aster
    from Accas import _F
    from Noyau.N_types import is_float
    from Utilitai.Utmess import UTMESS

    from Macro.impr_diag_campbell_utils import (
        CLASS_MODES, EXTR_FREQ, TRI_MODE_MACf, TRI_MODE_MACt, TRI_MODE_MACl, CALC_MACf,
        CALC_MACt, CALC_MACl, CALC_PREC, TRI_MODE_PREC_DI, affiche_tab, affiche_tabint,
        calc_pas, color_camp, sup_redon_list, sup_redon_listv, save_intersec
    )

    # On importe les definitions des commandes a utiliser dans la macro

    RECU_TABLE = self.get_cmd('RECU_TABLE')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    EXTR_TABLE = self.get_cmd('EXTR_TABLE')
    IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
    IMPR_RESU = self.get_cmd('IMPR_RESU')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')
    DETRUIRE = self.get_cmd('DETRUIRE')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    lvit = [None]
    # Extraire les vitesses de rotation
    nb_temp = 0
    while 1:
        try:
            lvit[nb_temp] = MODES['VITE_ROTA', nb_temp + 1]
            nb_temp = nb_temp + 1
            lvit.append([None])
        except KeyError:
            break

    VITE_ROTA = lvit[0:nb_temp]
    num_vit_tri = sup_redon_listv(VITE_ROTA)
    # Recupere les modes suivant l'ordre de la liste des vitesses de rotation
    nbV1 = len(VITE_ROTA)

    if nbV1 < 2:
        UTMESS('F', 'CAMPBELL_1')

    __lmo = [None] * nbV1
    for ii in range(0, nbV1):
        itri = num_vit_tri[ii]
        __lmo[ii] = EXTR_TABLE(TYPE_RESU='MODE_MECA',
                               TABLE=MODES,
                               NOM_PARA='NOM_SD',
                               FILTRE=_F(NOM_PARA='NUME_VITE', VALE_I=itri),)

    L_VIT1 = []

    if type(VITE_ROTA) == list:
        L_VIT1 = VITE_ROTA
    elif type(VITE_ROTA) == tuple:
        L_VIT1 = list(VITE_ROTA)
    elif is_float(VITE_ROTA):
        L_VIT1.append(VITE_ROTA)

    nbV = len(L_VIT1)

    chaine = '\n'
    aster.affiche('RESULTAT', chaine)
    chaine = 'Liste triee des vitesses en rad/s'
    aster.affiche('RESULTAT', chaine)
    for ii in range(nbV):
        chaine = '%15.5E' % L_VIT1[ii]
        aster.affiche('RESULTAT', chaine)

    #-------------------------------------------------------------------------
    # Tester le nombre de frequences calculees pour chaque vitesse de rotation
    #-------------------------------------------------------------------------

    nb_FREQ = []
    for ii in range(nbV):
        # frequences totales
        __tfreq = RECU_TABLE(CO=__lmo[ii], NOM_PARA='FREQ',)
        tab2 = __tfreq.EXTR_TABLE()
        tabf = tab2.FREQ
        nb_FREQ_prec = nb_FREQ
        nb_FREQ.append(len(tabf))

        DETRUIRE(CONCEPT=_F(NOM=(__tfreq)), INFO=1)

    nbf_max = max(nb_FREQ)
    nbf_min = min(nb_FREQ)
    NFREQ = nbf_min
    if nbf_max != nbf_min:
        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Les nombres de frequences sont differents pour les vitesses de rotation.'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Pour poursuivre le calcul, NFREQ = %d' % NFREQ
        aster.affiche('RESULTAT', chaine)
    else:
        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Nombre de valeurs propres detectees est %d' % NFREQ
        aster.affiche('RESULTAT', chaine)
    if NFREQ_CAMP > NFREQ:
        chaine = 'Nombre de frequences demandees pour le trace  %d' % NFREQ_CAMP
        aster.affiche('RESULTAT', chaine)
        NFREQ_CAMP = NFREQ - 4
        chaine = 'Nombre de frequences pour le trace  %d' % NFREQ_CAMP
        aster.affiche('RESULTAT', chaine)
    else:
        chaine = 'Nombre de frequences demandees pour le trace  %d' % NFREQ_CAMP
        aster.affiche('RESULTAT', chaine)
    if NFREQ_CAMP <= 0:
        chaine = 'Le trace du diagramme de Campbell s''arrete !.'
        aster.affiche('RESULTAT', chaine)

    if NFREQ_CAMP > 0:

# ------------------------------------------------------------------
# Classe les modes en flexion, en torsion , en traction/ compression
# ------------------------------------------------------------------
        Mf = []
        Mt = []
        Ml = []

        # Recuperer les noeuds du maillage
        # construction des vecteurs jeveux
        nom_mail = MAILLAGE.nom
        lenm = len(nom_mail)
        nom_mail = nom_mail + ' ' * (8 - lenm)
        vectnoeu = nom_mail + '.NOMNOE'
        L_GR_NOEUD = aster.getvectjev(vectnoeu)

        NOEU = len(L_GR_NOEUD)
        C_MODES = CLASS_MODES(
            self, __lmo, NFREQ, NFREQ_CAMP, L_GR_NOEUD, L_VIT1)

        NFREQ_f = C_MODES[0]
        NFREQ_t = C_MODES[1]
        NFREQ_l = C_MODES[2]
        Mf = C_MODES[3]
        Mt = C_MODES[4]
        Ml = C_MODES[5]

        # Initialisation des tableaux de connexion apres classement
        # en gardant la numerotation globale des modes
        NVT = C_MODES[6]
        NVTf_int = C_MODES[7]
        NVTt_int = C_MODES[8]
        NVTl_int = C_MODES[9]
        NFREQ_fc = C_MODES[10]
        NFREQ_tc = C_MODES[11]
        NFREQ_lc = C_MODES[12]

        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Nombre de frequences totale :' + str(NFREQ)
        aster.affiche('RESULTAT', chaine)
        chaine = 'Nombre de frequences en flexion :' + \
            str(NFREQ_f) + ' ' + str(NFREQ_fc)
        aster.affiche('RESULTAT', chaine)
        chaine = 'Nombre de frequences torsion :' + \
            str(NFREQ_t) + ' ' + str(NFREQ_tc)
        aster.affiche('RESULTAT', chaine)
        chaine = 'Nombre de frequences traction/compression :' + \
            str(NFREQ_l) + ' ' + str(NFREQ_lc)
        aster.affiche('RESULTAT', chaine)

        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Initialisation des tableaux de connexion'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Modes non classes'
        aster.affiche('RESULTAT', chaine)
        affiche_tabint(NVT, NFREQ, nbV)
        if NFREQ_f > 0:
            chaine = 'Modes de flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTf_int, NFREQ_f, nbV)
        if NFREQ_t > 0:
            chaine = 'Modes de torsion'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTt_int, NFREQ_t, nbV)
        if NFREQ_l > 0:
            chaine = 'Modes de traction/compression'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTl_int, NFREQ_l, nbV)

    #-----------------------
    # Extraire les frequences
    #-----------------------
        FREQ = EXTR_FREQ(
            self, __lmo, Mf, Mt, Ml, NFREQ, NFREQ_f, NFREQ_t, NFREQ_l)
        FRQ = FREQ[0]
        FRQf = FREQ[1]
        FRQt = FREQ[2]
        FRQl = FREQ[3]
        FRQ_max = FREQ[4]
        AMOf = FREQ[5]

        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Frequences totales'
        aster.affiche('RESULTAT', chaine)
        affiche_tab(FRQ, NFREQ, nbV)
        if NFREQ_f > 0:
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Frequences en flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQf, NFREQ_f, nbV)
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Amortissement reduit'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(AMOf, NFREQ_f, nbV)
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
        if NFREQ_t > 0:
            chaine = 'Frequences en torsion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQt, NFREQ_t, nbV)
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
        if NFREQ_l > 0:
            chaine = 'Frequences en traction/compression'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQl, NFREQ_l, nbV)

    # Initialisation des tableaux de connexion
    # nouveau numerotation de modes par type de mode
    # Sans tri
        if NFREQ_f > 0:
            NVTf = numpy.zeros((NFREQ_f, nbV), int)
            for ii in range(nbV):
                for jj in range(NFREQ_f):
                    NVTf[jj][ii] = jj + 1
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Tableau de connexion initial en flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTf, NFREQ_f, nbV)

        if NFREQ_t > 0:
            NVTt = numpy.zeros((NFREQ_t, nbV), int)
            for ii in range(nbV):
                for jj in range(NFREQ_t):
                    NVTt[jj][ii] = jj + 1
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Tableau de connexion initial en torsion'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTt, NFREQ_t, nbV)

        if NFREQ_l > 0:
            NVTl = numpy.zeros((NFREQ_l, nbV), int)
            for ii in range(nbV):
                for jj in range(NFREQ_l):
                    NVTl[jj][ii] = jj + 1
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Tableau de connexion initial en traction/compression'
            aster.affiche('RESULTAT', chaine)
            affiche_tabint(NVTl, NFREQ_l, nbV)

    # ------------------------------------------------------------------
    # Tri par forme des modes
    # Tri des frequences par calcul des coefficients MAC
    # Remplissage du tableau de connexion
    # ------------------------------------------------------------------
        if TYP_TRI == 2:
            # ------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en flexion
            # -----------------------------------------------------------------
            if NFREQ_f > 0:
                LMACf = CALC_MACf(self, Mf, NFREQ_f)
                chaine = '\n'
                aster.affiche('RESULTAT', chaine)
                chaine = ' Tri par forme des modes TRI_FORM_MOD'
                aster.affiche('RESULTAT', chaine)

                for ii in range(nbV - 1):
                    chaine = '\n'
                    aster.affiche('RESULTAT', chaine)
                    iv = nbV - ii - 2
                    NVTf_mac = TRI_MODE_MACf(
                        self, LMACf[iv], NFREQ_f, NVTf, iv)

                OMIN = L_VIT1[0]
                if(OMIN == 0):
                    for ii in range(NFREQ_f):
                        NVTf_mac[ii][0] = NVTf_mac[ii][1]

                chaine = '\n'
                aster.affiche('RESULTAT', chaine)
                chaine = 'Tableau de connexion en flexion'
                aster.affiche('RESULTAT', chaine)
                affiche_tabint(NVTf_mac, NFREQ_f, nbV)

            # ------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en torsion
            # -----------------------------------------------------------------
            if NFREQ_t > 0:
                LMACt = CALC_MACt(self, Mt, NFREQ_t)

                for ii in range(nbV - 1):
                    chaine = '\n'
                    aster.affiche('RESULTAT', chaine)
                    iv = nbV - ii - 2
                    NVTt = TRI_MODE_MACt(self, LMACt[iv], NFREQ_t, NVTt, iv)
                chaine = '\n'
                aster.affiche('RESULTAT', chaine)
                chaine = 'Tableau de connexion en torsion'
                aster.affiche('RESULTAT', chaine)
                affiche_tabint(NVTt, NFREQ_t, nbV)

            # ----------------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en traction/compression
            # -----------------------------------------------------------------
            if NFREQ_l > 0:
                LMACl = CALC_MACl(self, Ml, NFREQ_l)

                for ii in range(nbV - 1):
                    chaine = '\n'
                    aster.affiche('RESULTAT', chaine)
                    iv = nbV - ii - 2
                    NVTl = TRI_MODE_MACl(self, LMACl[iv], NFREQ_l, NVTl, iv)
                chaine = '\n'
                aster.affiche('RESULTAT', chaine)
                chaine = 'Tableau de connexion en traction/compression'
                aster.affiche('RESULTAT', chaine)
                affiche_tabint(NVTl, NFREQ_l, nbV)

        #--------------------------------------------------------------------------
        # Calcul le sens de precession pour les modes en flexion a une vitesse de rotation donnee
        #----------------------------------------------------------------------
        if NFREQ_f > 0:
            SENS = CALC_PREC(self, Mf, NFREQ_f, L_GR_NOEUD, TYP_PREC)
            chaine = '\n'
            aster.affiche('RESULTAT', chaine)
            chaine = 'Sens de precession pour les modes en flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(SENS, NFREQ_f, nbV)

        # ------------------------------------------------------------------
        # Tri des modes en flexion par une methode de proche en proche
        # avec verification du sens de precession
        # Remplissage du tableau de connexion
        # ------------------------------------------------------------------
        if TYP_TRI == 1:
            if NFREQ_f > 0:
                OMIN = L_VIT1[0]
                PREC_DI = TRI_MODE_PREC_DI(SENS, NFREQ_f, NVTf, nbV, OMIN)
                nb_prec_dir = PREC_DI[0]
                nb_prec_inv = PREC_DI[1]
                NVTf_prec = PREC_DI[2]

        # --------------------------------
        # Trace du diagramme de campbell
        # --------------------------------
        chaine = 'Trace du diagramme de campbell'
        aster.affiche('RESULTAT', chaine)

        # Conversion de la vitesse de rotation en tr/mn pour l'affichage
        OM = L_VIT1
        for ii in range(nbV):
            OM[ii] = OM[ii] * 30. / pi

        Vitesse_min = min(OM)
        Vitesse_max = max(OM)

        OM_int = [OM[ii]
                  for ii in range(len(OM))]
        # pour le calcul des points d'intersection

        legende_x = 'Vitesse (tr/mn)'
        if Vitesse_min < 0:
            if abs(Vitesse_min) > abs(Vitesse_max):
                legende_x = 'Vitesse negative, en abscisse la valeur absolue de la vitesse (tr/mn)'
                for ii in range(nbV):
                    OM[ii] = abs(OM[ii])

        __FX = DEFI_LIST_REEL(VALE=OM)

        # Mise en page graphique
        Vmin = min(OM)
        Vmax = max(OM)

        # Determination de la frequence maximale
        Fmax = 0.0
        for jf in range(NFREQ_fc):
            for iv in range(nbV):
                if TYP_TRI == 0:
                    jf1 = NVTf[jf][iv] - 1
                if TYP_TRI == 1:
                    jf1 = NVTf_prec[jf][iv] - 1
                if TYP_TRI == 2:
                    jf1 = NVTf_mac[jf][iv] - 1
                F1 = FRQf[jf1][iv]
                if Fmax < F1:
                    Fmax = F1

        for jf in range(NFREQ_tc):
            for iv in range(nbV):
                jf1 = NVTt[jf][iv] - 1
                F1 = FRQt[jf1][iv]
                if Fmax < F1:
                    Fmax = F1

        for jf in range(NFREQ_lc):
            for iv in range(nbV):
                jf1 = NVTl[jf][iv] - 1
                F1 = FRQl[jf1][iv]
                if Fmax < F1:
                    Fmax = F1

        Fmin = 0.0
        Fmax = Fmax * 1.1

        # Calcul des bornes et pas de la grille pour les vitesses de rotation
        BV = calc_pas(Vmin, Vmax)
        BVmin = BV[0]
        BVmax = BV[1]
        pasV = BV[2]

        # Calcul des bornes et pas de la grille pour les frequences
        BF = calc_pas(Fmin, Fmax)
        BFmin = BF[0]
        BFmax = BF[1]
        pasF = BF[2]

        chaine = '\n'
        aster.affiche('RESULTAT', chaine)
        chaine = 'Fmax ' + str(Fmax) + ' BFmax ' + str(BFmax)
        aster.affiche('RESULTAT', chaine)

        TITRE1 = 'Diagramme de Campbell'
        TITRE2 = 'Modes en flexion'

        DEFI_FICHIER(ACTION='ASSOCIER', UNITE=UNIT_FLE,)
        DEFI_FICHIER(ACTION='ASSOCIER', UNITE=UNIT_TOR,)
        DEFI_FICHIER(ACTION='ASSOCIER', UNITE=UNIT_LON,)
        DEFI_FICHIER(ACTION='ASSOCIER', UNITE=UNIT_TOT,)
        # ---------------------------------------------------
        # Trace du diagramme de campbell des modes en flexion
        # ---------------------------------------------------
        EPSI = 1.E-7
        LFONC = []
        mfac1 = {}
        ll = 0
        if NFREQ_fc > 0:
            for jf in range(NFREQ_fc):
                for iv in range(nbV - 1):
                    OM3 = - \
                        1.    # OM3 different de -1, Changement de precession
                    OM4 = -1.    # OM4 different de -1, Changement de stabilite
                    if TYP_TRI == 0:
                        jf1 = NVTf[jf][iv] - 1
                        jf2 = NVTf[jf][iv + 1] - 1
                    if TYP_TRI == 1:
                        jf1 = NVTf_prec[jf][iv] - 1
                        jf2 = NVTf_prec[jf][iv + 1] - 1
                    if TYP_TRI == 2:
                        jf1 = NVTf_mac[jf][iv] - 1
                        jf2 = NVTf_mac[jf][iv + 1] - 1

                    # Frequences
                    if jf1 >= 0 and jf2 >= 0:
                        F1 = FRQf[jf1][iv]
                        F2 = FRQf[jf2][iv + 1]
                        A1 = AMOf[jf1][iv]
                        A2 = AMOf[jf2][iv + 1]

                        # Vitesses
                        OM1 = OM[iv]
                        OM2 = OM[iv + 1]
                        S1 = SENS[jf1][iv]
                        S2 = SENS[jf2][iv + 1]

                        if OM1 == 0.0:
                            S1 = S2
                        if S1 * S2 < 0:  # Changement de precession
                            OM3 = (OM1 + OM2) / 2
                            F3 = (F1 + F2) / 2

                        A0 = abs(EPSI * (F1 + F2) / 2)
                        if ((A1 - A0) * (A2 - A0) < 0):   # Changement de stabilite
                            OM4 = (A2 * OM1 - A1 * OM2) / (A2 - A1)
                            aa = (F2 - F1) / (OM2 - OM1)
                            bb = (F2 * OM1 - F1 * OM2) / (OM1 - OM2)
                            F4 = aa * OM4 + bb

                        # OM4 en dehors de OM1, OM2
                        if OM4 >= OM2:
                            OM4 = -1

                        if OM4 <= OM1:
                            OM4 = -1

                        if (A1 < 0) and (abs(A1) < A0):
                            A1 = 0.0
                        if (A2 < 0) and (abs(A2) < A0):
                            A2 = 0.0

                        # Tracer le segment pour chaque intervalle avec le code de couleur et
                        # de style adequats

                        # 1 cas, Pas de changement sur la plage de vitesse
                        if ((OM3 == -1) and (OM4 == -1)):
                            __fx1 = DEFI_LIST_REEL(VALE=[OM1, OM2])
                            __fy1 = DEFI_LIST_REEL(VALE=[F1, F2])
                            CS2 = color_camp(S2, A1)
                            ICS2 = CS2[0]
                            IST2 = CS2[1]
                            IMA2 = CS2[2]

                            __foni = DEFI_FONCTION(
                                NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

                            DICO = {}
                            DICO["FONCTION"] = __foni
                            DICO["COULEUR"] = ICS2
                            DICO["STYLE"] = IST2
                            DICO["MARQUEUR"] = IMA2
                            DICO["LEGENDE"] = ''
                            LFONC.append(DICO)

                            DETRUIRE(CONCEPT=_F(NOM=(__fx1, __fy1)), INFO=1)

                        # 2 cas, Changement de sens de precession
                        elif (OM3 >= 0) and (OM4 == -1):
                            __fx1 = DEFI_LIST_REEL(
                                VALE=[OM1, OM3])
                            # Premiere partie
                            __fy1 = DEFI_LIST_REEL(VALE=[F1, F3])
                            __fx2 = DEFI_LIST_REEL(
                                VALE=[OM3, OM2])
                            # Deuxieme partie
                            __fy2 = DEFI_LIST_REEL(VALE=[F3, F2])
                            CS1 = color_camp(S1, A1)
                            ICS1 = CS1[0]
                            IST1 = CS1[1]
                            IMA1 = CS1[2]
                            CS2 = color_camp(S2, A1)
                            ICS2 = CS2[0]
                            IST2 = CS2[1]
                            IMA2 = CS2[2]

                            __foni = DEFI_FONCTION(
                                NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

                            DICO = {}
                            DICO["FONCTION"] = __foni
                            DICO["COULEUR"] = ICS1
                            DICO["STYLE"] = IST1
                            DICO["MARQUEUR"] = IMA1
                            DICO["LEGENDE"] = ''
                            LFONC.append(DICO)

                            __foni = DEFI_FONCTION(
                                NOM_PARA='VITE', VALE_PARA=__fx2, VALE_FONC=__fy2)

                            DICO = {}
                            DICO["FONCTION"] = __foni
                            DICO["COULEUR"] = ICS2
                            DICO["STYLE"] = IST2
                            DICO["MARQUEUR"] = IMA2
                            DICO["LEGENDE"] = ''
                            LFONC.append(DICO)

                            DETRUIRE(
                                CONCEPT=_F(NOM=(__fx1, __fy1, __fx2, __fy2)), INFO=1)

                        # 3 cas, de changement de stabilite
                        elif (OM3 == -1) and (OM4 >= 0):

                            __fx1 = DEFI_LIST_REEL(
                                VALE=[OM1, OM4])
                            # Premiere partie
                            __fy1 = DEFI_LIST_REEL(VALE=[F1, F4])
                            __fx2 = DEFI_LIST_REEL(
                                VALE=[OM4, OM2])
                            # Deuxieme partie
                            __fy2 = DEFI_LIST_REEL(VALE=[F4, F2])
                            CS1 = color_camp(S2, A1)
                            ICS1 = CS1[0]
                            IST1 = CS1[1]
                            IMA1 = CS1[2]
                            CS2 = color_camp(S2, A2)
                            ICS2 = CS2[0]
                            IST2 = CS2[1]
                            IMA2 = CS2[2]

                            __foni = DEFI_FONCTION(
                                NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

                            DICO = {}
                            DICO["FONCTION"] = __foni
                            DICO["COULEUR"] = ICS1
                            DICO["STYLE"] = IST1
                            DICO["MARQUEUR"] = IMA1
                            DICO["LEGENDE"] = ''
                            LFONC.append(DICO)

                            __foni = DEFI_FONCTION(
                                NOM_PARA='VITE', VALE_PARA=__fx2, VALE_FONC=__fy2)

                            DICO = {}
                            DICO["FONCTION"] = __foni
                            DICO["COULEUR"] = ICS2
                            DICO["STYLE"] = IST2
                            DICO["MARQUEUR"] = IMA2
                            DICO["LEGENDE"] = ''
                            LFONC.append(DICO)

                            DETRUIRE(
                                CONCEPT=_F(NOM=(__fx1, __fy1, __fx2, __fy2)), INFO=1)

                        # 4 et 5 cas de changement de sens de precession et de
                        # stabilite
                        elif (OM3 >= 0) and (OM4 >= 0):
                            # 4 eme cas
                            if (OM4 < OM3):
                                __fx1 = DEFI_LIST_REEL(
                                    VALE=[OM1, OM4])
                                # Premiere partie
                                __fy1 = DEFI_LIST_REEL(VALE=[F1, F4])
                                __fx2 = DEFI_LIST_REEL(
                                    VALE=[OM4, OM3])
                                # Deuxieme partie
                                __fy2 = DEFI_LIST_REEL(VALE=[F4, F3])
                                __fx3 = DEFI_LIST_REEL(
                                    VALE=[OM3, OM2])
                                # Troisieme partie
                                __fy3 = DEFI_LIST_REEL(VALE=[F3, F2])
                                CS1 = color_camp(S1, A1)
                                ICS1 = CS1[0]
                                IST1 = CS1[1]
                                IMA1 = CS1[2]
                                CS2 = color_camp(S1, A2)
                                ICS2 = CS2[0]
                                IST2 = CS2[1]
                                IMA2 = CS2[2]
                                CS3 = color_camp(S2, A2)
                                ICS3 = CS3[0]
                                IST3 = CS3[1]
                                IMA3 = CS3[2]

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS1
                                DICO["STYLE"] = IST1
                                DICO["MARQUEUR"] = IMA1
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx2, VALE_FONC=__fy2)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS2
                                DICO["STYLE"] = IST2
                                DICO["MARQUEUR"] = IMA2
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx3, VALE_FONC=__fy3)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS3
                                DICO["STYLE"] = IST3
                                DICO["MARQUEUR"] = IMA3
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                DETRUIRE(
                                    CONCEPT=_F(NOM=(__fx1, __fy1, __fx2, __fy2, __fx3, __fy3)), INFO=1)

                            # 5 eme cas
                            else:
                                __fx1 = DEFI_LIST_REEL(
                                    VALE=[OM1, OM3])
                                # Premiere partie
                                __fy1 = DEFI_LIST_REEL(VALE=[F1, F3])
                                __fx2 = DEFI_LIST_REEL(
                                    VALE=[OM3, OM4])
                                # Deuxieme partie
                                __fy2 = DEFI_LIST_REEL(VALE=[F3, F4])
                                __fx3 = DEFI_LIST_REEL(
                                    VALE=[OM4, OM2])
                                # Troisieme partie
                                __fy3 = DEFI_LIST_REEL(VALE=[F4, F2])
                                CS1 = color_camp(S1, A1)
                                ICS1 = CS1[0]
                                IST1 = CS1[1]
                                IMA1 = CS1[2]
                                CS2 = color_camp(S2, A1)
                                ICS2 = CS2[0]
                                IST2 = CS2[1]
                                IMA2 = CS2[2]
                                CS3 = color_camp(S2, A2)
                                ICS3 = CS3[0]
                                IST3 = CS3[1]
                                IMA3 = CS3[2]

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS1
                                DICO["STYLE"] = IST1
                                DICO["MARQUEUR"] = IMA1
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx2, VALE_FONC=__fy2)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS2
                                DICO["STYLE"] = IST2
                                DICO["MARQUEUR"] = IMA2
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                __foni = DEFI_FONCTION(
                                    NOM_PARA='VITE', VALE_PARA=__fx3, VALE_FONC=__fy3)

                                DICO = {}
                                DICO["FONCTION"] = __foni
                                DICO["COULEUR"] = ICS3
                                DICO["STYLE"] = IST3
                                DICO["MARQUEUR"] = IMA3
                                DICO["LEGENDE"] = ''
                                LFONC.append(DICO)

                                DETRUIRE(
                                    CONCEPT=_F(NOM=(__fx1, __fy1, __fx2, __fy2, __fx3, __fy3)), INFO=1)

            mfac1["COURBE"] = LFONC

            IMPR_FONCTION(
                UNITE=UNIT_FLE,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y = (BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1)

            IMPR_FONCTION(
                UNITE=UNIT_TOT,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y = (BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1)
            del(LFONC)
            del(mfac1, DICO)

        # ---------------------------------------------------
        # Trace du diagramme de campbell des modes en torsion
        # ---------------------------------------------------
        TITRE2 = 'Modes en Torsion'
        if NFREQ_tc > 0:
            LFONC = []
            mfac1 = {}
            for jj in range(NFREQ_tc):

                __fy1 = DEFI_LIST_REEL(
                    VALE=[FRQt[int(NVTt[jj][ii] - 1)][ii] for ii in range(nbV)])
                __foni = DEFI_FONCTION(
                    NOM_PARA='VITE', VALE_PARA=__FX, VALE_FONC=__fy1)

                DICO = {}
                DICO["FONCTION"] = __foni
                DICO["COULEUR"] = 1
                DICO["STYLE"] = 6
                DICO["MARQUEUR"] = 0
                DICO["LEGENDE"] = ''
                LFONC.append(DICO)

                DETRUIRE(CONCEPT=_F(NOM=(__fy1)), INFO=1)

            mfac1["COURBE"] = LFONC
            IMPR_FONCTION(
                UNITE=UNIT_TOR,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y =(BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                **mfac1)

            IMPR_FONCTION(
                UNITE=UNIT_TOT,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y =(BFmin, BFmax),
                TITRE = TITRE1,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                **mfac1)

            del(LFONC)
            del(mfac1, DICO)

        # ----------------------------------------------------------------
        # Trace du diagramme de campbell des modes en traction/compression
        # ----------------------------------------------------------------
        TITRE2 = 'Modes en traction/compression'
        if NFREQ_lc > 0:
            LFONC = []
            mfac1 = {}
            for jj in range(NFREQ_lc):

                __fy1 = DEFI_LIST_REEL(
                    VALE=[FRQl[int(NVTl[jj][ii] - 1)][ii] for ii in range(nbV)])
                __foni = DEFI_FONCTION(
                    NOM_PARA='VITE', VALE_PARA=__FX, VALE_FONC=__fy1)

                DICO = {}
                DICO["FONCTION"] = __foni
                DICO["COULEUR"] = 8
                DICO["STYLE"] = 8
                DICO["MARQUEUR"] = 0
                DICO["LEGENDE"] = ''
                LFONC.append(DICO)

                DETRUIRE(CONCEPT=_F(NOM=(__fy1)), INFO=1)

            mfac1["COURBE"] = LFONC
            IMPR_FONCTION(
                UNITE=UNIT_LON,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y =(BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                **mfac1)
            IMPR_FONCTION(
                UNITE=UNIT_TOT,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y =(BFmin, BFmax),
                TITRE = TITRE1,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                **mfac1)

            del(LFONC)
            del(mfac1, DICO)

#-------------------------------------------------------------------------

        # -----------------------------
        # Trace des droites de pentes S
        # -----------------------------

        # Pour S=1, on le trace automatiquement
        S = 1.0
        L_S1 = []

        if type(L_S) == list:
            L_S1 = L_S
        elif type(L_S) == tuple:
            L_S1 = list(L_S)
        elif is_float(L_S):
            L_S1.append(L_S)
        L_S1.append(S)

        # Supprimer la redondance dans la liste
        sup_redon_list(L_S1)

        # Faire une dictionnaire de courbe
        # Constituer de liste de dictionnaire de fonctions
        LFONC = []
        mfac1 = {}
        for ii in range(len(L_S1)):
            F1 = BVmin * L_S1[ii] / 60.
            F2 = BVmax * L_S1[ii] / 60.
            __fx1 = DEFI_LIST_REEL(VALE=[BVmin, BVmax])
            __fy1 = DEFI_LIST_REEL(VALE=[F1, F2])

            __foni = DEFI_FONCTION(
                NOM_PARA='VITE', VALE_PARA=__fx1, VALE_FONC=__fy1)

            DICO = {}
            DICO["FONCTION"] = __foni
            DICO["COULEUR"] = 1
            DICO["STYLE"] = 1
            DICO["MARQUEUR"] = 0
            DICO["LEGENDE"] = ''
            LFONC.append(DICO)

            DETRUIRE(CONCEPT=_F(NOM=(__fx1, __fy1)), INFO=1)

        mfac1["COURBE"] = LFONC
        if NFREQ_fc > 0:
            TITRE2 = 'Modes en flexion'
            IMPR_FONCTION(
                UNITE=UNIT_FLE,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y = (BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                **mfac1)

        if NFREQ_tc > 0:
            TITRE2 = 'Modes en Torsion'
            IMPR_FONCTION(
                UNITE=UNIT_TOR,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y = (BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1)
        if NFREQ_lc > 0:
            TITRE2 = 'Modes en traction/compression'
            IMPR_FONCTION(
                UNITE=UNIT_LON,
                FORMAT='XMGRACE',
                BORNE_X=(BVmin, BVmax),
                BORNE_Y = (BFmin, BFmax),
                TITRE = TITRE1,
                SOUS_TITRE = TITRE2,
                GRILLE_X = pasV,
                GRILLE_Y = pasF,
                LEGENDE_X = legende_x,
                LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1)

        IMPR_FONCTION(
            UNITE=UNIT_TOT,
            FORMAT='XMGRACE',
            BORNE_X=(BVmin, BVmax),
            BORNE_Y = (BFmin, BFmax),
            TITRE = TITRE1,
            GRILLE_X = pasV,
            GRILLE_Y = pasF,
            LEGENDE_X = legende_x,
            LEGENDE_Y = 'FREQ (Hz)',
            **mfac1)

        del(LFONC)
        del(mfac1, DICO)

        DEFI_FICHIER(ACTION='LIBERER', UNITE=UNIT_FLE,)
        DEFI_FICHIER(ACTION='LIBERER', UNITE=UNIT_TOR,)
        DEFI_FICHIER(ACTION='LIBERER', UNITE=UNIT_LON,)
        DEFI_FICHIER(ACTION='LIBERER', UNITE=UNIT_TOT,)

#-------------------------------------------------------------------------

        # --------------------------------------------------------------
        # Determination des points d'intersection avec les droites Y=AX
        # Calcul des coordonnees des points
        # --------------------------------------------------------------

        # Ecrire dans un fichier texte en sortie
        DEFI_FICHIER(TYPE='ASCII', UNITE=UNIT_INT,)
        nomfic = 'fort.' + str(UNIT_INT)
        FINT1 = open(nomfic, 'w')

        INTERSEC = []
        # Modes en flexion

        for ii in range(len(L_S1)):
            DICO = {}
            DICO["pente"] = L_S1[ii]
            ll = 0
            XY = [[None] * 2]
            for jf in range(NFREQ_fc):
                for iv in range(nbV - 1):
                    if TYP_TRI == 0:
                        jf1 = NVTf[jf][iv] - 1
                        jf2 = NVTf[jf][iv + 1] - 1
                    if TYP_TRI == 1:
                        jf1 = NVTf_prec[jf][iv] - 1
                        jf2 = NVTf_prec[jf][iv + 1] - 1
                    if TYP_TRI == 2:
                        jf1 = NVTf_mac[jf][iv] - 1
                        jf2 = NVTf_mac[jf][iv + 1] - 1
                    if jf1 >= 0 and jf2 >= 0:
                        X1 = OM[iv]
                        Y1 = FRQf[jf1][iv]
                        X2 = OM[iv + 1]
                        Y2 = FRQf[jf2][iv + 1]
                        A = (Y1 - Y2) / (X1 - X2)
                        B = Y1 - (A * X1)
                        pente = L_S1[ii]
                        P1 = B * 60. / (pente - A * 60.)
                        P2 = P1 * pente / 60.

                        if P1 >= X1 and P1 <= X2:
                            if P2 >= Fmin and P2 <= Fmax:
                                if OM_int[iv] <= 0 and OM_int[iv + 1] < 0:       # Vitesse negative
                                    P1 = -P1
                                XY[ll][0] = P1
                                XY[ll][1] = P2
                                # On ajoute une ligne supplementaire
                                XY.append([None] * 2)
                                ll = ll + 1

            L_XY = XY[0:ll]
            DICO["point"] = L_XY
            INTERSEC.append(DICO)

        # Sauvegarde des points d'intersection
        FINT1.write('\n')
        chaine = 'Mode en flexion'
        FINT1.write(chaine)
        save_intersec(INTERSEC, FINT1)

        del(XY, L_XY)
        del(INTERSEC, DICO)

        INTERSEC = []
        # Modes en torsion
        for ii in range(len(L_S1)):
            DICO = {}
            DICO["pente"] = L_S1[ii]
            ll = 0
            XY = [[None] * 2]
            for jf in range(NFREQ_tc):
                for iv in range(nbV - 1):
                    jf1 = NVTt[jf][iv] - 1
                    jf2 = NVTt[jf][iv + 1] - 1
                    if jf1 >= 0 and jf2 >= 0:
                        X1 = OM[iv]
                        Y1 = FRQt[jf1][iv]
                        X2 = OM[iv + 1]
                        Y2 = FRQt[jf2][iv + 1]
                        A = (Y1 - Y2) / (X1 - X2)
                        B = Y1 - (A * X1)
                        pente = L_S1[ii]
                        P1 = B * 60. / (pente - A * 60.)
                        P2 = P1 * pente / 60.

                        if P1 >= X1 and P1 <= X2:
                            if P2 >= Fmin and P2 <= Fmax:
                                if OM_int[iv] <= 0 and OM_int[iv + 1] < 0:       # Vitesse negative
                                    P1 = -P1
                                XY[ll][0] = P1
                                XY[ll][1] = P2
                                # On ajoute une ligne supplementaire
                                XY.append([None] * 2)
                                ll = ll + 1

            L_XY = XY[0:ll]
            DICO["point"] = L_XY
            INTERSEC.append(DICO)

        # Sauvegarde des points d'intersection
        FINT1.write('\n')
        FINT1.write('\n')
        chaine = 'Mode en Torsion'
        FINT1.write(chaine)
        save_intersec(INTERSEC, FINT1)

        del(XY, L_XY)
        del(INTERSEC, DICO)

#-------------------------------------------------------------------------
        INTERSEC = []
        # Modes en traction / compression
        for ii in range(len(L_S1)):
            DICO = {}
            DICO["pente"] = L_S1[ii]
            ll = 0
            XY = [[None] * 2]
            for jf in range(NFREQ_lc):
                for iv in range(nbV - 1):
                    jf1 = NVTl[jf][iv] - 1
                    jf2 = NVTl[jf][iv + 1] - 1
                    if jf1 >= 0 and jf2 >= 0:
                        X1 = OM[iv]
                        Y1 = FRQl[jf1][iv]
                        X2 = OM[iv + 1]
                        Y2 = FRQl[jf2][iv + 1]
                        A = (Y1 - Y2) / (X1 - X2)
                        B = Y1 - (A * X1)
                        pente = L_S1[ii]
                        P1 = B * 60. / (pente - A * 60.)
                        P2 = P1 * pente / 60.

                        if P1 >= X1 and P1 <= X2:
                            if P2 >= Fmin and P2 <= Fmax:
                                if OM_int[iv] <= 0 and OM_int[iv + 1] < 0:       # Vitesse negative
                                    P1 = -P1
                                XY[ll][0] = P1
                                XY[ll][1] = P2
                                # On ajoute une ligne supplementaire
                                XY.append([None] * 2)
                                ll = ll + 1

            L_XY = XY[0:ll]
            DICO["point"] = L_XY
            INTERSEC.append(DICO)

        # Sauvegarde des points d'intersection
        FINT1.write('\n')
        FINT1.write('\n')
        chaine = 'Mode en traction / compression'
        FINT1.write(chaine)
        save_intersec(INTERSEC, FINT1)

        del(XY, L_XY)
        del(INTERSEC, DICO)
        nbl = len(L_S1)
        for ii in range(nbl):
            il = nbl - ii - 1
            del L_S1[il]
        FINT1.close()
