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

# person_in_charge: marc.kham at edf.fr


def Triaxial_DR(K, G, phi, psi, cohes=0., sigma0=0., depzz=-.000001, epzz_max=-0.0003,):
    """
    VALIDATION DE LA LOI DE MOHR-COULOMB
    ====================================

    F = SIGMA1 - SIGMA3 + (SIGMA1 + SIGMA3) SIN(PHI) - 2C COS(PHI) <= 0

    SOLUTION ANALYTIQUE POUR UN ESSAI TRIAXIAL DRAINE

    CHARGEMENT IMPOSE: DEPSI1 = F(T)
                       SIGMA3 = CTE

    3 INCONNUES:       SIGMA1, EPSI3, DLAMBDA
    """

    import numpy as np
    from cmath import *

    # Parametres materiaux
    # ------------------------------
    tol = 1e-6
    c = cohes
    sigma1 = [sigma0]
    sigma3 = [sigma0]

    depsi1 = depzz
    epsi1_max = epzz_max
    epsi1 = list(np.arange(0., epsi1_max + depsi1, depsi1))
    # ----Fin

    # Definition de certaines constantes
    # ------------------------------------------
    s = np.sin(pi / 180. * phi)
    t = np.sin(pi / 180. * psi)
    cohes = 2. * c * np.cos(pi / 180. * phi)
    C = K - 2. * G / 3.
    D = 2. * (K + G / 3.)
    E = K + 4. * G / 3.
    det = E * D - 2. * C * C
    # ----Fin

    f = [0.]
    epsi1_pla = [0.]
    epsi3_pla = [0.]
    epsi1_ela = [sigma0 * (D - 2. * C) / det]
    epsi3_ela = [sigma0 * (E - C) / det]
    tlambda = [0.]
    ttlambda = [0.]
    DSDE = []

    # =========================================
    # boucle de chargement
    # =========================================

    for i, ep1 in enumerate(epsi1[1:]):

        print
        print '=========================================================='
        print '> pas i=', i, ' epsi1=', ep1

        # prediction elastique
        # ----------------------
        dep1 = depsi1
        dep3 = -C / D * depsi1
        dsigma1 = E * dep1 + 2. * C * dep3
        dsigma3 = D * dep3 + C * dep1

        epsi1_ela.append(epsi1_ela[i] + dep1)
        epsi3_ela.append(epsi3_ela[i] + dep3)
        sigma1.append(sigma1[i] + dsigma1)
        sigma3.append(sigma3[i] + dsigma3)

        f.append(None)
        tlambda.append(None)
        ttlambda.append(ttlambda[i])
        epsi1_pla.append(None)
        epsi3_pla.append(None)

        # =========================================
        # boucle de Newton
        # =========================================
        n = 0
        Newton = False
        while not Newton:

            # initialisation
            # -----------------------
            epsi1_pla[i + 1] = epsi1_pla[i]
            epsi3_pla[i + 1] = epsi1_pla[i]
            # ----Fin

            n += 1

            si = np.sign(sigma1[i + 1] - sigma3[i + 1])
            if si == 0.:
                si = 1.

            seuil = si * (sigma1[i + 1] - sigma3[i + 1]) + (
                sigma1[i + 1] + sigma3[i + 1]) * s - cohes

            ielas = (seuil / cohes < tol)

            print ' -----------'
            print '  * Newton no.', n, ' seuil =', seuil / cohes, ' si =', si

            if not ielas:
                # Cas plastique
                # ---------------------
                print
                print '  ====>>> Plasticite activee <<<===='
                print

                A = 2. * (K * s + G * (si + s / 3.))
                B = 2. * (2. * K * s - G * (si + s / 3.))
                BB = A * (t + si) + B * (t - si)

                dlambda = seuil / BB

                # print '  dlambda =',dlambda

                depsi1_pla = (t + si) * dlambda
                depsi3_pla = (t - si) * dlambda

            else:
                # Cas elastique
                # ---------------------
                print
                print '  ====<<< Elastique >>>===='
                print

                dlambda = 0.
                depsi1_pla = 0.
                depsi3_pla = 0.

            # Cacul de Dsigma
            # ---------------------
            dsigma1 = -E * depsi1_pla - 2. * C * depsi3_pla
            dsigma3 = -C * depsi1_pla - D * depsi3_pla

            # Equilibre de Newton?
            # ----------------------------------
            Newton = (abs((sigma3[i + 1] + dsigma3 - sigma0) / sigma0) < tol)

            if not Newton:
                if not ielas:
                    ddepsi3 = (sigma0 - sigma3[i + 1] - dsigma3) / (
                        1. - B * (t - si + C / D * (t + si)) / BB) / D
                else:
                    ddepsi3 = (sigma0 - sigma3[i + 1] - dsigma3) / D
                epsi3_ela[i + 1] += ddepsi3

                # Mis-a-jour Dsigma
                sigma1[i + 1] += 2. * C * ddepsi3
                sigma3[i + 1] += D * ddepsi3
                f[i + 1] = si * (sigma1[i + 1] - sigma3[i + 1]) + (
                    sigma1[i + 1] + sigma3[i + 1]) * s - cohes

                # Matrice tangente consistente
                # ----------------------------
                DSDE.append(
                    [[E - A * (
                      E * (
                      t + si) + C * (
                      t - si)) / BB, C - B * (
                        E * (t + si) + C * (t - si)) / BB, ],
                        [C - E * (C * (t + si) + E * (t - si)) / BB, E - B * (C * (t + si) + E * (t - si)) / BB, ]]
                )
            else:
                # Mise-a-jour Depsilon
                epsi1_ela[i + 1] -= depsi1_pla
                epsi3_ela[i + 1] -= depsi3_pla
                epsi1_pla[i + 1] += depsi1_pla
                epsi3_pla[i + 1] += depsi3_pla

                # Mise-a-jour Dsigma
                sigma1[i + 1] += dsigma1
                sigma3[i + 1] += dsigma3

                # Mise-a-jour VI
                tlambda[i + 1] = dlambda
                ttlambda[i + 1] += dlambda
                f[i + 1] = si * (sigma1[i + 1] - sigma3[i + 1]) + (
                    sigma1[i + 1] + sigma3[i + 1]) * s - cohes

                # Matrice tangente consistente
                # ----------------------------
                DSDE.append(
                    [[E, C, ],
                     [C, E, ]]
                )
            if n >= 5:
                print "  ====>>> Non Convergence <<<===="
                break

    # Post-Traitements
    # ---------------------------
    epsivp = list(2. * t * np.array(ttlambda))
    epsidp = list(np.sqrt(t * t + 3.) * np.array(ttlambda))

    return (epsi1, sigma1, sigma3, epsivp, epsidp,)
# ---Fin


def Defi_Xmcourbe(X=None, Y=None, FUNCTION=None, courbe=None, legend='X', color=0, mark=0,):

    if FUNCTION:
        def_co = {'FONCTION': FUNCTION}
    else:
        def_co = {'ABSCISSE': X}
        def_co['ORDONNEE'] = Y

    def_co['LEGENDE'] = legend
    def_co['COULEUR'] = color
    def_co['MARQUEUR'] = mark

    try:
        courbe.append(def_co)
    except:
        courbe = [def_co, ]

    return courbe
