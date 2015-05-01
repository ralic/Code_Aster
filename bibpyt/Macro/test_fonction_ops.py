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
# person_in_charge: nicolas.sellenet at edf.fr

import os

from Noyau.N_types import is_complex, is_str, is_sequence

from Cata.cata import formule, formule_c, fonction_sdaster, fonction_c, nappe_sdaster

epsi = 1e-15

# Format
ligne_fct_1 = """ ---- FONCTION         %(nom_para)s"""
ligne_fct_11 = """ ---- FONCTION         %(nom_para)s TITRE """
ligne_fct_2 = """      %(nom_fct)s %(val_para)s """
ligne_fct_22 = """      %(nom_fct)s %(val_para)s %(titre)s """
ligne_fct_3 = """      %(refe)s %(legende)s %(valref)s %(valcal)s %(erreur)s %(tole)s """
ligne_fct_4 = """%(testOk)s  %(refe)s %(legende)s %(valref)s %(valcal)s %(erreur)s %(tole)s """

ligne_nap_1 = """ ---- NAPPE            %(nom_para_0)s %(nom_para)s """
ligne_nap_2 = """      %(nom_nap)s %(val_para_0)s %(val_para)s"""

ligne_att_1 = """ ---- %(nom)s %(nom_attr)s %(nom_para)s %(vale)s """
ligne_att_2 = """ ---- %(nom)s %(nom_attr)s %(vale)s """
ligne_att_11 = """      %(nom)s %(nom_attr)s %(nom_para)s %(vale)s """
ligne_att_22 = """      %(nom)s %(nom_attr)s %(vale)s """
ligne_att_3 = """ %(testOk)s TEST_ATTRIBUTS """

ligne_separatrice = 80 * '-'

ligne_intspc   = """ ---- INTERSPECTRE        %(nom_para)s"""
ligne_intspc_1 = """      %(nom)s %(val_para)s"""

list_fct = ['REFERENCE', 'LEGENDE',
            'VALE_REFE', 'VALE_CALC', 'ERREUR', 'TOLE']
list_attr = ['ATTR', 'PARA', 'VALE']


def TesterValeur(nomPara, valPu, valRef, res, epsi, crit, sSigne):
    """
       Teste de la valeur calculee par rapport a la valeur de reference
    """
    import aster
    import cmath
    import math

    isTestOk = 0
    vtc = valRef[0]
    if is_sequence(vtc):
        assert((vtc[0] == 'RI') | (vtc[0] == 'MP')), vtc[0]
        if vtc[0] == 'RI':
            vtc = vtc[1] + 1j * vtc[2]
        else:
            vtc = vtc[1] * cmath.exp(1j * math.pi * vtc[2] / 180)
    if sSigne == 'OUI':
        res = abs(res)
        if is_complex(valRef[0]):
            vtc = abs(vtc)

    # Recherche de la valeur la plus proche de la valeur calculee
    # dans le tableau valRef
    minTmp = abs(res - vtc)
    curI = 0
    for i in range(len(valRef)):
        vtc = valRef[i]
        if is_sequence(vtc):
            assert((vtc[0] == 'RI') | (vtc[0] == 'MP')), vtc[0]
            if vtc[0] == 'RI':
                vtc = vtc[1] + 1j * vtc[2]
            else:
                vtc = vtc[1] * cmath.exp(1j * math.pi * vtc[2] / 180)
        if sSigne == 'OUI' and is_complex(vtc):
            vtc = abs(vtc)
        valTmp = abs(res - vtc)
        if valTmp < minTmp:
            valTmp = minTmp
            curI = i

    vtc = valRef[curI]
    if is_sequence(vtc):
        assert((vtc[0] == 'RI') | (vtc[0] == 'MP')), vtc[0]
        if vtc[0] == 'RI':
            vtc = vtc[1] + 1j * vtc[2]
        else:
            vtc = vtc[1] * cmath.exp(1j * math.pi * vtc[2] / 180)
    if sSigne == 'OUI' and is_complex(vtc):
        vtc = abs(vtc)

    testOk = 'NOOK'
    curEps = 0
    err = 0
    pourcent = ' '
    # Calcul de l'erreur commise
    if crit[0:4] == 'RELA':
        isTestOk = (abs(res - vtc) <= epsi * abs(vtc))
        if vtc != 0:
            if is_complex(res) or is_complex(vtc):
                err = abs(res - vtc) / abs(vtc) * 100
            else:
                err = (res - vtc) / vtc * 100
        else:
            err = 999.999999
        if isTestOk:
            testOk = ' OK '
        curEps = epsi * 100
        pourcent = '%'
    else:
        isTestOk = (abs(res - vtc) <= epsi)
        if is_complex(res) or is_complex(vtc):
            err = abs(res - vtc)
        else:
            err = res - vtc
        if isTestOk:
            testOk = ' OK '
        curEps = epsi

    return {'testOk': testOk, 'erreur': err, 'epsilon': curEps, 'valeurRef': vtc}

#------------------------------------------


def RoundValues(crit, res, vtc, err, curEps, nreg=False):
    """
       Effectue des troncatures en fonction des valeurs réelles fournies
       et retourne eventuellement des valeurs sans exposant.

       input :
         crit    (K)  :  'RELATIF' /'ABSOLU'
         res     (R)  :  valeur calculée par aster (réelle)
         vtc     (R)  :  valeur attendue par l'utilisateur (VALE_REFE ou VALE_CALC)
         err     (R)  :  erreur calculée en % (colonne ERREUR)
         curEps  (R)  :  tolérance acceptable pour l'erreur en % (colonne TOLE)
       output :
         resr    (K)  :  chaine représentant "res" (arrondie pour l'affichage)
         vtcr    (K)  :  chaine représentant "vtc" (arrondie pour l'affichage)
         errr    (K)  :  chaine représentant "err" (arrondie pour l'affichage)
         curEpsr (K)  :  chaine représentant "curEps" (arrondie pour l'affichage)
    """
    import math
    # valeur calculee, valeur de reference:
    #--------------------------------------
    res2 = """%20.15E """ % res
    vtc2 = """%20.15E """ % vtc

    # détermination du nombre de digits à afficher pour resr et vtc : ndigit
    # ----------------------------------------------------------------------
    if crit[0:4] == 'RELA':
        ndigit = -int(math.log10(abs(curEps)))
        ndigit = ndigit + 2      # +2 à cause des %
        ndigit = max(ndigit, 2)  # il en faut un minimum quand meme
    else:
        if curEps != 0.:
            ndigit = -int(math.log10(abs(curEps)))
        else:
            ndigit = 10
        if res != 0.:
            ndigit = ndigit + int(math.log10(abs(res)))
        ndigit = max(ndigit, 2)  # il en faut un minimum quand meme

    # Pour NON_REGRESSION, on veut toujours au moins 12 :
    if nreg:
        ndigit = max(12, ndigit)
    ndigit = min(ndigit, 15)  # limite de la double présision

    # arrondi des 2 valeurs rest et vtc :
    #------------------------------------
    chndec = "%25." + str(ndigit) + "E"
    rest = chndec % res
    rest = rest.strip()
    vtct = chndec % vtc
    vtct = vtct.strip()

    # écriture éventuelle sans exposant :
    #---------------------------------------------
    def sansExp(res, rest, crit, ndigit):
        ares = abs(res)
        if ares >= 0.01 and ares < 1.e5:
            if ares >= 0.01 and ares < 1.:
                nap = ndigit + 2
                ntot = nap + 1
            else:
                nav = int(math.log10(ares)) + 1
                nap = ndigit + 1 - nav
                ntot = nav + nap
            resr = "%*.*f" % (ntot, nap, res)
            resr = resr.strip()
            return resr
        else:
            return rest.strip()

    resr = sansExp(res, rest, crit, ndigit)
    vtcr = sansExp(vtc, vtct, crit, ndigit)

    # erreur et tolerance:
    #--------------------
    listEpsiOut = []
    listEpsiIn = [err, curEps]
    for erin in listEpsiIn:
        err2 = ("""%5.1E """ % (abs(erin))).strip()
        chdiff = """%5.1f"""
        ii = err2.find('E')
        expo = err2[ii + 2:ii + 4]
        sg = err2[ii + 1:ii + 2]
        nexpo = int(expo)
        if abs(erin) < 0.1:
            listEpsiOut.append(err2)
        elif abs(erin) > 0.1 and abs(erin) < 100000:
            # listEpsiOut.append((str(abs(erin)).strip())[:nexpo+2])
            listEpsiOut.append((chdiff % abs(erin)).strip())
        else:
            listEpsiOut.append(err2)

    errr = listEpsiOut[0]
    curEpsr = listEpsiOut[1]

    return (resr, vtcr, errr, curEpsr)
#------------------------------------------


def AfficherResultat(dicoValeur, nomPara, ref, legende, crit, res, valPu, txt, label=True):
    """
       Gestion de l'affichage par ajout de texte au tableau txt
       passe en parametre
    """
    testOk = dicoValeur['testOk']
    err = dicoValeur['erreur']
    curEps = dicoValeur['epsilon']
    vtc = dicoValeur['valeurRef']

    pourcent = ' '
    if crit[0:4] == 'RELA':
        pourcent = '%'

    if is_complex(res):
        if not is_complex(vtc):
            vtc0 = complex(vtc, 0)
        else:
            vtc0 = vtc
        resr, vtcr, errr, curEpsr = RoundValues(
            crit, res.real, vtc0.real, err, curEps)
        resc, vtcc, errr, curEpsr = RoundValues(
            crit, res.imag, vtc0.imag, err, curEps)
    else:
        vtc0 = vtc
        nreg = ref == 'NON_REGRESSION'
        res2, vtc2, errr, curEpsr = RoundValues(
            crit, res, vtc0, err, curEps, nreg=nreg)

    if is_complex(res):
        if(res.imag < 0):
            val_cal = resr.upper() + resc.upper() + 'j'
        else:
            val_cal = resr.upper() + '+' + resc.upper() + 'j'
    else:
        val_cal = res2.upper()

    if is_complex(vtc0):
        if(vtc0.imag < 0):
            val_ref = vtcr.upper() + vtcc.upper() + 'j'
        else:
            val_ref = vtcr.upper() + '+' + vtcc.upper() + 'j'
    else:
        val_ref = vtc2.upper()

    espace = (len(val_ref) - 8) * ' '
    chvalref = 'VALE_REFE' + espace
    espace = (len(val_cal) - 8) * ' '
    chvalcal = 'VALE_CALC' + espace

    if(len(val_ref) <= 16):
        nvref = 16
    elif(len(val_ref) <= 24):
        nvref = 24
    elif(len(val_ref) <= 36):
        nvref = 36
    else:
        nvref = 48

    if(len(val_cal) <= 16):
        nvcal = 16
    elif(len(val_cal) <= 24):
        nvcal = 24
    elif(len(val_cal) <= 36):
        nvcal = 36
    else:
        nvcal = 48

    # Ajout du texte en fonction du resultat: ligne 3
    current = {'refe': list_fct[0] + (16 - len(list_fct[0])) * ' ',
               'legende': list_fct[1] + (16 - len(list_fct[1])) * ' ',
               'valref': list_fct[2] + (nvref - len(list_fct[2])) * ' ',
               'valcal': list_fct[3] + (nvcal - len(list_fct[3])) * ' ',
               'erreur': list_fct[4] + (16 - len(list_fct[4])) * ' ',
               'tole': list_fct[5] + (16 - len(list_fct[5])) * ' ',
               }
    if label:
        txt.append(ligne_fct_3 % current)

    # Ajout du texte en fonction du resultat : ligne 4
    current = {'testOk': testOk,
               'refe': ref + (16 - len(ref)) * ' ',
               'legende': legende + (16 - len(legende)) * ' ',
               'valref': val_ref + (nvref - len(val_ref)) * ' ',
               'valcal': val_cal + (nvcal - len(val_cal)) * ' ',
               'erreur': str(errr) + pourcent + (16 - len(str(errr) + pourcent)) * ' ',
               'tole': str(curEpsr) + pourcent + (16 - len(str(curEpsr) + pourcent)) * ' ',
               }
    txt.append(ligne_fct_4 % current)


def get_valref(lafonc, kw, typ):
    valref = None
    if (type(lafonc) == formule_c) or (type(lafonc) == fonction_c):
        valref = kw['VALE_' + typ + '_C']
    else:
        valref = kw['VALE_' + typ]
    if is_str(valref[0]):
        valref = [valref, ]
    return valref

# -----------------------------------------------------------------------------


def test_fonction_ops(self, TEST_NOOK, VALEUR, ATTRIBUT, **args):
    """
       Corps de la macro TEST_FONCTION
    """
    macro = 'TEST_FONCTION'
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS
    from SD.sd_fonction import sd_fonction
    from Cata_Utils.t_fonction import t_fonction_c

    CALC_FONCTION = self.get_cmd('CALC_FONCTION')
    DETRUIRE = self.get_cmd('DETRUIRE')

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # txt sert a l'affichage dans le fichier RESULTAT
    txt = ['', ]
    txt.append(ligne_separatrice)

    if VALEUR != None:
        # Boucle sur les VALEURS
        for val in VALEUR:
            dres = val.cree_dict_valeurs(val.mc_liste)

            # Recherche des mots-cles simples
            ssigne = dres['VALE_ABS']
            epsi = dres['TOLE_MACHINE']
            crit = dres['CRITERE']
            fct = dres['FONCTION']
            nompara = dres['NOM_PARA']
            if nompara == None:
                nompara = ''
            ref = dres['REFERENCE'] or 'NON_REGRESSION'
            other_ref = ref != 'NON_REGRESSION' and dres['REFERENCE'] or None
            ver = None
            legende = dres['LEGENDE']
            if legende == None:
                legende = 'XXXX'
            nomfct = fct.get_name()

            # Transformation de nompara en liste
            if (not is_sequence(nompara)) and nompara != None:
                nompara = [nompara, ]

            # La fonction est directement de dres['FONCTION']
            lafonc = None
            titre = ''
            lafonc = fct
            res = 0.
            typeFct = ''
            valpu = dres['VALE_PARA']
            if not is_sequence(valpu):
                valpu = [valpu, ]

            valref = get_valref(lafonc, dres, 'CALC')
            if other_ref:
                valoth = get_valref(lafonc, dres, 'REFE')
                epsoth = dres['PRECISION']

            intervalle = dres['INTERVALLE']

            ier = 0
            # Distinction des cas
            # - "fonction" sur un intervalle
            # - "formule",
            # - "fonction" ou "nappe"
            if (type(lafonc) == fonction_sdaster) and intervalle != None:
                # XXX il faut utiliser lafonc.Parametres() !
                fctProl = lafonc.sdj.PROL.get()
                prolG = 'rien'
                if fctProl[4][0:1] == 'C':
                    prolG = 'CONSTANT'
                elif fctProl[4][0:1] == 'E':
                    prolG = 'EXCLU'
                elif fctProl[4][0:1] == 'L':
                    prolG = 'LINEAIRE'
                prolD = 'rien'
                if fctProl[4][1:2] == 'C':
                    prolD = 'CONSTANT'
                elif fctProl[4][1:2] == 'E':
                    prolD = 'EXCLU'
                elif fctProl[4][1:2] == 'L':
                    prolD = 'LINEAIRE'
                curInterpol = [fctProl[1][0:3], fctProl[1][4:7]]

                __fInt = CALC_FONCTION(INTEGRE=_F(FONCTION=lafonc,),
                                       PROL_DROITE=prolD,
                                       PROL_GAUCHE=prolG,
                                       INTERPOL=curInterpol)

                res1 = __fInt(intervalle[0])
                res2 = __fInt(intervalle[1])

                DETRUIRE(CONCEPT=_F(NOM=__fInt), INFO=1)

                res = (res2 - res1) / (intervalle[1] - intervalle[0])
                valpu[0] = intervalle[0]

            elif type(lafonc) in (formule, formule_c):
                # Lecture des valeurs de reference dans les mots-cles simples
                if type(lafonc) == formule_c:
                    typeFct = 'formule_c'
                else:
                    typeFct = 'formule'

                # On cherche les valeurs de reference passees a TEST_FONCTION et
                # on les trie grace a ceux de la formule
                paramFormule = lafonc.Parametres()['NOM_PARA']
                if not is_sequence(paramFormule):
                    paramFormule = [paramFormule, ]
                if nompara[0] == '':
                    nompara = paramFormule

                # On verifie que la formule a bien le meme nombre de parametres
                # que ceux passes a la fonction TEST_FONCTION
                if len(nompara) != len(paramFormule):
                    ier = 160
                    UTMESS('A+', 'FONCT0_9', valk=(lafonc.get_name()))
                    UTMESS('A', 'FONCT0_14', vali=(
                        len(nompara), len(paramFormule)))
                    return 0.

                # Trie des parametres passes a la fonction TEST_FONCTION pour
                # correspondre a l'ordre de ceux de la formule
                nParamOrdo = []
                vParamOrdo = []
                for iPN in range(len(paramFormule)):
                    nParamOrdo.append('')
                    # vParamOrdo.append('')

                compteur = 0
                for iPN in range(len(paramFormule)):
                    i = 0
                    for iPU in range(len(nompara)):
                        if paramFormule[iPN] == nompara[iPU]:
                            if nParamOrdo[iPN] == '':
                                vParamOrdo.append(valpu[iPU])
                                nParamOrdo[iPN] = paramFormule[iPN]
                                compteur = compteur + 1
                            else:
                                ier = 120
                                UTMESS(
                                    'A+', 'FONCT0_9', valk=(lafonc.get_name()))
                                UTMESS('A', 'FONCT0_15', valk=nompara)
                                res = 0.
                        i = i + 1
                    if nParamOrdo[iPN] == '':
                        ier = 130
                        UTMESS('A+', 'FONCT0_9', valk=(lafonc.get_name()))
                        UTMESS('A', 'FONCT0_16', valk=paramFormule)
                        UTMESS('A', 'FONCT0_17', valk=nompara)
                        return 0.

                # Si tout est Ok, on calcul la valeur de la formule
                if ier == 0:
                    res = lafonc(*vParamOrdo)

            # Cas fonction et nappe
            elif type(lafonc) in (fonction_sdaster, fonction_c, nappe_sdaster):
                # XXX il faut utiliser lafonc.Parametres() !
                # Recuperation du .PROL de la fonction
                fct_prol = lafonc.sdj.PROL.get_stripped()
                if fct_prol == None:
                    UTMESS('F', 'PREPOST3_93')

                nompu = ''
                if nompara[0] != '':
                    nompu = nompara[0]
                else:
                    nompu = fct_prol[2]
                    nompara = [nompu, ]

                # Une nappe a forcement 2 parametres
                if (fct_prol[0] == 'NAPPE') & (len(nompara) == 1):
                    UTMESS('A', 'PREPOST3_94')
                    break

                # Lecture de la valeur de reference
                if fct_prol[0] == 'FONCT_C':
                    typeFct = 'fonction_c'
                else:
                    if fct_prol[0] == 'NAPPE':
                        typeFct = 'nappe'
                    else:
                        typeFct = 'fonction'

                # Calcul de la fonction
                res = 0
                if type(lafonc) in (fonction_sdaster, fonction_c):
                    res = lafonc(valpu[0])
                else:
                    # Remise dans l'ordre des param
                    paramNappe = [fct_prol[2], fct_prol[6]]
                    vParamOrdo = ['', '']
                    for iPN in range(len(paramNappe)):
                        i = 0
                        for iPU in range(len(nompara)):
                            if paramNappe[iPN] == nompara[iPU]:
                                if vParamOrdo[iPN] != '':
                                    ier = 120
                                    UTMESS(
                                        'A+', 'FONCT0_9', valk=(lafonc.get_name()))
                                    UTMESS('A', 'FONCT0_15', valk=nompara)
                                else:
                                    vParamOrdo[iPN] = valpu[iPU]
                            i = i + 1
                        if vParamOrdo[iPN] == '':
                            ier = 130
                            UTMESS('A+', 'FONCT0_9', valk=(lafonc.get_name()))
                            UTMESS('A', 'FONCT0_16', valk=paramNappe)
                            UTMESS('A', 'FONCT0_17', valk=nompara)
                    res = lafonc(vParamOrdo[0], vParamOrdo[1])
            else:
                ier = 150

            # Construction de l'affiche du resultat
            current = {}

            nomLastPara = nompara[len(nompara) - 1]
            valLastPara = valpu[len(valpu) - 1]
            if (typeFct == 'nappe'):

                # ligne 1
                nb_espace = 16 - len(str(nompu))
                espace = nb_espace * ' '
                current['nom_para_0'] = str(nompu) + espace
                nb_espace = 16 - len(str(nomLastPara))
                espace = nb_espace * ' '
                current['nom_para'] = str(nomLastPara) + espace
                txt.append(ligne_nap_1 % current)

                # ligne 2
                current = {}
                nb_espace = 16 - len(nomfct)
                espace = nb_espace * ' '
                current['nom_nap'] = nomfct + espace
                nb_espace = 16 - len(str(valpu[0]))
                espace = nb_espace * ' '
                current['val_para_0'] = str(valpu[0]) + espace
                nb_espace = 16 - len(str(valLastPara))
                espace = nb_espace * ' '
                current['val_para'] = str(valLastPara) + espace
                txt.append(ligne_nap_2 % current)

            else:

                # ligne 1
                nb_espace = 16 - len(str(nomLastPara))
                espace = nb_espace * ' '
                current['nom_para'] = str(nomLastPara) + espace
                if(len(titre) > 1):
                    txt.append(ligne_fct_11 % current)
                else:
                    txt.append(ligne_fct_1 % current)

                # ligne 2
                current = {}
                nb_espace = 16 - len(nomfct)
                espace = nb_espace * ' '
                current['nom_fct'] = nomfct + espace
                nb_espace = 16 - len(str(valLastPara))
                espace = nb_espace * ' '
                current['val_para'] = str(valLastPara) + espace
                if(len(titre) > 1):
                    nb_espace = 33 - len(titre)
                    espace = nb_espace * ' '
                    current['titre'] = titre
                    txt.append(ligne_fct_22 % current)
                else:
                    txt.append(ligne_fct_2 % current)

            # Test des valeurs calculees
            curDict = TesterValeur(
                nomLastPara, valLastPara, valref, res, epsi, crit, ssigne)
            if other_ref:
                DictRef = TesterValeur(
                    nomLastPara, valLastPara, valoth, res, epsoth, crit, ssigne)

            if TEST_NOOK == 'OUI':
                if ier == 0:
                    testOk = curDict['testOk']
                    if testOk == ' OK ':
                        txt.append(
                            'NOOK PAS DE CHANCE LE TEST EST CORRECT !!!')
                    else:
                        AfficherResultat(
                            curDict, nomLastPara, 'NON_REGRESSION', legende,
                            crit, res, valLastPara, txt)
                        if other_ref:
                            AfficherResultat(
                                DictRef, nomLastPara, ref, legende,
                                crit, res, valLastPara, txt, label=False)
                elif ier == 120:
                    txt.append(' OK  PARAMETRE EN DOUBLE')
                elif ier == 130:
                    txt.append(' OK  PARAMETRE NON CORRECT')
                elif ier == 150:
                    txt.append(' OK  TYPE DE FONCTION NON TRAITE')
                elif ier == 160:
                    txt.append(' OK  PAS ASSEZ DE PARAMETRES')
            else:
                if ier != 0:
                    txt.append('NOOK PB INTERPOLATION. VOIR MESSAGE CI-DESSUS')
                else:
                    AfficherResultat(
                        curDict, nomLastPara, 'NON_REGRESSION', legende,
                        crit, res, valLastPara, txt)
                    if other_ref:
                        AfficherResultat(DictRef, nomLastPara, ref, legende,
                                         crit, res, valLastPara, txt, label=False)
            txt.append(' ')

    if ATTRIBUT != None:
        first_affiche_ligne1 = True
        resu_test_attr = ' OK '
        # Boucle sur le mot-cle ATTRIBUT
        for attr in ATTRIBUT:
            dres = attr.cree_dict_valeurs(attr.mc_liste)
            # Lecture des mots-cles simples
            ref = dres['REFERENCE']
            ver = None
            fonction = dres['FONCTION']
            fctProl = fonction.sdj.PROL.get_stripped()
            typeFct = fctProl[0]
            para = dres['PARA']
            fctPara = fonction.sdj.PARA.get()

            pos = 0
            # Cas particulier d'une nappe qui a 2 dimensions
            if typeFct == 'NAPPE':
                if para != None:
                    # Recherche de la fonction liee a para
                    precPara = dres['PREC_PARA']
                    critPara = dres['CRIT_PARA']
                    LOK = 0
                    compteur = 0
                    for curPara in fctPara:
                        if critPara[0:4] == 'RELA':
                            LOK = (
                                abs(para - curPara) <= precPara * abs(curPara))
                        else:
                            LOK = (abs(para - curPara) <= precPara)
                        if LOK:
                            pos = compteur
                            break
                        compteur = compteur + 1
                    if not LOK:
                        UTMESS('A', 'PREPOST3_95')
                else:
                    para = fctPara[0]

            # Lecture des parametres de reference
            nomAttr = dres['ATTR']
            valAttrRef = dres['ATTR_REFE']

            # Recherche de la valeur de l'attribut dans le .PROL
            nompu = ''
            testOk = 'NOOK'
            if nomAttr == 'INTERPOL_FONC':
                nompu = fctProl[7 + 2 * (pos)] + ' '
            elif nomAttr == 'INTERPOL':
                nompu = fctProl[1] + ' '
            elif nomAttr == 'NOM_PARA_FONC':
                nompu = fctProl[6]
            elif nomAttr == 'NOM_PARA':
                nompu = fctProl[2]
            elif nomAttr == 'NOM_RESU':
                nompu = fctProl[3]
            elif nomAttr == 'PROL_GAUCHE_FONC':
                prolFonc = fctProl[7 + 2 * (pos) + 1]
                nompu = prolFonc[0:1]
                if nompu == 'E':
                    nompu = 'EXCLU'
                elif nompu == 'C':
                    nompu = 'CONSTANT'
                elif nompu == 'L':
                    nompu = 'LINEAIRE'
            elif nomAttr == 'PROL_DROITE_FONC':
                prolFonc = fctProl[7 + 2 * (pos) + 1]
                nompu = prolFonc[1:2]
                if nompu == 'E':
                    nompu = 'EXCLU'
                elif nompu == 'C':
                    nompu = 'CONSTANT'
                elif nompu == 'L':
                    nompu = 'LINEAIRE'
            elif nomAttr == 'PROL_GAUCHE':
                prolFonc = fctProl[4]
                nompu = prolFonc[0:1]
                if nompu == 'E':
                    nompu = 'EXCLU'
                elif nompu == 'C':
                    nompu = 'CONSTANT'
                elif nompu == 'L':
                    nompu = 'LINEAIRE'
            elif nomAttr == 'PROL_DROITE':
                prolFonc = fctProl[4]
                nompu = prolFonc[1:2]
                if nompu == 'E':
                    nompu = 'EXCLU'
                elif nompu == 'C':
                    nompu = 'CONSTANT'
                elif nompu == 'L':
                    nompu = 'LINEAIRE'

            # Test de la valeur
            if (nompu == valAttrRef):
                testOk = ' OK '
            if TEST_NOOK == 'OUI':
                if testOk == ' OK ':
                    testOk = 'NOOK'
                else:
                    testOk = ' OK '
            if testOk == 'NOOK':
                resu_test_attr = 'NOOK'

            # Construction de l'affichage
            nomFct = fonction.get_name()

            # ligne 1 (affichée qu'à la première occurrence)
            current = {}
            if first_affiche_ligne1:
                first_affiche_ligne1 = False
                if typeFct == 'NAPPE':
                    nb_espace = 16 - len('NAPPE')
                    espace = nb_espace * ' '
                    current['nom'] = 'NAPPE' + espace
                else:
                    nb_espace = 16 - len('FONCTION')
                    espace = nb_espace * ' '
                    current['nom'] = 'FONCTION' + espace

                nb_espace = 16 - len(list_attr[0])
                espace = nb_espace * ' '
                current['nom_attr'] = list_attr[0] + espace
                if typeFct == 'NAPPE':
                    nb_espace = 16 - len(list_attr[1])
                    espace = nb_espace * ' '
                    current['nom_para'] = list_attr[1] + espace
                nb_espace = 16 - len(list_attr[2])
                espace = nb_espace * ' '
                current['vale'] = list_attr[2] + espace
                if typeFct == 'NAPPE':
                    txt.append(ligne_att_1 % current)
                else:
                    txt.append(ligne_att_2 % current)

            # ligne 2
            current = {}
            nb_espace = 16 - len(nomFct)
            espace = nb_espace * ' '
            current['nom'] = nomFct + espace
            nb_espace = 16 - len(nomAttr)
            espace = nb_espace * ' '
            current['nom_attr'] = nomAttr + espace
            if typeFct == 'NAPPE':
                nb_espace = 16 - len(str(para))
                espace = nb_espace * ' '
                current['nom_para'] = str(para) + espace
            nb_espace = 16 - len(nompu)
            espace = nb_espace * ' '
            current['vale'] = nompu + espace
            if typeFct == 'NAPPE':
                txt.append(ligne_att_11 % current)
            else:
                txt.append(ligne_att_22 % current)
        current = {}
        current['testOk'] = resu_test_attr
        txt.append(ligne_att_3 % current)

    # On affiche txt dans le fichier RESULTAT
    aster.affiche('RESULTAT', os.linesep.join(txt))

    return ier
