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

# --------------------------------------------------------------
# --------------------------------------------------------------


def SomListStr(x):
    """
    """
    str_tmp = ""
    cpt = 0
    while cpt < len(x):
        assert type(x[cpt]) is str
        str_tmp += "'" + x[cpt] + "', "
        cpt += 1
    return str_tmp[:-2]
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def ListR_2_Str(x):
    """
    """
    char = ""
    for i in xrange(len(x)):
        assert type(x[i]) is float
#    if str("%E"%(x[i]))[0] == "-" : deb = " "
        if str(x[i])[0] == "-":
            deb = " "
        else:
            deb = "  "
        if i == len(x) - 1:
            fin = ""
        else:
            fin = "\n"
#    char += deb + str("%E"%(x[i])) + fin
        char += deb + str(x[i]) + fin
    return char
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def int_2_str(i, lon):
    """
    """
    assert type(i) is int and type(lon) is int
    return "0" * (len(str(lon)) - len(str(i))) + str(i)
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def affiche_infos_essai(str_n_essai, type_essai, val_PRES_CONF, val2, val3=0.):
    """
    """
    import aster

    mesg1 = " ESSAI_" + type_essai + " numero " + str_n_essai + " :"

    if type_essai == "TD":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > EPSI_IMPOSE = "
    elif type_essai == "TND":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > EPSI_IMPOSE = "
    elif type_essai == "CISA_C":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > EPSI_IMPOSE = "
    elif type_essai == "TND_C":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > SIGM_IMPOSE = "
    elif type_essai == "TD_A":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > EPSI_IMPOSE = "
    elif type_essai == "TD_NA":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > EPSI_IMPOSE = "
    elif type_essai == "OEDO_C":
        mesg2 = "  > PRES_CONF = "
        mesg3 = "  > SIGM_IMPOSE = "
        mesg4 = "  > SIGM_DECH = "
    elif type_essai == "ISOT_C":
        mesg2 = "  > PRES_CONF   = "
        mesg3 = "  > SIGM_IMPOSE = "
        mesg4 = "  > SIGM_DECH = "
    # Pour nouvel essai
    # elif type_essai == "XXX":
        # mesg2 = "  > PRES_CONF = "
        # mesg3 = "  > XXX       = "
    else:
        assert False

    mesg2 += str("%E" % (val_PRES_CONF)) + " "
    if val2 < 0.:
        mesg3 += str("%E" % (val2)) + " "
    else:
        mesg3 += " " + str("%E" % (val2)) + " "
    if val3 == 0.:
        mesg4 = ""
    else:
        mesg4 += str("%E" % (val3)) + " "

    lonmax = max(len(mesg2), len(mesg3), len(mesg4))
    lonmax = max(len(mesg1), lonmax)

    separ1 = "\n  "
    separ2 = "-" * (lonmax + 2)
    separ3 = "|"

    mesg = separ1
    mesg += separ2 + separ1
    mesg += separ3 + mesg1 + " " * (lonmax - len(mesg1)) + separ3 + separ1
    mesg += separ2 + separ1
    mesg += separ3 + mesg2 + " " * (lonmax - len(mesg2)) + separ3 + separ1
    mesg += separ3 + mesg3 + " " * (lonmax - len(mesg3)) + separ3 + separ1
    if len(mesg4) != 0:
        mesg += separ3 + mesg4 + " " * (lonmax - len(mesg4)) + separ3 + separ1
    mesg += separ2

    aster.affiche('MESSAGE', mesg)
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def verif_essais(COMPORTEMENT, ESSAI_TD,
                 ESSAI_TND,
                 ESSAI_CISA_C,
                 ESSAI_TND_C,
                 ESSAI_TD_A,
                 ESSAI_TD_NA,
                 ESSAI_OEDO_C,
                 ESSAI_ISOT_C,):
                           # ESSAI_XXX,):
    """
    effectuer les verifications de certaines donnees en entree de la macro
    qui ne peuvent etre faites dans le catalogue
    """
    from Utilitai.Utmess import UTMESS

    List_essais = []

    # --------------------------------------------------------------
    # Verifications specifiques a chaque type d'essai
    # --------------------------------------------------------------

    # ---
    # Essai "TD"
    # ---
    if ESSAI_TD != None:

        typ_essai = "ESSAI_TD"
        List_essais += ESSAI_TD.List_F()

        # Lois de comportement autorisees. (lois de sol elasto-plastiques non
        # visco)
        RdC_OK = ['HUJEUX',
                  'DRUCK_PRAGER',
                  'DRUCK_PRAG_N_A',
                  'CAM_CLAY',
                  'CJS',
                  'MOHR_COULOMB']
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc in RdC_OK):
            UTMESS('F', 'COMPOR2_39', valk=(
                typ_essai, SomListStr(RdC_OK), nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_TD.List_F()):

            # Le "bon" nbre d'elts a-t-il ete renseigne pr les MotCles simples
            # -> PRES_CONF, EPSI_IMPOSE, TABLE_RESU ?
            char = "<PRES_CONF>, <EPSI_IMPOSE>"
            test = len(DicoEssai['PRES_CONF']) == len(DicoEssai['EPSI_IMPOSE'])
            if DicoEssai.has_key('TABLE_RESU'):
                char += ", <TABLE_RESU>"
                test = test and len(DicoEssai['PRES_CONF']) == len(
                    DicoEssai['TABLE_RESU'])
            if not test:
                UTMESS('F', 'COMPOR2_31', valk=(
                    typ_essai, char), vali=(iocc + 1))

            # on s'assure que tous les PRES_CONF et EPSI_IMPOSE sont bien < 0.
            for i in xrange(len(DicoEssai['PRES_CONF'])):
                if DicoEssai['PRES_CONF'][i] >= 0.:
                    UTMESS(
                        'F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF",), vali=(iocc + 1),
                        valr=(DicoEssai['PRES_CONF'][i]))
                if DicoEssai['EPSI_IMPOSE'][i] >= 0.:
                    UTMESS(
                        'F', 'COMPOR2_32', valk=(typ_essai, "EPSI_IMPOSE"), vali=(iocc + 1),
                        valr=(DicoEssai['EPSI_IMPOSE'][i]))
            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

    # ---
    # Essai "TND"
    # ---
    if ESSAI_TND != None:

        typ_essai = "ESSAI_TND"
        List_essais += ESSAI_TND.List_F()

        # Lois de comportement autorisees. (lois de sol elasto-plastiques non
        # visco)
        RdC_OK = ['HUJEUX',
                  'DRUCK_PRAGER',
                  'DRUCK_PRAG_N_A',
                  'CAM_CLAY',
                  'CJS',
                  'MOHR_COULOMB']
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc in RdC_OK):
            UTMESS('F', 'COMPOR2_39', valk=(
                typ_essai, SomListStr(RdC_OK), nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_TND.List_F()):

            # Le "bon" nbre d'elts a-t-il ete renseigne pr les MotCles simples
            # -> PRES_CONF, EPSI_IMPOSE, TABLE_RESU ?
            char = "<PRES_CONF>, <EPSI_IMPOSE>"
            test = len(DicoEssai['PRES_CONF']) == len(DicoEssai['EPSI_IMPOSE'])
            if DicoEssai.has_key('TABLE_RESU'):
                char += ", <TABLE_RESU>"
                test = test and len(DicoEssai['PRES_CONF']) == len(
                    DicoEssai['TABLE_RESU'])
            if not test:
                UTMESS('F', 'COMPOR2_31', valk=(
                    typ_essai, char), vali=(iocc + 1))

            # on s'assure que tous les PRES_CONF et EPSI_IMPOSE sont bien < 0.
            for i in xrange(len(DicoEssai['PRES_CONF'])):
                if DicoEssai['PRES_CONF'][i] >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(DicoEssai['PRES_CONF'][i]), vali=(iocc + 1))
                if DicoEssai['EPSI_IMPOSE'][i] >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "EPSI_IMPOSE"),
                           valr=(DicoEssai['EPSI_IMPOSE'][i]), vali=(iocc + 1))
            # on s'assure que 0. < BIOT_COEF <= 1
            biot = DicoEssai['BIOT_COEF']
            if biot <= 0. or biot > 1.:
                UTMESS('F', 'COMPOR2_33', valk=(typ_essai),
                       valr=(biot), vali=(iocc + 1))
            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

    # ---
    # Essai "CISA_C"
    # ---
    if ESSAI_CISA_C != None:

        typ_essai = "ESSAI_CISA_C"
        List_essais += ESSAI_CISA_C.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_CISA_C.List_F()):

            # coherence du nbre de TABLE_RESU avec le nbre de PRES_CONF
            if DicoEssai.has_key('TABLE_RESU'):
                n1 = len(DicoEssai['PRES_CONF'])
                n2 = len(DicoEssai['TABLE_RESU'])
                if not n2 == n1 + 1:
                    UTMESS('F', 'COMPOR2_35', valk=(
                        typ_essai), vali=(iocc + 1, n1, n1 + 1, n2))

            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

            # on s'assure que tous les PRES_CONF sont bien < 0.
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))
            # on s'assure que tous les GAMMA_IMPOSE,GAMMA_ELAS sont bien > 0.
            for epsimpo in DicoEssai['GAMMA_IMPOSE']:
                if epsimpo <= 0.:
                    UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "GAMMA_IMPOSE"),
                           valr=(epsimpo), vali=(iocc + 1))
            if DicoEssai['GAMMA_ELAS'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "GAMMA_ELAS"),
                       valr=(DicoEssai['GAMMA_ELAS']), vali=(iocc + 1))

            # on s'assure que la liste des GAMMA_IMPOSE est croissante
            clef = 'GAMMA_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort()
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "croissante"),
                    vali=(iocc + 1))

    # ---
    # Essai "TND_C"
    # ---
    if ESSAI_TND_C != None:

        typ_essai = "ESSAI_TND_C"
        List_essais += ESSAI_TND_C.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_TND_C.List_F()):

            # coherence du nbre de TABLE_RESU avec le nbre de PRES_CONF
            if DicoEssai.has_key('TABLE_RESU'):
                n1 = len(DicoEssai['PRES_CONF'])
                n2 = len(DicoEssai['TABLE_RESU'])
                if not n2 == n1 + 1:
                    UTMESS('F', 'COMPOR2_35', valk=(
                        typ_essai), vali=(iocc + 1, n1, n1 + 1, n2))

            # on s'assure que SIGM_IMPOSE > 0.
            for sigimpo in DicoEssai['SIGM_IMPOSE']:
                if sigimpo <= 0.:
                    UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "SIGM_IMPOSE"),
                           valr=(sigimpo), vali=(iocc + 1))
            # on s'assure que PRES_CONF < 0. et PRES_CONF+SIGM_IMPOSE < 0.
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))
                for sigimpo in DicoEssai['SIGM_IMPOSE']:
                    if pconf + sigimpo >= 0.:
                        UTMESS('F', 'COMPOR2_37', valk=(typ_essai),
                               vali=(iocc + 1), valr=(pconf, sigimpo, pconf + sigimpo))
            # on s'assure que KZERO et UN_SUR_K sont > 0. et 0. < BIOT_COEF <=
            # 1
            if DicoEssai['UN_SUR_K'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "UN_SUR_K"),
                       valr=(DicoEssai['UN_SUR_K']), vali=(iocc + 1))
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))
            biot = DicoEssai['BIOT_COEF']
            if biot <= 0. or biot > 1.:
                UTMESS('F', 'COMPOR2_33', valk=(typ_essai),
                       valr=(biot), vali=(iocc + 1))

            # on s'assure que 0. < RU_MAX <= 1
            if DicoEssai['RU_MAX'] <= 0. or DicoEssai['RU_MAX'] > 1.:
                UTMESS('F', 'COMPOR2_33', valk=(typ_essai, "RU_MAX"),
                       valr=(DicoEssai['RU_MAX']), vali=(iocc + 1))

            # on s'assure que la liste des SIGM_IMPOSE est croissante
            clef = 'SIGM_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort()
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "croissante"),
                    vali=(iocc + 1))

    # ---
    # Essai "TD_A"
    # ---
    if ESSAI_TD_A != None:

        typ_essai = "ESSAI_TD_A"
        List_essais += ESSAI_TD_A.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_TD_A.List_F()):

            # coherence du nbre de TABLE_RESU avec le nbre de PRES_CONF
            if DicoEssai.has_key('TABLE_RESU'):
                n1 = len(DicoEssai['PRES_CONF'])
                n2 = len(DicoEssai['TABLE_RESU'])
                if not n2 == n1 + 1:
                    UTMESS('F', 'COMPOR2_35', valk=(
                        typ_essai), vali=(iocc + 1, n1, n1 + 1, n2))

            # on s'assure que EPSI_IMPOSE,EPSI_ELAS sont > 0.
            for epsimpo in DicoEssai['EPSI_IMPOSE']:
                if epsimpo <= 0.:
                    UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "EPSI_IMPOSE"),
                           valr=(epsimpo), vali=(iocc + 1))
            if DicoEssai['EPSI_ELAS'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "EPSI_ELAS"),
                       valr=(DicoEssai['EPSI_ELAS']), vali=(iocc + 1))

            # on s'assure que PRES_CONF < 0.
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))

            # on s'assure que la liste des EPSI_IMPOSE est croissante
            clef = 'EPSI_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort()
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "croissante"),
                    vali=(iocc + 1))

            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))
    # ---
    # Essai "TD_NA"
    # ---
    if ESSAI_TD_NA != None:

        typ_essai = "ESSAI_TD_NA"
        List_essais += ESSAI_TD_NA.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_TD_NA.List_F()):

            # coherence du nbre de TABLE_RESU avec le nbre de PRES_CONF
            if DicoEssai.has_key('TABLE_RESU'):
                n1 = len(DicoEssai['PRES_CONF'])
                n2 = len(DicoEssai['TABLE_RESU'])
                if not n2 == n1 + 1:
                    UTMESS('F', 'COMPOR2_35', valk=(
                        typ_essai), vali=(iocc + 1, n1, n1 + 1, n2))

            # on s'assure que EPSI_IMPOSE < 0. et que EPSI_ELAS est > 0
            for epsimpo in DicoEssai['EPSI_IMPOSE']:
                if epsimpo >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "EPSI_IMPOSE"),
                           valr=(epsimpo), vali=(iocc + 1))
            if DicoEssai['EPSI_ELAS'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "EPSI_ELAS"),
                       valr=(DicoEssai['EPSI_ELAS']), vali=(iocc + 1))

            # on s'assure que PRES_CONF < 0.
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))

            # on s'assure que la liste des EPSI_IMPOSE est décroissante
            clef = 'EPSI_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort(reverse=True)
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "décroissante"),
                    vali=(iocc + 1))

            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

    # ---
    # Essai "OEDO_C"
    # ---
    if ESSAI_OEDO_C != None:

        typ_essai = "ESSAI_OEDO_C"
        List_essais += ESSAI_OEDO_C.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_OEDO_C.List_F()):

            # Le "bon" nbre d'elts a-t-il ete renseigne pr les MotCles simples
            # -> PRES_CONF, SIGM_DECH, TABLE_RESU ?
            char = "<PRES_CONF>, <SIGM_DECH>"
            test = len(DicoEssai['PRES_CONF']) == len(DicoEssai['SIGM_DECH'])
            if DicoEssai.has_key('TABLE_RESU'):
                char += ", <TABLE_RESU>"
                test = test and len(DicoEssai['PRES_CONF']) == len(
                    DicoEssai['TABLE_RESU'])
            if not test:
                UTMESS('F', 'COMPOR2_31', valk=(
                    typ_essai, char), vali=(iocc + 1))

            # on s'assure que SIGM_IMPOSE < 0.
            for sigimpo in DicoEssai['SIGM_IMPOSE']:
                if sigimpo >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "SIGM_IMPOSE"),
                           valr=(sigimpo), vali=(iocc + 1))

            # on s'assure que tous les PRES_CONF sont bien < 0. et que
            # PRES_CONF+SIGM_IMPOSE < =SIGM_DECH
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))

                sdech = DicoEssai['SIGM_DECH'][
                    DicoEssai['PRES_CONF'].index(pconf)]
                for sigimpo in DicoEssai['SIGM_IMPOSE']:
                    if pconf + sigimpo > sdech:
                        UTMESS('F', 'COMPOR2_47', valk=(typ_essai),
                               vali=(iocc + 1), valr=(pconf, sigimpo, pconf + sigimpo, sdech))

            # on s'assure que SIGM_DECH <= PRES_CONF
            for pconf in DicoEssai['PRES_CONF']:
                sdech = DicoEssai['SIGM_DECH'][
                    DicoEssai['PRES_CONF'].index(pconf)]
                if sdech > pconf:
                    UTMESS('F', 'COMPOR2_48', valk=(typ_essai),
                           vali=(iocc + 1), valr=(sdech, pconf))

            # on s'assure que la liste des SIGM_IMPOSE est décroissante
            clef = 'SIGM_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort(reverse=True)
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "décroissante"),
                    vali=(iocc + 1))

            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

    # ---
    # Essai "ISOT_C"
    # ---
    if ESSAI_ISOT_C != None:

        typ_essai = "ESSAI_ISOT_C"
        List_essais += ESSAI_ISOT_C.List_F()

        # Lois de comportement autorisees. (HUJEUX)
        nom_rdc = COMPORTEMENT.List_F()[0]['RELATION']
        if not(nom_rdc == 'HUJEUX'):
            UTMESS('F', 'COMPOR2_39', valk=(typ_essai, 'HUJEUX', nom_rdc))

        for iocc, DicoEssai in enumerate(ESSAI_ISOT_C.List_F()):

            # Le "bon" nbre d'elts a-t-il ete renseigne pr les MotCles simples
            # -> PRES_CONF, SIGM_DECH, TABLE_RESU ?
            char = "<PRES_CONF>, <SIGM_DECH>"
            test = len(DicoEssai['PRES_CONF']) == len(DicoEssai['SIGM_DECH'])
            if DicoEssai.has_key('TABLE_RESU'):
                char += ", <TABLE_RESU>"
                test = test and len(DicoEssai['PRES_CONF']) == len(
                    DicoEssai['TABLE_RESU'])
            if not test:
                UTMESS('F', 'COMPOR2_31', valk=(
                    typ_essai, char), vali=(iocc + 1))

            # on s'assure que SIGM_IMPOSE < 0.
            for sigimpo in DicoEssai['SIGM_IMPOSE']:
                if sigimpo >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "SIGM_IMPOSE"),
                           valr=(sigimpo), vali=(iocc + 1))

            # on s'assure que tous les PRES_CONF sont bien < 0. et que
            # PRES_CONF+SIGM_IMPOSE <= SIGM_DECH
            for pconf in DicoEssai['PRES_CONF']:
                if pconf >= 0.:
                    UTMESS('F', 'COMPOR2_32', valk=(typ_essai, "PRES_CONF"),
                           valr=(pconf), vali=(iocc + 1))

                sdech = DicoEssai['SIGM_DECH'][
                    DicoEssai['PRES_CONF'].index(pconf)]
                for sigimpo in DicoEssai['SIGM_IMPOSE']:
                    if pconf + sigimpo > sdech:
                        UTMESS('F', 'COMPOR2_47', valk=(typ_essai),
                               vali=(iocc + 1), valr=(pconf, sigimpo, pconf + sigimpo, sdech))

            # on s'assure que SIGM_DECH <= PRES_CONF
            for pconf in DicoEssai['PRES_CONF']:
                sdech = DicoEssai['SIGM_DECH'][
                    DicoEssai['PRES_CONF'].index(pconf)]
                if sdech > pconf:
                    UTMESS('F', 'COMPOR2_48', valk=(typ_essai),
                           vali=(iocc + 1), valr=(sdech, pconf))

            # on s'assure que la liste des SIGM_IMPOSE est décroissante
            clef = 'SIGM_IMPOSE'
            list_tmp = list(DicoEssai[clef])
            list_tmp.sort(reverse=True)
            if not(DicoEssai[clef] == tuple(list_tmp)):
                UTMESS(
                    'F', 'COMPOR2_38', valk=(typ_essai, clef, ListR_2_Str(DicoEssai[clef]), "décroissante"),
                    vali=(iocc + 1))

            # on s'assure que KZERO est > 0
            if DicoEssai['KZERO'] <= 0.:
                UTMESS('F', 'COMPOR2_34', valk=(typ_essai, "KZERO"),
                       valr=(DicoEssai['KZERO']), vali=(iocc + 1))

    # ---
    # Essai "XXX"
    # ---
    # if ESSAI_XXX != None :
    #
    #  typ_essai    = "ESSAI_XXX"
    #  List_essais += ESSAI_XXX.List_F() ...
    # --------------------------------------------------------------
    # Verification coherence des MCS GRAPHIQUE/TABLE_REF (tout type d'essai)
    # --------------------------------------------------------------
    for DicoEssai in List_essais:
        if DicoEssai.has_key('TABLE_REF'):
            for table_tmp in DicoEssai['TABLE_REF']:

                # on s'assure chaque TABLE_REF est bien construite...
                table_ref = table_tmp.EXTR_TABLE().values()
                nom_tbref = table_tmp.nom

                list_paras = ['TYPE', 'LEGENDE', 'ABSCISSE', 'ORDONNEE']
                if not set(table_ref.keys()) == set(list_paras):
                    UTMESS('F', 'COMPOR2_44', valk=(
                        nom_tbref, SomListStr(table_ref.keys())))
                typc = table_ref['TYPE']
                logi_typc = [x == None for x in typc]
                logi_typc[0] = type(typc[0]) is str
                if not logi_typc == [True] * len(typc):
                    UTMESS('F', 'COMPOR2_45', valk=(nom_tbref, 'TYPE'))
                lege = table_ref['LEGENDE']
                logi_lege = [x == None for x in lege]
                logi_lege[0] = type(lege[0]) is str
                if not logi_lege == [True] * len(lege):
                    UTMESS('F', 'COMPOR2_45', valk=(nom_tbref, 'LEGENDE'))
                absc = table_ref['ABSCISSE']
                ordo = table_ref['ORDONNEE']
                logi_absc = [type(x) is float for x in absc]
                logi_ordo = [type(x) is float for x in ordo]
                test = len(absc) == len(ordo)
                test = test and logi_absc == [True] * len(absc)
                test = test and logi_ordo == [True] * len(absc)
                if not test:
                    UTMESS('F', 'COMPOR2_46', valk=(nom_tbref))

                # on s'assure que le TYPE indique dans chaque TABLE_REF figure bien dans
                # la liste des GRAPHIQUE demandes en sortie pour l'esssai
                # courant
                if not typc[0].replace(" ", "") in DicoEssai['GRAPHIQUE']:
                    UTMESS('F', 'COMPOR2_43', valk=(nom_tbref, typc[0],
                                                    SomListStr(DicoEssai['GRAPHIQUE'])))
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def affiche_alarm_TND_C(str_n_essai, pres, dsig, codret, NB_CYCLE, ncycrit, ncyerro):
    """
    """
    from Utilitai.Utmess import UTMESS

    assert codret in ['0', '1', '2', '3']
    charg1 = str("%E" % (pres))
    charg2 = str("%E" % (dsig))

    nom_essai = "ESSAI_TND_C"

    kval_char1 = "  > PRES_CONF   = " + charg1 + "\n"
    kval_char2 = "  > SIGM_IMPOSE = " + charg2 + "\n"
    kval_cycl = "  > NB_CYCLE    = " + str(NB_CYCLE) + "\n"

    # codret '0' : CALC_POINT_MAT va jusqu'au bout et critere atteint -> pas d'alarme
    # codret '1' : CALC_POINT_MAT va jusqu'au bout et critere non atteint
    # codret '2' : CALC_POINT_MAT s'arrete en NonConvergenceError et critere atteint
    # codret '3' : CALC_POINT_MAT s'arrete en NonConvergenceError et critere
    # non atteint

    Lk = [nom_essai, str_n_essai, kval_char1, kval_char2, kval_cycl]

    if codret == '1':
        UTMESS('A', 'COMPOR2_40', valk=Lk, vali=NB_CYCLE)
    elif codret == '2':
        UTMESS('A', 'COMPOR2_41', valk=Lk, vali=(ncyerro, ncycrit))
    elif codret == '3':
        UTMESS('A', 'COMPOR2_42', valk=Lk, vali=ncyerro)
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def preparer_graphique(niveau, DicoEssai, str_fich, Courbes, NomsFich, Leg_x, Leg_y, Ech_x, Ech_y):
    """
    XXX
    """

    ext = ".dat"

    # "niveau1": courbes "recapitulatives" pour les essais cycliques
    if niveau == '1':
        for TypGraph in DicoEssai['GRAPHIQUE']:
            if TypGraph == "GAMMA-G":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "GAMMA"
                Leg_y[TypGraph] = "G/Gmax"
                Ech_x[TypGraph] = "LOG"
                Ech_y[TypGraph] = "LIN"
            if TypGraph == "GAMMA-D":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "GAMMA"
                Leg_y[TypGraph] = "D"
                Ech_x[TypGraph] = "LOG"
                Ech_y[TypGraph] = "LIN"
            if TypGraph == "EPSI-E":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "EPSI"
                Leg_y[TypGraph] = "E/Emax"
                Ech_x[TypGraph] = "LOG"
                Ech_y[TypGraph] = "LIN"
            if TypGraph == "G-D":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "G/Gmax"
                Leg_y[TypGraph] = "D"
                Ech_x[TypGraph] = "LIN"
                Ech_y[TypGraph] = "LIN"
            if TypGraph == "NCYCL-DSIGM":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "NCYCL"
                Leg_y[TypGraph] = "DSIGM"
            if TypGraph == "P-EPS_VOL":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "P"
                Leg_y[TypGraph] = "EPS_VOL"
            if TypGraph == "SIG_AXI-EPS_VOL":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "SIG_AXI"
                Leg_y[TypGraph] = "EPS_VOL"
            # if TypGraph == "XXX" :
                # Courbes[TypGraph]  = []
                # NomsFich[TypGraph] = str_fich+TypGraph+ext
                # Leg_x[TypGraph]    = "XXX"
                # Leg_y[TypGraph]    = "XXX"

    # "niveau2": resultats bruts pour chaque valeurs chargement
    elif niveau == '2':
        for TypGraph in DicoEssai['GRAPHIQUE']:
            if TypGraph == "P-Q":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "P"
                Leg_y[TypGraph] = "Q"
            if TypGraph == "EPS_AXI-Q":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "EPS_AXI"
                Leg_y[TypGraph] = "Q"
            if TypGraph == "EPS_VOL-Q":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "EPS_VOL"
                Leg_y[TypGraph] = "Q"
            if TypGraph == "EPS_AXI-EPS_VOL":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "EPS_AXI"
                Leg_y[TypGraph] = "EPS_VOL"
            if TypGraph == "EPS_AXI-PRE_EAU":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "EPS_AXI"
                Leg_y[TypGraph] = "PRE_EAU"
            if TypGraph == "SIG_AXI-PRE_EAU":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "SIG_AXI"
                Leg_y[TypGraph] = "PRE_EAU"
            if TypGraph == "GAMMA-SIGXY":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "GAMMA"
                Leg_y[TypGraph] = "SIGXY"
            if TypGraph == "P-EPS_VOL":
                Courbes[TypGraph] = []
                NomsFich[TypGraph] = str_fich + TypGraph + ext
                Leg_x[TypGraph] = "P"
                Leg_y[TypGraph] = "EPS_VOL"
            # if TypGraph == "XXX" :
                # Courbes[TypGraph]  = []
                # NomsFich[TypGraph] = str_fich+TypGraph+ext
                # Leg_x[TypGraph]    = "XXX"
                # Leg_y[TypGraph]    = "XXX"

    else:
        assert False
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def remplir_graphique(DicoEssai, Courbes, Resu_x, Resu_y, str_leg, TypGraph_in):
    """
    XXX
    """
    # sortie si pr une qqcque raison les listes de resultats sont vides
    if Resu_x == [] or Resu_y == []:
        return

    # types de graphiques disponibles en sortie pour :
    #   -> essai "TD"
    TD_graph = ["P-Q", "EPS_AXI-Q", "EPS_AXI-EPS_VOL", "P-EPS_VOL"]
    #   -> essai "TND"
    TND_graph = ["P-Q", "EPS_AXI-Q", "EPS_AXI-PRE_EAU"]
    #   -> essai "CISA_C"
    CISA_C_graph = ["GAMMA-SIGXY", "GAMMA-G", "GAMMA-D", "G-D"]
    #   -> essai "TND_C"
    TND_C_graph = ["P-Q", "NCYCL-DSIGM",
                   "SIG_AXI-PRE_EAU", "EPS_AXI-PRE_EAU", "EPS_AXI-Q"]
    #   -> essai "TD_A"
    TD_A_graph = ["P-Q", "EPS_AXI-Q", "EPS_VOL-Q",
                  "EPS_AXI-EPS_VOL", "P-EPS_VOL", "EPSI-E"]
    #   -> essai "TD_NA"
    TD_NA_graph = ["P-Q", "EPS_AXI-Q", "EPS_VOL-Q",
                   "EPS_AXI-EPS_VOL", "P-EPS_VOL", "EPSI-E"]
    #   -> essai "OEDO_C"
    OEDO_C_graph = ["P-EPS_VOL", "SIG_AXI-EPS_VOL"]
    #   -> essai "ISOT_C"
    ISOT_C_graph = ["P-EPS_VOL"]

    L_graph_ok = set(TD_graph + TND_graph + CISA_C_graph +
                     TND_C_graph + OEDO_C_graph + ISOT_C_graph + TD_A_graph + TD_NA_graph)
    assert TypGraph_in in L_graph_ok

    for TypGraph in DicoEssai['GRAPHIQUE']:
        if TypGraph == TypGraph_in:
            dico_tmp = {}
            dico_tmp['LEGENDE'] = str_leg
            dico_tmp['ABSCISSE'] = Resu_x
            dico_tmp['ORDONNEE'] = Resu_y
            Courbes[TypGraph].append(dico_tmp)
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def impr_graphique(self, DicoEssai, Courbes, NomsFich, Leg_x, Leg_y, Ech_x, Ech_y):
    """
    Tracer une liste de courbes dans une liste de fichiers
    """
#  import os
    from Accas import _F

    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')
    IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
    INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
    DETRUIRE = self.get_cmd('DETRUIRE')

    # [10,90] = domaine de validite des unites logiques pour IMPR_FONCTION.
    # -> On passe par la boucle ci-dessous pour trouver une unite libre car
    #    INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE') peut renvoyer des valeurs
    #    hors de ce domaine de validite...
    unite = None
    for iul in xrange(10, 90 + 1):
        __ULINFO = INFO_EXEC_ASTER(LISTE_INFO='ETAT_UNITE', UNITE=iul)
        if __ULINFO['ETAT_UNITE', 1] == 'FERME   ':
            unite = iul
            DETRUIRE(CONCEPT=_F(NOM=__ULINFO), INFO=1)
            break
        DETRUIRE(CONCEPT=_F(NOM=__ULINFO), INFO=1)
    assert type(unite) is int

    # ---
    # boucle sur les types de graphiques demandes en sortie
    # ---
    for cle in Courbes:

        # on passe a l'iteration suivante si les listes de resultats sont vides
        if len(Courbes[cle]) == 0:
            DEFI_FICHIER(ACTION='ASSOCIER',
                         FICHIER='./REPE_OUT/' + NomsFich[cle],
                         UNITE=unite,)
            DEFI_FICHIER(ACTION='LIBERER', UNITE=unite,)
            continue

        # s'il faut superposer aux resultats des courbes issues de TABLE_REF...
        if DicoEssai.has_key('TABLE_REF'):
            for table_tmp in DicoEssai['TABLE_REF']:
                table_ref = table_tmp.EXTR_TABLE().values()
                typr = table_ref['TYPE'][0].replace(" ", "")
                lege = table_ref['LEGENDE'][0]
                absc = table_ref['ABSCISSE']
                ordo = table_ref['ORDONNEE']
                if cle == typr:
                    dico_tmp = {}
                    dico_tmp['LEGENDE'] = lege
                    dico_tmp['ABSCISSE'] = absc
                    dico_tmp['ORDONNEE'] = ordo
                    Courbes[cle].append(dico_tmp)

        # fic = os.path.join( os.getcwd(), 'REPE_OUT', Nom_Fichiers[cle])
        DEFI_FICHIER(ACTION='ASSOCIER',
                     FICHIER='./REPE_OUT/' + NomsFich[cle],
                     UNITE=unite,)

        if Ech_x.has_key(cle) and Ech_y.has_key(cle):

            IMPR_FONCTION(FORMAT='XMGRACE', UNITE=unite,
                          COURBE=Courbes[cle],
                          LEGENDE_X=Leg_x[cle],
                          LEGENDE_Y=Leg_y[cle],
                          ECHELLE_X=Ech_x[cle],
                          ECHELLE_Y=Ech_y[cle],)

        else:

            IMPR_FONCTION(FORMAT='XMGRACE', UNITE=unite,
                          COURBE=Courbes[cle],
                          LEGENDE_X=Leg_x[cle],
                          LEGENDE_Y=Leg_y[cle],)

        DEFI_FICHIER(ACTION='LIBERER', UNITE=unite,)
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def remplir_tables(self, typ_essai, str_n_essai, DicoEssai, Resu_in):
    """
    """
    from Accas import _F
    if DicoEssai.has_key('TABLE_RESU'):

        CREA_TABLE = self.get_cmd('CREA_TABLE')

        # ---
        # Essai "TD"
        # ---
        if typ_essai == "TD":

            PRES_CONF = DicoEssai['PRES_CONF']
            EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_TD numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"

                TABLRES = CREA_TABLE(
                    TITRE=titre_table,
                    LISTE=(
                        _F(LISTE_R=EPSI_IMPOSE[i], PARA='EPSI_IMPOSE'),
                        _F(LISTE_R=Resu_in['INST'][i], PARA='INST'),
                        _F(LISTE_R=Resu_in['EPS_AXI'][i], PARA='EPS_AXI'),
                        _F(LISTE_R=Resu_in['EPS_LAT'][i], PARA='EPS_LAT'),
                        _F(LISTE_R=Resu_in['EPS_VOL'][i], PARA='EPS_VOL'),
                        _F(LISTE_R=Resu_in['SIG_AXI'][i], PARA='SIG_AXI'),
                        _F(LISTE_R=Resu_in['SIG_LAT'][i], PARA='SIG_LAT'),
                        _F(LISTE_R=Resu_in['P'][i], PARA='P'),
                        _F(LISTE_R=Resu_in['Q'][i], PARA='Q'),))

        # ---
        # Essai "TND"
        # ---
        elif typ_essai == "TND":

            PRES_CONF = DicoEssai['PRES_CONF']
            EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_TND numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"

                TABLRES = CREA_TABLE(
                    TITRE=titre_table,
                    LISTE=(
                        _F(LISTE_R=EPSI_IMPOSE[i], PARA='EPSI_IMPOSE'),
                        _F(LISTE_R=Resu_in['INST'][i], PARA='INST'),
                        _F(LISTE_R=Resu_in['EPS_AXI'][i], PARA='EPS_AXI'),
                        _F(LISTE_R=Resu_in['EPS_LAT'][i], PARA='EPS_LAT'),
                        _F(LISTE_R=Resu_in['SIG_AXI'][i], PARA='SIG_AXI'),
                        _F(LISTE_R=Resu_in['SIG_LAT'][i], PARA='SIG_LAT'),
                        _F(LISTE_R=Resu_in['P'][i], PARA='P'),
                        _F(LISTE_R=Resu_in['Q'][i], PARA='Q'),
                        _F(LISTE_R=Resu_in['PRE_EAU'][i], PARA='PRE_EAU'),
                    ))

        # ---
        # Essai "CISA_C"
        # ---
        elif typ_essai == "CISA_C":

            PRES_CONF = DicoEssai['PRES_CONF']
            GAMMA_IMPOSE = DicoEssai['GAMMA_IMPOSE']
            LdicoResGlob = []

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_CISA_C numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"
                LdicoRes = []
                for j in xrange(len(GAMMA_IMPOSE)):
                    stjp1 = int_2_str(j + 1, len(GAMMA_IMPOSE))
                    LdicoRes += [
                        {'PARA': 'GAMMA_IMPOSE_' + stjp1, 'LISTE_R': [GAMMA_IMPOSE[j]]}]
                    LdicoRes += [
                        {'PARA': 'INST_' + stjp1, 'LISTE_R': Resu_in['INST'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'GAMMA_' + stjp1, 'LISTE_R': Resu_in['GAMMA'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_XY_' + stjp1, 'LISTE_R': Resu_in['SIG_XY'][i][j]}]
                TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

                stip1 = int_2_str(i + 1, len(PRES_CONF))
                LdicoResGlob += [
                    {'PARA': 'PRES_CONF_' + stip1, 'LISTE_R': [PRES_CONF[i]]}]
                LdicoResGlob += [
                    {'PARA': 'GAMMA_IMPOSE_' + stip1, 'LISTE_R': GAMMA_IMPOSE}]
                LdicoResGlob += [
                    {'PARA': 'G_SUR_GMAX_' + stip1, 'LISTE_R': Resu_in['G_SUR_GMAX'][i]}]
                LdicoResGlob += [
                    {'PARA': 'DAMPING_' + stip1, 'LISTE_R': Resu_in['DAMPING'][i]}]

            self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][-1])
            titre_table = "Resultats globaux : ESSAI_CISA_C numero " + \
                str_n_essai + "\n"
            TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoResGlob))

        # ---
        # Essai "TND_C"
        # ---
        elif typ_essai == "TND_C":

            PRES_CONF = DicoEssai['PRES_CONF']
            SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']
            LdicoResGlob = []

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_TND_C numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"
                LdicoRes = []
                for j in xrange(len(SIGM_IMPOSE)):
                    stjp1 = int_2_str(j + 1, len(SIGM_IMPOSE))
                    LdicoRes += [
                        {'PARA': 'SIGM_IMPOSE_' + stjp1, 'LISTE_R': [SIGM_IMPOSE[j]]}]
                    LdicoRes += [
                        {'PARA': 'INST_' + stjp1, 'LISTE_R': Resu_in['INST'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_AXI_' + stjp1, 'LISTE_R': Resu_in['EPS_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_LAT_' + stjp1, 'LISTE_R': Resu_in['EPS_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_VOL_' + stjp1, 'LISTE_R': Resu_in['EPS_VOL'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_AXI_' + stjp1, 'LISTE_R': Resu_in['SIG_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_LAT_' + stjp1, 'LISTE_R': Resu_in['SIG_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'P_' + stjp1, 'LISTE_R': Resu_in['P'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'Q_' + stjp1, 'LISTE_R': Resu_in['Q'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'PRE_EAU_' + stjp1, 'LISTE_R': Resu_in['PRE_EAU'][i][j]}]
                TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

                stip1 = int_2_str(i + 1, len(PRES_CONF))
                LdicoResGlob += [
                    {'PARA': 'PRES_CONF_' + stip1, 'LISTE_R': [PRES_CONF[i]]}]
                LdicoResGlob += [
                    {'PARA': 'NCYCL_' + stip1, 'LISTE_I': Resu_in['NCYCL'][i]}]
                LdicoResGlob += [
                    {'PARA': 'SIGM_IMPOSE_' + stip1, 'LISTE_R': Resu_in['DSIGM'][i]}]

            self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][-1])
            titre_table = "Resultats globaux : ESSAI_TND_C numero " + \
                str_n_essai + "\n"
            TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoResGlob))

        # ---
        # Essai "TD_A"
        # ---
        elif typ_essai == "TD_A":

            PRES_CONF = DicoEssai['PRES_CONF']
            EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
            LdicoResGlob = []

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_TD_A numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"
                LdicoRes = []
                for j in xrange(len(EPSI_IMPOSE)):
                    stjp1 = int_2_str(j + 1, len(EPSI_IMPOSE))
                    LdicoRes += [
                        {'PARA': 'EPSI_IMPOSE_' + stjp1, 'LISTE_R': [EPSI_IMPOSE[j]]}]
                    LdicoRes += [
                        {'PARA': 'INST_' + stjp1, 'LISTE_R': Resu_in['INST'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_AXI_' + stjp1, 'LISTE_R': Resu_in['EPS_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_LAT_' + stjp1, 'LISTE_R': Resu_in['EPS_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_VOL_' + stjp1, 'LISTE_R': Resu_in['EPS_VOL'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_AXI_' + stjp1, 'LISTE_R': Resu_in['SIG_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_LAT_' + stjp1, 'LISTE_R': Resu_in['SIG_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'P_' + stjp1, 'LISTE_R': Resu_in['P'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'Q_' + stjp1, 'LISTE_R': Resu_in['Q'][i][j]}]
                TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

                stip1 = int_2_str(i + 1, len(PRES_CONF))
                LdicoResGlob += [
                    {'PARA': 'PRES_CONF_' + stip1, 'LISTE_R': [PRES_CONF[i]]}]
                LdicoResGlob += [
                    {'PARA': 'EPSI_IMPOSE_' + stip1, 'LISTE_R': EPSI_IMPOSE}]
                LdicoResGlob += [
                    {'PARA': 'E_SUR_EMAX_' + stip1, 'LISTE_R': Resu_in['E_SUR_EMAX'][i]}]

            self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][-1])
            titre_table = "Resultats globaux : ESSAI_TD_A numero " + \
                str_n_essai + "\n"
            TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoResGlob))

        # ---
        # Essai "TD_NA"
        # ---
        elif typ_essai == "TD_NA":

            PRES_CONF = DicoEssai['PRES_CONF']
            EPSI_IMPOSE = DicoEssai['EPSI_IMPOSE']
            LdicoResGlob = []

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = "Resultats bruts : ESSAI_TD_NA numero " + str_n_essai\
                    + " / PRES_CONF = " + \
                    str("%E" % (PRES_CONF[i])) + "\n"
                LdicoRes = []
                for j in xrange(len(EPSI_IMPOSE)):
                    stjp1 = int_2_str(j + 1, len(EPSI_IMPOSE))
                    LdicoRes += [
                        {'PARA': 'EPSI_IMPOSE_' + stjp1, 'LISTE_R': [EPSI_IMPOSE[j]]}]
                    LdicoRes += [
                        {'PARA': 'INST_' + stjp1, 'LISTE_R': Resu_in['INST'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_AXI_' + stjp1, 'LISTE_R': Resu_in['EPS_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_LAT_' + stjp1, 'LISTE_R': Resu_in['EPS_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'EPS_VOL_' + stjp1, 'LISTE_R': Resu_in['EPS_VOL'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_AXI_' + stjp1, 'LISTE_R': Resu_in['SIG_AXI'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'SIG_LAT_' + stjp1, 'LISTE_R': Resu_in['SIG_LAT'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'P_' + stjp1, 'LISTE_R': Resu_in['P'][i][j]}]
                    LdicoRes += [
                        {'PARA': 'Q_' + stjp1, 'LISTE_R': Resu_in['Q'][i][j]}]
                TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

                stip1 = int_2_str(i + 1, len(PRES_CONF))
                LdicoResGlob += [
                    {'PARA': 'PRES_CONF_' + stip1, 'LISTE_R': [PRES_CONF[i]]}]
                LdicoResGlob += [
                    {'PARA': 'EPSI_IMPOSE_' + stip1, 'LISTE_R': EPSI_IMPOSE}]
                LdicoResGlob += [
                    {'PARA': 'E_SUR_EMAX_' + stip1, 'LISTE_R': Resu_in['E_SUR_EMAX'][i]}]

            self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][-1])
            titre_table = "Resultats globaux : ESSAI_TD_NA numero " + \
                str_n_essai + "\n"
            TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoResGlob))

        # ---
        # Essai "OEDO_C"
        # ---
        elif typ_essai == "OEDO_C":

            PRES_CONF = DicoEssai['PRES_CONF']
            SIGM_DECH = DicoEssai['SIGM_DECH']
            SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = " ESSAI_OEDO_C numero " + str_n_essai                         \
                    + " / PRES_CONF = " + str("%E" % (PRES_CONF[i]))\
                    + " / SIGM_DECH = " + \
                    str("%E" % (SIGM_DECH[i])) + "\n"
                LdicoRes = []
                stip1 = int_2_str(i + 1, len(PRES_CONF))
                for j in xrange(len(SIGM_IMPOSE)):

                    resu_vide = Resu_in['EPS_VOL'][i] == []
                    resu_vide = resu_vide or Resu_in['P'][i] == []
                    resu_vide = resu_vide or Resu_in['SIG_AXI'][i] == []
                    resu_vide = resu_vide or Resu_in['SIG_LAT'][i] == []
                    resu_vide = resu_vide or Resu_in['INST'][i] == []
                    # en cas d'abscence de resultats on passe a l'iteration
                    # suivante (boucle j)
                    if resu_vide:
                        continue

                # for j in xrange(len(SIGM_IMPOSE)):
                # stjp1 = int_2_str(j+1,len(SIGM_IMPOSE))
                LdicoRes += [
                    {'PARA': 'SIGM_IMPOSE_' + stip1, 'LISTE_R': Resu_in['SIGM_IMPOSE'][i]}]
                LdicoRes += [
                    {'PARA': 'INST_' + stip1, 'LISTE_R': Resu_in['INST'][i]}]
                LdicoRes += [
                    {'PARA': 'EPS_VOL_' + stip1, 'LISTE_R': Resu_in['EPS_VOL'][i]}]
                LdicoRes += [
                    {'PARA': 'P_' + stip1, 'LISTE_R': Resu_in['P'][i]}]
                LdicoRes += [
                    {'PARA': 'SIG_AXI_' + stip1, 'LISTE_R': Resu_in['SIG_AXI'][i]}]
                LdicoRes += [
                    {'PARA': 'SIG_LAT_' + stip1, 'LISTE_R': Resu_in['SIG_LAT'][i]}]
                # si il n'y a aucun resultat ds LdicoRes on cree une table vide
                if LdicoRes == []:
                    TABLRES = CREA_TABLE(TITRE=titre_table)
                else:
                    TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

        # ---
        # Essai "ISOT_C"
        # ---
        elif typ_essai == "ISOT_C":

            PRES_CONF = DicoEssai['PRES_CONF']
            SIGM_DECH = DicoEssai['SIGM_DECH']
            SIGM_IMPOSE = DicoEssai['SIGM_IMPOSE']

            for i in xrange(len(PRES_CONF)):

                self.DeclareOut('TABLRES', DicoEssai['TABLE_RESU'][i])
                titre_table = " ESSAI_ISOT_C numero " + str_n_essai\
                    + " / PRES_CONF = " + str("%E" % (PRES_CONF[i])) \
                    + " / SIGM_DECH = " + \
                    str("%E" % (SIGM_DECH[i])) + "\n"
                LdicoRes = []
                stip1 = int_2_str(i + 1, len(PRES_CONF))
                for j in xrange(len(SIGM_IMPOSE)):

                    resu_vide = Resu_in['EPS_VOL'][i] == []
                    resu_vide = resu_vide or Resu_in['P'][i] == []
                    resu_vide = resu_vide or Resu_in['Q'][i] == []
                    resu_vide = resu_vide or Resu_in['INST'][i] == []
                    # en cas d'abscence de resultats on passe a l'iteration
                    # suivante (boucle j)
                    if resu_vide:
                        continue
                # for j in xrange(len(SIGM_IMPOSE)):
                    # stjp1 = int_2_str(j+1,len(SIGM_IMPOSE))
                LdicoRes += [
                    {'PARA': 'SIGM_IMPOSE_' + stip1, 'LISTE_R': Resu_in['SIGM_IMPOSE'][i]}]
                LdicoRes += [
                    {'PARA': 'INST_' + stip1, 'LISTE_R': Resu_in['INST'][i]}]
                LdicoRes += [
                    {'PARA': 'EPS_VOL_' + stip1, 'LISTE_R': Resu_in['EPS_VOL'][i]}]
                LdicoRes += [
                    {'PARA': 'P_' + stip1, 'LISTE_R': Resu_in['P'][i]}]
                LdicoRes += [
                    {'PARA': 'Q_' + stip1, 'LISTE_R': Resu_in['Q'][i]}]
                # si il n'y a aucun resultat ds LdicoRes on cree une table vide
                if LdicoRes == []:
                    TABLRES = CREA_TABLE(TITRE=titre_table)
                else:
                    TABLRES = CREA_TABLE(TITRE=titre_table, LISTE=(LdicoRes))

        # ---
        # Pour nouvel essai
        # ---
        # elif typ_essai == "XXX":
        # ---
        #  ...
        # ---
        else:
            assert False
# --------------------------------------------------------------
# --------------------------------------------------------------


# --------------------------------------------------------------
# --------------------------------------------------------------
def Calc_Gs_max(self, GAMMA_ELAS, PRES_CONF, KZERO, MATER, COMPORTEMENT, CONVERGENCE):
    """
    Pour l'essai CISA_C : calcul du module de cisaillement secant max
    (EPSI_ELAS doit etre telle qu'on reste bien dans le domaine elastique)
    """
    from Accas import _F

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')

    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=_F(JUSQU_A=1.,
                                           NOMBRE=1,),)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),)

    __CHAR1 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., 0., 1., -0.5 * GAMMA_ELAS,),)

    __CHAR2 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., KZERO * PRES_CONF, 1., KZERO * PRES_CONF,),)

    __CHAR3 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., PRES_CONF, 1., PRES_CONF,),)

    __EVOL = SIMU_POINT_MAT(
        COMPORTEMENT=COMPORTEMENT.List_F(),
        CONVERGENCE=CONVERGENCE.List_F(),
        MATER=MATER,
        INCREMENT=_F(LIST_INST=__DLIST,
                     INST_INIT=0.,
                     INST_FIN=1.,),
        NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
        ARCHIVAGE=_F(LIST_INST=__RLIST,),
        SIGM_IMPOSE=_F(SIXX=__CHAR2,
                           SIYY=__CHAR2,
                           SIZZ=__CHAR3,),
        EPSI_IMPOSE=_F(EPXY=__CHAR1,),
        SIGM_INIT=_F(SIXX=KZERO * PRES_CONF,
                     SIYY=KZERO * PRES_CONF,
                     SIZZ=PRES_CONF,),
        EPSI_INIT=_F(EPXX=0,
                     EPYY=0,
                     EPZZ=0,
                     EPXY=0,
                     EPXZ=0,
                     EPYZ=0,),)

    TabRes = __EVOL.EXTR_TABLE().values()
    sig_xy = TabRes['SIXY'][-1]
    eps_xy = TabRes['EPXY'][-1]

    DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __CHAR3, __EVOL,
                             __RLIST, __DLIST),),
             INFO=1)

    return 0.5 * sig_xy / eps_xy
# --------------------------------------------------------------
# --------------------------------------------------------------

# --------------------------------------------------------------
# --------------------------------------------------------------


def Calc_Es_max_TA(self, EPSI_ELAS, PRES_CONF, KZERO, MATER, COMPORTEMENT, CONVERGENCE):
    """
    Pour l'essai TD_A  : calcul du module de Young cyclique équivalent max
    (EPSI_ELAS doit etre telle qu'on reste bien dans le domaine elastique)
    """
    from Accas import _F
    import numpy as NP

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')

    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=_F(JUSQU_A=5.,
                                           NOMBRE=5,),)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),)

    __CHAR1 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., 0., 1., -1. * EPSI_ELAS, 3., EPSI_ELAS, 5., -1. * EPSI_ELAS),)

    __CHAR2 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., KZERO * PRES_CONF, 5., KZERO * PRES_CONF,),)

    __EVOL = SIMU_POINT_MAT(
        COMPORTEMENT=COMPORTEMENT.List_F(),
        CONVERGENCE=CONVERGENCE.List_F(),
        MATER=MATER,
        INCREMENT=_F(LIST_INST=__DLIST,
                     INST_INIT=0.,
                     INST_FIN=5.,
                     ),
        NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
        ARCHIVAGE=_F(LIST_INST=__RLIST,),
        SIGM_IMPOSE=_F(SIXX=__CHAR2,
                           SIYY=__CHAR2,),
        EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
        SIGM_INIT=_F(SIXX=KZERO * PRES_CONF,
                     SIYY=KZERO * PRES_CONF,
                     SIZZ=PRES_CONF,),
        EPSI_INIT=_F(EPXX=0,
                     EPYY=0,
                     EPZZ=0,
                     EPXY=0,
                     EPXZ=0,
                     EPYZ=0,),)

    TabRes = __EVOL.EXTR_TABLE().values()
    inst = TabRes['INST']
    sig_zz = NP.array(TabRes['SIZZ'])
    sig_xx = NP.array(TabRes['SIXX'])
    q = sig_zz - sig_xx

    DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __EVOL,
                             __RLIST, __DLIST),),
             INFO=1)

    return 0.5 * abs(q[inst.index(5.)] - q[inst.index(3.)]) / EPSI_ELAS

# --------------------------------------------------------------
# --------------------------------------------------------------

# --------------------------------------------------------------
# --------------------------------------------------------------


def Calc_Es_max_TNA(self, EPSI_ELAS, PRES_CONF, KZERO, MATER, COMPORTEMENT, CONVERGENCE):
    """
    Pour l'essai TD_NA  : calcul du module de Young cyclique équivalentmax
    (EPSI_ELAS doit etre telle qu'on reste bien dans le domaine elastique)
    """
    from Accas import _F
    import numpy as NP

    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    DEFI_LIST_INST = self.get_cmd('DEFI_LIST_INST')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    SIMU_POINT_MAT = self.get_cmd('SIMU_POINT_MAT')
    DETRUIRE = self.get_cmd('DETRUIRE')

    __RLIST = DEFI_LIST_REEL(DEBUT=0.,
                             INTERVALLE=_F(JUSQU_A=3.,
                                           NOMBRE=3,),)

    __DLIST = DEFI_LIST_INST(DEFI_LIST=_F(LIST_INST=__RLIST),
                             ECHEC=_F(SUBD_METHODE='MANUEL',
                                      SUBD_PAS=10,
                                      SUBD_NIVEAU=10,),)

    __CHAR1 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., 0., 1., -1. * EPSI_ELAS, 2., 0., 3., -1. * EPSI_ELAS),)

    __CHAR2 = DEFI_FONCTION(NOM_PARA='INST',
                            VALE=(0., KZERO * PRES_CONF, 3., KZERO * PRES_CONF,),)

    __EVOL = SIMU_POINT_MAT(
        COMPORTEMENT=COMPORTEMENT.List_F(),
        CONVERGENCE=CONVERGENCE.List_F(),
        MATER=MATER,
        INCREMENT=_F(LIST_INST=__DLIST,
                     INST_INIT=0.,
                     INST_FIN=3.,
                     ),
        NEWTON=_F(MATRICE='TANGENTE', REAC_ITER=1,),
        ARCHIVAGE=_F(LIST_INST=__RLIST,),
        SIGM_IMPOSE=_F(SIXX=__CHAR2,
                           SIYY=__CHAR2,),
        EPSI_IMPOSE=_F(EPZZ=__CHAR1,),
        SIGM_INIT=_F(SIXX=KZERO * PRES_CONF,
                     SIYY=KZERO * PRES_CONF,
                     SIZZ=PRES_CONF,),
        EPSI_INIT=_F(EPXX=0,
                     EPYY=0,
                     EPZZ=0,
                     EPXY=0,
                     EPXZ=0,
                     EPYZ=0,),)

    TabRes = __EVOL.EXTR_TABLE().values()
    inst = TabRes['INST']
    sig_zz = NP.array(TabRes['SIZZ'])
    sig_xx = NP.array(TabRes['SIXX'])
    q = sig_zz - sig_xx

    DETRUIRE(CONCEPT=_F(NOM=(__CHAR1, __CHAR2, __EVOL,
                             __RLIST, __DLIST),),
             INFO=1)

    return abs(q[inst.index(3.)] - q[inst.index(2.)]) / EPSI_ELAS

# --------------------------------------------------------------
# --------------------------------------------------------------
