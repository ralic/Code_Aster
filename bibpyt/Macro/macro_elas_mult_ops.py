# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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


def macro_elas_mult_ops(self, MODELE, CHAM_MATER, CARA_ELEM, NUME_DDL,
                        CHAR_MECA_GLOBAL, LIAISON_DISCRET,
                        CAS_CHARGE, SOLVEUR, **args):
    """
       Ecriture de la macro MACRO_ELAS_MULT
    """
    ier = 0
    import types
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS

    # On met le mot cle NUME_DDL dans une variable locale pour le proteger
    numeddl = NUME_DDL
    # On importe les definitions des commandes a utiliser dans la macro
    CALC_MATR_ELEM = self.get_cmd('CALC_MATR_ELEM')
    NUME_DDL = self.get_cmd('NUME_DDL')
    ASSE_MATRICE = self.get_cmd('ASSE_MATRICE')
    FACTORISER = self.get_cmd('FACTORISER')
    CALC_VECT_ELEM = self.get_cmd('CALC_VECT_ELEM')
    ASSE_VECTEUR = self.get_cmd('ASSE_VECTEUR')
    RESOUDRE = self.get_cmd('RESOUDRE')
    CREA_RESU = self.get_cmd('CREA_RESU')
    CALC_CHAMP = self.get_cmd('CALC_CHAMP')
    CALCUL = self.get_cmd('CALCUL')
    EXTR_TABLE = self.get_cmd('EXTR_TABLE')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Le concept sortant (de type mult_elas ou fourier_elas) est nommé
    # 'nomres' dans le contexte de la macro

    self.DeclareOut('nomres', self.sd)

    ielas = 0
    ifour = 0
    for m in CAS_CHARGE:
        if m['NOM_CAS']:
            ielas = 1                 # mot clé NOM_CAS      présent sous CAS_CHARGE
            tyresu = 'MULT_ELAS'
        else:
            ifour = 1                 # mot clé MODE_FOURIER présent sous CAS_CHARGE
            tyresu = 'FOURIER_ELAS'
    if ielas == 1 and ifour == 1:
        UTMESS('F', 'ELASMULT0_1')

    if (numeddl in self.sdprods) or (numeddl == None):
        # Si le concept numeddl est dans self.sdprods ou n est pas nommé
        # il doit etre  produit par la macro
        # il faudra donc appeler la commande NUME_DDL
        lnume = 1
    else:
        lnume = 0

    if ielas == 1:
        motscles = {}
        if CHAR_MECA_GLOBAL:
            motscles['CHARGE'] = CHAR_MECA_GLOBAL
        if CHAM_MATER:
            motscles['CHAM_MATER'] = CHAM_MATER
        if CARA_ELEM:
            motscles['CARA_ELEM'] = CARA_ELEM
        __nomrig = CALC_MATR_ELEM(
            OPTION='RIGI_MECA', MODELE=MODELE, **motscles)

        if lnume:
            # On peut passer des mots cles egaux a None. Ils sont ignores
            motscles = {}
            if numeddl != None:
                self.DeclareOut('num', numeddl)
                num = NUME_DDL(MATR_RIGI=__nomrig, **motscles)
            else:
                _num = NUME_DDL(MATR_RIGI=__nomrig, **motscles)
                num = _num
        else:
            num = numeddl

        __nomras = ASSE_MATRICE(MATR_ELEM=__nomrig, NUME_DDL=num)

        __nomras = FACTORISER(reuse=__nomras, MATR_ASSE=__nomras,
                              NPREC          =SOLVEUR['NPREC'],
                              STOP_SINGULIER =SOLVEUR['STOP_SINGULIER'],
                              METHODE        =SOLVEUR['METHODE'],
                              RENUM          =SOLVEUR['RENUM'],
                              )
#
# boucle sur les items de CAS_CHARGE

    nomchn = []
    lcharg = []
    iocc = 0
    for m in CAS_CHARGE:
        iocc = iocc + 1

        # calcul de lcharg : liste des listes de char_meca (mots clé CHAR_MECA
        # et CHAR_MECA_GLOBAL)
        xx1 = m['CHAR_MECA']
        if type(xx1) != type((1,)):
            xx1 = (xx1,)
        xx2 = CHAR_MECA_GLOBAL
        if type(xx2) != type((1,)):
            xx2 = (xx2,)
        lchar1 = []
        for chargt in (xx1 + xx2):
            if chargt:
                lchar1.append(chargt)
        lcharg.append(lchar1)
        assert len(lchar1) > 0

        if ifour:
            motscles = {}
            if CHAR_MECA_GLOBAL:
                motscles['CHARGE'] = CHAR_MECA_GLOBAL
            if CHAM_MATER:
                motscles['CHAM_MATER'] = CHAM_MATER
            if CARA_ELEM:
                motscles['CARA_ELEM'] = CARA_ELEM
            motscles['MODE_FOURIER'] = m['MODE_FOURIER']
            __nomrig = CALC_MATR_ELEM(
                OPTION='RIGI_MECA', MODELE=MODELE, **motscles)

            if lnume:
                _num = NUME_DDL(MATR_RIGI=__nomrig, )
                num = _num
                lnume = 0

            __nomras = ASSE_MATRICE(MATR_ELEM=__nomrig, NUME_DDL=num)

            __nomras = FACTORISER(reuse=__nomras, MATR_ASSE=__nomras,
                              NPREC          =SOLVEUR['NPREC'],
                              STOP_SINGULIER =SOLVEUR['STOP_SINGULIER'],
                              METHODE        =SOLVEUR['METHODE'],
                              RENUM          =SOLVEUR['RENUM'],
                              )

        if m['VECT_ASSE'] == None:
            motscles = {}
            l_calc_varc = False
            if CHAM_MATER:
                motscles['CHAM_MATER'] = CHAM_MATER
                iret, ibid, answer = aster.dismoi('EXI_VARC', CHAM_MATER.nom, 'CHAM_MATER', 'F')
                if answer == 'OUI':
                    l_calc_varc = True
            if CARA_ELEM:
                motscles['CARA_ELEM'] = CARA_ELEM
            if ifour:
                motscles['MODE_FOURIER'] = m['MODE_FOURIER']
            if len(lchar1) > 0:
                motscles['CHARGE'] = lchar1
            __nomvel = CALC_VECT_ELEM(OPTION='CHAR_MECA', **motscles)
                
                # chargement du aux variables de commandes
            if l_calc_varc :
                motscles = {}
                if CARA_ELEM:
                    motscles['CARA_ELEM'] = CARA_ELEM
                if ifour:
                    motscles['MODE_FOURIER'] = m['MODE_FOURIER']
                
                __list1=DEFI_LIST_REEL(DEBUT=0.0,
                                    INTERVALLE=_F(JUSQU_A=1.0,
                                                  NOMBRE=1,),);

                if CHAR_MECA_GLOBAL :
                    excit = []
                    for ch in CHAR_MECA_GLOBAL:
                        excit.append({'CHARGE' : ch})
                    __cont1=CALCUL(OPTION=('FORC_VARC_ELEM_P'),
                                MODELE=MODELE,
                                CHAM_MATER = CHAM_MATER,
                                INCREMENT=_F(LIST_INST=__list1,
                                             NUME_ORDRE=1),
                                EXCIT=excit,
                                COMPORTEMENT=_F(RELATION='ELAS',),
                                **motscles
                                )
                else:
                    __cont1=CALCUL(OPTION=('FORC_VARC_ELEM_P'),
                                MODELE=MODELE,
                                CHAM_MATER = CHAM_MATER,
                                INCREMENT=_F(LIST_INST=__list1,
                                             NUME_ORDRE=1),
                                COMPORTEMENT=_F(RELATION='ELAS',),
                                **motscles
                                )

                __vvarcp=EXTR_TABLE(TYPE_RESU='VECT_ELEM_DEPL_R',
                                 TABLE=__cont1,
                                 NOM_PARA='NOM_SD',
                                 FILTRE=_F(NOM_PARA='NOM_OBJET',
                                           VALE_K='FORC_VARC_ELEM_P'),)
                                           
                __nomasv = ASSE_VECTEUR(VECT_ELEM=(__nomvel,__vvarcp), NUME_DDL=num)
            else:
                __nomasv = ASSE_VECTEUR(VECT_ELEM=(__nomvel,), NUME_DDL=num)
        else:
            __nomasv = m['VECT_ASSE']

        __nomchn = RESOUDRE(
            MATR=__nomras, CHAM_NO=__nomasv, TITRE=m['SOUS_TITRE'])
        nomchn.append(__nomchn)

# fin de la boucle sur les items de CAS_CHARGE
#

    motscles = {}
    iocc = 0
    motscle2 = {}
    if CHAM_MATER:
        motscle2['CHAM_MATER'] = CHAM_MATER
        iret, ibid, nom_ma = aster.dismoi('EXI_VARC', CHAM_MATER.nom, 'CHAM_MATER', 'F')
        print 'toto:',nom_ma
    if CARA_ELEM:
        motscle2['CARA_ELEM'] = CARA_ELEM
    if ielas:
        motscles['AFFE'] = []
        for m in CAS_CHARGE:
            if len(lcharg[iocc]) > 0:
                motscles['AFFE'].append(_F(MODELE=MODELE,
                                           CHAM_GD=nomchn[iocc],
                                           NOM_CAS=m['NOM_CAS'],
                                           CHARGE=lcharg[iocc],
                                           **motscle2))
            else:
                motscles['AFFE'].append(_F(MODELE=MODELE,
                                           CHAM_GD=nomchn[iocc],
                                           NOM_CAS=m['NOM_CAS'],
                                           **motscle2))
            iocc = iocc + 1
    else:
        motscles['AFFE'] = []
        for m in CAS_CHARGE:
            if len(lcharg[iocc]) > 0:
                motscles['AFFE'].append(_F(MODELE=MODELE,
                                           CHAM_GD=nomchn[iocc],
                                           NUME_MODE=m['MODE_FOURIER'],
                                           TYPE_MODE=m['TYPE_MODE'],
                                           CHARGE=lcharg[iocc],
                                           **motscle2))
            else:
                motscles['AFFE'].append(_F(MODELE=MODELE,
                                           CHAM_GD=nomchn[iocc],
                                           NUME_MODE=m['MODE_FOURIER'],
                                           TYPE_MODE=m['TYPE_MODE'],
                                           **motscle2))
            iocc = iocc + 1

    if self.reuse:
        motscles['reuse'] = self.reuse
    nomres = CREA_RESU(
        OPERATION='AFFE', TYPE_RESU=tyresu, NOM_CHAM='DEPL', **motscles)

#
# boucle sur les items de CAS_CHARGE pour SIEF_ELGA

    iocc = 0
    for m in CAS_CHARGE:
        iocc = iocc + 1

        if m['OPTION'] == 'SIEF_ELGA':
            motscles = {}
            if ielas:
                motscles['NOM_CAS'] = m['NOM_CAS']
            else:
                motscles['NUME_MODE'] = m['MODE_FOURIER']
            CALC_CHAMP(reuse=nomres,
                       RESULTAT=nomres,
                       CONTRAINTE='SIEF_ELGA',
                       **motscles)

# fin de la boucle sur les items de CAS_CHARGE
#
    return ier
