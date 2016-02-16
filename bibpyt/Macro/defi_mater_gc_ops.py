# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: jean-luc.flejou at edf.fr

from Accas import _F
import aster
import numpy
import numpy as NP
from Utilitai.Utmess import UTMESS


def FaitMessage(Dico):
    message = ""
    cpt = 1
    for xk, xv in Dico.iteritems():
        message += " %s = %15.8E," % (xk, xv)
        if (len(message) > 80 * cpt):
            message += "\n  "
            cpt += 1
    return message


def BetonEC2(Classe):
    Dico = {}
    # Décodage de la classe
    sfck, sfckc = Classe[1:].split('/')
    #
    Dico['fck'] = float(sfck)
    Dico['fckc'] = float(sfckc)
    # Contrainte en MPa et déformation en ‰
    Dico['fcm'] = Dico['fck'] + 8.0
    Dico['nu'] = 0.20
    #
    if (Dico['fck'] <= 50.0):
        Dico['fctm'] = 0.30 * NP.power(Dico['fck'], 2.0 / 3.0)
    else:
        Dico['fctm'] = 2.12 * NP.log(1.0 + Dico['fcm'] / 10.0)
    #
    Dico['ecm'] = 22.0E+03 * NP.power(Dico['fcm'] / 10.0, 0.3)
    #
    Dico['epsi_c1'] = 0.7 * NP.power(Dico['fcm'], 0.31)
    if (Dico['epsi_c1'] >= 2.8):
        Dico['epsi_c1'] = 2.8
    #
    if (Dico['fck'] > 50.0):
        Dico['epsi_cu1'] = 2.80 + 27.000 * \
            NP.pow((98.0 - Dico['fcm']) / 100.0, 4.0)
        Dico['epsi_c2'] = 2.00 + 0.085 * NP.pow(Dico['fck'] - 50.0, 0.53)
        Dico['epsi_cu2'] = 2.60 + 35.000 * \
            NP.pow((90.0 - Dico['fck']) / 100.0, 4.0)
        Dico['n']        = 1.40 + 23.400 * \
            NP.pow((90.0 - Dico['fck']) / 100.0, 4.0)
        Dico['epsi_c3'] = 1.75 + 0.550 * (Dico['fck'] - 50.0) / 40.0
        Dico['epsi_cu3'] = 2.60 + 35.000 * \
            NP.pow((90.0 - Dico['fck']) / 100.0, 4.0)
    else:
        Dico['epsi_cu1'] = 3.50
        Dico['epsi_c2'] = 2.00
        Dico['epsi_cu2'] = 3.50
        Dico['n'] = 2.00
        Dico['epsi_c3'] = 1.75
        Dico['epsi_cu3'] = 3.50
    #
    return Dico


def BetonBAEL91(fcj):
    Dico = {}
    # Contrainte en MPa
    Dico['fcj'] = fcj
    #
    Dico['eij'] = 11000.0 * NP.power(Dico['fcj'], 0.333333)
    Dico['ftj'] = 0.60 + 0.06 * Dico['fcj']
    Dico['epsi_c'] = 0.620E-3 * NP.power(Dico['fcj'], 0.333333)
    Dico['nu'] = 0.20
    #
    return Dico


def Mazars_Unil(DMATER, args):
    """
    MAZARS_UNIL = Paramètres de la loi de comportement
        UNITE_LONGUEUR = unité du problème [M|MM]
        FCJ    [Unite] = Contrainte au pic en compression
        EIJ    [Unite] = Module d'young
        EPSI_C         = Déformation au pic en compression
        FTJ    [Unite] = Contrainte au pic en traction
        NU             = Coefficient de poisson
        EPSD0          = Déformation, seuil d'endommagement
        K              = Paramètre de décroissance post-pic en cisaillement
        AC             = Paramètre de décroissance post-pic en compression
        BC             = 1/(Déformation au pic en compression)
        AT             = Paramètre de décroissance post-pic en traction
        BT             = 1/(Déformation au pic en traction)
        SIGM_LIM       = Contrainte limite pour post-traitement
        EPSI_LIM       = Déformation limite pour post-traitement

    Masse volumique, dilatation, amortissements
        RHO            = Masse volumique
        ALPHA          = Coefficient de dilatation
        AMOR_ALPHA     =
        AMOR_BETA      =
        AMOR_HYST      =
    """
    #
    MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)
    #
    # Obligatoire : Règlement de codification
    Regle = MATER['CODIFICATION']
    # Liste des paramètes matériaux facultatifs mais nécessaires pour calculer
    # les valeurs des paramètres de MAZARS
    listepara = ['NU', 'EPSD0', 'K', 'BT',
                 'AT', 'BC', 'AC', 'SIGM_LIM', 'EPSI_LIM']
    #
    if (Regle == 'BAEL91'):
        # Obligatoire : FCJ UNITE_CONTRAINTE
        if (MATER['UNITE_CONTRAINTE'] == "MPa"):
            coeff = 1.0
        elif (MATER['UNITE_CONTRAINTE'] == "Pa"):
            coeff = 1.0E+06
        beton = BetonBAEL91(MATER['FCJ'] / coeff)
        #
        FCJ = beton['fcj'] * coeff
        EIJ = beton['eij'] * coeff
        FTJ = beton['ftj'] * coeff
        EPSI_C = beton['epsi_c']
        NU = beton['nu']
        SIGM_LIM = 0.6 * FCJ
        EPSI_LIM = 3.5 / 1000.0
        #
        for xx in listepara:
            MATER[xx] = None
        #
    elif (Regle == 'EC2'):
        # Obligatoire CLASSE UNITE_CONTRAINTE
        if (MATER['UNITE_CONTRAINTE'] == "MPa"):
            coeff = 1.0
        elif (MATER['UNITE_CONTRAINTE'] == "Pa"):
            coeff = 1.0E+06
        beton = BetonEC2(MATER['CLASSE'])
        #
        FCJ = beton['fcm'] * coeff
        EIJ = beton['ecm'] * coeff
        FTJ = beton['fctm'] * coeff
        EPSI_C = beton['epsi_c1'] / 1000.0
        NU = beton['nu']
        SIGM_LIM = 0.6 * FCJ
        EPSI_LIM = beton['epsi_cu1']
        #
        for xx in listepara:
            MATER[xx] = None
        #
    elif (Regle == 'ESSAI'):
        # Obligatoire FCJ , EIJ, FTJ, EPSI_C
        FCJ = MATER['FCJ']
        EIJ = MATER['EIJ']
        FTJ = MATER['FTJ']
        EPSI_C = MATER['EPSI_C']
        MATER['UNITE_CONTRAINTE'] = ''
    # L'ordre dans la liste est important à cause des dépendances des relations
    # Les coefficients FCJ , EIJ, FTJ, EPSI_C doivent déjà être définis
    listepara = ['NU', 'EPSD0', 'K', 'BT',
                 'AT', 'BC', 'AC', 'SIGM_LIM', 'EPSI_LIM']
    for xx in listepara:
        if (MATER.has_key(xx)):
            if (MATER[xx] != None):
                exec('%s = %s' % (xx, MATER[xx]))
            elif (xx == 'NU'):
                NU = 0.200
            elif (xx == 'EPSD0'):
                EPSD0 = FTJ / EIJ
            elif (xx == 'K'):
                K = 0.7
            elif (xx == 'BT'):
                BT = EIJ / FTJ
            elif (xx == 'AT'):
                AT = 0.90
            elif (xx == 'BC'):
                BC = 1.0 / (NU * (2.0 ** 0.5) * EPSI_C)
            elif (xx == 'AC'):
                NUB = NU * (2.0 ** 0.5)
                ECNUB = EPSI_C * NUB
                AC = (FCJ * NUB / EIJ - EPSD0) / (
                    ECNUB * NP.exp(BC * EPSD0 - BC * ECNUB) - EPSD0)
            elif (xx == 'SIGM_LIM'):
                SIGM_LIM = 0.6 * FCJ
            elif (xx == 'EPSI_LIM'):
                EPSI_LIM = 3.5 / 1000.0
    #
    mclef = {}
    #
    mclef['INFO'] = 1
    if (args.has_key('INFO')):
        mclef['INFO'] = args['INFO']
    #
    # Mot clef ELAS
    mclef['ELAS'] = {'E': EIJ, 'NU': NU}
    listepara = ['RHO', 'ALPHA', 'AMOR_ALPHA', 'AMOR_BETA', 'AMOR_HYST']
    for xx in listepara:
        if (args.has_key(xx)):
            if (args[xx] != None):
                mclef['ELAS'][xx] = args[xx]
    # Mot clef MATER
    mclef[
        'MAZARS'] = {'K': K, 'EPSD0': EPSD0, 'AC': AC, 'AT': AT, 'BC': BC, 'BT': BT,
                     'SIGM_LIM': SIGM_LIM, 'EPSI_LIM': EPSI_LIM}
    #
    # On affiche dans tous les cas
    if (len(MATER['UNITE_CONTRAINTE']) > 0):
        message0 = "MAZARS [%s]" % MATER['UNITE_CONTRAINTE']
    else:
        message0 = "MAZARS"
    #
    message1 = FaitMessage(mclef['ELAS'])
    message2 = FaitMessage(mclef['MAZARS'])
    Dico = {'FCJ': FCJ, 'FTJ': FTJ, 'EPSI_C': EPSI_C}
    message3 = FaitMessage(Dico)
    #
    UTMESS('I', 'COMPOR1_75', valk=(message0, message1, message2, message3))
    #
    return mclef


def Acier_Cine_Line(DMATER, args):
    """
    ACIER = Paramètes matériaux de l'acier
        E              = Module d'Young
        D_SIGM_EPSI    = Module plastique
        SY             = Limite élastique
        SIGM_LIM       = Contrainte limite pour post-traitement
        EPSI_LIM       = Déformation limite pour post-traitement

    Masse volumique, dilatation, amortissements
        RHO            = Masse volumique
        ALPHA          = Coefficient de dilatation
        AMOR_ALPHA     =
        AMOR_BETA      =
        AMOR_HYST      =
    """
        #
    MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)
    # Obligatoire E
    E = MATER['E']
    # Obligatoire SY
    SY = MATER['SY']
    #
    listepara = ['D_SIGM_EPSI', 'NU', 'SIGM_LIM', 'EPSI_LIM']
    for xx in listepara:
        if (MATER.has_key(xx)):
            if (MATER[xx] != None):
                exec('%s = %s' % (xx, MATER[xx]))
            elif (xx == 'NU'):
                NU = 0.30
            elif (xx == 'D_SIGM_EPSI'):
                D_SIGM_EPSI = E / 1.0E+04
            elif (xx == 'SIGM_LIM'):
                SIGM_LIM = SY / 1.1
            elif (xx == 'EPSI_LIM'):
                EPSI_LIM = 10.0 / 1000.0
    #
    mclef = {}
    #
    mclef['INFO'] = 1
    if (args.has_key('INFO')):
        mclef['INFO'] = args['INFO']
    #
    # Mot clef ELAS
    mclef['ELAS'] = {'E': E, 'NU': NU}
    listepara = ['RHO', 'ALPHA', 'AMOR_ALPHA', 'AMOR_BETA', 'AMOR_HYST']
    for xx in listepara:
        if (args.has_key(xx)):
            if (args[xx] != None):
                mclef['ELAS'][xx] = args[xx]
    # Mot clef MATER
    mclef['ECRO_LINE'] = {'D_SIGM_EPSI': D_SIGM_EPSI, 'SY': SY,
                          'SIGM_LIM': SIGM_LIM, 'EPSI_LIM': EPSI_LIM}
    # On affiche dans tous les cas
    message1 = FaitMessage(mclef['ELAS'])
    message2 = FaitMessage(mclef['ECRO_LINE'])
    Dico = {'EPSI_ELAS': SY / E}
    message3 = FaitMessage(Dico)
    #
    UTMESS('I', 'COMPOR1_75',
           valk=("ECRO_LINE", message1, message2, message3))
    #
    return mclef


def Ident_Endo_Fiss_Exp(ft,fc,beta,prec=1E-10,itemax=100):

    # Estimation initiale
    A = (2.0/3.0 + 3*beta**2)**0.5
    r = fc/ft
    C = 3**0.5
    L = A*(r-1)
    p0 = 2*(1-C)
    pp = (1-L)
    delta = pp**2-p0
    x = -pp+delta**0.5

    # Resolution de l'equation par methode de Newton
    for i in range(itemax):
        f  = L*x + (2+numpy.exp(-2*r*x))**0.5 - (2+numpy.exp(2*x))**0.5
        if abs(f) < prec: break
        df = L - r*numpy.exp(-2*r*x)/(2+numpy.exp(-2*r*x))**0.5 - numpy.exp(2*x)/(2+numpy.exp(2*x))**0.5
        x  = x - f/df
    else:
        UTMESS('F', 'COMPOR1_87' )

    tau  = A*x + (2+numpy.exp(2*x))**0.5
    sig0 = ft/x

    return (sig0,tau)



def ConfinedTension(nu,sig0,tau,beta,prec=1E-10,itemax=100):

    # Initialisation
    s = numpy.array((1-nu,nu,nu))
    L  = (2.0/3.0*(1-2*nu)**2 + 3*beta**2*(1+nu)**2)**0.5

    # Estimation initiale
    xe = numpy.log(tau**2-2)/(2*s[0])
    xl = tau/L
    x  = min(xe,xl)

    # Resolution de l'equation par methode de Newton
    for i in range(itemax):
        ep  = numpy.exp(x*s)
        epr = numpy.dot(ep,ep)**0.5
        f  = L*x + epr - tau
        if abs(f) < prec: break
        df = L + numpy.add.reduce(ep*ep*s)/epr
        x  = x - f/df
    else:
        UTMESS('F', 'COMPOR1_87' )

    sig1 = x*sig0*(1-nu)
    return sig1



def Endo_Fiss_Exp(DMATER,args):
    """
    ENDO_FISS_EXP = Paramètes utilisateurs de la loi ENDO_FISS_EXP
      E              = Module de Young
      NU             = Coefficient de Poisson
      FT             = Limite en traction simple
      FC             = Limite en compression simple
      GF             = Energie de fissuration
      P              = Parametre dominant de la loi cohésive asymptotique
      G_INIT         = Energie de fissuration initiale (via la pente initiale de la loi cohésive)
      Q              = Parametre secondaire de la loi cohesive asymptotique
      Q_REL          = Parametre Q exprime de maniere relative par rapport a Qmax(P)
      LARG_BANDE     = Largeur de bande d'endommagement (2*D)
      REST_RIGI_FC   = Restauration de rigidité pour eps=fc/E (0=sans)
    """

    MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)

    # Lecture et interpretation des parametres utilisateurs
    E   = float(MATER['E'])
    NU  = float(MATER['NU'])
    GF  = float(MATER['GF'])
    FT  = float(MATER['FT'])
    FC  = float(MATER['FC'])
    CRM = float(MATER['COEF_RIGI_MINI'])
    D   = float(MATER['LARG_BANDE']/2.0)
    rrc = float(MATER['REST_RIGI_FC'])

    # Valeur par défaut
    beta = 0.1

    # Parametres de la fonction seuil
    if FC/FT < 5.83 :
        UTMESS('F', 'COMPOR1_86', valr=(float(FC)/float(FT),) )
    (sig0,tau) = Ident_Endo_Fiss_Exp(FT,FC,beta)
    sigc = ConfinedTension(NU,sig0,tau,beta)


    # Parametres de la fonction d'adoucissement
    if MATER['P'] <> None:
        P = float(MATER['P'])
    else:
        G1 = float(MATER['G_INIT'])
        P  = ((3*numpy.pi*GF)/(4*G1))**(2.0/3.0) - 2
        if P<1: UTMESS('F','COMPOR1_93')      


    if MATER['Q'] <> None:
        Q = float(MATER['Q'])
    elif MATER['Q_REL'] <> None:
        qmax = (1.11375+0.565239*P-0.003322*P**2)*(1-numpy.exp(-1.98935*P)) - 0.01
        Q = qmax * float(MATER['Q_REL'])
    else:
        Q = 0.0


    # Parametres internes au modele
    rig = E*(1-NU)/((1+NU)*(1-2*NU))
    K = 0.75*GF/D
    C = 0.375*GF*D
    M = 1.5*rig*GF/(D*sigc**2)

    if M < P+2 :
        UTMESS('F','COMPOR1_94',valr=(float(M),float(P)))


    # restauration de rigidite
    if rrc == 0.0:
        gamma = 0
    else:
        gamma = -1.0/(FC/E*numpy.log(rrc))


    # Parametres pour DEFI_MATERIAU    
    mclef = {
     'ELAS':            {'E':E, 'NU':NU},
     'ENDO_FISS_EXP':   {'M':M,'P':P,'Q':Q,'K':K,'TAU':tau,'SIG0':sig0, 'BETA':beta,
                         'COEF_RIGI_MINI':CRM, 'REST_RIGIDITE':gamma},
     'NON_LOCAL':       {'C_GRAD_VARI':C, 'PENA_LAGR':1.E3*K},
     }

    mclef['INFO'] = 1
    if 'INFO' in args:
        mclef['INFO'] = args['INFO']
    return mclef


def defi_mater_gc_ops(self, MAZARS, ACIER, ENDO_FISS_EXP, **args):
    """
    C'est : un parmi : ACIER  MAZARS  ENDO_FISS_EXP
    """
    ier = 0
    # La macro compte pour 1 dans la numérotation des commandes
    self.set_icmd(1)
    DEFI_MATERIAU = self.get_cmd('DEFI_MATERIAU')
    #
    # Le concept sortant (de type mater_sdaster) est nommé 'Materiau' dans le
    # contexte de la macro
    self.DeclareOut('Materiau', self.sd)
    #
    if (MAZARS != None):
        mclef = Mazars_Unil(MAZARS[0], args)
    if (ACIER != None):
        mclef = Acier_Cine_Line(ACIER[0], args)
    if (ENDO_FISS_EXP != None):
        mclef = Endo_Fiss_Exp(ENDO_FISS_EXP[0], args)
    # Définition du matériau
    Materiau = DEFI_MATERIAU(**mclef)
