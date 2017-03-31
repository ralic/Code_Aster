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

import os


def defi_sol_equi_ops(self, TITRE, INFO, **args):
    """
       Macro DEFI_SOL_EQUI
    """
    import aster

    from Accas import _F
    from Utilitai.UniteAster import UniteAster
    from Utilitai.Utmess import UTMESS
    from Utilitai.Table import Table
    from math import log, sqrt, floor, pi, sin

  #--------------------------------------------------------------------------------
  # On importe les definitions des commandes a utiliser dans la macro
  #
    CREA_TABLE = self.get_cmd("CREA_TABLE")
    DYNA_VIBRA = self.get_cmd('DYNA_VIBRA')
    DETRUIRE = self.get_cmd('DETRUIRE')
    REST_SPEC_TEMP = self.get_cmd('REST_SPEC_TEMP')
    DEFI_INTE_SPEC = self.get_cmd('DEFI_INTE_SPEC')
    GENE_FONC_ALEA = self.get_cmd('GENE_FONC_ALEA')
    DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')
    CALC_FONCTION = self.get_cmd('CALC_FONCTION')
    IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
    IMPR_TABLE = self.get_cmd('IMPR_TABLE')
    CALC_TABLE = self.get_cmd('CALC_TABLE')
    LIRE_TABLE = self.get_cmd('LIRE_TABLE')
    LIRE_FONCTION = self.get_cmd('LIRE_FONCTION')
    RECU_FONCTION = self.get_cmd('RECU_FONCTION')
    DEFI_LIST_REEL = self.get_cmd('DEFI_LIST_REEL')
    AFFE_CHAR_MECA = self.get_cmd('AFFE_CHAR_MECA')
    AFFE_CHAR_MECA_F = self.get_cmd('AFFE_CHAR_MECA_F')
    DEFI_GROUP = self.get_cmd('DEFI_GROUP')
    LIRE_MAILLAGE=self.get_cmd('LIRE_MAILLAGE')
    CREA_MAILLAGE = self.get_cmd('CREA_MAILLAGE')
    MODI_MAILLAGE = self.get_cmd('MODI_MAILLAGE')
    # PRE_GIBI=self.get_cmd('PRE_GIBI')
    IMPR_RESU=self.get_cmd('IMPR_RESU')
    AFFE_MODELE = self.get_cmd('AFFE_MODELE')
    NUME_DDL = self.get_cmd('NUME_DDL')
    CALC_MATR_ELEM = self.get_cmd('CALC_MATR_ELEM')
    ASSE_MATRICE = self.get_cmd('ASSE_MATRICE')
    CALC_VECT_ELEM = self.get_cmd('CALC_VECT_ELEM')
    ASSE_VECTEUR = self.get_cmd('ASSE_VECTEUR')
    CREA_RESU = self.get_cmd('CREA_RESU')
    CREA_CHAMP = self.get_cmd('CREA_CHAMP')
    CALC_CHAR_SEISME = self.get_cmd('CALC_CHAR_SEISME')
    CALC_CHAMP = self.get_cmd('CALC_CHAMP')
    CALC_FONC_INTERP = self.get_cmd('CALC_FONC_INTERP')
    FORMULE = self.get_cmd('FORMULE')
    AFFE_MATERIAU = self.get_cmd('AFFE_MATERIAU')
    DEFI_MATERIAU = self.get_cmd('DEFI_MATERIAU')
    DEFI_NAPPE = self.get_cmd('DEFI_NAPPE')
    AFFE_CARA_ELEM = self.get_cmd('AFFE_CARA_ELEM')

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    self.DeclareOut("tabout", self.sd)

    # Le concept sortant (de type table_sdaster)
    # RASTAK DVe 12/2013

# calculs de deconvolution ou reconvolution sur une colonne de sol en 2D
# avec approche harmonique lineaire equivalente (courbes G et D gamma)

    titre = 'Etude Reconvolution 500m TRI'

    nbsol = 1

    nbacc = 1

    SURF = args['SURF']

# coefficient de variabilite sur le profil de modules Emax : 2/3 ou 3/2
# par exemple
    cvar = args['COEF_VARI_MATE']
    
# formulation methode d'amortissement
    if args['CORR_AMOR'] == 'OUI':
      formulation = 'LYSMER'
    else:
      formulation = 'SCHNABEL' 

# definition de l'operation a effectuer :
# input = 'CL' : l'accelerogramme est defini en champ libre et
# on calcule le signal deconvolue au Rocher Affleurant defini au toit du substratum
# input = 'RA' : l'accelerogramme est injecte au Rocher Affleurant defini au toit du substratum
# et on calcule le signal reconvolue au Rocher Affleurant defini au toit
# du substratum
    if args['LIEU_SIGNAL'] == 'AFFLEURANT':
        input = 'RA'
    elif args['LIEU_SIGNAL'] == 'CHAMP_LIBRE':
        input = 'CL'

    if args['LIAISON'] == 'PERIODIQUE':
        lliaison = 'OUI'
    elif args['LIAISON'] == 'SANS':
        lliaison = 'NON'

# Possibilite d utiliser des masses penalisées :
# lmassp ='OUI' ou 'NON'
    lmassp = 'NON'
    if args['UNITE_TRAN_INIT'] != None :
      input = 'RA'
      lliaison = 'NON'
      if args['MASS_PENA'] != None:
        lmassp = 'OUI'

# Possibilite d utiliser une longueur caracteristique :
# llcara ='OUI' ou 'NON'
    llcara = 'NON'
    if args['UNITE_TRAN_INIT'] != None :
      if args['LONG_CARA'] != None :
        llcara = 'OUI'

# Possibilite de faire une verification en temporel avec coeff Rayleigh :
# veriftmp ='OUI' ou 'NON'

    veriftmp = 'NON'

# Definition de  l'acceleration maximale du sol
# ATTENTION : verifier les unites m/s2 ou g
# coefu = 1;
    coefu = args['COEF_AMPL_ACCE']
    kh = 1
    zpa = 1
    coefzpa = kh / zpa
# frequence de coupure pour calcul des FDT
   # fcoup = 35.
    fcoup = args['FREQ_COUP']

# Groupes de mailles et maillage
    mail0 = args['MAILLAGE']
    grma_droit = args['GROUP_MA_DROITE']
    grma_gauch = args['GROUP_MA_GAUCHE']
    grma_subst = args['GROUP_MA_SUBSTR']
    grma_colon = args['GROUP_MA_COL']
    if args['GROUP_MA_LATE'] != None:
      grma_late = args['GROUP_MA_LATE']
    else:
      grma_late = 'LATE'
#

# impression des graphiques au format postscipt 'EPS' ou '' pour format GRACE
    pilimp = ''

# frequences pour le calcul des coefficients de Rayleigh
    f1 = 1.
    f2 = 15.
# definition des parametres alpha = aamult*AH et beta = abmult*AH
    aamult = 1 / (2. * 3.14 * (f1 + f2))
    abmult = 2. * 3.14 * f1 * f2 / (f1 + f2)
# definition d un facteur de correction sur les parametres de Rayleigh pour obtenir
# l amortissement hysteretique en moyenne sur la plage de frequence
    ca = 1 / (0.5 + f1 * f2 / (f1 + f2) * log(f2 / f1) / (f2 - f1))
    text = ('aamult=' + str(aamult) + ' abmult=' + str(abmult) + ' ca=' + str(ca) )
    aster.affiche('MESSAGE', text)

# coefficient pour calcul gamma_eff / gamma_max
    geff = args['COEF_GAMMA']
    # 0.65

# tolerance pour convergence sur evolution module E
    tole = args['RESI_RELA']
    # 0.05
    nmaxit = args['NMAX_ITER']
    # 10; # nombre maximal d'iteration
#  unites tables resu
    utabresu = args['UNITE_TABLE_RESU']
    utabtran = args['UNITE_RESU_TRAN']
    utabspec = args['UNITE_RESU_SPEC']

# Parametres FFT ### TRES IMPORTANT ###
    if args['UNITE_TRAN_INIT'] != None :
      utabtranin = args['UNITE_TRAN_INIT']
      __fonc_acce=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[1,1],INDIC_RESU=[1,3],
                   );
    else :
      fonc_acce = args['FONC_SIGNAL']
      __fonc_acce = CALC_FONCTION(COMB=(
                       _F(FONCTION=fonc_acce, COEF=1.0),
                             ),
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',                        
                         );

    tt, vale_s = __fonc_acce.Valeurs()
    dt = tt[1] - tt[0]
    nbdt = len(vale_s)
    tfin = tt[nbdt - 1]
    if args['CHARGEMENT'] == 'ONDE_PLANE':
       n = 0
       while n < 50 :
          if 2**n >= nbdt :
             nbdt = 2**n
             break
          n = n+1
    tfin = (nbdt-1)*dt
    text = ('tfin=' + str(tfin) )
    aster.affiche('MESSAGE', text)

    fmax = 0.5 / dt  # facteur 1/2 car FFT calculee avec SYME='NON'
    df = 2.0 * fmax / nbdt
    fmax = (0.5 / dt) - df
    NB4 = 4*int(floor(nbdt / 4))
    if NB4 < nbdt :
       text = ('NB4 =' + str(NB4) + ' < NBDT =' + str(nbdt) 
               + ' non multiple de 4')
       aster.affiche('MESSAGE', text)

    if args['FREQ_COUP'] != None:
        fcoup = args['FREQ_COUP']
    else:
        fcoup = fmax
    fmaxc = fcoup
    if fmax < fcoup:
        fmaxc = fmax
    text = ('df=' + str(df) + ' fmax=' + str(fmax) + ' fmaxc=' + str(fmaxc) )
    aster.affiche('MESSAGE', text)

# On genere les frequences
    __lfreq = DEFI_LIST_REEL(DEBUT=0.0, INTERVALLE=_F(JUSQU_A=fmax, PAS=df),)
    __lfreq0 = DEFI_LIST_REEL(DEBUT=df, INTERVALLE=_F(JUSQU_A=fmax, PAS=df),)

# On genere les pas de temps pour lire laccelerogramme a deconvoluer
    __linst = DEFI_LIST_REEL(DEBUT=0.0, INTERVALLE=_F(JUSQU_A=tfin, PAS=dt),)

# definition des frequences de calcul
    __lfreqc = DEFI_LIST_REEL(
        DEBUT=0.1, INTERVALLE=_F(JUSQU_A=fmaxc, PAS=0.1),)

# Definition de la fonction unite et du filtre en frequence
    __UN = DEFI_FONCTION(NOM_PARA='FREQ', VALE=(0., 1., fmax, 1.,),
                         INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',)

    __FILTRE = DEFI_FONCTION(NOM_PARA='FREQ',
                             VALE_C=(0., 0., 0., df, 1., 0., fmaxc, 1., 0., (fmaxc+df), 0., 0.),
                                 INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',)

    __FILTRE2 = DEFI_FONCTION(NOM_PARA='FREQ',
                             VALE_C=(0., 0., 0., df, -1., 0., fmaxc, -1., 0., (fmaxc+df), 0., 0.),
                                 INTERPOL='LIN', PROL_DROITE = 'CONSTANT', PROL_GAUCHE = 'CONSTANT',)

#
# Lecture des acceleros CL
#
    ifsax = []

# for a in range (1,nbacc+1):
    a = 1

    __ACCEX = CALC_FONCTION(LIST_PARA=__linst,
                            PROL_DROITE='CONSTANT',
                            PROL_GAUCHE='CONSTANT', 
        COMB=_F(FONCTION=__fonc_acce, COEF=coefu * coefzpa))

    if args['LIST_FREQ_SPEC_OSCI'] != None:
      __SAX = CALC_FONCTION(
        SPEC_OSCI=_F(FONCTION=__ACCEX,AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))
    else:
      __SAX = CALC_FONCTION(
        SPEC_OSCI=_F(FONCTION=__ACCEX, AMOR_REDUIT=0.05, NORME=9.81))
    ifsax.append(
        _F(FONCTION=__SAX, MARQUEUR=0, LEGENDE='SAXi_' + str(a)),)

# On calcule la FFT

    __AHX = CALC_FONCTION(FFT=_F(FONCTION=__ACCEX, METHODE='COMPLET',))

    __ASPEFCI = DEFI_FONCTION ( NOM_PARA='FREQ', 
                          VALE_C=(
            0.0    ,  1.0,  0.0,
            df     ,  1.0,  0.0,
            fmaxc   ,  1.0,  0.0,
            fmaxc+df  ,  0.0,  0.0,
                  ),
           PROL_DROITE      =  'CONSTANT' ,
           PROL_GAUCHE     =  'CONSTANT' ,
    )
   
    __ACI01F = DEFI_INTE_SPEC ( DIMENSION= 1,
                  PAR_FONCTION =_F( NUME_ORDRE_I = 1,
                                    NUME_ORDRE_J = 1,
                                    FONCTION  =  __ASPEFCI ,),
                   INFO= 1,
                              )
                              

    __VCIF01X = GENE_FONC_ALEA ( INTE_SPEC =  __ACI01F,
                             DUREE_TIRAGE   = tfin+dt ,
                             FREQ_INIT=0.,
                             FREQ_FIN=fmax,
                             NB_TIRAGE=1,
                             INFO= 1
                              )


    __FCI01FX = RECU_FONCTION(INTE_SPEC=__VCIF01X ,
                     NUME_ORDRE = 1,
                     INTERPOL='LIN',
                     PROL_DROITE='CONSTANT',
                     PROL_GAUCHE='CONSTANT',
                    )

    __ACCEX0 = CALC_FONCTION(COMB=(
                       _F(FONCTION=__FCI01FX, COEF=1.0/sqrt(2.0*pi*fmax) ),
                             ),
                        LIST_PARA=__linst,
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',                        
                         );
                            
    __ACCEX0H = CALC_FONCTION(FFT=_F(FONCTION=__ACCEX0, METHODE='COMPLET',))
    if input == 'CL' :
      __VITEX = CALC_FONCTION(INTEGRE=_F(FONCTION=__ACCEX0, ))
    if input == 'RA' :
      __VITEX = CALC_FONCTION(INTEGRE=_F(FONCTION=__ACCEX, ))

#
# DEBUT DE LA BOUCLE       #
#
# for s in range (1,nbsol+1):
#  for v in range (1,len(cvar)+1) :
#    for a in range (1,nbacc+1):
    a = 1
    v = 1
    s = 1

    deltaE = 100
    # initialisation de l erreur sur E
    iter = 0
    # indicateur du nombre d iterations
    etat = 'debut'

    legendeT = 's' + str(s) + 'v' + str(v) + 'a' + str(a)
    legende = '-acce' + str(a) + '-sol' + str(s) + '-cvar=' + str(v)
    if args['TABLE_MATER_ELAS'] != None:
        __TMAT = CALC_TABLE( TABLE=args['TABLE_MATER_ELAS'],
         ACTION=_F(OPERATION='EXTR',NOM_PARA=('Y','M','RHO','Emax','NU','AH','GDgam')));
        tmat = __TMAT.EXTR_TABLE()
        NCOU = len(tmat) - 1
        text = ('NCOU=' + str(NCOU) )
        aster.affiche('MESSAGE', text)
        nbmat = 0
        for k in range(1, NCOU + 2):
            if __TMAT['GDgam', k] > nbmat:
                nbmat = __TMAT['GDgam', k]
        text = ('nbmat=' + str(nbmat) )
        aster.affiche('MESSAGE', text)
        __GG = [None] * (nbmat + 1)
        __DG = [None] * (nbmat + 1)

        UL = UniteAster()
        utabequ = UL.Libre(action='ASSOCIER')
        IMPR_TABLE(TABLE=args['TABLE_GEQUI_GMAX'], UNITE=utabequ)
        IMPR_TABLE(TABLE=args['TABLE_AMOR_EQUI'], UNITE=utabequ)

        for j in range(1, nbmat + 1):
            __GG[j] = LIRE_FONCTION(UNITE=utabequ, NOM_PARA='EPSI',
                                    INTERPOL=('LOG','LIN'), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
                                    INDIC_PARA=[1, 1], INDIC_RESU=[1, j + 1],
                                    )
            __DG[j] = LIRE_FONCTION(UNITE=utabequ, NOM_PARA='EPSI',
                                    INTERPOL=('LOG','LIN'), PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',
                                    INDIC_PARA=[2, 1], INDIC_RESU=[2, j + 1],
                                    )
        UL.EtatInit()
    else:
    # 1. dictionnaires des MATERIAUX
        MATERIAU = args['MATERIAU']
        l_mate = []
        for Mi in MATERIAU:
            dM = Mi.cree_dict_valeurs(Mi.mc_liste)
            l_mate.append(dM)
        nbmat = len(l_mate)

    # 2. dictionnaires des COUCHES
        COUCHE = args['COUCHE']
        l_couche = []
        for Ci in COUCHE:
            dC = Ci.cree_dict_valeurs(Ci.mc_liste)
            l_couche.append(dC)
        NCOU = len(l_couche) - 1

        __GG0 = [None] * (nbmat + 1)
        __DG0 = [None] * (nbmat + 1)
        __GG = [None] * (nbmat + 1)
        __DG = [None] * (nbmat + 1)

        j = 0
        for mate in l_mate:
            j += 1
            l_para = mate["GAMMA"]
            vale_G = mate["G_GMAX"]
            vale_D = mate["D"]
            __GG0[j] = DEFI_FONCTION(NOM_PARA='EPSI', NOM_RESU='G_Gmax',
                                    INTERPOL=('LOG','LIN'),
                                    PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                                    ORDONNEE=tuple(vale_G),
                                    ABSCISSE=tuple(l_para))

            __DG0[j] = DEFI_FONCTION(NOM_PARA='EPSI', NOM_RESU='D',
                                    INTERPOL=('LOG','LIN'),
                                    PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',
                                    ORDONNEE=tuple(vale_D),
                                    ABSCISSE=tuple(l_para))
                                    
        __lpara = DEFI_LIST_REEL(VALE=tuple(l_para))

        for j in range(1, nbmat + 1):
            if args['LIST_EPSI'] != None:
              __GG[j] = CALC_FONCTION(
              COMB=_F(FONCTION=__GG0[j], COEF=1.), LIST_PARA=args['LIST_EPSI'],
              NOM_PARA='EPSI', NOM_RESU='G_Gmax',
              INTERPOL=('LOG','LIN'),
              PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT', 
              )
              __DG[j] = CALC_FONCTION(
              COMB=_F(FONCTION=__DG0[j], COEF=1.), LIST_PARA=args['LIST_EPSI'],
              NOM_PARA='EPSI', NOM_RESU='D',
              INTERPOL=('LOG','LIN'),
              PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT', 
              )
            else:
              __GG[j] = CALC_FONCTION(
              COMB=_F(FONCTION=__GG0[j], COEF=1.), LIST_PARA=__lpara,
              NOM_PARA='EPSI', NOM_RESU='G_Gmax',
              INTERPOL=('LOG','LIN'),
              PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT', 
              )
              __DG[j] = CALC_FONCTION(
              COMB=_F(FONCTION=__DG0[j], COEF=1.), LIST_PARA=__lpara,
              NOM_PARA='EPSI', NOM_RESU='D',
              INTERPOL=('LOG','LIN'),
              PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT', 
              )

            if j == 1:
                __tabfgg = CREA_TABLE(
                    FONCTION=_F(FONCTION=__GG[j], PARA=('EPSI', 'GG' + str(j))))
                __tabfdg = CREA_TABLE(
                    FONCTION=_F(FONCTION=__DG[j], PARA=('EPSI', 'DG' + str(j))))

            else:
                __tabtmpgg = CREA_TABLE(
                    FONCTION=_F(FONCTION=__GG[j], PARA=('EPSI', 'GG' + str(j))))
                __tabtmpdg = CREA_TABLE(
                    FONCTION=_F(FONCTION=__DG[j], PARA=('EPSI', 'DG' + str(j))))
                __tabfgg = CALC_TABLE(reuse=__tabfgg, TABLE=__tabfgg,
                                      ACTION=_F(OPERATION='COMB', TABLE=__tabtmpgg, NOM_PARA='EPSI'))

                __tabfdg = CALC_TABLE(reuse=__tabfdg, TABLE=__tabfdg,
                                      ACTION=_F(OPERATION='COMB', TABLE=__tabtmpdg, NOM_PARA='EPSI'))

                DETRUIRE(CONCEPT=_F(NOM=(__tabtmpgg, __tabtmpdg,),))

        IMPR_TABLE(TABLE=__tabfgg, UNITE=6,
                   TITRE=('Table des fonctions G_GMAX de Gamma par materiau'),)
        IMPR_TABLE(TABLE=__tabfdg, UNITE=6,
                   TITRE=('Table des fonctions AMOR de Gamma par materiau'),)

    # definition de la table initiale
    # para/typ pre-trie les colonnes
        tabini = Table(para=["Y", "M", "RHO", "Emax", "NU", "AH", "GDgam"],
                       typ=["R", "K8", "R", "R", "R", "R", "I"])

        Y = 0.
        for couche in l_couche:
            Y = Y + couche["EPAIS"]
            id_mate = couche["NUME_MATE"]
            tabini.append(
                {'RHO': couche["RHO"], 'NU': couche["NU"], 'Emax': couche["E"],
                 'M': couche["GROUP_MA"], 'Y': Y, 'GDgam': id_mate,
                 'AH': couche["AMOR_HYST"]})

     # creation de la table
        dprod = tabini.dict_CREA_TABLE()
        __TMAT = CREA_TABLE(**dprod)

    IMPR_TABLE(TABLE=__TMAT, UNITE=6)

    if SURF == 'NON' :
        NCOU2 = args['NIVE_COUCH_ENFO']
        if NCOU2 > NCOU:
            NCOU2 = NCOU
        # nombre de sous-couches par couche enfoncee        
        nsco = args['NB_RECEPTEUR']

    if args['NOM_CMP'] == 'DY':
       nmaxit = 2

# Lecture du maillage
    if args['MAILLAGE'] != None:
      __mailla = CREA_MAILLAGE(MAILLAGE=mail0,
                             CREA_POI1=(
                             _F(NOM_GROUP_MA='PCOL',
                                GROUP_MA=grma_colon,),
                             _F(NOM_GROUP_MA='PLATE',
                                GROUP_MA=(grma_droit,grma_gauch,),),
                             ),
                             )

      __mailla = DEFI_GROUP(reuse=__mailla,
                          MAILLAGE=__mailla,
                          CREA_GROUP_NO=_F(GROUP_MA=(grma_droit,),),
                          )
      if args['GROUP_MA_LATE'] == None:
        __mailla = DEFI_GROUP(reuse=__mailla,
                          MAILLAGE=__mailla,
                          CREA_GROUP_MA=_F(NOM=grma_late, UNION=(grma_droit,grma_gauch,),),
                          )

      __mailla = DEFI_GROUP(reuse=__mailla,
                          MAILLAGE=__mailla,
                          CREA_GROUP_NO=_F(GROUP_MA=(grma_gauch,),),
                          )
      __mailla = DEFI_GROUP(reuse=__mailla,
                          MAILLAGE=__mailla,
                          CREA_GROUP_NO=_F(GROUP_MA=('PLATE',),),
                          )
    else:
      text = ('NCOU=' + str(NCOU))
      aster.affiche('MESSAGE', text)
      NCOUB=NCOU+1
      larcol=1.0
      resultfile = open("dse.mail", 'w')
      resultfile.write( " TITRE \n")
      resultfile.write( " %  GIBI FECIT \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " COOR_3D \n")
      resultfile.write( " N1        0.0000000000000D+00  0.000000000000D+00  0.0000000000000D+00 \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " N"+str(k+1)+"        0.0000000000000D+00  "+str(-1*__TMAT['Y',k])+"   0.0000000000000D+00 \n")
      resultfile.write( " N"+str(NCOUB+2)+"        "+str(larcol)+"000000000000D+00  0.0000000000000D+00  0.0000000000000D+00 \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " N"+str(NCOUB+2+k)+"        "+str(larcol)+"000000000000D+00  "+str(-1*__TMAT['Y',k])+" 0.0000000000000D+00 \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " QUAD4  \n")  
      for k in range(1,NCOUB+1):     
        resultfile.write( "  M"+str(k)+"       N"+str(k)+"       N"+str(k+1)+"       N"+str(NCOUB+2+k)+"      N"+str(NCOUB+1+k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " SEG2   \n") 
      resultfile.write( " M"+str(NCOUB+1)+"      N"+str(NCOUB+1)+"      N"+str(2*NCOUB+2)+"      \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      j = NCOUB+1
      resultfile.write( " SEG2   \n")
      for k in range(1,NCOUB+1):
        j = j+1     
        resultfile.write( "  M"+str(j)+"       N"+str(k)+"       N"+str(k+1)+" \n")
      for k in range(1,NCOUB+1):
        j = j+1     
        resultfile.write( "  M"+str(j)+"       N"+str(NCOUB+1+k)+"      N"+str(NCOUB+2+k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n") 
      for k in range(1,NCOUB+2):     
        resultfile.write( " GROUP_NO \n")
        resultfile.write( " P"+str(k-1)+"       N"+str(k)+"       \n")
        resultfile.write( " FINSF \n")
        resultfile.write( " % \n")
      resultfile.write( " GROUP_MA \n")
      resultfile.write( str(grma_colon)  + " \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " M"+str(k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " GROUP_MA \n")
        resultfile.write( str(__TMAT['M',k])+"       \n")
        resultfile.write( " M"+str(k)+"       \n")
        resultfile.write( " FINSF \n")
        resultfile.write( " % \n")
      resultfile.write( " GROUP_MA \n")
      resultfile.write( str(grma_subst)  + " \n")
      resultfile.write( " M"+str(NCOUB+1)+"      \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " GROUP_MA \n")
      resultfile.write( str(grma_late)  + " \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " M"+str(NCOUB+1+k)+" \n")
      for k in range(1,NCOUB+1):     
        resultfile.write( " M"+str(2*NCOUB+1+k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      for k in range(1,NCOUB+1):
        resultfile.write( " GROUP_MA \n")
        resultfile.write( " G"+str(__TMAT['M',k])+"      \n")
        resultfile.write( " M"+str(NCOUB+1+k)+"       \n")
        resultfile.write( " FINSF \n")
        resultfile.write( " % \n")
        resultfile.write( " GROUP_MA \n")
        resultfile.write( " D"+str(__TMAT['M',k])+"     \n")
        resultfile.write( " M"+str(2*NCOUB+1+k)+"       \n")
        resultfile.write( " FINSF \n")
        resultfile.write( " % \n")
        resultfile.write( " GROUP_MA \n")
        resultfile.write( " L"+str(__TMAT['M',k])+"     \n")
        resultfile.write( " M"+str(NCOUB+1+k)+"       \n")
        resultfile.write( " M"+str(2*NCOUB+1+k)+"       \n")
        resultfile.write( " FINSF \n")
        resultfile.write( " % \n")
      resultfile.write( " GROUP_NO \n")
      resultfile.write( str(grma_gauch)  + " \n")
      for k in range(1,NCOUB+2):     
        resultfile.write( " N"+str(k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " GROUP_NO \n")
      resultfile.write( str(grma_droit)  + " \n")
      for k in range(1,NCOUB+2):     
        resultfile.write( " N"+str(NCOUB+1+k)+" \n")
      resultfile.write( " FINSF \n")
      resultfile.write( " % \n")
      resultfile.write( " FIN \n")
  
      resultfile.close()

      UL = UniteAster()
      umail = UL.Libre(action='ASSOCIER', nom='dse.mail')
      __mail0=LIRE_MAILLAGE(UNITE=umail, FORMAT='ASTER');
      __mailla = CREA_MAILLAGE(MAILLAGE=__mail0,
                             CREA_POI1=(
                             _F(NOM_GROUP_MA='PCOL',
                                GROUP_MA=grma_colon,),
                             _F(NOM_GROUP_MA='PLATE',
                                GROUP_MA=(grma_late,),),
                             ),
                              )
      __mailla = DEFI_GROUP(reuse=__mailla,
                          MAILLAGE=__mailla,
                          CREA_GROUP_NO=_F(GROUP_MA=('PLATE',),),
                           )
      UL.EtatInit()
    if args['UNITE_TRAN_INIT'] != None :
      NPC = NCOU+3 
      # __faccX=[None]*NPC
      l_para = []
      l_foncx = []
      l_vitex = []
      l_deplx = []

      for k in range(1, NPC):
         if k < NPC-1 :
           if lmassp == 'OUI':
             __faccex0=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[1,1],INDIC_RESU=[1,NPC+2-k],
                   );
           else:
             __fvitex=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[2,1],INDIC_RESU=[2,NPC+2-k],
                   );
             __fdeplx=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[3,1],INDIC_RESU=[3,NPC+2-k],
                   );
           l_para.append(-1.0*__TMAT['Y',NPC-1-k])
         else:
           if lmassp == 'OUI':
             __faccex0=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[1,1],INDIC_RESU=[1,2],
                   );
           else:
             __fvitex=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[2,1],INDIC_RESU=[2,2],
                   );
             __fdeplx=LIRE_FONCTION(UNITE=utabtranin,NOM_PARA='INST',
               INTERPOL = 'LIN',PROL_DROITE = 'CONSTANT',
               INDIC_PARA=[3,1],INDIC_RESU=[3,2],
                   );
           l_para.append(0.0)           
         if lmassp == 'OUI':
           __faccX=CALC_FONCTION(COMB=_F(FONCTION=__faccex0,COEF=-1.0*args['MASS_PENA'],),);
           DETRUIRE(CONCEPT=_F(NOM=(__faccex0,),), INFO = 1);
           l_foncx.append(__faccX)
         else:
           l_vitex.append(__fvitex)
           l_deplx.append(__fdeplx)
      if lmassp == 'OUI':  
        __NSEISMX = DEFI_NAPPE(NOM_PARA='Y', INTERPOL=('LIN','LIN',),                     
                               PROL_GAUCHE='CONSTANT', PROL_DROITE='CONSTANT',
                               PARA= tuple(l_para), FONCTION=tuple(l_foncx),);
      else:
        __FONCVX = DEFI_NAPPE(NOM_PARA='X', INTERPOL=('LIN','LIN',),                     
                               PROL_GAUCHE='CONSTANT', PROL_DROITE='CONSTANT',
                               PARA= tuple(l_para), FONCTION=tuple(l_vitex),);
        __FONCDX = DEFI_NAPPE(NOM_PARA='X', INTERPOL=('LIN','LIN',),                     
                               PROL_GAUCHE='CONSTANT', PROL_DROITE='CONSTANT',
                               PARA= tuple(l_para), FONCTION=tuple(l_deplx),);

    #IMPR_RESU(RESU=_F(MAILLAGE=__mailla,));
    if lliaison == 'OUI':
      __mailla=MODI_MAILLAGE(reuse =__mailla,
                MAILLAGE=__mailla,
                ORIE_PEAU_2D=_F(GROUP_MA=grma_subst,GROUP_MA_SURF=grma_colon,),
                );
      __MODELE = AFFE_MODELE(MAILLAGE=__mailla,
                           AFFE=(
                           _F(GROUP_MA=grma_colon, PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN',),
                           _F(GROUP_MA=grma_subst, PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN_ABSO',),
                           ),)

# Conditions de periodicite: relation d'egalite entre les deplacements
# des noeuds des bords droit et gauche du modele
      __CON_LIM = AFFE_CHAR_MECA(MODELE=__MODELE,
                               LIAISON_GROUP=
                              (_F(GROUP_NO_1=grma_gauch,
                                  GROUP_NO_2=grma_droit,
                                  DDL_1=('DX',),
                                  DDL_2=('DX',),
                                  COEF_MULT_1=1.,
                                  COEF_MULT_2=-1.,
                                  COEF_IMPO=0.,),
                                  _F(GROUP_NO_1=grma_gauch,
                                     GROUP_NO_2=grma_droit,
                                     DDL_1=('DY',),
                                     DDL_2=('DY',),
                                     COEF_MULT_1 = 1.,
                                     COEF_MULT_2 = -1.,
                                     COEF_IMPO = 0.,),
                               ),
                               )
    else:

      __mailla=MODI_MAILLAGE(reuse =__mailla,
                MAILLAGE=__mailla,
                ORIE_PEAU_2D=_F(GROUP_MA=(grma_subst,grma_late),GROUP_MA_SURF=grma_colon,),
                );
      if lmassp == 'OUI':
        __MODELE = AFFE_MODELE(MAILLAGE=__mailla,
                           AFFE=(
                           _F(GROUP_MA=grma_colon, PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN',),
                           _F(GROUP_MA='PLATE', PHENOMENE='MECANIQUE',
                              MODELISATION='2D_DIS_T',),
                           _F(GROUP_MA=(grma_subst,grma_late), PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN_ABSO',),
                           ),)

        __ELEM=AFFE_CARA_ELEM(MODELE=__MODELE,
                    DISCRET=(
                             _F(GROUP_MA='PLATE',
                                    REPERE='GLOBAL',
                                    CARA='M_T_D_N',
                                    VALE=args['MASS_PENA']),
                            ),        
                    );
      else:
        __MODELE = AFFE_MODELE(MAILLAGE=__mailla,
                           AFFE=(
                           _F(GROUP_MA=grma_colon, PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN',),
                           _F(GROUP_MA=(grma_subst,grma_late), PHENOMENE='MECANIQUE',
                              MODELISATION='D_PLAN_ABSO',),
                           ),)

    # On genere la liste initiale pour Emax AH ratio G/Gmax et des profondeurs
    E = [[]] * nmaxit
    AH = [[]] * nmaxit
    rat = [[]] * nmaxit

    #E[0] = [] * (NCOU + 1)
    #AH[0] = [] * (NCOU + 1)
    #rat[0] = [] * (NCOU + 1)
    E[0] = [] * (NCOU + 2)
    AH[0] = [] * (NCOU + 2)
    rat[0] = [] * (NCOU + 2)
    lprof = []
    lprof2 = []

    lprof.append(0.)
    lprof2.append(0.)
    E[0].append(0)
    AH[0].append(0)
    rat[0].append(1)
    self.update_const_context({'cvar': cvar})
    __fEmax = FORMULE(NOM_PARA=('Emax'), VALE = 'cvar*Emax')
    __fAH = FORMULE(NOM_PARA=('AH'), VALE = 'AH')

    __TMAT = CALC_TABLE(reuse=__TMAT, TABLE=__TMAT,
                        ACTION=(
                                _F(OPERATION = 'OPER',
                                   FORMULE=__fEmax, NOM_PARA = 'E0',),
                                _F(OPERATION = 'OPER',
                                   FORMULE=__fAH, NOM_PARA = 'AH0',),
                                ))

    #for k in range(1, NCOU + 1):
    for k in range(1, NCOU + 2):
        if k < NCOU+1 :
          lprof.append(__TMAT['Y', k])
          rat[0].append(1)
        lprof2.append(__TMAT['Y', k])
        E[0].append(__TMAT['E0', k])
        AH[0].append(__TMAT['AH0', k])

    while etat <> 'fin':
        text = ('iter=' + str(iter))
        aster.affiche('MESSAGE', text)

        if iter == 0:

            __SOLH = [None] * (NCOU + 2)

            for j in range(1, NCOU + 1):
              if formulation == 'LYSMER':
                if llcara == 'OUI':
                  __SOLH[j] = DEFI_MATERIAU(
                                ELAS=_F(E=__TMAT['E0', j]*(1.-__TMAT['AH0',j]*__TMAT['AH0',j]/2),
                                RHO=__TMAT['RHO', j],
                                NU=__TMAT['NU', j],
                                LONG_CARA=args['LONG_CARA'],
                                AMOR_HYST=__TMAT['AH0', j]*sqrt(1-__TMAT['AH0',j]*__TMAT['AH0',j]/4)/(1.-__TMAT['AH0',j]*__TMAT['AH0',j]/2),
                                ))
                else:
                  __SOLH[j] = DEFI_MATERIAU(
                                ELAS=_F(E=__TMAT['E0', j]*(1.-__TMAT['AH0',j]*__TMAT['AH0',j]/2),
                                RHO=__TMAT['RHO', j],
                                NU=__TMAT['NU', j],
                                AMOR_HYST=__TMAT['AH0', j]*sqrt(1-__TMAT['AH0',j]*__TMAT['AH0',j]/4)/(1.-__TMAT['AH0',j]*__TMAT['AH0',j]/2),
                                ))
              else:                                
                if llcara == 'OUI':
                  __SOLH[j] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', j],
                                                  RHO=__TMAT['RHO', j],
                                                  NU=__TMAT['NU', j],
                                                  LONG_CARA=args['LONG_CARA'],
                                                  AMOR_HYST=__TMAT['AH0', j],
                                                  ))
                else:
                  __SOLH[j] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', j],
                                                  RHO=__TMAT['RHO', j],
                                                  NU=__TMAT['NU', j],
                                                  AMOR_HYST=__TMAT['AH0', j],
                                                  ))

            if llcara == 'OUI':
              __SOLH[NCOU + 1] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                                     RHO=__TMAT[
                                                         'RHO', NCOU + 1],
                                                     NU=__TMAT['NU', NCOU + 1],
                                                     LONG_CARA=args['LONG_CARA'],
                                                     AMOR_HYST=__TMAT[
                                                         'AH0', NCOU + 1],
                                                     ))
            else:
              __SOLH[NCOU + 1] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                                     RHO=__TMAT[
                                                         'RHO', NCOU + 1],
                                                     NU=__TMAT['NU', NCOU + 1],
                                                     AMOR_HYST=__TMAT[
                                                         'AH0', NCOU + 1],
                                                     ))

        else:

            __SOLH = [None] * (NCOU + 2)

            for j in range(1, NCOU + 1):
              if formulation == 'LYSMER':
                if llcara == 'OUI':
                  __SOLH[j] = DEFI_MATERIAU(
                                ELAS=_F(E=__TMAT[('E' + str(iter)), j]*(1.-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/2),
                                RHO=__TMAT['RHO', j],
                                NU=__TMAT['NU', j],
                                LONG_CARA=args['LONG_CARA'],
                                AMOR_HYST=__TMAT[('AH' + str(iter)), j]*sqrt(1-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/4)/(1.-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/2),
                                ))
                else:
                  __SOLH[j] = DEFI_MATERIAU(
                                ELAS=_F(E=__TMAT[('E' + str(iter)), j]*(1.-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/2),
                                RHO=__TMAT['RHO', j],
                                NU=__TMAT['NU', j],
                                AMOR_HYST=__TMAT[('AH' + str(iter)), j]*sqrt(1-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/4)/(1.-__TMAT[('AH'+str(iter)),j]*__TMAT[('AH'+str(iter)),j]/2),
                                ))
              else:            
                if llcara == 'OUI':
                  __SOLH[j] = DEFI_MATERIAU(
                    ELAS=_F(E=__TMAT[('E' + str(iter)), j],
                            RHO=__TMAT['RHO', j],
                            NU=__TMAT['NU', j],
                            LONG_CARA=args['LONG_CARA'],
                            AMOR_HYST=__TMAT[('AH' + str(iter)), j],
                            ))
                else:
                  __SOLH[j] = DEFI_MATERIAU(
                    ELAS=_F(E=__TMAT[('E' + str(iter)), j],
                            RHO=__TMAT['RHO', j],
                            NU=__TMAT['NU', j],
                            AMOR_HYST=__TMAT[
                            ('AH' + str(iter)), j],
                            ))
            if llcara == 'OUI':
              __SOLH[NCOU + 1] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                                     RHO=__TMAT[
                                                         'RHO', NCOU + 1],
                                                     NU=__TMAT['NU', NCOU + 1],
                                                     LONG_CARA=args['LONG_CARA'],
                                                     AMOR_HYST=__TMAT['AH0', NCOU + 1],
                                                     ))
            else:
              __SOLH[NCOU + 1] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                                     RHO=__TMAT[
                                                         'RHO', NCOU + 1],
                                                     NU=__TMAT['NU', NCOU + 1],
                                                     AMOR_HYST=__TMAT['AH0', NCOU + 1],
                                                     ))

            # Le substratum est considere comme elastique et garde les proprietes initiales
            # La valeur d'amortissement specifiee n'est pas prise en compte  :
            # l'amortissement des FA (rho.Vs) est calcule avec l'option
            # AMOR_MECA

        __SOLHSUBS = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                           RHO=__TMAT['RHO', NCOU + 1],
                                           NU=__TMAT['NU', NCOU + 1],
                                           AMOR_HYST=__TMAT['AH0', NCOU + 1],
                                           ))

            # Boucle pour affectation du materiau j au GROUP_MA 'Mj'

        tSOLH = tuple(__SOLH)

        affmat = []

        for j in range(1, NCOU + 2):

            affmat.append(_F(GROUP_MA=__TMAT['M', j], MATER=tSOLH[j]))
            if args['MAILLAGE'] == None:
              affmat.append(_F(GROUP_MA='L'+__TMAT['M', j], MATER=tSOLH[j]))
              #affmat.append(_F(GROUP_MA='G'+__TMAT['M', j], MATER=tSOLH[j]))
              #affmat.append(_F(GROUP_MA='D'+__TMAT['M', j], MATER=tSOLH[j]))

        affmat.append(_F(GROUP_MA=grma_subst, MATER=__SOLHSUBS))

        __CHAMPMAH = AFFE_MATERIAU(MAILLAGE=__mailla,
                                   AFFE=affmat,
                                   )
        if lliaison == 'OUI':
            # MATRICE DE RIGIDITE ELEMENTAIRE

          __RIGI_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA',
                                    MODELE=__MODELE, CHAM_MATER=__CHAMPMAH, CHARGE=__CON_LIM,)
            # Amor meca hyst pour colonne

          __RIGH_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA_HYST',
                                    MODELE=__MODELE, CHAM_MATER=__CHAMPMAH, RIGI_MECA=__RIGI_ELH, CHARGE=__CON_LIM,)
            # MATRICE DE MASSE ELEMENTAIRE

          __MASS_ELH = CALC_MATR_ELEM(OPTION='MASS_MECA',
                                    MODELE=__MODELE,
                                    CHAM_MATER=__CHAMPMAH,
                                    CHARGE=__CON_LIM,)
            # MATRICE D AMORTISSEMENT ELEMENTAIRE (pour prise en compte
            # amortissement frontiere absorbante)

          __AMOR_ELH = CALC_MATR_ELEM(OPTION='AMOR_MECA',
                                    MODELE=__MODELE,
                                    CHAM_MATER=__CHAMPMAH,
                                    RIGI_MECA=__RIGI_ELH,
                                    MASS_MECA=__MASS_ELH,
                                    CHARGE=__CON_LIM,)
        else:
          if lmassp == 'OUI':
            # MATRICE DE RIGIDITE ELEMENTAIRE

            __RIGI_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA',
                             MODELE=__MODELE, CARA_ELEM=__ELEM, CHAM_MATER=__CHAMPMAH,)
            # Amor meca hyst pour colonne

            __RIGH_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA_HYST',
                             MODELE=__MODELE, CARA_ELEM=__ELEM, CHAM_MATER=__CHAMPMAH, RIGI_MECA=__RIGI_ELH,)
            # MATRICE DE MASSE ELEMENTAIRE

            __MASS_ELH = CALC_MATR_ELEM(OPTION='MASS_MECA',
                             MODELE=__MODELE, CARA_ELEM=__ELEM, CHAM_MATER=__CHAMPMAH,)

            # MATRICE D AMORTISSEMENT ELEMENTAIRE (pour prise en compte
            # amortissement frontiere absorbante)

            __AMOR_ELH = CALC_MATR_ELEM(OPTION='AMOR_MECA',
                                    MODELE=__MODELE, CARA_ELEM=__ELEM, CHAM_MATER=__CHAMPMAH,
                                    RIGI_MECA=__RIGI_ELH,
                                    MASS_MECA=__MASS_ELH,)
          else:
            # MATRICE DE RIGIDITE ELEMENTAIRE

            __RIGI_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA',
                             MODELE=__MODELE, CHAM_MATER=__CHAMPMAH,)
            # Amor meca hyst pour colonne

            __RIGH_ELH = CALC_MATR_ELEM(OPTION='RIGI_MECA_HYST',
                             MODELE=__MODELE, CHAM_MATER=__CHAMPMAH, RIGI_MECA=__RIGI_ELH,)
            # MATRICE DE MASSE ELEMENTAIRE

            __MASS_ELH = CALC_MATR_ELEM(OPTION='MASS_MECA',
                                    MODELE=__MODELE, CHAM_MATER=__CHAMPMAH,)

            # MATRICE D AMORTISSEMENT ELEMENTAIRE (pour prise en compte
            # amortissement frontiere absorbante)

            __AMOR_ELH = CALC_MATR_ELEM(OPTION='AMOR_MECA',
                                    MODELE=__MODELE, CHAM_MATER=__CHAMPMAH,
                                    RIGI_MECA=__RIGI_ELH,
                                    MASS_MECA=__MASS_ELH,)

            # NUMEROTATION DES DDL

        __NUMEDDL = NUME_DDL(MATR_RIGI=__RIGI_ELH,)
            #
            # MATRICE DE RIGIDITE GLOBALE

        __RIGIHYST = ASSE_MATRICE(MATR_ELEM=__RIGH_ELH, NUME_DDL=__NUMEDDL,)
        __RIGIDITE = ASSE_MATRICE(MATR_ELEM=__RIGI_ELH, NUME_DDL=__NUMEDDL,)

            #
            # MATRICE DE MASSE GLOBALE

        __MASSEH = ASSE_MATRICE(MATR_ELEM=__MASS_ELH, NUME_DDL=__NUMEDDL,)

            #
            # MATRICE D AMORTISSEMENT GLOBALE

        __AMORTIH = ASSE_MATRICE(MATR_ELEM=__AMOR_ELH, NUME_DDL=__NUMEDDL,)

            # Chargement sismique monoappui sur la base du modele
        if args['NOM_CMP'] == 'DX':
          __VECASX = CALC_CHAR_SEISME(
            MATR_MASS=__MASSEH, DIRECTION=(1., 0., 0.,), MONO_APPUI='OUI')
          if lliaison == 'OUI':
            __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'S',
                DIST=-1.0*__TMAT['Y', NCOU + 1],
                FONC_SIGNAL = __VITEX, GROUP_MA=grma_subst)
                     );
          else:
            if args['UNITE_TRAN_INIT'] != None :
              if lmassp == 'OUI' :
                __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                  ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                  TYPE_ONDE = 'S',
                  DIST=-1.0*__TMAT['Y', NCOU + 1], #DIST_REFLECHI=0.,
                  FONC_SIGNAL = __VITEX, GROUP_MA=(grma_subst,))
                      );
                __FSEISMX=AFFE_CHAR_MECA_F(MODELE=__MODELE,
                  FORCE_NODALE=(_F(GROUP_NO='PLATE',FX=__NSEISMX,),),
                      );
              else:
                #__ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                __FSEISMX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                  ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                  TYPE_ONDE = 'S',
                  DEPL_IMPO=__FONCDX,
                  #FONC_SIGNAL = __FONCVX, GROUP_MA=(grma_subst,grma_late))
                  FONC_SIGNAL = __FONCVX, GROUP_MA=(grma_late))
                      );
                __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'S',
                DIST=-1.0*__TMAT['Y', NCOU + 1],
                FONC_SIGNAL = __VITEX, GROUP_MA=grma_subst)
                     );
            else:
              __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'S',
                DIST=-1.0*__TMAT['Y', NCOU + 1], DIST_REFLECHI=0.,
                FONC_SIGNAL = __VITEX, GROUP_MA=(grma_subst,grma_late))
                     );
        else:
          __VECASX = CALC_CHAR_SEISME(
            MATR_MASS=__MASSEH, DIRECTION=(0., 1., 0.,), MONO_APPUI='OUI')
          if lliaison == 'OUI':
            __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'P',
                DIST=-1.0*__TMAT['Y', NCOU + 1],
                FONC_SIGNAL = __VITEX, GROUP_MA=grma_subst)
                     );
          else:
            if args['UNITE_TRAN_INIT'] != None :
              if lmassp == 'OUI' :
                __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                  ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                  TYPE_ONDE = 'P',
                  DIST=-1.0*__TMAT['Y', NCOU + 1], #DIST_REFLECHI=0.,
                  FONC_SIGNAL = __VITEX, GROUP_MA=(grma_subst,))
                      );
                __FSEISMX=AFFE_CHAR_MECA_F(MODELE=__MODELE,
                  FORCE_NODALE=(_F(GROUP_NO='PLATE',FY=__NSEISMX,),),
                      );
              else:
                #__ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                __FSEISMX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                  ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                  TYPE_ONDE = 'P',
                  DEPL_IMPO=__FONCDX,
                  #FONC_SIGNAL = __FONCVX, GROUP_MA=(grma_subst,grma_late))
                  FONC_SIGNAL = __FONCVX, GROUP_MA=(grma_late))
                      );
                __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'P',
                DIST=-1.0*__TMAT['Y', NCOU + 1],
                FONC_SIGNAL = __VITEX, GROUP_MA=grma_subst)
                     );
            else:
              __ONDEX=AFFE_CHAR_MECA_F(  MODELE=__MODELE,
                ONDE_PLANE=_F( DIRECTION = (0., 1., 0.,), 
                TYPE_ONDE = 'P',
                DIST=-1.0*__TMAT['Y', NCOU + 1], DIST_REFLECHI=0.,
                FONC_SIGNAL = __VITEX, GROUP_MA=(grma_subst,grma_late))
                     );

            #
            # CALCUL HARMONIQUE SUR BASE PHYSIQUE #
            #

            # On excite la base de la colonne avec un bruit blanc

        if args['CHARGEMENT'] == 'ONDE_PLANE':
          for k in range(1,nbdt+1):
            if args['UNITE_TRAN_INIT'] != None :
              if lmassp == 'OUI':
                __VECTEL1=CALC_VECT_ELEM(INST=(k-1)*dt, OPTION='CHAR_MECA', CARA_ELEM=__ELEM, 
                                       CHAM_MATER=__CHAMPMAH, CHARGE=(__ONDEX,__FSEISMX))
              else:
                __VECTEL1=CALC_VECT_ELEM(INST=(k-1)*dt, OPTION='CHAR_MECA',
                                       CHAM_MATER=__CHAMPMAH, CHARGE=(__ONDEX,__FSEISMX))
 
            else: 
              __VECTEL1=CALC_VECT_ELEM(INST=(k-1)*dt, OPTION='CHAR_MECA', CHAM_MATER=__CHAMPMAH, CHARGE=__ONDEX)
            __VECA1=ASSE_VECTEUR(VECT_ELEM=__VECTEL1, NUME_DDL=__NUMEDDL)
            if k==1:
              __CHA_ON=CREA_RESU(OPERATION='AFFE',
                  TYPE_RESU='DYNA_TRANS',
                  MATR_RIGI=__RIGIDITE,
                  MATR_MASS=__MASSEH,
                  NOM_CHAM='DEPL',
                  AFFE=(_F(CHAM_GD=__VECA1,INST=0.0,),),);
            else:
              __CHA_ON=CREA_RESU(reuse=__CHA_ON,OPERATION='AFFE',
                  TYPE_RESU='DYNA_TRANS',
                  MATR_RIGI=__RIGIDITE,
                  MATR_MASS=__MASSEH,
                  NOM_CHAM='DEPL',
                  AFFE=(_F(CHAM_GD=__VECA1,INST=(k-1)*dt,),),);
              DETRUIRE(INFO=1,CONCEPT=_F(NOM=(__VECTEL1,__VECA1)));
  
          __CHAONF = REST_SPEC_TEMP( RESULTAT = __CHA_ON ,
                          METHODE = 'PROL_ZERO' ,
                          SYMETRIE = 'NON' ,
                          NOM_CHAM = 'DEPL' ,  );


          __dynah0 = DYNA_VIBRA    ( TYPE_CALCUL='HARM', BASE_CALCUL='PHYS',
                        MATR_RIGI=__RIGIHYST,
                        MATR_MASS=__MASSEH,
                        MATR_AMOR = __AMORTIH,
                        SOLVEUR=_F(
                                STOP_SINGULIER='NON',
                                NPREC=15,
                                METHODE='MUMPS',),
                        EXCIT   = _F( VECT_ASSE = __VECASX,
                                      COEF_MULT = 0.
                                       ),
                        EXCIT_RESU=(
                                 _F( RESULTAT = __CHAONF,
                                     COEF_MULT_C= -1.0 -0.0j,),
                                 ),
                        FREQ= df,
                        TOUT_CHAM= 'OUI'
                        );
                        
          __DEP0=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_C',
                 OPERATION='EXTR',
                 RESULTAT=__dynah0,
                 NOM_CHAM='DEPL',
                 FREQ=df,
                 INFO=1,);                        
                        
          __VIT0=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_C',
                 OPERATION='EXTR',
                 RESULTAT=__dynah0,
                 NOM_CHAM='VITE',
                 FREQ=df,
                 INFO=1,);
                 
          __ACC0=CREA_CHAMP(TYPE_CHAM='NOEU_DEPL_C',
                 OPERATION='EXTR',
                 RESULTAT=__dynah0,
                 NOM_CHAM='ACCE',
                 FREQ=df,
                 INFO=1,);
                 
          __DYNHARM=CREA_RESU(OPERATION='AFFE',
                  TYPE_RESU='DYNA_HARMO',
                  MATR_RIGI=__RIGIDITE,
                  MATR_MASS=__MASSEH,
                  NOM_CHAM='DEPL',
                  AFFE=(_F(CHAM_GD=__DEP0,FREQ=0.0,),),);
                  
          __DYNHARM=CREA_RESU(reuse=__DYNHARM, OPERATION='AFFE',
                  TYPE_RESU='DYNA_HARMO',
                  MATR_RIGI=__RIGIDITE,
                  MATR_MASS=__MASSEH,
                  NOM_CHAM='VITE',
                  AFFE=(_F(CHAM_GD=__VIT0,FREQ=0.0,),),);
                  
          __DYNHARM=CREA_RESU(reuse=__DYNHARM, OPERATION='AFFE',
                  TYPE_RESU='DYNA_HARMO',
                  MATR_RIGI=__RIGIDITE,
                  MATR_MASS=__MASSEH,
                  NOM_CHAM='ACCE',
                  AFFE=(_F(CHAM_GD=__ACC0,FREQ=0.0,),),);
                                                
          __DYNHARM = DYNA_VIBRA    ( reuse=__DYNHARM, RESULTAT=__DYNHARM,
                        TYPE_CALCUL='HARM', BASE_CALCUL='PHYS',
                        MATR_RIGI=__RIGIHYST,
                        MATR_MASS=__MASSEH,
                        MATR_AMOR = __AMORTIH,
                        SOLVEUR=_F(
                                STOP_SINGULIER='NON',
                                NPREC=15,
                                METHODE='MUMPS',),
                        EXCIT   = _F( VECT_ASSE = __VECASX,
                                      COEF_MULT = 0.
                                       ),
                        EXCIT_RESU=(
                                 _F( RESULTAT = __CHAONF,
                                     COEF_MULT_C= -1.0 -0.0j,),
                                 ),
                        LIST_FREQ= __lfreq0,
                        TOUT_CHAM= 'OUI'
                      );
        else:
          if args['LIST_FREQ'] != None:
            __DYNHARM = DYNA_VIBRA    (TYPE_CALCUL='HARM', BASE_CALCUL='PHYS',
                                   MODELE=__MODELE,
                                   CHAM_MATER=__CHAMPMAH,
                                   MATR_MASS=__MASSEH,
                                   MATR_RIGI=__RIGIHYST,
                                   MATR_AMOR=__AMORTIH,
                                   LIST_FREQ=args['LIST_FREQ'],
                                   EXCIT=(
                                   _F(VECT_ASSE=__VECASX, FONC_MULT=__UN,),
                                   ),
                                   SOLVEUR=_F(RENUM='METIS',
                                              STOP_SINGULIER='OUI',
                                              METHODE='MUMPS',
                                              NPREC=17,
                                              ),
                                   )

          else:
            __DYNHARM = DYNA_VIBRA    (TYPE_CALCUL='HARM', BASE_CALCUL='PHYS',
                                   MODELE=__MODELE,
                                   CHAM_MATER=__CHAMPMAH,
                                   MATR_MASS=__MASSEH,
                                   MATR_RIGI=__RIGIHYST,
                                   MATR_AMOR=__AMORTIH,
                                   LIST_FREQ=__lfreqc,
                                   EXCIT=(
                                   _F(VECT_ASSE=__VECASX, FONC_MULT=__UN,),
                                   ),
                                   SOLVEUR=_F(RENUM='METIS',
                                              STOP_SINGULIER='OUI',
                                              METHODE='MUMPS',
                                              NPREC=17,
                                              ),
                                   )

            #
            #
            #                    DEBUT DU POST TRAITEMENT
            #

        if iter == 0:
            __mailla = DEFI_GROUP(reuse=__mailla,
                                  MAILLAGE=__mailla,
                                  CREA_GROUP_NO=_F(GROUP_MA=__TMAT['M', 1],),
                                  )

            __mailla = DEFI_GROUP(reuse=__mailla,
                                  MAILLAGE=__mailla,
                                  CREA_GROUP_NO=_F(
                                      NOM='PN0', GROUP_NO=__TMAT['M', 1],
                                  POSITION='INIT'),
                                  )

            __mailla = DEFI_GROUP(reuse=__mailla,
                                  MAILLAGE=__mailla,
                                  CREA_GROUP_NO=_F(
                                      GROUP_MA=__TMAT['M', NCOU + 1],),
                                  )

            __mailla = DEFI_GROUP(reuse=__mailla,
                                  MAILLAGE=__mailla,
                                  CREA_GROUP_NO=_F(
                                  NOM='PNRA', GROUP_NO=__TMAT['M', NCOU + 1],
                                  POSITION='FIN'),
                                  )

            #

            # On recupere le signal en Champ Libre (CL) et au substratum (Bore
            # Hole)

        __AHuXrCL = RECU_FONCTION(RESULTAT=__DYNHARM,
                                  NOM_CHAM='ACCE',
                                  NOM_CMP=args['NOM_CMP'],
                                  GROUP_NO='PN0',
                                  INTERPOL='LIN',
                                  PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        __AHuXrBH = RECU_FONCTION(RESULTAT=__DYNHARM,
                                  NOM_CHAM='ACCE',
                                  NOM_CMP=args['NOM_CMP'],
                                  GROUP_NO='PNRA',
                                  INTERPOL='LIN',
                                  PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        __mAHuXrCL = CALC_FONCTION(
            EXTRACTION=_F(FONCTION=__AHuXrCL, PARTIE='MODULE'))
        __mAHuXrBH = CALC_FONCTION(
            EXTRACTION=_F(FONCTION=__AHuXrBH, PARTIE='MODULE'))

            #
            # Definition de la fonction de transfert entre RA et CL #
            #

        self.update_const_context(
            {'FILTRE': __FILTRE, 'AHuXrCL': __AHuXrCL, 'AHuXrBH': __AHuXrBH, 'AHX': __AHX, 'ACCEX0H': __ACCEX0H})
        if args['CHARGEMENT'] == 'MONO_APPUI':
          __formFDT = FORMULE(
              NOM_PARA='FREQ', VALE_C='FILTRE(FREQ)*(1.+0.j )/(1.+0.j + AHuXrCL(FREQ))')
          __formFDT2 = FORMULE(
              NOM_PARA='FREQ', VALE_C='FILTRE(FREQ)*(1.+0.j + AHuXrCL(FREQ))/(1.+0.j + AHuXrBH(FREQ))')
        else:
          __formFDT = FORMULE(
            NOM_PARA='FREQ', VALE_C='FILTRE(FREQ)*AHX(FREQ)/AHuXrCL(FREQ)')

          __formFDT2 = FORMULE(
            NOM_PARA='FREQ', VALE_C='FILTRE(FREQ)*AHuXrCL(FREQ)/ACCEX0H(FREQ)')

        __FDT_RACL = CALC_FONC_INTERP(NOM_PARA='FREQ',
                                      LIST_PARA=__lfreq,
                                      FONCTION=__formFDT,
                                      PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        __FDT_CLBH = CALC_FONC_INTERP(NOM_PARA='FREQ',
                                      LIST_PARA=__lfreq,
                                      FONCTION=__formFDT2,
                                      PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

        __mFDTRACL = CALC_FONCTION(
            EXTRACTION=_F(FONCTION=__FDT_RACL, PARTIE='MODULE'))
        __mFDTCLBH = CALC_FONCTION(
            EXTRACTION=_F(FONCTION=__FDT_CLBH, PARTIE='MODULE'))

            #
            # Definition de l accelerogramme au Rocher Affleurant   #
            #
        if input == 'CL':
          if args['CHARGEMENT'] == 'MONO_APPUI':
            __AHX_RA = CALC_FONCTION(
                MULT=(_F(FONCTION= __AHX,), _F(FONCTION=__FDT_RACL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
          else:
            __AHX_RA = CALC_FONCTION(
                MULT=(_F(FONCTION= __ACCEX0H,), _F(FONCTION=__FDT_RACL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)

        if input == 'RA':
            __AHX_RA = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)

        #IMPR_FONCTION(COURBE=(_F(FONCTION=__AHX_RA),))
            # Calcul des accelerogrammes et spectres de sol au RA

        __AX_RAf = CALC_FONCTION(FFT=_F(FONCTION=__AHX_RA,
                                        METHODE='COMPLET',
                                        SYME='NON',),PROL_DROITE='CONSTANT',
                                 )
        __AX_RA = CALC_FONCTION(
            COMB=_F(FONCTION=__AX_RAf, COEF=1.), LIST_PARA=__linst,
            INTERPOL='LIN',
            PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT', )
        DETRUIRE(CONCEPT=_F(NOM=__AX_RAf,))
        #IMPR_FONCTION(COURBE=(_F(FONCTION=__AX_RA),))
        
        if args['LIST_FREQ_SPEC_OSCI'] != None:
          __SAX_RA = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_RA,AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))
        else:
          __SAX_RA = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_RA, AMOR_REDUIT=0.05, NORME=9.81))

            # Calcul des accelerogrammes et spectres de sol en CL et BH
        if args['CHARGEMENT'] == 'MONO_APPUI':
          __AHXrCL = CALC_FONCTION(
             MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__AHuXrCL,), _F(FONCTION=__FILTRE,)),
             LIST_PARA=__lfreq, NOM_PARA='FREQ',)
          __AHXrBH = CALC_FONCTION(
             MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__AHuXrBH,), _F(FONCTION=__FILTRE,)),
             LIST_PARA=__lfreq, NOM_PARA='FREQ',)
        else:
          if input == 'CL':
            __AHXrCL = CALC_FONCTION(
              MULT=(_F(FONCTION=__AHX), _F(FONCTION=__FILTRE,)),
              LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            __AHXrBH = CALC_FONCTION(
               MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__AHuXrBH,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
          if input == 'RA':
            __AHXrCL = CALC_FONCTION(
              MULT=(_F(FONCTION=__AHuXrCL), _F(FONCTION=__FILTRE,)),
              LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            __AHXrBH = CALC_FONCTION(
              MULT=(_F(FONCTION=__AHuXrBH), _F(FONCTION=__FILTRE,)),
              LIST_PARA=__lfreq, NOM_PARA='FREQ',)

        __AXrCL = CALC_FONCTION(
            FFT=_F(FONCTION=__AHXrCL, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
        __AXrBH = CALC_FONCTION(
            FFT=_F(FONCTION=__AHXrBH, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)

        if args['CHARGEMENT'] == 'MONO_APPUI':
          __AX_CL = CALC_FONCTION(
            COMB=(_F(FONCTION=__AXrCL, COEF=1.,), _F(FONCTION=__AX_RA, COEF=1.,),), LIST_PARA=__linst,)
          __AX_BH = CALC_FONCTION(
            COMB=(_F(FONCTION=__AXrBH, COEF=1.,), _F(FONCTION=__AX_RA, COEF=1.,),), LIST_PARA=__linst,)
        else:
          __AX_CL = CALC_FONCTION(
            COMB=(_F(FONCTION=__AXrCL, COEF=1.,), ), LIST_PARA=__linst,)
          __AX_BH = CALC_FONCTION(
            COMB=(_F(FONCTION=__AXrBH, COEF=1.,), ), LIST_PARA=__linst,)

        if args['LIST_FREQ_SPEC_OSCI'] != None:
          __SAX_CL = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_CL,AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))    
          __SAX_BH = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_BH,AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))        
        else:        
          __SAX_CL = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_CL, AMOR_REDUIT=0.05, NORME=9.81))
          __SAX_BH = CALC_FONCTION(
            SPEC_OSCI=_F(FONCTION=__AX_BH, AMOR_REDUIT=0.05, NORME=9.81))

        __ftvCL = RECU_FONCTION(
                GROUP_NO=('PN0'), RESULTAT=__DYNHARM, NOM_CHAM='VITE', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
        __ftvRA = RECU_FONCTION(
                GROUP_NO=('PNRA'), RESULTAT=__DYNHARM, NOM_CHAM='VITE', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
        __ftdCL = RECU_FONCTION(
                GROUP_NO=('PN0'), RESULTAT=__DYNHARM, NOM_CHAM='DEPL', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
        __ftdRA = RECU_FONCTION(
                GROUP_NO=('PNRA'), RESULTAT=__DYNHARM, NOM_CHAM='DEPL', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')

        if args['CHARGEMENT'] == 'ONDE_PLANE':
           if input == 'CL':
                __vCLh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftvCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __dCLh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftdCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __vRAh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftvRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __dRAh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftdRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
           if input == 'RA':
                __vCLh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftvCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __dCLh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftdCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __vRAh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftvRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
                __dRAh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftdRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
        else:
              __vCLh = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftvCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              __dCLh = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftdCL,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              __vRAh = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftvRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              __dRAh = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftdRA,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
        
        __VX_CL = CALC_FONCTION(INTEGRE=_F(FONCTION=__AX_CL,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
   
        __deplx0=CALC_FONCTION(INTEGRE=_F(FONCTION=__VX_CL,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
        self.update_const_context(
            {'deplx0': __deplx0, 'tfin': tfin})

        __deplxF=FORMULE(NOM_PARA='INST',
            VALE='deplx0(INST)-(INST*deplx0(tfin)/tfin)')

        __DX_CL=CALC_FONC_INTERP(FONCTION=__deplxF, NOM_PARA = 'INST',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=__linst);
                        
        DETRUIRE(CONCEPT=_F(NOM=(__deplx0,__deplxF)));

        __VX_RA = CALC_FONCTION(INTEGRE=_F(FONCTION=__AX_RA,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
   
        __deplx0=CALC_FONCTION(INTEGRE=_F(FONCTION=__VX_RA,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
        self.update_const_context(
            {'deplx0': __deplx0, 'tfin': tfin})

        __deplxF=FORMULE(NOM_PARA='INST',
            VALE='deplx0(INST)-(INST*deplx0(tfin)/tfin)')


        __DX_RA=CALC_FONC_INTERP(FONCTION=__deplxF, NOM_PARA = 'INST',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=__linst);
                        
        DETRUIRE(CONCEPT=_F(NOM=(__deplx0,__deplxF)));

        __VX_BH = CALC_FONCTION(INTEGRE=_F(FONCTION=__AX_BH,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
   
        __deplx0=CALC_FONCTION(INTEGRE=_F(FONCTION=__VX_BH,),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
        self.update_const_context(
            {'deplx0': __deplx0, 'tfin': tfin})

        __deplxF=FORMULE(NOM_PARA='INST',
            VALE='deplx0(INST)-(INST*deplx0(tfin)/tfin)')


        __DX_BH=CALC_FONC_INTERP(FONCTION=__deplxF, NOM_PARA = 'INST',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=__linst);
                        
        DETRUIRE(CONCEPT=_F(NOM=(__deplx0,__deplxF)));

        #__VX_CL = CALC_FONCTION(
        #        FFT=_F(FONCTION=__vCLh, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
        #__VX_RA = CALC_FONCTION(
        #        FFT=_F(FONCTION=__vRAh, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
        #__DX_CL = CALC_FONCTION(
        #        FFT=_F(FONCTION=__dCLh, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
        #__DX_RA = CALC_FONCTION(
        #        FFT=_F(FONCTION=__dRAh, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)

            # Calcul des contraintes et deformations dans tous les elements de
            # la colonne
        if args['LIST_FREQ'] != None:
          __DYNHARM = CALC_CHAMP(
            RESULTAT=__DYNHARM,
            reuse=__DYNHARM,
            MODELE=__MODELE,
            CHAM_MATER=__CHAMPMAH,

            GROUP_MA=(grma_colon,),

            LIST_FREQ=args['LIST_FREQ'],
            CONTRAINTE = ('SIGM_ELGA',
                          ),
            DEFORMATION = ('EPSI_ELGA',
                           'EPSI_NOEU',
                           ),
          )
        elif args['CHARGEMENT'] == 'ONDE_PLANE':
          __DYNHARM = CALC_CHAMP(
            RESULTAT=__DYNHARM,
            reuse=__DYNHARM,
            MODELE=__MODELE,
            CHAM_MATER=__CHAMPMAH,

            GROUP_MA=(grma_colon,),

            LIST_FREQ=__lfreq,
            CONTRAINTE = ('SIGM_ELGA',
                          ),
            DEFORMATION = ('EPSI_ELGA',
                           'EPSI_NOEU',
                           ),
          )
        else:
          __DYNHARM = CALC_CHAMP(
            RESULTAT=__DYNHARM,
            reuse=__DYNHARM,
            MODELE=__MODELE,
            CHAM_MATER=__CHAMPMAH,

            GROUP_MA=(grma_colon,),

            LIST_FREQ=__lfreqc,
            CONTRAINTE = ('SIGM_ELGA',
                          ),
            DEFORMATION = ('EPSI_ELGA',
                           'EPSI_NOEU',
                           ),
          )
            # Boucle de postraitement

        __axa = [None] * (NCOU + 1)
        __vix = [None] * (NCOU + 1)
        __dex = [None] * (NCOU + 1)
        __epxy = [None] * (NCOU + 1)
        __gam = [None] * (NCOU + 1)
        __tau = [None] * (NCOU + 1)

        lmaccx = []
        gamax = []

        __accxcla = CALC_FONCTION(ABS=_F(FONCTION=__AX_CL))
        maccxcl = max(__accxcla.Ordo()) / 9.81
        DETRUIRE(CONCEPT=_F(NOM=(__accxcla)), INFO = 1)

        lmaccx.append(maccxcl)
        gamax.append(0)
        __axa.append(0)
        __vix.append(0)
        __dex.append(0)
        __epxy.append(0)
        __tau.append(0)

        for k in range(1, NCOU + 1):
            if iter == 0:
                if k !=  NCOU:
                   __mailla = DEFI_GROUP(reuse=__mailla,
                                      MAILLAGE=__mailla,
                                      CREA_GROUP_NO=_F(
                                          GROUP_MA=__TMAT['M', k + 1],),
                                      )

                __mailla = DEFI_GROUP(reuse=__mailla,
                                      MAILLAGE=__mailla,
                                      CREA_GROUP_NO=_F(
                                      NOM=('PN' + str(k)), GROUP_NO=__TMAT['M', k + 1],
                                      POSITION='INIT'),
                                      )

            __fthr = RECU_FONCTION(
                GROUP_NO=('PN' + str(k)), RESULTAT=__DYNHARM, NOM_CHAM='ACCE', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
            if args['CHARGEMENT'] == 'ONDE_PLANE':
              if input == 'CL':
                __axhr = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__fthr,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              if input == 'RA':
                __axhr = CALC_FONCTION(
                  MULT=(_F(FONCTION=__fthr,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)              
            else:
              __axhr = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__fthr,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)

            __axr = CALC_FONCTION(
                FFT=_F(FONCTION=__axhr, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
            if args['CHARGEMENT'] == 'ONDE_PLANE':
              __axa[k] = CALC_FONCTION(
                COMB=(_F(FONCTION=__axr, COEF=1.,),), LIST_PARA=__linst,)
            else:
              __axa[k] = CALC_FONCTION(
                COMB=(_F(FONCTION=__axr, COEF=1.,), _F(FONCTION=__AX_RA, COEF=1.,),), LIST_PARA=__linst,)

            __ftvi = RECU_FONCTION(
                GROUP_NO=('PN' + str(k)), RESULTAT=__DYNHARM, NOM_CHAM='VITE', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
            if args['CHARGEMENT'] == 'ONDE_PLANE':
              if input == 'CL':
                __vih = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftvi,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              if input == 'RA':
                __vih = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftvi,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            else:
              __vih = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftvi,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)

            #__vix[k] = CALC_FONCTION(
            #    FFT=_F(FONCTION=__vih, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)

            __ftde = RECU_FONCTION(
                GROUP_NO=('PN' + str(k)), RESULTAT=__DYNHARM, NOM_CHAM='DEPL', NOM_CMP=args['NOM_CMP'], INTERPOL='LIN',
                          PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
            if args['CHARGEMENT'] == 'ONDE_PLANE':
              if input == 'CL':
                __deh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftde,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              if input == 'RA':
                __deh = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftde,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            else:
              __deh = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftde,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)

            #__dex[k] = CALC_FONCTION(
            #    FFT=_F(FONCTION=__deh, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)
            
            __vix[k] = CALC_FONCTION(INTEGRE=_F(FONCTION=__axa[k],),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);
   
            __deplx0=CALC_FONCTION(INTEGRE=_F(FONCTION=__vix[k],),   
               PROL_DROITE='CONSTANT',PROL_GAUCHE='CONSTANT',);

            self.update_const_context(
                {'deplx0': __deplx0, 'tfin': tfin})

            __deplxF=FORMULE(NOM_PARA='INST',
                     VALE='deplx0(INST)-(INST*deplx0(tfin)/tfin)')

            __dex[k]=CALC_FONC_INTERP(FONCTION=__deplxF, NOM_PARA = 'INST',
                        PROL_DROITE='CONSTANT',
                        PROL_GAUCHE='CONSTANT',
                        LIST_PARA=__linst);

            __ftep = RECU_FONCTION(
                GROUP_MA=__TMAT['M', k], RESULTAT=__DYNHARM, NOM_CHAM='EPSI_ELGA', POINT=1, NOM_CMP='EPXY', INTERPOL='LIN',
                PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT')
            if args['CHARGEMENT'] == 'ONDE_PLANE':
              if input == 'CL':
                __eph = CALC_FONCTION(
                  MULT=(_F(FONCTION=__FDT_RACL,), _F(FONCTION=__ftep,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
              if input == 'RA':
                __eph = CALC_FONCTION(
                  MULT=(_F(FONCTION=__ftep,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            else:
              __eph = CALC_FONCTION(
                MULT=(_F(FONCTION=__AHX_RA,), _F(FONCTION=__ftep,), _F(FONCTION=__FILTRE,)), LIST_PARA=__lfreq, NOM_PARA='FREQ',)
            __epxy[k] = CALC_FONCTION(
                FFT=_F(FONCTION=__eph, METHODE='COMPLET', SYME='NON',),PROL_DROITE='CONSTANT',)

            # Calcul de la distorsion : gamma = 2*epxy ; tau = sixy
            __gam[k] = CALC_FONCTION(
                LIST_PARA=__linst, COMB=(_F(FONCTION=__epxy[k], COEF=(2),),
                                         ),)
               
            if formulation == 'LYSMER':
              f2Getoil = E[iter][k] / (1+ __TMAT['NU',k]) * (1.-AH[iter][k]*AH[iter][k]/2 + (AH[iter][k]*sqrt(1-AH[iter][k]*AH[iter][k]/4))*1.j) ;
            else: 
              f2Getoil = (E[iter][k] / (1+ __TMAT['NU',k]) ) * (1.+ (AH[iter][k])*1.j);

            __tauh = CALC_FONCTION(COMB_C=_F(FONCTION=__eph,COEF_C = f2Getoil),LIST_PARA=__lfreq,NOM_PARA='FREQ',);
            
            __tau[k] = CALC_FONCTION(FFT=_F(FONCTION=__tauh,METHODE='COMPLET',SYME='NON',),PROL_DROITE='CONSTANT',);

            # Calcul des max
            __accxa = CALC_FONCTION(ABS=_F(FONCTION=__axa[k]))
            __gama = CALC_FONCTION(ABS=_F(FONCTION=__gam[k]))

            maccx = max(__accxa.Ordo())
            mgam = max(__gama.Ordo())

            lmaccx.append(maccx / 9.81)
            gamax.append(mgam)

            DETRUIRE(CONCEPT=_F(NOM=(__accxa, __gama,
                                     __axhr, __axr, __eph,
                                     __vih, __deh,
                                     __ftep, __fthr,
                                     __ftvi, __ftde,
                                     )), INFO = 1)

        # construction des profils

        __paccx = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='accxmax',
                                ORDONNEE=tuple(lmaccx),
                                ABSCISSE=tuple(lprof,))

        __pgamax = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='gamma_max',
                                 ORDONNEE=tuple(gamax),
                                 ABSCISSE=tuple(lprof,))

        # Lecture sur les courbes G/Gmax et D
        ind = []
        diff = []

        iter = iter + 1

        ind = []
        diff = []

        #E[iter] = [] * (NCOU + 1)
        #AH[iter] = [] * (NCOU + 1)
        #rat[iter] = [] * (NCOU + 1)
        E[iter] = [] * (NCOU + 2)
        AH[iter] = [] * (NCOU + 2)
        rat[iter] = [] * (NCOU + 2)

        E[iter].append(0)
        AH[iter].append(0)
        rat[iter].append(1)

        ind.append(0)
        diff.append(0)

        for k in range(1, NCOU + 1):

            ind.append(__TMAT['GDgam', k])

            rat[iter].append(__GG[ind[k]](geff * gamax[k]))

            AH[iter].append(2 * __DG[ind[k]](geff * gamax[k]))

            E[iter].append((rat[iter][k]) * __TMAT['E0', k])

            diff.append(
                abs(((E[iter - 1][k]) - (E[iter][k]))) / (E[iter - 1][k]))
                    
        AH[iter].append(__TMAT['AH0', NCOU + 1])
        E[iter].append(__TMAT['E0', NCOU + 1])

        deltaE = max(max(diff), abs(min(diff)))

        text = ('deltaE=' + str(deltaE))
        aster.affiche('MESSAGE', text)

        __Enew = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='DX',
                               ORDONNEE=tuple(E[iter]),
                               ABSCISSE=tuple(lprof2,))

        __AHnew = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='DX',
                                ORDONNEE=tuple(AH[iter]),
                                ABSCISSE=tuple(lprof2,))

        __rGnew = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='G/Gmax',
                                ORDONNEE=tuple(rat[iter]),
                                ABSCISSE=tuple(lprof,))

        __rGold = DEFI_FONCTION(NOM_PARA='Y', NOM_RESU='G/Gmax',
                                ORDONNEE=tuple(rat[iter - 1]),
                                ABSCISSE=tuple(lprof,))

        if deltaE > tole:

            if iter == (nmaxit - 1):

                etat = 'fin'

            else:

                __TEnew = CREA_TABLE(FONCTION=_F(FONCTION=__Enew),)
                __TAHnew = CREA_TABLE(FONCTION=_F(FONCTION=__AHnew),)

                __TEnew = CALC_TABLE(reuse=__TEnew, TABLE=__TEnew,
                                     ACTION=_F(OPERATION='RENOMME', NOM_PARA=('DX', ('E' + str(iter)))))

                __TAHnew = CALC_TABLE(reuse=__TAHnew, TABLE=__TAHnew,
                                      ACTION=_F(OPERATION='RENOMME', NOM_PARA=('DX', ('AH' + str(iter)))))

                __TMAT = CALC_TABLE(reuse=__TMAT, TABLE=__TMAT,
                                    ACTION=(
                                    _F(OPERATION='COMB',
                                       TABLE=__TEnew, NOM_PARA='Y'),
                                    _F(OPERATION='COMB',
                                       TABLE=__TAHnew, NOM_PARA='Y'),
                                    ))
                DETRUIRE(CONCEPT=_F(NOM=(
                    __NUMEDDL, __RIGIHYST, __RIGH_ELH, __RIGIDITE,
                    __SOLHSUBS, __CHAMPMAH, __RIGI_ELH, __MASS_ELH, __MASSEH, __AMOR_ELH, __AMORTIH,
                    __VECASX, __ONDEX, __DYNHARM,
                    __formFDT, __FDT_RACL, __mFDTRACL,
                    __formFDT2, __FDT_CLBH, __mFDTCLBH,
                    __AHuXrCL, __AHuXrBH, __mAHuXrCL, __mAHuXrBH,
                    __AHX_RA, __AX_RA, __SAX_RA,
                    __AX_CL, __AHXrCL, __AXrCL, __SAX_CL,
                    __AX_BH, __AHXrBH, __AXrBH, __SAX_BH,
                    __paccx, __pgamax,
                    __rGnew, __rGold,
                    __Enew, __AHnew, __TEnew, __TAHnew,

                ),), INFO = 1)
                if args['CHARGEMENT'] == 'ONDE_PLANE':
                  DETRUIRE(CONCEPT=_F(NOM=(
                    __DEP0, __VIT0, __ACC0,
                    __CHA_ON, __CHAONF, __dynah0,
                  ),), INFO = 1)
                for k in range(1, NCOU + 1):
                    DETRUIRE(
                        CONCEPT=_F(NOM=(__SOLH[k], __axa[k], __epxy[k], __gam[k], __tau[k] ),), INFO = 1)

        if deltaE < tole:
            etat = 'fin'

        if etat == 'fin':

            __tabred = CREA_TABLE(FONCTION=_F(FONCTION=__rGold),)

            __tabgam = CREA_TABLE(FONCTION=_F(FONCTION=__pgamax),)
            __tabacc = CREA_TABLE(FONCTION=_F(FONCTION=__paccx),)

            self.update_const_context(
                {'ca': ca, 'aamult': aamult, 'abmult': abmult})
            __fAB = FORMULE(NOM_PARA=('AHfin'), VALE = 'ca*abmult*AHfin')
            __fAA = FORMULE(NOM_PARA=('AHfin'), VALE = 'ca*aamult*AHfin')
            
            __fEf = FORMULE(NOM_PARA=('Efin'), VALE = 'Efin')
            __fAHf = FORMULE(NOM_PARA=('AHfin'), VALE = 'AHfin')
            __fGf = FORMULE(NOM_PARA=('Efin','NU'), VALE = 'Efin/(2.0*(1+NU))')
            __fVSf = FORMULE(NOM_PARA=('Efin','NU','RHO'), VALE = 'sqrt(Efin/(2.0*(1+NU)*RHO))')
            __fVPf = FORMULE(NOM_PARA=('Efin','NU','RHO'), VALE = 'sqrt(Efin*(1-NU)/((1-2.0*NU)*(1+NU)*RHO))')

            __TMAT = CALC_TABLE(reuse=__TMAT, TABLE=__TMAT,
                                ACTION=(
                                _F(OPERATION='COMB',
                                   TABLE=__tabred, NOM_PARA='Y'),
                                _F(OPERATION='COMB',
                                   TABLE=__tabgam, NOM_PARA='Y'),
                                    _F(OPERATION='COMB',
                                       TABLE=__tabacc, NOM_PARA='Y'),
                                    _F(OPERATION='RENOMME', NOM_PARA=(
                                      ('E' + str(iter - 1)), 'Efin'),),
                                    _F(OPERATION='RENOMME', NOM_PARA=(
                                      ('AH' + str(iter - 1)), 'AHfin'),),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fAA, NOM_PARA = 'AAfin',),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fAB, NOM_PARA = 'ABfin',),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fGf, NOM_PARA = 'Gfin',),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fVSf, NOM_PARA = 'VSfin',),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fVPf, NOM_PARA = 'VPfin',),
                                ))
            if iter == 1 :
               __TMAT = CALC_TABLE(reuse=__TMAT, TABLE=__TMAT,
                            ACTION=(
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fEf, NOM_PARA = 'E0',),
                                    _F(OPERATION = 'OPER',
                                       FORMULE=__fAHf, NOM_PARA = 'AH0',),
                                ))
                                
            DETRUIRE(
                CONCEPT=_F(NOM=(__tabred, __tabgam, __tabacc, __fAA, __fAB),))

            # Recuperation des historiques d acceleration et deformation dans
            # chaque couche

            __SPEC = [None] * (NCOU + 1)

            if v == 1:
                if s == 1:
                    if a == 1:
                        __tabaccx = CREA_TABLE(
                            FONCTION=_F(FONCTION=__AX_CL, PARA=('INST', 'AX_CL')),)
                        __tabAXRA = CREA_TABLE(
                            FONCTION=_F(FONCTION=__AX_RA, PARA=('INST', 'AX_RA')),)
                        __tabaccx = CALC_TABLE(TABLE=__tabaccx, reuse=__tabaccx,
                                    ACTION=_F(OPERATION='COMB', TABLE=__tabAXRA, NOM_PARA='INST'),)
                        __tabvitx = CREA_TABLE(
                            FONCTION=_F(FONCTION=__VX_CL, PARA=('INST', 'VX_CL')),)
                        __tabVXRA = CREA_TABLE(
                            FONCTION=_F(FONCTION=__VX_RA, PARA=('INST', 'VX_RA')),)
                        __tabvitx = CALC_TABLE(TABLE=__tabvitx, reuse=__tabvitx,
                                    ACTION=_F(OPERATION='COMB', TABLE=__tabVXRA, NOM_PARA='INST'),)
                        __tabdepx = CREA_TABLE(
                            FONCTION=_F(FONCTION=__DX_CL, PARA=('INST', 'DX_CL')),)
                        __tabDXRA = CREA_TABLE(
                            FONCTION=_F(FONCTION=__DX_RA, PARA=('INST', 'DX_RA')),)
                        __tabdepx = CALC_TABLE(TABLE=__tabdepx, reuse=__tabdepx,
                                    ACTION=_F(OPERATION='COMB', TABLE=__tabDXRA, NOM_PARA='INST'),)

                        __tabadec = CALC_TABLE(TABLE=__tabaccx,
                                               ACTION=(
                                               _F(OPERATION='COMB',
                                                  TABLE=__tabAXRA, NOM_PARA='INST'),
                                               _F(OPERATION='RENOMME', NOM_PARA=(
                                                  'AX_CL', 'CL' + legendeT),),
                                                   _F(OPERATION='RENOMME', NOM_PARA=(
                                                      'AX_RA', 'RA' + legendeT),),
                                               ))

            DETRUIRE(CONCEPT=_F(NOM=(__tabAXRA,__tabVXRA,__tabDXRA,),))

            for k in range(1, NCOU + 1):
                if k == 1:

                    __tabgam = CREA_TABLE(
                        FONCTION=_F(FONCTION=__gam[k], PARA=('INST', 'GAM' + str(k))))
                        
                    __tabtau = CREA_TABLE(
                        FONCTION=_F(FONCTION=__tau[k], PARA=('INST', 'TAU' + str(k))))

                    __tabatmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__axa[k], PARA=('INST', 'ACCE' + str(k))))
                    __tabaccx = CALC_TABLE(TABLE=__tabaccx, reuse=__tabaccx,
                                           ACTION=_F(OPERATION='COMB', TABLE=__tabatmp, NOM_PARA='INST'),)
                    __tabvtmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__vix[k], PARA=('INST', 'VITE' + str(k))))
                    __tabvitx = CALC_TABLE(TABLE=__tabvitx, reuse=__tabvitx,
                                           ACTION=_F(OPERATION='COMB', TABLE=__tabvtmp, NOM_PARA='INST'),)
                    __tabdtmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__dex[k], PARA=('INST', 'DEPL' + str(k))))
                    __tabdepx = CALC_TABLE(TABLE=__tabdepx, reuse=__tabdepx,
                                           ACTION=_F(OPERATION='COMB', TABLE=__tabdtmp, NOM_PARA='INST'),)

                    if args['LIST_FREQ_SPEC_OSCI'] != None:
                      __SPEC[k] = CALC_FONCTION(
                        SPEC_OSCI=_F(FONCTION=__axa[k],AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))
                    else:
                      __SPEC[k] = CALC_FONCTION(
                        SPEC_OSCI=_F(FONCTION=__axa[k], AMOR_REDUIT=0.05, NORME=9.81))

                    DETRUIRE(CONCEPT=_F(NOM=(__tabatmp,__tabvtmp,__tabdtmp,),))

                else:
                    __tabatmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__axa[k], PARA=('INST', 'ACCE' + str(k))))
                    __tabvtmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__vix[k], PARA=('INST', 'VITE' + str(k))))
                    __tabdtmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__dex[k], PARA=('INST', 'DEPL' + str(k))))
                    __tabgtmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__gam[k], PARA=('INST', 'GAM' + str(k))))
                    __tabttmp = CREA_TABLE(
                        FONCTION=_F(FONCTION=__tau[k], PARA=('INST', 'TAU' + str(k))))

                    if args['LIST_FREQ_SPEC_OSCI'] != None:
                      __SPEC[k] = CALC_FONCTION(
                        SPEC_OSCI=_F(FONCTION=__axa[k],AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))
                    else:
                      __SPEC[k] = CALC_FONCTION(
                        SPEC_OSCI=_F(FONCTION=__axa[k], AMOR_REDUIT=0.05, NORME=9.81))

                    __tabaccx = CALC_TABLE(
                        reuse=__tabaccx, TABLE=__tabaccx,
                        ACTION=_F(OPERATION='COMB', TABLE=__tabatmp, NOM_PARA='INST'))
                    __tabvitx = CALC_TABLE(TABLE=__tabvitx, reuse=__tabvitx,
                                           ACTION=_F(OPERATION='COMB', TABLE=__tabvtmp, NOM_PARA='INST'),)
                    __tabdepx = CALC_TABLE(TABLE=__tabdepx, reuse=__tabdepx,
                                           ACTION=_F(OPERATION='COMB', TABLE=__tabdtmp, NOM_PARA='INST'),)

                    __tabgam = CALC_TABLE(reuse=__tabgam, TABLE=__tabgam,
                                          ACTION=_F(OPERATION='COMB', TABLE=__tabgtmp, NOM_PARA='INST'))
                    __tabtau = CALC_TABLE(reuse=__tabtau, TABLE=__tabtau,
                                          ACTION=_F(OPERATION='COMB', TABLE=__tabttmp, NOM_PARA='INST'))
                    DETRUIRE(CONCEPT=_F(NOM=(__tabatmp, __tabgtmp, __tabttmp, __tabvtmp, __tabdtmp,),))

            __tabAXBH = CREA_TABLE(
                            FONCTION=_F(FONCTION=__AX_BH, PARA=('INST', 'AX_BH')),)
            __tabaccx = CALC_TABLE(TABLE=__tabaccx, reuse=__tabaccx,
                            ACTION=_F(OPERATION='COMB', TABLE=__tabAXBH, NOM_PARA='INST'),)
            __tabVXBH = CREA_TABLE(
                            FONCTION=_F(FONCTION=__VX_BH, PARA=('INST', 'VX_BH')),)
            __tabvitx = CALC_TABLE(TABLE=__tabvitx, reuse=__tabvitx,
                            ACTION=_F(OPERATION='COMB', TABLE=__tabVXBH, NOM_PARA='INST'),)
            __tabDXBH = CREA_TABLE(
                            FONCTION=_F(FONCTION=__DX_BH, PARA=('INST', 'DX_BH')),)
            __tabdepx = CALC_TABLE(TABLE=__tabdepx, reuse=__tabdepx,
                            ACTION=_F(OPERATION='COMB', TABLE=__tabDXBH, NOM_PARA='INST'),)

            DETRUIRE(CONCEPT=_F(NOM=(__tabAXBH,__tabVXBH,__tabDXBH,),))

            IMPR_FONCTION(UNITE=6,
                          FORMAT='TABLEAU',
                          TITRE='Fonctions de Transfert entre CL et RA et Module des FFT',
                          COURBE=(
                          _F(FONCTION=__mFDTRACL, MARQUEUR=0.,
                             LEGENDE='FdT RA/CL' + legende + 'deltaE =' + str(deltaE),),
                          _F(FONCTION=__mFDTCLBH, MARQUEUR=0.,
                             LEGENDE='FdT CL/BH' + legende + 'deltaE =' + str(deltaE),),
                          ))

            if veriftmp == 'NON':

                IMPR_FONCTION(UNITE=8,
                              FORMAT='TABLEAU',
                              TITRE='Spectre CL et RA - input =' + input,
                              COURBE=(
                              _F(FONCTION=__SAX_CL, MARQUEUR=0.,
                                 LEGENDE='SAX_CL' + legende + 'deltaE =' + str(deltaE),),
                              _F(FONCTION=__SAX_RA, MARQUEUR=0.,
                                 LEGENDE='SAX_RA' + legende + 'deltaE =' + str(deltaE),),
                              ))

            IMPR_TABLE(UNITE=utabresu, TABLE=__TMAT, SEPARATEUR=args['SEPARATEUR'],
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les valeurs max sont calculees au 1er Point de Gauss de la couche definie par sa cote inferieure Y'),
                       )

            IMPR_TABLE(UNITE=utabtran, TABLE=__tabaccx,
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les acce max sont calculees a la base de la couche definie par sa cote inferieure Y'),
                       )
            IMPR_TABLE(UNITE=utabtran, TABLE=__tabvitx,
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les vitesses sont calculees a la base de la couche definie par sa cote inferieure Y'),
                       )
            IMPR_TABLE(UNITE=utabtran, TABLE=__tabdepx,
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les deplacements sont calcules a la base de la couche definie par sa cote inferieure Y'),
                       )

            IMPR_TABLE(UNITE=utabtran, TABLE=__tabgam,
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les valeurs max sont calculees au 1er Point de Gauss de la couche definie par sa cote inferieure Y'),
                       )
                       
            IMPR_TABLE(UNITE=utabtran, TABLE=__tabtau,
                       TITRE=('Resultats du calcul lineaire equivalent pour le sol' + str(s) + 'avec E=' + str(cvar) + '*E0 \
               Les valeurs max sont calculees au 1er Point de Gauss de la couche definie par sa cote inferieure Y'),
                       )                       

            ifspec = []

            ifspec.append(
                _F(FONCTION=__SAX_CL, MARQUEUR=0, LEGENDE='SAX_CL' + legende + 'deltaE =' + str(deltaE)),)
            ifspec.append(
                _F(FONCTION=__SAX_RA, MARQUEUR=0, LEGENDE='SAX_RA' + legende + 'deltaE =' + str(deltaE)),)

            for d in range(1, NCOU + 1):
                ifspec.append(
                    _F(FONCTION=__SPEC[d], MARQUEUR=0, LEGENDE='SAX_' + str(d) + legende + 'deltaE =' + str(deltaE)),)

            ifspec.append(
                _F(FONCTION=__SAX_BH, MARQUEUR=0, LEGENDE='SAX_BH' + legende + 'deltaE =' + str(deltaE)),)

            IMPR_FONCTION(UNITE=utabspec,
                          FORMAT='TABLEAU',
                          SOUS_TITRE=titre,
                          TITRE='Spectres CL et RA et ppt - input =' + input,
                          COURBE=(ifspec
                                  ))

            # Destruction de tous les concepts pour permettre une utilisation
            # en INCLUDE dans boucle

            if veriftmp == 'OUI':

                __SOLR = [None] * (NCOU + 2)

                for j in range(1, NCOU + 1):

                    __SOLR[j] = DEFI_MATERIAU(ELAS=_F(E=__TMAT['Efin', j],
                                                      RHO=__TMAT['RHO', j],
                                                      NU=__TMAT['NU', j],
                                                      AMOR_ALPHA=__TMAT[
                                                          'AAfin', j],
                                                      AMOR_BETA=__TMAT[
                                                          'ABfin', j],
                                                      ))

                __SOLR[NCOU + 1] = DEFI_MATERIAU(
                    ELAS=_F(E=__TMAT['E0', NCOU + 1],
                            RHO=__TMAT['RHO', NCOU + 1],
                            NU=__TMAT['NU', NCOU + 1],
                            ))

                __SOLRSUBS = DEFI_MATERIAU(ELAS=_F(E=__TMAT['E0', NCOU + 1],
                                           RHO=__TMAT['RHO', NCOU + 1],
                                           NU=__TMAT['NU', NCOU + 1],
                                                   ))

            # Boucle pour affectation du materiau j au GROUP_MA 'Mj'

                tSOLR = tuple(__SOLR)

                affmatR = []

                for j in range(1, NCOU + 2):

                    affmatR.append(
                        _F(GROUP_MA=__TMAT['M', j], MATER=tSOLR[j]))

                affmatR.append(_F(GROUP_MA=grma_subst, MATER=__SOLRSUBS))

                __CHAMPMAR = AFFE_MATERIAU(MAILLAGE=__mailla,
                                           AFFE=affmatR,
                                           )

                __RIGI_ELR = CALC_MATR_ELEM(OPTION='RIGI_MECA',
                                            MODELE=__MODELE, CHAM_MATER=__CHAMPMAR, CHARGE=__CON_LIM,)

                __NUMEDDLR = NUME_DDL(MATR_RIGI=__RIGI_ELR,)

                __RIGIDITE = ASSE_MATRICE(
                    MATR_ELEM=__RIGI_ELR, NUME_DDL=__NUMEDDLR,)

                __MASS_ELR = CALC_MATR_ELEM(OPTION='MASS_MECA',
                                            MODELE=__MODELE,
                                            CHAM_MATER=__CHAMPMAR,
                                            CHARGE=__CON_LIM,)

                __MASSER = ASSE_MATRICE(
                    MATR_ELEM=__MASS_ELR, NUME_DDL=__NUMEDDLR,)

                __AMOR_ELR = CALC_MATR_ELEM(OPTION='AMOR_MECA',
                                            MODELE=__MODELE,
                                            CHAM_MATER=__CHAMPMAR,
                                            RIGI_MECA=__RIGI_ELR,
                                            MASS_MECA=__MASS_ELR,
                                            CHARGE=__CON_LIM,)

                __AMORTIR = ASSE_MATRICE(
                    MATR_ELEM=__AMOR_ELR, NUME_DDL=__NUMEDDLR,)

                __VECASXR = CALC_CHAR_SEISME(
                    MATR_MASS=__MASSER, DIRECTION=(1., 0., 0.,), MONO_APPUI='OUI')

                __DYNTEMP = DYNA_VIBRA    (TYPE_CALCUL='TRAN', BASE_CALCUL='PHYS',
                                           MATR_MASS=__MASSER,
                                           MATR_RIGI=__RIGIDITE,
                                           MATR_AMOR=__AMORTIR,

                                           INCREMENT=_F(LIST_INST=__linst,),

                                           EXCIT=_F(VECT_ASSE=__VECASXR,
                                                    FONC_MULT=__AX_RA,),
                                           SOLVEUR=_F(RENUM='METIS',
                                                      STOP_SINGULIER='OUI',
                                                      METHODE='MUMPS',
                                                      NPREC=15,
                                                      ),
                                           )

                __AXrCLv = RECU_FONCTION(RESULTAT=__DYNTEMP,
                                         NOM_CHAM='ACCE',
                                         NOM_CMP=args['NOM_CMP'],
                                         GROUP_NO='P0',
                                         INTERPOL='LIN',
                                         PROL_DROITE='CONSTANT', PROL_GAUCHE='CONSTANT',)

                __AX_CLv = CALC_FONCTION(
                    COMB=(_F(FONCTION=__AXrCLv, COEF=1.,), _F(FONCTION=__AX_RA, COEF=1.,),), LIST_PARA=__linst,)
                if args['LIST_FREQ_SPEC_OSCI'] != None:
                  __SAX_CLv = CALC_FONCTION(
                    SPEC_OSCI=_F(FONCTION=__AX_CLv,AMOR_REDUIT=0.05,LIST_FREQ=args['LIST_FREQ_SPEC_OSCI'],NORME=9.81))                
                else:
                  __SAX_CLv = CALC_FONCTION(
                    SPEC_OSCI=_F(FONCTION=__AX_CLv, AMOR_REDUIT=0.05, NORME=9.81))

                IMPR_FONCTION(UNITE=6,
                              FORMAT='XMGRACE', PILOTE=pilimp,
                              BORNE_X=(0., 35),
                              GRILLE_X = 5.,
                              GRILLE_Y = 0.1,
                              LEGENDE_X = 'Frequence (Hz)',
                              LEGENDE_Y = 'Acceleration (g)',
                              SOUS_TITRE = titre,
                              TITRE = 'Spectres CL et RA - input = ' + input,
                              COURBE = (
                              _F(FONCTION=__SAX_CL, MARQUEUR=0.,
                                 LEGENDE='SAX_CL' + legende + 'deltaE =' + str(deltaE),),
                              _F(FONCTION=__SAX_RA, MARQUEUR=0.,
                                 LEGENDE='SAX_RA' + legende + 'deltaE =' + str(deltaE),),
                              ))

                DETRUIRE(CONCEPT=_F(NOM=(
                    __NUMEDDLR, __RIGIDITE,
                    __SOLRSUBS, __CHAMPMAR, __RIGI_ELR,
                    __MASS_ELR, __MASSER, __AMOR_ELR, __AMORTIR,
                    __VECASXR,
                    __DYNTEMP,
                    __AXrCLv, __AX_CLv, __SAX_CLv,
                ),), INFO = 1)

                for k in range(1, NCOU + 2):
                    DETRUIRE(CONCEPT=_F(NOM=SOLR[k],), INFO=1)

            DETRUIRE(CONCEPT=_F(NOM=(
                                __NUMEDDL,
                                __RIGIHYST, __RIGH_ELH,
                                __SOLHSUBS, __CHAMPMAH, __RIGI_ELH, __MASS_ELH, __MASSEH,
                                __AMOR_ELH, __AMORTIH,
                                __VECASX,
                                __DYNHARM,
                                __formFDT, __FDT_RACL, __mFDTRACL,
                                __formFDT2, __FDT_CLBH, __mFDTCLBH,
                                __AHuXrCL, __AHuXrBH, __mAHuXrCL, __mAHuXrBH,
                                __AHX_RA, __AX_RA, __SAX_RA,
                                __AX_CL, __AHXrCL, __AXrCL, __SAX_CL,
                                __paccx, __pgamax, __rGnew, __rGold,
                                __Enew, __AHnew,
                                __tabaccx, __tabdepx, __tabvitx, __tabgam,
                                ),), INFO = 1)

            for k in range(1, NCOU + 1):
                DETRUIRE(
                    CONCEPT=_F(NOM=(__SPEC[k], __axa[k], __vix[k], __dex[k], __epxy[k], __gam[k],)), INFO = 1)

            for k in range(1, NCOU + 2):
                DETRUIRE(CONCEPT=_F(NOM=__SOLH[k],), INFO=1)

            if deltaE > tole:
                text = ('CONVERGENCE NON ATTEINTE NOMBRE ITERATIONS=' + str(iter) + ' deltaE=' + str(deltaE))
                aster.affiche('MESSAGE', text)

            else:
                text = ('CONVERGENCE ATTEINTE NOMBRE ITERATIONS=' + str(iter) + ' deltaE=' + str(deltaE))
                aster.affiche('MESSAGE', text)

    # definition de la table
    # para/typ pre-trie les colonnes
    tab = Table(
        para=["NUME_COUCHE", "EPAIS", "RHO", "E", "NU", "AMOR_HYST",
              "RECEPTEUR", "SOURCE", "NUME_MATE", "SUBSTRATUM"],
        typ=["I", "R", "R", "R", "R", "R", "K8", "K8", "I", "K8"])


    if SURF == 'OUI':
        ic = 0
        for k in range(1, NCOU + 1):
            if k > 1:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': k, 'NUME_MATE': k,
                     'EPAIS': (__TMAT['Y', k] - __TMAT['Y', k - 1]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AHfin', k]})
            else:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': k, 'NUME_MATE': k,
                     'EPAIS': __TMAT['Y', k], 'SOURCE': 'OUI',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

        ic = ic + 1
        tab.append(
            {'RHO': __TMAT['RHO', ic], 'NU': __TMAT['NU', ic], 'E': __TMAT['E0', ic],
             'NUME_COUCHE': ic, 'NUME_MATE': ic,
             'EPAIS': (__TMAT['Y', ic] - __TMAT['Y', ic - 1]), 'SOURCE': 'NON',
             'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AH0', ic]})

        tab.append(
            {'RHO': __TMAT['RHO', ic], 'NU': __TMAT['NU', ic], 'E': __TMAT['E0', ic],
             'NUME_COUCHE': (ic + 1), 'NUME_MATE': ic, 'SUBSTRATUM': 'OUI',
             'EPAIS': 0.0, 'SOURCE': 'NON',
             'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AH0', ic]})

    else:
        ic = 0
        for k in range(1, NCOU2 + 1):
            if k > 1:
              if nsco == 4:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'OUI',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})
              else:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.5 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.5 * (__TMAT['Y', k] - __TMAT['Y', k - 1])), 'SOURCE': 'OUI',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})
            else:
              if nsco == 4:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * __TMAT['Y', k]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * __TMAT['Y', k]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})
                     
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * __TMAT['Y', k]), 'SOURCE': 'OUI',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.25 * __TMAT['Y', k]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})
              else:
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.5 * __TMAT['Y', k]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (0.5 * __TMAT['Y', k]), 'SOURCE': 'OUI',
                     'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

        if NCOU2 < NCOU:
            ic = ic + 1
            k = NCOU2 + 1
            tab.append(
                {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                 'NUME_COUCHE': ic, 'NUME_MATE': k,
                 'EPAIS': (__TMAT['Y', k] - __TMAT['Y', k - 1]), 'SOURCE': 'OUI',
                 'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AHfin', k]})

            for k in range(NCOU2 + 2, NCOU + 1):
                ic = ic + 1
                tab.append(
                    {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['Efin', k],
                     'NUME_COUCHE': ic, 'NUME_MATE': k,
                     'EPAIS': (__TMAT['Y', k] - __TMAT['Y', k - 1]), 'SOURCE': 'NON',
                     'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AHfin', k]})

            ic = ic + 1
            k = NCOU + 1
            tab.append(
                {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['E0', k],
                 'NUME_COUCHE': ic, 'NUME_MATE': k,
                 'EPAIS': (__TMAT['Y', k] - __TMAT['Y', k - 1]), 'SOURCE': 'NON',
                 'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AH0', k]})

        else:
            ic = ic + 1
            k = NCOU + 1
            tab.append(
                {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['E0', k],
                 'NUME_COUCHE': ic, 'NUME_MATE': k,
                 'EPAIS': (__TMAT['Y', k] - __TMAT['Y', k - 1]), 'SOURCE': 'OUI',
                 'RECEPTEUR': 'OUI', 'AMOR_HYST': __TMAT['AH0', k]})

        tab.append(
            {'RHO': __TMAT['RHO', k], 'NU': __TMAT['NU', k], 'E': __TMAT['E0', k],
             'NUME_COUCHE': (ic + 1), 'NUME_MATE': k, 'SUBSTRATUM': 'OUI',
             'EPAIS': 0.0, 'SOURCE': 'NON',
             'RECEPTEUR': 'NON', 'AMOR_HYST': __TMAT['AH0', k]})

    DETRUIRE(CONCEPT=_F(NOM=__TMAT), INFO=1)
    IMPR_RESU(RESU=_F(MAILLAGE=__mailla,),FORMAT='ASTER',UNITE=8);

    IMPR_TABLE(TABLE=__tabadec,
               TITRE=('Resultats du calcul lineaire equivalent pour les differents accelero \
     Les acce max sont calculees a la base de la couche definie par sa cote inferieure Y'),
               )
    # creation de la table
    dprod = tab.dict_CREA_TABLE()
    tabout = CREA_TABLE(**dprod)

    return ier
