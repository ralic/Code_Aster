#@ MODIF macr_cara_poutre_ops Macro  DATE 28/01/2003   AUTEUR JMBHH01 J.M.PROIX 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE JMBHH01 J.M.PROIX

def macr_cara_poutre_ops(self,UNITE_MAILLAGE,SYME_X,SYME_Y,GROUP_MA_BORD,
                              GROUP_MA,ORIG_INER,**args):
  """
     Ecriture de la macro MACR_CARA_POUTRE
  """
  import types
  from Accas import _F
  ier=0
  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  LIRE_MAILLAGE   =self.get_cmd('LIRE_MAILLAGE')
  DEFI_GROUP      =self.get_cmd('DEFI_GROUP')
  CREA_MAILLAGE   =self.get_cmd('CREA_MAILLAGE')
  AFFE_MODELE     =self.get_cmd('AFFE_MODELE')
  DEFI_MATERIAU   =self.get_cmd('DEFI_MATERIAU')
  AFFE_MATERIAU   =self.get_cmd('AFFE_MATERIAU')
  DEFI_FONCTION   =self.get_cmd('DEFI_FONCTION')
  DEFI_CONSTANTE  =self.get_cmd('DEFI_CONSTANTE')
  AFFE_CHAR_THER  =self.get_cmd('AFFE_CHAR_THER')
  AFFE_CHAR_THER_F=self.get_cmd('AFFE_CHAR_THER_F')
  THER_LINEAIRE   =self.get_cmd('THER_LINEAIRE')
  CALC_VECT_ELEM  =self.get_cmd('CALC_VECT_ELEM')
  CALC_MATR_ELEM  =self.get_cmd('CALC_MATR_ELEM')
  NUME_DDL        =self.get_cmd('NUME_DDL')
  ASSE_VECTEUR    =self.get_cmd('ASSE_VECTEUR')
  POST_ELEM       =self.get_cmd('POST_ELEM')
  # La macro compte pour 1 dans la numerotation des commandes
  #self.icmd=1
  self.set_icmd(1)

  # Le concept sortant (de type tabl_cara_geom) est nommé 'nomres' dans 
  # le contexte de la macro
  
  self.DeclareOut('nomres',self.sd)

#  if GROUP_MA_BORD and GROUP_MA:
#     if not LIAISON:
#        ier=ier+1
#        self.cr.fatal("Avec GROUP_MA, il faut obligatoirement preciser LIAISON, LONGUEUR ET MATERIAU")
#        return ier
#
  __nomlma=LIRE_MAILLAGE(UNITE=UNITE_MAILLAGE,)

  __nomamo=AFFE_MODELE(MAILLAGE=__nomlma,
                       AFFE=_F(TOUT='OUI',
                               PHENOMENE='MECANIQUE',
                               MODELISATION='D_PLAN',),   )

  __nomdma=DEFI_MATERIAU(ELAS=_F(E=1.0,NU=0.,RHO=1.0),)


  __nomama=AFFE_MATERIAU(MAILLAGE=__nomlma,
                         AFFE=_F(TOUT='OUI',
                                 MATER=__nomdma,),  )

# --- CALCUL DES CARACTERISTIQUES GEOMETRIQUES DE LA SECTION :
#     ------------------------------------------------------

  motsimps={}
  if GROUP_MA  : motsimps['GROUP_MA']  = GROUP_MA
  if SYME_X    : motsimps['SYME_X']    = SYME_X
  if SYME_Y    : motsimps['SYME_Y']    = SYME_Y
  motsimps['ORIG_INER'] = ORIG_INER
  mfact=_F(TOUT='OUI',**motsimps)
  nomres=POST_ELEM(MODELE=__nomamo,
                   CHAM_MATER=__nomama,
                   CARA_GEOM=mfact    )

# nb  :  si GROUP_MA n existe pas : le mot clé est ignoré

#
#     ==================================================================
# --- = CALCUL DE LA CONSTANTE DE TORSION SUR TOUT LE MAILLAGE         =
# --- =     OU DU  CENTRE DE TORSION/CISAILLEMENT                      =
# --- =        DES COEFFICIENTS DE CISAILLEMENT                        =
# --- =     ET DE L INERTIE DE GAUCHISSEMENT                           =
# --- = ON CREE UN MODELE PLAN 2D THERMIQUE REPRESENTANT LA SECTION    =
# --- = DE LA POUTRE CAR ON A A RESOUDRE DES E.D.P. AVEC DES LAPLACIENS=
#     ==================================================================

  if GROUP_MA_BORD and not GROUP_MA:

# --- TRANSFORMATION DES GROUP_MA EN GROUP_NO SUR-LESQUELS
# --- ON POURRA APPLIQUER DES CONDITIONS DE TEMPERATURE IMPOSEE :
#     ---------------------------------------------------------
     motscles={}
     if type(GROUP_MA_BORD)==types.StringType:
        motscles['CREA_GROUP_NO']=_F(GROUP_MA=GROUP_MA_BORD,)
     else:
        motscles['CREA_GROUP_NO']=[]
        for grma in GROUP_MA_BORD:
           motscles['CREA_GROUP_NO'].append(_F(GROUP_MA=grma,))
     __nomlma=DEFI_GROUP(reuse=__nomlma,
                         MAILLAGE=__nomlma,
                         **motscles)


# --- CREATION D UN MAILLAGE IDENTIQUE AU PREMIER A CECI PRES
# --- QUE LES COORDONNEES SONT EXPRIMEES DANS LE REPERE PRINCIPAL
# --- D INERTIE DONT L ORIGINE EST LE CENTRE DE GRAVITE DE LA SECTION :
#     ---------------------------------------------------------------

     __nomapi=CREA_MAILLAGE(MAILLAGE=__nomlma,
                            REPERE=_F(TABLE=nomres,
                                      NOM_ORIG='CDG',  ),  )

# --- AFFECTATION DU PHENOMENE 'THERMIQUE' AU MODELE EN VUE DE
# --- LA CONSTRUCTION D UN OPERATEUR LAPLACIEN SUR CE MODELE :
#     ------------------------------------------------------

     __nomoth=AFFE_MODELE(MAILLAGE=__nomapi,
                          AFFE=_F(TOUT='OUI',
                                  PHENOMENE='THERMIQUE',
                                  MODELISATION='PLAN',), )

# --- POUR LA CONSTRUCTION DU LAPLACIEN, ON  DEFINIT UN
# --- PSEUDO-MATERIAU DONT LES CARACTERISTIQUES THERMIQUES SONT :
# --- LAMBDA = 1, RHO*CP = 0 :
#     ----------------------

     __nomath=DEFI_MATERIAU(THER=_F(LAMBDA=1.0,RHO_CP=0.,),)

# --- DEFINITION D UN CHAM_MATER A PARTIR DU MATERIAU PRECEDENT :
#     ---------------------------------------------------------

     __chmath=AFFE_MATERIAU(MAILLAGE=__nomapi,
                            AFFE=_F(TOUT='OUI',
                                    MATER=__nomath,),   )

#
#     ------------------------------------------------------------
# --- - CALCUL DE LA CONSTANTE DE TORSION PAR RESOLUTION         -
# --- - D UN LAPLACIEN AVEC UN TERME SOURCE EGAL A -2            -
# --- - L INCONNUE ETANT NULLE SUR LE CONTOUR DE LA SECTION :    -
# --- -    LAPLACIEN(PHI) = -2 DANS LA SECTION                   -
# --- -    PHI = 0 SUR LE CONTOUR :                              -
#     ------------------------------------------------------------
#
# --- ON IMPOSE LA VALEUR 0 A L INCONNUE SCALAIRE SUR LE CONTOUR
# --- DE LA SECTION
# --- ET ON A UN TERME SOURCE EGAL A -2 DANS TOUTE LA SECTION :
#     -------------------------------------------------------

     motscles={}
     if args.has_key('GROUP_MA_INTE'):
        if args['GROUP_MA_INTE'] != None :
           motscles['LIAISON_UNIF']=_F(GROUP_MA=args['GROUP_MA_INTE'],DDL='TEMP'),
     __chart1=AFFE_CHAR_THER(MODELE=__nomoth,
                             TEMP_IMPO   =_F(GROUP_NO=GROUP_MA_BORD,
                                             TEMP=0. ),
                             SOURCE      =_F(TOUT='OUI',
                                             SOUR=2.0),
                             **motscles  )

# ---  POUR CHAQUE TROU DE LA SECTION :
# ---  .ON A IMPOSE QUE PHI EST CONSTANT SUR LE CONTOUR INTERIEUR
# ---   EN FAISANT LE LIAISON_UNIF DANS LE AFFE_CHAR_THER PRECEDENT
# ---  .ON IMPOSE EN PLUS D(PHI)/DN = 2*AIRE(TROU)/L(TROU)
# ---        OU D/DN DESIGNE LA DERIVEE PAR RAPPORT A LA
# ---        NORMALE ET L DESIGNE LA LONGUEUR DU BORD DU TROU :
#     -------------------------------------------------------

     if args.has_key('GROUP_MA_INTE'):
        lgmaint=args['GROUP_MA_INTE']
        if lgmaint != None :
           __tbaire=POST_ELEM(MODELE=__nomoth,
                           AIRE_INTERNE=_F(GROUP_MA_BORD=args['GROUP_MA_INTE'],),  )

           motscles={}
           motscles['FLUX_REP']=[]
        
           if type(lgmaint)==types.StringType:
              motscles['FLUX_REP']=_F(GROUP_MA=args['GROUP_MA_INTE'],CARA_TORSION=__tbaire)
           else:
              motscles['FLUX_REP']=[]
              for grma in lgmaint:
                 motscles['FLUX_REP'].append(_F(GROUP_MA=grma,CARA_TORSION=__tbaire),)
           __chart2=AFFE_CHAR_THER(MODELE=__nomoth,**motscles)

# --- RESOLUTION DE LAPLACIEN(PHI) = -2
# --- AVEC PHI = 0 SUR LE CONTOUR :
#     ----------------------------------------

     motscles={}
     motscles['EXCIT']=[_F(CHARGE=__chart1,),]
     if args.has_key('GROUP_MA_INTE'):
        if lgmaint != None :
           motscles['EXCIT'].append(_F(CHARGE=__chart2,))
     __tempe1=THER_LINEAIRE(MODELE=__nomoth,
                            CHAM_MATER=__chmath,
                            SOLVEUR=_F(STOP_SINGULIER='NON',),
                            **motscles   )

#
#     ----------------------------------------------
# --- - CALCUL DU  CENTRE DE TORSION/CISAILLEMENT  -
# --- - ET DES COEFFICIENTS DE CISAILLEMENT :      -
#     ----------------------------------------------
#
# --- POUR LE CALCUL DES CONSTANTES DE CISAILLEMENT, ON VA DEFINIR
# --- UN PREMIER TERME SOURCE, SECOND MEMBRE DE L EQUATION DE LAPLACE
# --- PAR UNE FONCTION EGALE A Y :
#     --------------------------

     __fnsec1=DEFI_FONCTION(NOM_PARA='X',
                            VALE=(0.,0.,10.,10.),
                            PROL_DROITE='LINEAIRE',
                            PROL_GAUCHE='LINEAIRE',
                           )

     __fnsec0=DEFI_CONSTANTE(VALE=0.,)

# --- LE TERME SOURCE CONSTITUANT LE SECOND MEMBRE DE L EQUATION
# --- DE LAPLACE EST PRIS EGAL A Y DANS TOUTE LA SECTION :
#     --------------------------------------------------


     motscles={}
     if args.has_key('NOEUD'):
        motscles['TEMP_IMPO']=(_F(NOEUD=args['NOEUD'],TEMP=__fnsec0))
     __chart2=AFFE_CHAR_THER_F(MODELE=__nomoth,
                               SOURCE=_F(TOUT='OUI',
                                         SOUR=__fnsec1,),
                               **motscles   )

# --- RESOLUTION DE     LAPLACIEN(PHI) = -Y
# ---              AVEC D(PHI)/D(N) = 0 SUR LE CONTOUR :
#     ------------------------------------------------

     __tempe2=THER_LINEAIRE(MODELE=__nomoth,
                            CHAM_MATER=__chmath,
                            EXCIT=_F(CHARGE=__chart2,), 
                            SOLVEUR=_F(STOP_SINGULIER='NON',),
                           )

# --- POUR LE CALCUL DES CONSTANTES DE CISAILLEMENT, ON VA DEFINIR
# --- UN PREMIER TERME SOURCE, SECOND MEMBRE DE L EQUATION DE LAPLACE
# --- PAR UNE FONCTION EGALE A Z :
#     --------------------------

     __fnsec2=DEFI_FONCTION(NOM_PARA='Y',
                            VALE=(0.,0.,10.,10.),
                            PROL_DROITE='LINEAIRE',
                            PROL_GAUCHE='LINEAIRE',
                           )

# --- LE TERME SOURCE CONSTITUANT LE SECOND MEMBRE DE L EQUATION
# --- DE LAPLACE EST PRIS EGAL A Z DANS TOUTE LA SECTION :
#     --------------------------------------------------

     motscles={}
     if args.has_key('NOEUD'):
        motscles['TEMP_IMPO']=_F(NOEUD=args['NOEUD'],TEMP=__fnsec0)
     __chart3=AFFE_CHAR_THER_F(MODELE=__nomoth,
                               SOURCE=_F(TOUT='OUI',
                                         SOUR=__fnsec2,),
                               **motscles)

# --- RESOLUTION DE     LAPLACIEN(PHI) = -Z
# ---              AVEC D(PHI)/D(N) = 0 SUR LE CONTOUR :
#     ------------------------------------------------

     __tempe3=THER_LINEAIRE(MODELE=__nomoth,
                            CHAM_MATER=__chmath,
                            EXCIT=_F(CHARGE=__chart3,), 
                            SOLVEUR=_F(STOP_SINGULIER='NON',),
                           )

# --- CALCUL DE LA CONSTANTE DE TORSION :
#     ---------------------------------

     motscles={}
     if args.has_key('GROUP_MA_INTE'):
        lgmaint=args['GROUP_MA_INTE']
        if lgmaint != None :
           motscles['CARA_POUTRE']=_F(CARA_GEOM=nomres,
                                   LAPL_PHI=__tempe1,
                                   TOUT='OUI',
                                   OPTION='CARA_TORSION',
                                   GROUP_MA_INTE=args['GROUP_MA_INTE'],)
        else:
           motscles['CARA_POUTRE']=_F(CARA_GEOM=nomres,
                                   LAPL_PHI=__tempe1,
                                   TOUT='OUI',
                                   OPTION='CARA_TORSION',      )
     nomres=POST_ELEM(reuse=nomres,
                      MODELE=__nomoth,
                      CHAM_MATER=__chmath,
                      **motscles  )

# --- CALCUL DES COEFFICIENTS DE CISAILLEMENT ET DES COORDONNEES DU
# --- CENTRE DE CISAILLEMENT/TORSION :
#     ------------------------------

     nomres=POST_ELEM(reuse=nomres,
                      MODELE=__nomoth,
                      CHAM_MATER=__chmath,
                      CARA_POUTRE=_F(CARA_GEOM=nomres,
                                     LAPL_PHI_Y=__tempe2,
                                     LAPL_PHI_Z=__tempe3,
                                     TOUT='OUI',
                                     OPTION='CARA_CISAILLEMENT',),  )

#
#     ------------------------------------------------------------
# --- - CALCUL DE L INERTIE DE GAUCHISSEMENT PAR RESOLUTION  DE  -
# --- -    LAPLACIEN(OMEGA) = 0     DANS LA SECTION              -
# --- -    AVEC D(OMEGA)/D(N) = Z*NY-Y*NZ   SUR LE               -
# --- -    CONTOUR DE LA SECTION                                 -
# --- -    NY ET NZ SONT LES COMPOSANTES DU VECTEUR N NORMAL     -
# --- -    A CE CONTOUR                                          -
# --- -    ET SOMME_S(OMEGA.DS) = 0                              -
# --- -    OMEGA EST LA FONCTION DE GAUCHISSEMENT                -
# --- -    L INERTIE DE GAUCHISSEMENT EST SOMME_S(OMEGA**2.DS)   -
#     ------------------------------------------------------------
#
# --- CREATION D UN MAILLAGE DONT LES COORDONNEES SONT EXPRIMEES
# --- DANS LE REPERE PRINCIPAL D INERTIE MAIS AVEC COMME ORIGINE
# --- LE CENTRE DE TORSION DE LA SECTION, ON VA DONC UTILISER
# --- LE MAILLAGE DE NOM NOMAPI DONT LES COORDONNEES SONT
# --- EXPRIMEES DANS LE REPERE PRINCIPAL D'INERTIE, L'ORIGINE
# --- ETANT LE CENTRE DE GRAVITE DE LA SECTION (QUI EST DONC
# --- A CHANGER)  :
#     ----------

     __nomapt=CREA_MAILLAGE(MAILLAGE=__nomapi,
                            REPERE=_F(TABLE=nomres,
                                      NOM_ORIG='TORSION',)  )

# --- AFFECTATION DU PHENOMENE 'THERMIQUE' AU MODELE EN VUE DE
# --- LA CONSTRUCTION D UN OPERATEUR LAPLACIEN SUR CE MODELE :
#     ------------------------------------------------------

     __nomot2=AFFE_MODELE(MAILLAGE=__nomapt,
                          AFFE=_F(TOUT='OUI',
                                  PHENOMENE='THERMIQUE',
                                  MODELISATION='PLAN', )  )

# --- DEFINITION D UN CHAM_MATER A PARTIR DU MATERIAU PRECEDENT :
#     ---------------------------------------------------------

     __chmat2=AFFE_MATERIAU(MAILLAGE=__nomapt,
                            AFFE=_F(TOUT='OUI',
                                    MATER=__nomath, ), )

# --- POUR LE CALCUL DE L INERTIE DE GAUCHISSEMENT, ON VA DEFINIR
# --- LA COMPOSANTE SELON Y DU FLUX A IMPOSER SUR LE CONTOUR
# --- PAR UNE FONCTION EGALE A -X :
#     ---------------------------

     __fnsec3=DEFI_FONCTION(NOM_PARA='X',
                            VALE=(0.,0.,10.,-10.),
                            PROL_DROITE='LINEAIRE',
                            PROL_GAUCHE='LINEAIRE',
                           )

# --- POUR LE CALCUL DE L INERTIE DE GAUCHISSEMENT, ON VA DEFINIR
# --- LA COMPOSANTE SELON X DU FLUX A IMPOSER SUR LE CONTOUR
# --- PAR UNE FONCTION EGALE A Y :
#     --------------------------

     __fnsec4=DEFI_FONCTION(NOM_PARA='Y',
                            VALE=(0.,0.,10.,10.),
                            PROL_DROITE='LINEAIRE',
                            PROL_GAUCHE='LINEAIRE',
                           )

# --- DANS LE BUT D IMPOSER LA RELATION LINEAIRE ENTRE DDLS
# ---  SOMME_SECTION(OMEGA.DS) = 0 ( CETTE CONDITION
# --- VENANT DE L EQUATION D EQUILIBRE SELON L AXE DE LA POUTRE
# --- N = 0, N ETANT L EFFORT NORMAL)
# --- ON CALCULE LE VECTEUR DE CHARGEMENT DU A UN TERME SOURCE EGAL
# --- A 1., LES TERMES DE CE VECTEUR SONT EGAUX A
# --- SOMME_SECTION(NI.DS) ET SONT DONC LES COEFFICIENTS DE
# --- LA RELATION LINEAIRE A IMPOSER.
# --- ON DEFINIT DONC UN CHARGEMENT DU A UN TERME SOURCE EGAL A 1 :
#     -----------------------------------------------------------

     __chart4=AFFE_CHAR_THER(MODELE=__nomot2,
                             SOURCE=_F(TOUT='OUI',
                                       SOUR=1.0),  )

# --- ON CALCULE LE VECT_ELEM DU AU CHARGEMENT PRECEDENT
# --- IL S AGIT DES VECTEURS ELEMENTAIRES DONT LE TERME
# --- AU NOEUD COURANT I EST EGAL A SOMME_SECTION(NI.DS) :
#     --------------------------------------------------

     __vecel=CALC_VECT_ELEM(CHARGE=__chart4,
                            OPTION='CHAR_THER'
                            )

# --- ON CALCULE LE MATR_ELEM DES MATRICES ELEMENTAIRES
# --- DE CONDUCTIVITE UNIQUEMENT POUR GENERER LE NUME_DDL
# --- SUR-LEQUEL S APPUIERA LE CHAMNO UTILISE POUR ECRIRE LA
# --- RELATION LINEAIRE ENTRE DDLS :
#     ----------------------------

     __matel=CALC_MATR_ELEM(MODELE=__nomot2,
                            CHAM_MATER=__chmat2,
                            CHARGE=__chart4,
                            OPTION='RIGI_THER',)

# --- ON DEFINIT LE NUME_DDL ASSOCIE AU MATR_ELEM DEFINI
# --- PRECEDEMMENT POUR CONSTRUIRE LE CHAMNO UTILISE POUR ECRIRE LA
# --- RELATION LINEAIRE ENTRE DDLS :
#     ----------------------------

     __numddl=NUME_DDL(MATR_RIGI=__matel,
                       METHODE='LDLT',    )

# --- ON CONSTRUIT LE CHAMNO QUI VA ETRE UTILISE POUR ECRIRE LA
# --- RELATION LINEAIRE ENTRE DDLS :
#     ----------------------------

     __chamno=ASSE_VECTEUR(VECT_ELEM=__vecel,
                           NUME_DDL=__numddl,    )

# --- ON IMPOSE LA RELATION LINEAIRE ENTRE DDLS
# ---  SOMME_SECTION(OMEGA.DS) = 0 ( CETTE CONDITION
# --- VENANT DE L EQUATION D EQUILIBRE SELON L AXE DE LA POUTRE
# --- N = 0, N ETANT L EFFORT NORMAL)
# --- POUR IMPOSER CETTE RELATION ON PASSE PAR LIAISON_CHAMNO,
# --- LES TERMES DU CHAMNO (I.E. SOMME_SECTION(NI.DS))
# --- SONT LES COEFFICIENTS DE LA RELATION LINEAIRE :
#     ---------------------------------------------

     __chart5=AFFE_CHAR_THER(MODELE=__nomot2,
                             LIAISON_CHAMNO=_F(CHAM_NO=__chamno,
                                               COEF_IMPO=0.),    )

# --- LE CHARGEMENT EST UN FLUX REPARTI NORMAL AU CONTOUR
# --- DONT LES COMPOSANTES SONT +Z (I.E. +Y) ET -Y (I.E. -X)
# --- SELON LA DIRECTION NORMALE AU CONTOUR :
#     -------------------------------------

     __chart6=AFFE_CHAR_THER_F(MODELE=__nomot2,
                               FLUX_REP=_F(GROUP_MA=GROUP_MA_BORD,
                                           FLUX_X  =__fnsec4,
                                           FLUX_Y  =__fnsec3,),    )

# --- RESOLUTION DE     LAPLACIEN(OMEGA) = 0
# --- AVEC D(OMEGA)/D(N) = Z*NY-Y*NZ   SUR LE CONTOUR DE LA SECTION
# --- ET SOMME_SECTION(OMEGA.DS) = 0 ( CETTE CONDITION
# --- VENANT DE L EQUATION D EQUILIBRE SELON L AXE DE LA POUTRE
# --- N = 0, N ETANT L EFFORT NORMAL)  :
#     -------------------------------

     __tempe4=THER_LINEAIRE(MODELE=__nomot2,
                            CHAM_MATER=__chmat2,
                            EXCIT=(_F(CHARGE=__chart5,),
                                   _F(CHARGE=__chart6,),),
                            SOLVEUR=_F(METHODE='LDLT',
                                       RENUM='SANS',
                                       STOP_SINGULIER='NON',),   )
    
# --- CALCUL DE L INERTIE DE GAUCHISSEMENT :
#     -------------------------------------

     nomres=POST_ELEM(reuse=nomres,
                      MODELE=__nomot2,
                      CHAM_MATER=__chmat2,
                      CARA_POUTRE=_F(CARA_GEOM=nomres,
                                     LAPL_PHI=__tempe4,
                                     TOUT='OUI',
                                     OPTION='CARA_GAUCHI'),  )

#
#     ==================================================================
# --- = CALCUL DE LA CONSTANTE DE TORSION SUR CHAQUE GROUPE            =
# --- =     ET DU  CENTRE DE TORSION/CISAILLEMENT                      =
# --- =        DES COEFFICIENTS DE CISAILLEMENT                        =
#     ==================================================================
#


  if GROUP_MA_BORD and GROUP_MA:

     if type(GROUP_MA_BORD)==types.StringType :
        l_group_ma_bord=[GROUP_MA_BORD,]
     else:
        l_group_ma_bord= GROUP_MA_BORD
     if type(GROUP_MA)==types.StringType :
        l_group_ma=[GROUP_MA,]
     else:
        l_group_ma= GROUP_MA

     if args.has_key('NOEUD'):
       if type(args['NOEUD'])==types.StringType :
          l_noeud=[args['NOEUD'],]
       else:
          l_noeud= args['NOEUD']

     if len(l_group_ma)!=len(l_group_ma_bord):
        ier=ier+1
        self.cr.fatal("GROUP_MA et GROUP_MA_BORD incoherents")
        return ier
     if args.has_key('NOEUD') and (len(l_group_ma)!=len(l_noeud)):
        ier=ier+1
        self.cr.fatal("GROUP_MA et NOEUD incoherents")
        return ier

     for i in range(0,len(l_group_ma_bord)):

# --- TRANSFORMATION DES GROUP_MA EN GROUP_NO SUR-LESQUELS
# --- ON POURRA APPLIQUER DES CONDITIONS DE TEMPERATURE IMPOSEE :
#     ---------------------------------------------------------

        __nomlma=DEFI_GROUP(reuse=__nomlma,
                            MAILLAGE=__nomlma,
                            CREA_GROUP_NO=_F(GROUP_MA=l_group_ma_bord[i],)  )


# --- CREATION D UN MAILLAGE IDENTIQUE AU PREMIER A CECI PRES
# --- QUE LES COORDONNEES SONT EXPRIMEES DANS LE REPERE PRINCIPAL
# --- D INERTIE DONT L ORIGINE EST LE CENTRE DE GRAVITE DE LA SECTION :
#     ---------------------------------------------------------------

        __nomapi=CREA_MAILLAGE(MAILLAGE=__nomlma,
                               REPERE=_F(TABLE=nomres,
                                         NOM_ORIG='CDG',
                                         GROUP_MA=l_group_ma[i],  ),  )

# --- AFFECTATION DU PHENOMENE 'THERMIQUE' AU MODELE EN VUE DE
# --- LA CONSTRUCTION D UN OPERATEUR LAPLACIEN SUR CE MODELE :
#     ------------------------------------------------------

        __nomoth=AFFE_MODELE(MAILLAGE=__nomapi,
                             AFFE=_F(GROUP_MA=l_group_ma[i],
                                     PHENOMENE='THERMIQUE',
                                     MODELISATION='PLAN',  )  )

# --- POUR LA CONSTRUCTION DU LAPLACIEN, ON  DEFINIT UN
# --- PSEUDO-MATERIAU DONT LES CARACTERISTIQUES THERMIQUES SONT :
# --- LAMBDA = 1, RHO*CP = 0 :
#     ----------------------

        __nomath=DEFI_MATERIAU(THER=_F(LAMBDA=1.0,
                                       RHO_CP=0.0,  ),  )

# --- DEFINITION D UN CHAM_MATER A PARTIR DU MATERIAU PRECEDENT :
#     ---------------------------------------------------------

        __chmath=AFFE_MATERIAU(MAILLAGE=__nomapi,
                               AFFE=_F(TOUT='OUI',
                                       MATER=__nomath ),  )

#
#     ------------------------------------------------------------
# --- - CALCUL DE LA CONSTANTE DE TORSION PAR RESOLUTION         -
# --- - D UN LAPLACIEN AVEC UN TERME SOURCE EGAL A -2            -
# --- - L INCONNUE ETANT NULLE SUR LE CONTOUR DE LA SECTION :    -
# --- -    LAPLACIEN(PHI) = -2 DANS LA SECTION                   -
# --- -    PHI = 0 SUR LE CONTOUR :                              -
#     ------------------------------------------------------------
#
# --- ON IMPOSE LA VALEUR 0 A L INCONNUE SCALAIRE SUR LE CONTOUR
# --- DE LA SECTION
# --- ET ON A UN TERME SOURCE EGAL A -2 DANS TOUTE LA SECTION :
#     -------------------------------------------------------

        __chart1=AFFE_CHAR_THER(MODELE=__nomoth,
                                TEMP_IMPO=_F(GROUP_NO=l_group_ma_bord[i],
                                             TEMP=0.0       ),
                                SOURCE=_F(TOUT='OUI',
                                          SOUR=2.0       )          )

# --- RESOLUTION DE     LAPLACIEN(PHI) = -2
# ---              AVEC PHI = 0 SUR LE CONTOUR :
#     ----------------------------------------

        __tempe1=THER_LINEAIRE(MODELE=__nomoth,
                               CHAM_MATER=__chmath,
                               EXCIT=_F(CHARGE=__chart1, ),
                               SOLVEUR=_F(STOP_SINGULIER='NON',)    )

#
#     ----------------------------------------------
# --- - CALCUL DU  CENTRE DE TORSION/CISAILLEMENT  -
# --- - ET DES COEFFICIENTS DE CISAILLEMENT :      -
#     ----------------------------------------------
#
# --- POUR LE CALCUL DES CONSTANTES DE CISAILLEMENT, ON VA DEFINIR
# --- UN PREMIER TERME SOURCE, SECOND MEMBRE DE L EQUATION DE LAPLACE
# --- PAR UNE FONCTION EGALE A Y :
#     --------------------------

        __fnsec1=DEFI_FONCTION(NOM_PARA='X',
                               VALE=(0.,0.,10.,10.),
                               PROL_DROITE='LINEAIRE',
                               PROL_GAUCHE='LINEAIRE',        )

        __fnsec0=DEFI_CONSTANTE(VALE=0.,)

# --- LE TERME SOURCE CONSTITUANT LE SECOND MEMBRE DE L EQUATION
# --- DE LAPLACE EST PRIS EGAL A Y DANS TOUTE LA SECTION :
#     --------------------------------------------------

        __chart2=AFFE_CHAR_THER_F(MODELE=__nomoth,
                                  TEMP_IMPO=_F(NOEUD=l_noeud[i],
                                               TEMP=__fnsec0),
                                  SOURCE=_F(TOUT='OUI',
                                            SOUR=__fnsec1)       )

# --- RESOLUTION DE     LAPLACIEN(PHI) = -Y
# ---              AVEC D(PHI)/D(N) = 0 SUR LE CONTOUR :
#     ------------------------------------------------

        __tempe2=THER_LINEAIRE(MODELE=__nomoth,
                               CHAM_MATER=__chmath,
                               EXCIT=_F(CHARGE=__chart2, ),
                               SOLVEUR=_F(STOP_SINGULIER='NON',)         )

# --- POUR LE CALCUL DES CONSTANTES DE CISAILLEMENT, ON VA DEFINIR
# --- UN PREMIER TERME SOURCE, SECOND MEMBRE DE L EQUATION DE LAPLACE
# --- PAR UNE FONCTION EGALE A Z :
#     --------------------------

        __fnsec2=DEFI_FONCTION(NOM_PARA='Y',
                               VALE=(0.,0.,10.,10.),
                               PROL_DROITE='LINEAIRE',
                               PROL_GAUCHE='LINEAIRE',        )

# --- LE TERME SOURCE CONSTITUANT LE SECOND MEMBRE DE L EQUATION
# --- DE LAPLACE EST PRIS EGAL A Z DANS TOUTE LA SECTION :
#     --------------------------------------------------

        __chart3=AFFE_CHAR_THER_F(MODELE=__nomoth,
                                  TEMP_IMPO=_F(NOEUD=l_noeud[i],
                                               TEMP=__fnsec0),
                                  SOURCE=_F(TOUT='OUI',
                                            SOUR=__fnsec2)       )

# --- RESOLUTION DE     LAPLACIEN(PHI) = -Z
# ---              AVEC D(PHI)/D(N) = 0 SUR LE CONTOUR :
#     ------------------------------------------------

        __tempe3=THER_LINEAIRE(MODELE=__nomoth,
                               CHAM_MATER=__chmath,
                               EXCIT=_F(CHARGE=__chart3, ),
                               SOLVEUR=_F(STOP_SINGULIER='NON',)         )

# --- CALCUL DE LA CONSTANTE DE TORSION :
#     ---------------------------------

        nomres=POST_ELEM(reuse=nomres,
                         MODELE=__nomoth,
                         CHAM_MATER=__chmath,
                         CARA_POUTRE=_F(CARA_GEOM=nomres,
                                        LAPL_PHI=__tempe1,
                                        GROUP_MA=l_group_ma[i],
                                        OPTION='CARA_TORSION' ),     )

# --- CALCUL DES COEFFICIENTS DE CISAILLEMENT ET DES COORDONNEES DU
# --- CENTRE DE CISAILLEMENT/TORSION :
#     ------------------------------

        nomres=POST_ELEM(reuse=nomres,
                         MODELE=__nomoth,
                         CHAM_MATER=__chmath,
                         CARA_POUTRE=_F(CARA_GEOM=nomres,
                                        LAPL_PHI_Y=__tempe2,
                                        LAPL_PHI_Z=__tempe3,
                                        GROUP_MA=l_group_ma[i],
                                        LONGUEUR=args['LONGUEUR'],
                                        MATERIAU=args['MATERIAU'],
                                        LIAISON =args['LIAISON'],
                                        OPTION='CARA_CISAILLEMENT' ),   )

  return ier

