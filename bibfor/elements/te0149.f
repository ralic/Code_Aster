      SUBROUTINE TE0149 ( OPTION , NOMTE )
      IMPLICIT  REAL*8  (A-H,O-Z)
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     CALCUL DU VECTEUR ELEMENTAIRE CONTRAINTE
C     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'SIGM_ELNO_DEPL'
C        'SIPO_ELNO_DEPL'
C        'SIGM_ELNO_SIEF'
C        'SIPO_ELNO_SIEF'
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      PARAMETER           (NBRES=4,NBREF=6)
      REAL*8              VALRES(NBRES),VALREF(NBREF)
      CHARACTER*2         CODRES(NBRES),CODREF(NBREF)
      CHARACTER*8  NOMPAR,NOMRES(NBRES),NOMREF(NBREF)
      CHARACTER*16 CH16
      CHARACTER*24 SUROPT
      REAL*8       ZERO, E, NU, ALPHA, RHO
      REAL*8       KLV(78), KLC(12,12), EFGE(12)
C     ------------------------------------------------------------------
      DATA NOMRES / 'E', 'NU', 'ALPHA', 'RHO'/
      DATA NOMREF / 'E', 'NU' , 'RHO' , 'RHO_F_IN' , 'RHO_F_EX' ,
     &             'CM'/
C     ------------------------------------------------------------------
      ZERO   = 0.D0
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
C
      CALL JEVECH ( 'PMATERC', 'L', LMATER )
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = ZERO
      ELSE
         NBPAR  = 1
         NOMPAR = 'TEMP'
         VALPAR = 0.5D0*(ZR(ITEMPE) + ZR(ITEMPE+1))
      ENDIF
      CALL JEVECH ( 'PSUROPT' , 'L' , LOPT )
      SUROPT = ZK24(LOPT)
      IF ( SUROPT.EQ. 'MASS_FLUI_STRU' ) THEN
         CALL JEVECH ( 'PCAGEPO', 'L', LSECR )
         LSECR = LSECR-1
         ITSEC = NINT(ZR(LSECR+13))
         IF ( ITSEC .EQ. 2 ) THEN
C           --- SECTION CIRCULAIRE SECTIONS INITIALE ET FINALE
            R1    =  ZR(LSECR+ 9)
            EP1   =  ZR(LSECR+10)
CJMP            R2    =  ZR(LSECR+11)
CJMP            EP2   =  ZR(LSECR+12)
         ELSE
            CALL U2MESS('F','ELEMENTS3_30')
         ENDIF
        CALL JEVECH ( 'PABSCUR', 'L', LABSC )
        ABSMOY = ( ZR(LABSC-1+1) + ZR(LABSC-1+2) ) /2.D0
        CALL RCVALA(ZI(LMATER),' ','ELAS_FLUI',1,'ABSC',ABSMOY,NBREF,
     &                                      NOMREF,VALREF,CODREF,'FM')
        E     = VALREF(1)
        NU   = VALREF(2)
        ALPHA=0.D0
        RHOS  = VALREF(3)
        RHOFI = VALREF(4)
        RHOFE = VALREF(5)
        CM    = VALREF(6)
        PHIE = R1*2.D0
        IF ( PHIE .EQ. 0.D0 ) THEN
           CALL U2MESS('F','ELEMENTS3_26')
        ENDIF
        PHII = ( PHIE - 2.D0*EP1 )
        CALL RHOEQU(RHO,RHOS,RHOFI,RHOFE,CM,PHII,PHIE)
C
      ELSE
         CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,
     &                                2,NOMRES,VALRES,CODRES,'FM')
         CALL RCVALA(ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,VALPAR,
     &                         2,NOMRES(3),VALRES(3),CODRES(3),'  ')
         IF ( CODRES(3) .NE. 'OK' ) VALRES(3) = ZERO
         IF ( CODRES(4) .NE. 'OK' ) VALRES(4) = ZERO
         E     = VALRES(1)
         NU    = VALRES(2)
         ALPHA = VALRES(3)
         RHO   = VALRES(4)
      ENDIF
C
C     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
C
      CALL PORIGI ( NOMTE, E, NU, KLV )
C
C     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
C
      CALL VECMA ( KLV, 78, KLC, 12 )
C
      IF ( OPTION .EQ. 'SIGM_ELNO_DEPL' ) THEN
C
C     --- CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE ---
         CALL POEFGR ( NOMTE, KLC, E, NU, RHO, ALPHA, EFGE )
C
C     NOEUD 1 EFGE(1)  = N   EFGE(2)  = VY   EFGE(3)  = VZ
C             EFGE(4)  = MT  EFGE(5)  = MFY  EFGE(6)  = MFZ
C     NOEUD 2 EFGE(7)  = N   EFGE(8)  = VY   EFGE(9)  = VZ
C             EFGE(10) = MT  EFGE(11) = MFY  EFGE(12) = MFZ
C
         CALL JEVECH ( 'PCONTRR' , 'E' , JEFFO )
         CALL POSIGR ( NOMTE, EFGE, ZR(JEFFO) )
C
C
      ELSEIF ( OPTION .EQ. 'SIPO_ELNO_DEPL' ) THEN
C
C     --- CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE ---
      CALL POEFGR ( NOMTE, KLC, E, NU, RHO, ALPHA, EFGE )
C
C     NOEUD 1 EFGE(1)  = N   EFGE(2)  = VY   EFGE(3)  = VZ
C             EFGE(4)  = MT  EFGE(5)  = MFY  EFGE(6)  = MFZ
C     NOEUD 2 EFGE(7)  = N   EFGE(8)  = VY   EFGE(9)  = VZ
C             EFGE(10) = MT  EFGE(11) = MFY  EFGE(12) = MFZ
C
         CALL JEVECH ( 'PCONTPO' , 'E' , JEFFO )
         CALL POSIPR ( NOMTE, EFGE, ZR(JEFFO) )
C
      ELSEIF ( OPTION .EQ. 'SIGM_ELNO_SIEF' ) THEN
         CALL JEVECH ( 'PSIEFNOR' , 'L' , JEFGE )
         DO 10 K = 1,6
            EFGE(K) = -ZR(JEFGE+K-1)
 10      CONTINUE
         DO 12 K = 7,12
            EFGE(K) = ZR(JEFGE+K-1)
 12      CONTINUE
         CALL JEVECH ( 'PCONTRR' , 'E' , JEFFO )
         CALL POSIGR ( NOMTE, EFGE, ZR(JEFFO) )
C
      ELSEIF ( OPTION .EQ. 'SIPO_ELNO_SIEF' ) THEN
         CALL JEVECH ( 'PSIEFNOR' , 'L' , JEFGE )
         DO 20 K = 1,6
            EFGE(K) = -ZR(JEFGE+K-1)
 20      CONTINUE
         DO 22 K = 7,12
            EFGE(K) = ZR(JEFGE+K-1)
 22      CONTINUE
         CALL JEVECH ( 'PCONTPO' , 'E' , JEFFO )
         CALL POSIPR ( NOMTE, EFGE, ZR(JEFFO) )
C
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS3_27',1,CH16)
      ENDIF
C
      END
