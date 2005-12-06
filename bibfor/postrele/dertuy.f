      SUBROUTINE DERTUY ( E, SIGY, SIGU, SIGH, EPS, SIG, LRD, KRD,
     &                    EPAI, LFIS, LLIG, RINT, NBPOIN, NBPOI1,
     &                    RESULT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 22/02/99   AUTEUR F1BHHAJ J.ANGLES 
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
C TOLE CRP_20
C
C     COMMANDE DER
C     DER : DIAGRAMME D'EVALUATION DE LA RUPTURE
C     CALCUL D'UN FACTEUR DE CORRECTION ELASTOPLASTIQUE DE KI A PARTIR
C     DE LA REGLE R6.
C
C     INN :   E      MODULE D'YOUNG
C             SIGY   LIMITE D'ELASTICITE
C             SIGU   CONTRAINTE ULTIME
C             SIGH   CONTRAINTE ADMISSIBLE DU MATERIAU A LA TEMPERATURE
C                    CONSIDEREE
C             EPS    ADRESSE DE LA PREMIERE VALEUR DE EPSILON
C             SIG    ADRESSE DE LA PREMIERE VALEUR DE SIGMA
C             LRD    ADRESSE DE LA PREMIERE VALEUR DE LRD
C             KRD    ADRESSE DE LA PREMIERE VALEUR DE KRD
C             EPAI   EPAISSEUR DU TUYAU
C             LFIS   LONGUEUR DE LA FISSURE
C             LLIG   LONGUEUR DU LIGAMENT
C             RINT   RAYON INTERNE DU TUYAU
C             NBPOIN NOMBBRE DE POINT DE LA FONCTION SIG(EPS)
C             NBPOI1 NOMBBRE DE POINT DE LA FONCTION KRD(LRD)
C             RESULT CONTIENT LE NOM ASSOCIE A LA COMMANDE DEVR_TUYAU
C
C     -------------------- DECLARATION ---------------------------------
C
      IMPLICIT            NONE
      INTEGER             NBPOIN, NBPOI1
      REAL*8              E, SIGY, SIGU, SIGH, EPS(NBPOIN), SIG(NBPOIN)
      REAL*8              LRD(NBPOI1), KRD(NBPOI1)
      REAL*8              EPAI,LFIS,LLIG,RINT
      CHARACTER*8         RESULT
C
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
C     -------------------- DEBUT DES DECLARATIONS ----------------------
C
      INTEGER      IGEOM,NBMAT,TEST,CODRET,NBP,NBP1,NBP2,NBP3
      INTEGER      NBPASS,NBPESB,NLRET,NLRET1,NLFID,NLFID1,NAST
      INTEGER      IADLRI,IALRIM,IADLRD,IALRDM,IADAST,IADESB,IADASS
      INTEGER      I,J,K,IBID
C
      REAL*8       VAL(2),TASB(7),T2EST(8),TFFDI(8,7),TAST(20),TFFDD(20)
      REAL*8       PREC,Z1,Z2,R8PI
      REAL*8       LRDC(100),KRDC(100),LRIC(100),KRIC(100)
      REAL*8       LRET(100),LRIET(100),KRIET(100),RI(100)
      REAL*8       LFID(100),LFIDP
      REAL*8       LRDET(100),KRDET(100),LFIDEP(100)
      REAL*8       QD(100),RD(100)
      REAL*8       SIGF,EPSF
      REAL*8       VAL1,RHO,QI,LRETMI,LRETMA,LRHMAX,LFIDEQ,LFIDMA
      REAL*8       COASS,B,COESB
      REAL*8       XN,XN1,YN,YN1,XP,FFI1,FFI2,FFORMI,FFORMD
      REAL*8       DLRET,LFIDIN,LFIDSU,DLFID,RARDRI
      REAL*8       VR0(1),VR1(4),VR2(4),VR3(2),VR4(3),VR5(4),VR6(3)
      REAL*8       SIGINF,SIGSUP,LRIMAX,KRIMAX,LRDMAX,KRDMAX,QDMAX
C
      COMPLEX*16   CBID
C
      CHARACTER*8  NOMMAT,NOMVAL(2),KBID
      CHARACTER*16 NOMCMD,LPARA0(1),LPARA1(4),LPARA2(4),LPARA3(2)
      CHARACTER*16 LPARA4(3),LPARA5(4),LPARA6(3)
C
C -------- DEFINITION DES TABLES D'INTERPOLATION POUR LE CALCUL --------
C          DU FACTEUR DE FORME
C
C ----- TABLE D'INTERPOLATION POUR LE FACTEUR DE FORME DU DEFAUT -------
C       INTERNE, GEOMETRIE : A/(A+S) = A/B
C
      DATA TASB  / 0.D0,0.2D0,0.3D0,0.4D0,0.5D0,0.6D0,0.7D0 /
C
C ----- TABLE D'INTERPOLATION POUR LE FACTEUR DE FORME DU DEFAUT -------
C       INTERNE, GEOMETRIE : 2E/T
C
      DATA T2EST / 0.D0,0.3D0,0.5D0,0.6D0,0.7D0,0.8D0,0.9D0,1.D0 /
C
C ----- TABLE DES FACTEURS DE FORME POUR LE DEFAUT INTERNE -------------
C
      DATA TFFDI / 1.D0    , 1.D0    , 1.D0    , 1.D0    , 1.D0    ,
     &             1.D0    , 1.D0    , 1.D0    ,
     &             1.015D0 , 1.015D0 , 1.015D0 , 1.015D0 , 1.015D0 ,
     &             1.015D0 , 1.015D0 , 1.015D0 ,
     &             1.054D0 , 1.054D0 , 1.05D0  , 1.05D0  , 1.046D0 ,
     &             1.039D0 , 1.031D0 , 1.031D0 ,
     &             1.107D0 , 1.107D0 , 1.1D0   , 1.092D0 , 1.085D0 ,
     &             1.07D0  , 1.061D0 , 1.057D0 ,
     &             1.185D0 , 1.177D0 , 1.161D0 , 1.154D0 , 1.138D0 ,
     &             1.119D0 , 1.1D0   , 1.092D0 ,
     &             1.3D0   , 1.292D0 , 1.27D0  , 1.246D0 , 1.223D0 ,
     &             1.192D0 , 1.165D0 , 1.154D0 ,
     &             1.48D0  , 1.465D0 , 1.423D0 , 1.4D0   , 1.346D0 ,
     &             1.285D0 , 1.254D0 , 1.238D0 /
C
C ----- TABLE D'INTERPOLATION POUR LE FACTEUR DE FORME DU DEFAUT -------
C       DEBOUCHANT, GEOMETRIE : A/T
C
      DATA TAST  / 0.D0   , 0.02D0 , 0.03D0 , 0.04D0 , 0.05D0 ,
     &             0.06D0 , 0.07D0 , 0.08D0 , 0.09D0 , 0.1D0  ,
     &             0.15D0 , 0.2D0  , 0.25D0 , 0.3D0  , 0.35D0 ,
     &             0.4D0  , 0.45D0 , 0.5D0  , 0.55D0 , 0.6D0  /
C
C ----- TABLE DES FACTEURS DE FORME POUR LE DEFAUT DEBOUCHANT ----------
C
      DATA TFFDD / 1.122D0 , 1.128D0 , 1.132D0 , 1.141D0 , 1.15D0  ,
     &             1.159D0 , 1.167D0 , 1.18D0  , 1.189D0 , 1.2D0   ,
     &             1.276D0 , 1.372D0 , 1.503D0 , 1.666D0 , 1.862D0 ,
     &             2.11D0  , 2.43D0  , 2.828D0 , 3.68D0  , 4.439D0 /
C
C ----- DEFINITION D'UN QUASI ZERO -------------------------------------
C
      DATA PREC  / 1.D-25 /
C
C ----- LISTE DES PARAMETRES DE LA TABLE DES RESULTATS -----------------
C
      DATA LPARA0 / 'LONG_FISS' /
      DATA LPARA1 / 'EPAIS' , 'LONG_LIGA_INT' , 'LONG_FISS' , 'DEXT' /
      DATA LPARA2 / 'E' , 'SY_02' , 'SU' , 'SH' /
      DATA LPARA3 / 'FACT_QI' , 'RHO_LR' /
      DATA LPARA4 / 'SIGM_ETOIL' , 'LR_ETOIL' , 'LONG_FISS_EQUI' /
      DATA LPARA5 / 'LR_FISS_DEB_INT' , 'KR_FISS_DEB_INT' ,
     &              'LR_FISS_NON_DEB' , 'KR_FISS_NON_DEB' /
      DATA LPARA6 / 'SY_LR_MIN' , 'SY_LR_MAX' , 'LONG_FISS_MAXI' /
C
C     -------------------- FIN DES DECLARATIONS ------------------------
C
C     ----- CALCUL DE LA COUPURE DU DER OPTION 2 -----------------------
C
      SIGF = (SIGY + SIGU)/2
      LRDMAX = SIGF/SIGY
C-- 1 ------ DETERMINATION DU NOMBRE DE POINT (EPS,SIG) DE LA ----------
C     ------ COURBE DE TRACTION PRECEDENT (EPSF,SIGF) ------------------
      IF ( SIGF .LE. SIG(1) ) THEN
        NBP = 1
      ELSE IF ( SIGF .GE. SIG(NBPOIN) ) THEN
        NBP = NBPOIN-1
      ELSE
        DO 10 K=1, NBPOIN
          IF ( SIGF .LE. SIG(K) ) THEN
            NBP = K-1
            GOTO 11
          END IF
 10     CONTINUE
      END IF
C
 11   CONTINUE
C-- 2 ------ INTERPOLATION LINEAIRE (Y = XP*X + B) ---------------------
      XN = EPS(NBP)
      XN1 = EPS(NBP+1)
      YN = SIG(NBP)
      YN1 = SIG(NBP+1)
      XP = (YN1-YN)/(XN1-XN)
      EPSF = ((SIGF-YN)/XP) + XN
C
C     ----- CONSTRUCTION DE LA COURBE KRD(LRD) COUPEE A LRDMAX ---------
C
      Z1 = (E*EPSF)/SIGF
      Z2 = (SIGY*LRDMAX**3)/(2*E*EPSF)
C
      NBP1 = NBP+1
      NBP2 = NBP+2
      NBP3 = NBP+3
      IF ( NBP3 .GT. 100 ) THEN
        CALL UTMESS ('F', 'DERTUY_F1', 'LA COURBE KRD(LRD) COUPEE'
     &               //' A PLUS DE 100 POINTS')
      END IF
C
      DO 20 K=1, NBP1
        LRDC(K) = LRD(K)
        KRDC(K) = KRD(K)
 20   CONTINUE
      LRDC(NBP2) = LRDMAX
      KRDC(NBP2) = 1.D0/SQRT(Z1+Z2)
      LRDC(NBP3) = LRDMAX
      KRDC(NBP3) = 0.D0
C
C     ----- CALCUL DU DECALAGE RHO -------------------------------------
C
      IF ( (LLIG/LFIS) .LE. 2.D0 ) THEN
        VAL1 = 0.12D0
        RHO = (VAL1*LFIS)/LLIG
      ELSE
        RHO = 0.D0
      END IF
C
C     ----- CALCUL DU FACTEUR D'AFFAIBLISSEMENT DU DEFAUT INTERNE ------
C
      QI = 1.D0 - (2.D0*(LFIS/EPAI)*(RINT+LLIG+(LFIS/2.D0))
     &              /(2.D0*RINT+EPAI))
C
C     ----- RESPECT DU NIVEAU DE CONTRAINTE MAXIMUM 2,4 SIGH -----------
C
      LRETMA = QI*(LRDMAX - RHO)
      LRHMAX = (2.4D0*SIGH*R8PI())/(4.D0*SIGY)
      IF ( LRHMAX .GE. LRETMA ) THEN
C ----- CONSTRUCTION DE LA PREMIERE TABLE DE RESULTAT ------------------
        CALL TBCRSD (RESULT, 'G')
        CALL TBAJPA (RESULT, 1, 'LONG_FISS_EQUI', 'R')
        LFIDEQ = LLIG+LFIS
        VR0(1) = LFIDEQ
        CALL TBAJLI (RESULT, 1, LPARA0, IBID, VR0, CBID, KBID, 0)
C
        CALL UTMESS ('A', 'DERTUY_A1', 'LE NIVEAU DE CONTRAINTE'
     &               //' DEPASSE LE NIVEAU MAX AUTORISE.')
        CALL UTMESS ('A', 'DERTUY_A2', 'ON NE PEUT GARANTIR'
     &               //' LA TENUE DU LIGAMENT.')
        CALL UTMESS ('A', 'DERTUY_A3', 'LE DEFAUT DEBOUCHANT'
     &               //' EQUIVALENT EST DONNE DANS LE FICHIER RESULTAT'
     &               //' DANS LA MEME UNITE DE LONGUEUR QUE LES'
     &               //' DONNEES GEOMETRIQUES.')
        GOTO 9999
      END IF
C
C     ----- CALCUL DU FACTEUR DE FORME DU DEFAUT INTERNE ---------------
C
      B = (LFIS/2.D0)+LLIG
      COASS = (LFIS/2.D0)/B
      IF (B .GT. (EPAI/2.D0)) THEN
        CALL UTMESS ('F', 'DERTUY_F2', 'LA LONGUEUR DU LIGAMENT PLUS LA'
     &               //' DEMI LONGUEUR DE LA FISSURE EST SUPERIEURE A'
     &               //' LA DEMI EPAISSEUR : LLIG+(LFIS/2)>(EPAI/2).')
      END IF
      COESB = 1.D0 - ((2.D0*B)/EPAI)
      NBPASS = 7
      NBPESB = 8
C
      IF ( COESB .LE. T2EST(1) ) THEN
        IADESB = 1
        GOTO 31
      ELSE IF ( COESB .GE. T2EST(NBPESB) ) THEN
        IADESB = NBPESB-1
        GOTO 31
      ELSE
        DO 30 I=1, NBPESB
          IF ( COESB .LE. T2EST(I) ) THEN
            IADESB = I-1
            GOTO 31
          END IF
 30     CONTINUE
      END IF
C
 31   CONTINUE
C
      IF ( COASS .LE. TASB(1) ) THEN
        IADASS = 1
        GOTO 41
      ELSE IF ( COASS .GE. TASB(NBPASS) ) THEN
        IADASS = NBPASS-1
        GOTO 41
      ELSE
        DO 40 I=1, NBPASS
          IF ( COASS .LE. TASB(I) ) THEN
            IADASS = I-1
            GOTO 41
          END IF
 40     CONTINUE
      END IF
C
 41   CONTINUE
C
      XN = T2EST(IADESB)
      XN1 = T2EST(IADESB+1)
      YN = TFFDI(IADESB,IADASS)
      YN1 = TFFDI(IADESB+1,IADASS)
      XP = (YN1-YN)/(XN1-XN)
      FFI1 = XP*(COESB-XN)+YN
C
      XN = T2EST(IADESB)
      XN1 = T2EST(IADESB+1)
      YN = TFFDI(IADESB,IADASS+1)
      YN1 = TFFDI(IADESB+1,IADASS+1)
      XP = (YN1-YN)/(XN1-XN)
      FFI2 = XP*(COESB-XN)+YN
C
      XN = TASB(IADASS)
      XN1 = TASB(IADASS+1)
      YN = FFI1
      YN1 = FFI2
      XP = (YN1-YN)/(XN1-XN)
      FFORMI = XP*(COASS-XN)+YN
C
C     ----- DETERMINATION DU DER DU DEFAUT INTERNE A PARTIR ------------
C     ----- DE RHO ET DU DER OPTION 2 (COURBE KRD(LRD) DECALEE DE RHO) -
C
      LRIC(1) = 0.D0
      KRIC(1) = 1.D0
      DO 50 K=2, NBP3
        IF ( LRDC(K) .GT. RHO ) THEN
          LRIC(K) = LRDC(K) - RHO
          KRIC(K) = KRDC(K)
        ELSE
          LRIC(K) = 0.D0
          KRIC(K) = 1.D0
        END IF
 50   CONTINUE
C
C     ----- BOUCLE SUR LR* AVEC LRET = SIG/SIGY -----------------------
C
      LRETMI = 0.05D0
      NLRET = 20
      NLRET1 = NLRET+1
      DLRET = (LRHMAX-LRETMI)/NLRET
      IF ( NLRET .GT. 100 ) THEN
        CALL UTMESS ('F', 'DERTUY_F3', 'LA COURBE LR_ETOILE A'
     &               //' PLUS DE 100 POINTS')
      END IF
C
      LFIDIN = LFIS/4.D0
      LFIDSU = 2.D0*LFIS
      IF ( LFIS .GT. 0.3D0*EPAI ) THEN
        CALL UTMESS ('F', 'DERTUY_F4', 'LE DEFAUT DEBOUCHANT MAXIMAL'
     &               //' EST TROP LONG POUR QUE L''ON PUISSE CALCULER'
     &               //' LE FACTEUR DE FORME CORRESPONDANT.')
      END IF
      NLFID = 50
      NLFID1 = NLFID+1
      DLFID = (LFIDSU-LFIDIN)/NLFID
      IF ( NLFID .GT. 100 ) THEN
        CALL UTMESS ('F', 'DERTUY_F5', 'LA COURBE '
     &               //'  KRD_ETOIL(LRD_ETOIL) A PLUS DE 100 POINTS')
      END IF
C
      DO 100 I=1, NLRET1
        LRET(I) = LRETMI + (I-1)*DLRET
        LRIET(I) = LRET(I)/QI
        IF ( LRIET(I) .LE. LRIC(1) ) THEN
          CALL UTMESS ('F', 'DERTUY_F6', 'LRIET EST INFERIEUR'
     &                 //' OU EGAL A ZERO')
        ELSE IF ( LRIET(I) .GE. LRIC(NBP3) ) THEN
          CALL UTMESS ('F', 'DERTUY_F7', 'LRIET EST SUPERIEUR A'
     &                 //' LRDMAX-RHO')
        ELSE
          DO 60 K=2, NBP3
            IF ( LRIET(I) .LE. LRIC(K) ) THEN
              IADLRI = K-1
              GOTO 61
            END IF
 60       CONTINUE
        END IF
C
 61     CONTINUE
C
        IF ( ABS(LRIC(IADLRI)) .LE. PREC .AND.
     &       ABS(LRIC(IADLRI+1)) .LE. PREC ) THEN
          LRIET(I) = 0.D0
          KRIET(I) = 1.D0
        ELSE
          XN = LRIC(IADLRI)
          XN1 = LRIC(IADLRI+1)
          YN = KRIC(IADLRI)
          YN1 = KRIC(IADLRI+1)
          XP = (YN1-YN)/(XN1-XN)
          KRIET(I) = XP*(LRIET(I)-XN)+YN
        END IF
        RI(I) = (LFIS/2.D0)*(FFORMI/KRIET(I))**2
C
C     ----- BOUCLE SUR LA TAILLE DU DEFAUT DEBOUCHANT ------------------
C
        DO 200 J=1, NLFID1
          LFIDP = LFIDIN + (J-1)*DLFID
C -- CALCUL DU FACTEUR D'AFFAIBLISSEMENT DU DEFAUT DEBOUCHANT QD -------
          QD(J) = 1.D0 - ((LFIDP/EPAI)*(2.D0*RINT+LFIDP))
     &                      /(2.D0*RINT+EPAI)
C -- CALCUL DU LR* DU DEFAUT DEBOUCHANT --------------------------------
          LRDET(J) = LRET(I)/QD(J)
C -- CALCUL DU KR* DU DEFAUT DEBOUCHANT --------------------------------
          IF ( LRDET(J) .LE. LRDC(1) ) THEN
            CALL UTMESS ('F', 'DERTUY_F8', 'LRDET EST INFERIEUR'
     &                   //' OU EGAL A ZERO')
          ELSE IF ( LRDET(J) .GE. LRDC(NBP3) ) THEN
            CALL UTMESS ('F', 'DERTUY_F9', 'LRDET EST SUPERIEUR'
     &                   //' A LRDMAX')
          ELSE
            DO 70 K=2, NBP3
              IF ( LRDET(J) .LE. LRDC(K) ) THEN
                IADLRD = K-1
                GOTO 71
              END IF
 70         CONTINUE
          END IF
C
 71       CONTINUE
C
          XN = LRDC(IADLRD)
          XN1 = LRDC(IADLRD+1)
          YN = KRDC(IADLRD)
          YN1 = KRDC(IADLRD+1)
          XP = (YN1-YN)/(XN1-XN)
          KRDET(J) = XP*(LRDET(J)-XN)+YN
C -- CALCUL DU FACTEUR DE FORME DU DEFAUT DEBOUCHANT -------------------
          LFIDEP(J) = LFIDP/EPAI
          NAST = 20
          IF ( LFIDEP(J) .LE. TAST(1) ) THEN
            IADAST = 1
            GOTO 81
          ELSE IF ( LFIDEP(J) .GE. TAST(NAST) ) THEN
            IADAST = NAST-1
            GOTO 81
          ELSE
            DO 80 K=2, NAST
              IF ( LFIDEP(J) .LE. TAST(K) ) THEN
                IADAST = K-1
                GOTO 81
              END IF
 80         CONTINUE
          END IF
C
 81       CONTINUE
C
          XN = TAST(IADAST)
          XN1 = TAST(IADAST+1)
          YN = TFFDD(IADAST)
          YN1 = TFFDD(IADAST+1)
          XP = (YN1-YN)/(XN1-XN)
          FFORMD = XP*(LFIDEP(J)-XN)+YN
          RD(J) = LFIDP*(FFORMD/KRDET(J))**2
C -- CALCUL DU RAPPORT : RARDRI = RD/RI --------------------------------
          RARDRI = RD(J)/RI(I)
C -- TEST DE LA VALEUR DE RARDRI ---------------------------------------
          IF ( RARDRI .GE. 1.D0 ) THEN
            GOTO 201
          END IF
 200    CONTINUE
C
 201    CONTINUE
        LFID(I) = LFIDP
 100   CONTINUE
C
C     ----- CONSTRUCTION DE LA TABLE DES RESULTATS ---------------------
C
      CALL TBCRSD (RESULT, 'G')
C
      CALL TBAJPA (RESULT, 1, 'EPAIS', 'R')
      CALL TBAJPA (RESULT, 1, 'LONG_LIGA_INT', 'R')
      CALL TBAJPA (RESULT, 1, 'LONG_FISS', 'R')
      CALL TBAJPA (RESULT, 1, 'DEXT', 'R')
      CALL TBAJPA (RESULT, 1, 'SIGM_ETOIL', 'R')
      CALL TBAJPA (RESULT, 1, 'LR_ETOIL', 'R')
      CALL TBAJPA (RESULT, 1, 'LONG_FISS_EQUI', 'R')
      CALL TBAJPA (RESULT, 1, 'SY_LR_MIN' , 'R')
      CALL TBAJPA (RESULT, 1, 'SY_LR_MAX' , 'R')
      CALL TBAJPA (RESULT, 1, 'LONG_FISS_MAXI', 'R')
      CALL TBAJPA (RESULT, 1, 'FACT_QI', 'R')
      CALL TBAJPA (RESULT, 1, 'RHO_LR', 'R')
      CALL TBAJPA (RESULT, 1, 'E', 'R')
      CALL TBAJPA (RESULT, 1, 'SY_02', 'R')
      CALL TBAJPA (RESULT, 1, 'SU', 'R')
      CALL TBAJPA (RESULT, 1, 'SH', 'R')
      CALL TBAJPA (RESULT, 1, 'LR_FISS_DEB_INT', 'R')
      CALL TBAJPA (RESULT, 1, 'KR_FISS_DEB_INT', 'R')
      CALL TBAJPA (RESULT, 1, 'LR_FISS_NON_DEB', 'R')
      CALL TBAJPA (RESULT, 1, 'KR_FISS_NON_DEB', 'R')
C
      VR1(1) = EPAI
      VR1(2) = LLIG
      VR1(3) = LFIS
      VR1(4) = 2*(RINT+EPAI)
      CALL TBAJLI (RESULT, 4, LPARA1, IBID, VR1, CBID, KBID, 0)
C
      VR2(1) = E
      VR2(2) = SIGY
      VR2(3) = SIGU
      VR2(4) = SIGH
      CALL TBAJLI (RESULT, 4, LPARA2, IBID, VR2, CBID, KBID, 0)
C
      VR3(1) = QI
      VR3(2) = RHO
      CALL TBAJLI (RESULT, 2, LPARA3, IBID, VR3, CBID, KBID, 0)
C
      DO 300 I=1, NLRET1
        VR4(1) = LRET(I)*SIGY
        VR4(2) = LRET(I)
        VR4(3) = LFID(I)
        CALL TBAJLI (RESULT, 3, LPARA4, IBID, VR4, CBID, KBID, 0)
 300  CONTINUE
C
      DO 310 I=1, NBP3
        VR5(1) = LRDC(I)
        VR5(2) = KRDC(I)
        VR5(3) = LRIC(I)
        VR5(4) = KRIC(I)
        CALL TBAJLI (RESULT, 4, LPARA5, IBID, VR5, CBID, KBID, 0)
 310  CONTINUE
C
      LFIDMA = LFID(1)
      DO 320 I=1, NLRET1
        IF ( LFIDMA .LE. LFID(I) ) THEN
          LFIDMA = LFID(I)
        END IF
 320  CONTINUE
C
      LFIDEQ = LLIG + LFIS
      IF ( LFIDMA .GE. LFIDEQ ) THEN
        LFIDMA = LFIDEQ
      END IF
C -- EMPLACEMENT DE LA PARTIE DU PROGRAMME NON EXPLOITE
      SIGINF = LRETMI*SIGY
      SIGSUP = LRETMA*SIGY
      VR6(1) = SIGINF
      VR6(2) = SIGSUP
      VR6(3) = LFIDMA
      CALL TBAJLI (RESULT, 3, LPARA6, IBID, VR6, CBID, KBID, 0)
C
9999  CONTINUE
C
      END
