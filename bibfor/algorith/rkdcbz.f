      SUBROUTINE RKDCBZ( MOD,  NVI,   VINI,  COEFT, E,
     &                   NU,   ALPHA, X,     DTIME, SIGI,
     &                   EPSD, DETOT, TPERD, DTPER, TPEREF,
     &                   DVIN, IMAT )
      IMPLICIT REAL*8(A-H,O-Z)
C       ===============================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/04/2003   AUTEUR JMBHH01 J.M.PROIX 
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
C       ---------------------------------------------------------------
C     POLY_CFC  : RELATION DE COMPORTEMENT POLYCRISTALLIN (B.Z.)
C                  A 13 PARAMETRES
C                  POUR MATERIAU CUBIQUE A FACES CENTREES
C                 (CHARGEMENT MONOTONE SEULEMENT)
C     INTEGRATION DE LA LOI (POLY_CFC) PAR UNE METHODE DE RUNGE KUTTA
C
C     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
C     INTERNES DU MODELE
C     ----------------------------------------------------------------
C     IN  MOD     :  TYPE DE MODELISATION
C         NVI     :  NOMBRE DE VARIABLES INTERNES
C         VINI    :  VARIABLES INTERNES
C         COEFT   :  COEFFICIENTS MATERIAU INELASTIQUE
C         E       :  COEFFICIENT MODULE D'YOUNG
C         NU      :  COEFFICIENT DE POISSON
C         ALPHA   :  COEFFICIENT DE DILATATION THERMIQUE
C         X       :  INSTANT COURANT
C         DTIME   :  INTERVALLE DE TEMPS
C         SIGI    :  CONTRAINTES A L'INSTANT COURANT
C         EPSD    :  DEFORMATION TOTALE A T
C         DETOT   :  INCREMENT DE DEFORMATION TOTALE
C         TPERD   :  TEMPERATURE A T
C         DTPER   :  INTERVALLE DE TEMPERATURE ENTRE T+DT ET T
C         TPEREF  :  TEMPERATURE DE REFERENCE
C     OUT DVIN    :  DERIVEES DES VARIABLES INTERNES
C     ----------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER     IADTRC, NM, NBSG, NC2, NGRAIN, NP, NG
      CHARACTER*2 CODE
      CHARACTER*8 MOD
      REAL*8 NU
      PARAMETER(NMAT=50)
      REAL*8 GRB(6),SIGLOC(6),DEVLOC(6),EVIGR(6),PTQ(12)
      REAL*8 SIGI(6),EPSD(6),DETOT(6)
      REAL*8 EVI(6),BETGR(240),GAMMA(480),ALMIC(480),GVCUM(480)
      REAL*8 DEVI(6),DBETGR(240),DGAMMA(480),DALMIC(480),DGVCUM(480)
      REAL*8 COEFT(NMAT)
      REAL*8 VINI(NVI)
      REAL*8 DVIN(NVI)
      REAL*8 N,K
      DATA ZE/0.0D0/ UD/0.5D0/ UN/1.0D0/ TD/1.5D0/ SMALL/1.0D-20/
C     ----------------------------------------------------------------
C
C --    COEFFICIENTS MATERIAU INELASTIQUE
C
        N=COEFT(3)
        K=COEFT(4)
        TAU0=COEFT(5)
        Q1=COEFT(6)
        B1=COEFT(7)
        HL=COEFT(8)
        Q2=COEFT(9)
        B2=COEFT(10)
        C1=COEFT(11)
        D1=COEFT(12)
        C2=COEFT(13)
C
C --    LA TEXTURE 
C
        CALL RCADMA ( IMAT, 'POLY_CFC', 'TEXTURE', IADTRC, CODE, 'FM' )
C        NBBLOC = NINT( ZR(IADTRC) )
        NM     = NINT( ZR(IADTRC+1) )
        NBSG   = NINT( ZR(IADTRC+2) )
        NC2    = NINT( ZR(IADTRC+(NM*NBSG)+3) )
        NGRAIN = NINT( ZR(IADTRC+(NM*NBSG)+4) )
C        NC3    = NINT( ZR(IADTRC+(NM*NBSG+NC2*NGRAIN)+5) )
C        NL3    = NINT( ZR(IADTRC+(NM*NBSG+NC2*NGRAIN)+6) )
        NP = IADTRC + 3 + NM*NBSG + 2 - 1
        NG = IADTRC + 3 + NM*NBSG + 2 + NC2*NGRAIN + 2 - 1
C
C --    VARIABLES INTERNES
C
        DO 1 I=1,6
          EVI(I)=VINI(I)
   1    CONTINUE
        DO 2 J=1,NGRAIN
          JK=6*(J-1)
          DO 3 KK=1,6
            BETGR(JK+KK)=VINI(6+(JK+KK))
   3      CONTINUE
          JL=12*(J-1)
          DO 4 L=1,12
            GAMMA(JL+L)=VINI(246+(JL+L))
            ALMIC(JL+L)=VINI(726+(JL+L))
            GVCUM(JL+L)=VINI(1206+(JL+L))
   4      CONTINUE
   2    CONTINUE
        EVCUM=VINI(NVI-1)
C       ----------------------------------------------------------------
        DO 10 I=1,6
          GRB(I)=ZE
          DEVI(I)=ZE
   10   CONTINUE
        CALL CALSIG(EVI,MOD,E,NU,ALPHA,X,DTIME,EPSD,DETOT,
     &              TPERD,DTPER,TPEREF,SIGI)
        DO 11 IGR=1,NGRAIN
          IBG=6*(IGR-1)
          DO 12 I=1,6
            GRB(I)=GRB(I)+BETGR(IBG+I)*ZR(NP+(4*IGR))
   12     CONTINUE
   11   CONTINUE
C
C --    LOCALISATION BZ
C
          DEMU=E/(UN+NU)
          IF (EVCUM.GT.ZE) THEN
            SIGMI=ZE
            DO 13 I=1,6
              SIGMI=SIGMI+SIGI(I)**2
   13       CONTINUE
            SIGMI=SQRT(TD*SIGMI-UD*(SIGI(1)+SIGI(2)+SIGI(3))**2)
            ALFBZ=UN/(UN+TD*DEMU*EVCUM/SIGMI)
          ELSE
            ALFBZ=UN
          END IF
          DLOC=ZE
          DA=ZE
          BETA=(4.0D0-5.0D0*NU)/(UN-NU)/7.5D0
          DMUMB=DEMU*(UN-BETA)
C
C --    BOUCLE SUR LES GRAINS
C
        DO 14 IGR=1,NGRAIN
          KSG=(IGR-1)*12
          DO 15 I=1,6
            DEVLOC(I)=ZE
            EVIGR(I)=ZE
   15     CONTINUE
          IBG=6*(IGR-1)
          DO 16 I=1,6
            SIGLOC(I)=SIGI(I)+ALFBZ*DMUMB*(GRB(I)-BETGR(IBG+I))
   16     CONTINUE
C
C --      BOUCLE SUR LES SYSTEMES
C
          DO 18 ISG=1,12
            LMS=6*(KSG+(ISG-1))
            LSG=KSG+ISG
            IF (ISG.EQ.1) THEN
              RRR=ZE
              IF (ABS(Q1).GT.SMALL) THEN
                DO 19 JSG=1,12
                  PTQ(JSG)=UN-EXP(-B1*GVCUM(KSG+JSG))
                  RRR=RRR+PTQ(JSG)
   19           CONTINUE
              ELSE
                DO 20 JSG=1,12
                  PTQ(JSG)=ZE
   20           CONTINUE
              END IF
            END IF
            TMX=ZE
            DO 21 I=1,6
              TMX = TMX + SIGLOC(I)*ZR(NG+LMS+I)
   21       CONTINUE
            XSG=C1*ALMIC(LSG)
            TMX=TMX-XSG-C2*GAMMA(LSG)
CJMP

            PTQ(ISG)=UN-EXP(-B1*GVCUM(LSG))

            CRT=ABS(TMX)-TAU0-Q1*(HL*RRR+(UN-HL)*PTQ(ISG))
            CRT=CRT-Q2*(UN-EXP(-B2*GVCUM(LSG)))
            CRT=CRT+UD*D1*XSG*XSG/C1
            IF (CRT.LE.ZE) THEN
              DGVCUM(LSG)=ZE
              DALMIC(LSG)=ZE
              DGAMMA(LSG)=ZE
            ELSE
              SIGN=TMX/ABS(TMX)
              DGAMCU=(CRT/K)**N
CV            DGAMCU=EXP(LOG(CRT/K)*N)
              DGVCUM(LSG)=DGAMCU
              DALMIC(LSG)=DGAMCU*(SIGN-D1*ALMIC(LSG))
              DGAMMA(LSG)=DGAMCU*SIGN
              DO 22 I=1,6
                DEVLOC(I)=DEVLOC(I)+DGAMCU*SIGN*ZR(NG+LMS+I)
   22         CONTINUE
            ENDIF
            DO 23 I=1,6
              EVIGR(I)=EVIGR(I)+GAMMA(LSG)*ZR(NG+LMS+I)
   23       CONTINUE
   18     CONTINUE
          DECUMG=ZE
          DO 25 I=1,6
            DECUMG=DECUMG+DEVLOC(I)**2
   25     CONTINUE
          DECUMG=SQRT(DECUMG/TD)
          DO 26 I=1,6
            DEVI(I)=DEVI(I)+DEVLOC(I)*ZR(NP+(4*IGR))
            TACCOM=BETGR(IBG+I)-DA*EVIGR(I)
            DBETGR(IBG+I)=DEVLOC(I)-DLOC*TACCOM*DECUMG
   26     CONTINUE
   14   CONTINUE
        DEVCUM=ZE
        DO 27 I=1,6
          DEVCUM=DEVCUM+DEVI(I)**2
   27   CONTINUE
        DEVCUM=SQRT(DEVCUM/TD)
CJMP        IF (DEVCUM.LE.ZE) THEN
CJMP        ELSE
CJMP        END IF
        DETAT=ZE
C
C --    DERIVEES DES VARIABLES INTERNES
C
CJMP
        CALL R8INIR ( NVI , 0.D0 , DVIN , 1 )
        DO 5 I=1,6
          DVIN(I)=DEVI(I)
   5    CONTINUE
        DO 6 J=1,NGRAIN
          JK=6*(J-1)
          DO 7 KK=1,6
            DVIN(6+(JK+KK))=DBETGR(JK+KK)
   7      CONTINUE
          JL=12*(J-1)
          DO 8 L=1,12
            DVIN(246+(JL+L))=DGAMMA(JL+L)
            DVIN(726+(JL+L))=DALMIC(JL+L)
            DVIN(1206+(JL+L))=DGVCUM(JL+L)
   8      CONTINUE
   6    CONTINUE
        DVIN(NVI-1)=DEVCUM
        DVIN(NVI)=DETAT
        END
