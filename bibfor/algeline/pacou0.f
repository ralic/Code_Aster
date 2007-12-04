      SUBROUTINE PACOU0 (X,FVEC,QT,R,C,D,FVCOLD,G,P,S,T,W,XOLD,WORK,
     &                   CHECK,VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,
     &                   VECR5,VECI1,VG,INDIC,NBM,NMODE,NT)
      IMPLICIT REAL*8 (A-H,O-Z)
C -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C TOLE  CRP_21
C------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      REAL*8 QT(NT,*),R(NT,*),X(*),FVEC(*)
      REAL*8 C(*),D(*),FVCOLD(*),G(*),P(*)
      REAL*8 S(*),T(*),W(*),XOLD(*),WORK(*)
      REAL*8 MASG(*),AMOR(*)
      REAL*8 VECR1(*),VECR2(*),VECR3(*),VECR4(*),VECR5(*)
      INTEGER VECI1(*)
      LOGICAL RESTRT,SING,SKIP,CHECK
      CHARACTER*8 TYPFLU
      PARAMETER (EPS=1.0D-8,TOLX=EPS,TOLMIN=10.D0*EPS)
      PARAMETER (TOLF=1.D-04)
      PARAMETER (MAXITS=200,STPMX=100.D0)
C
C FONCTION FMIN
C -------------
      REAL*8 PACOU2
C
C -------------------------------------------------------------------
C
C --- TEST SEVERE POUR VOIR SI ON N'EST PAS DEJA SUR UN ZERO.
C
      CHECK = .FALSE.
      N = NT
C
      F = PACOU2 (X,FVEC,VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     &            VECI1,VG,INDIC,NBM,NMODE,NT)
      TEST = 0.0D0
      DO 11 I = 1,N
        IF (ABS(FVEC(I)).GT.TEST) TEST = ABS(FVEC(I))
   11 CONTINUE
      IF (TEST.LT.0.01D0*TOLF) GOTO 9999
C
      SUM = 0.0D0
      DO 12 I = 1,N
        SUM = SUM + X(I)**2
   12 CONTINUE
      STPMAX = STPMX*MAX(SQRT(SUM),DBLE(N))
      RESTRT = .TRUE.
C
C --- BOUCLE PRINCIPALE.
C
      DO 44 ITS = 1,MAXITS
        IF (RESTRT) THEN
C
          CALL PACOU1 (X,FVEC,R,WORK,SQRT(EPS),VECR1,VECR2,TYPFLU,
     &                 VECR3,AMOR,MASG,VECR4,VECR5,VECI1,VG,INDIC,NBM,
     &                 NMODE,NT)
          CALL PACOU4 (R,N,C,D,SING)
          IF (SING) THEN
            CHECK = .TRUE.
            GOTO 9999
          END IF
          DO 14 I = 1,N
            DO 13 J = 1,N
              QT(I,J) = 0.0D0
   13       CONTINUE
            QT(I,I) = 1.0D0
   14     CONTINUE
          DO 18 K = 1,N - 1
            IF (ABS(C(K)).GT.1.0D-30) THEN
              DO 17 J = 1,N
                SUM = 0.0D0
                DO 15 I = K,N
                  SUM = SUM + R(I,K)*QT(I,J)
   15           CONTINUE
                SUM = SUM/C(K)
                DO 16 I = K,N
                  QT(I,J) = QT(I,J) - SUM*R(I,K)
   16           CONTINUE
   17         CONTINUE
            END IF
   18     CONTINUE
          DO 21 I = 1,N
            R(I,I) = D(I)
            DO 19 J = 1,I - 1
              R(I,J) = 0.0D0
   19       CONTINUE
   21     CONTINUE
        ELSE
          DO 22 I = 1,N
            S(I) = X(I) - XOLD(I)
   22     CONTINUE
          DO 24 I = 1,N
            SUM = 0.0D0
            DO 23 J = 1,N
              SUM = SUM + R(I,J)*S(J)
   23       CONTINUE
            T(I) = SUM
   24     CONTINUE
          SKIP = .TRUE.
          DO 26 I = 1,N
            SUM = 0.0D0
            DO 25 J = 1,N
              SUM = SUM + QT(J,I)*T(J)
   25       CONTINUE
            W(I) = FVEC(I) - FVCOLD(I) - SUM
            IF (ABS(W(I)).GE.EPS* (ABS(FVEC(I))+ABS(FVCOLD(I)))) THEN
              SKIP = .FALSE.

            ELSE
              W(I) = 0.0D0
            END IF
   26     CONTINUE
          IF (.NOT.SKIP) THEN
            DO 28 I = 1,N
              SUM = 0.0D0
              DO 27 J = 1,N
                SUM = SUM + QT(I,J)*W(J)
   27         CONTINUE
              T(I) = SUM
   28       CONTINUE
            DEN = 0.0D0
            DO 29 I = 1,N
              DEN = DEN + S(I)**2
   29       CONTINUE
            DO 31 I = 1,N
              S(I) = S(I)/DEN
   31       CONTINUE
C
            CALL PACOU5(R,QT,N,T,S)
            DO 32 I = 1,N
              IF (ABS(R(I,I)).LE.1.0D-30) THEN
                CHECK = .TRUE.
                GOTO 9999
              END IF
              D(I) = R(I,I)
   32       CONTINUE
          END IF
        END IF
C
        DO 34 I = 1,N
          SUM = 0.0D0
          DO 33 J = 1,N
            SUM = SUM + QT(I,J)*FVEC(J)
   33     CONTINUE
          G(I) = SUM
   34   CONTINUE
        DO 36 I = N,1,-1
          SUM = 0.0D0
          DO 35 J = 1,I
            SUM = SUM + R(J,I)*G(J)
   35     CONTINUE
          G(I) = SUM
   36   CONTINUE
        DO 37 I = 1,N
          XOLD(I) = X(I)
          FVCOLD(I) = FVEC(I)
   37   CONTINUE
        FOLD = F
        DO 39 I = 1,N
          SUM = 0.0D0
          DO 38 J = 1,N
            SUM = SUM + QT(I,J)*FVEC(J)
   38     CONTINUE
          P(I) = -SUM
   39   CONTINUE
C
        CALL PACOU7 (R,N,D,P)
C
        CALL PACOU3 (XOLD,FOLD,G,P,X,F,FVEC,STPMAX,CHECK,TOLX,VECR1,
     &               VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,VECI1,
     &               VG,INDIC,NBM,NMODE,NT)
        TEST = 0.0D0
        DO 41 I = 1,N
          IF (ABS(FVEC(I)).GT.TEST) TEST = ABS(FVEC(I))
   41   CONTINUE
        IF (TEST.LT.TOLF) THEN
          CHECK = .FALSE.
          GOTO 9999
        END IF
        IF (CHECK) THEN
          IF (RESTRT) THEN
            GOTO 9999
          ELSE
            TEST = 0.00D0
            DEN = MAX(F,.50D0*DBLE(N))
            DO 42 I = 1,N
              TEMP = ABS(G(I))*MAX(ABS(X(I)),1.0D0)/DEN
              IF (TEMP.GT.TEST) TEST = TEMP
   42       CONTINUE
            IF (TEST.LT.TOLMIN) THEN
              GOTO 9999
            ELSE
              RESTRT = .TRUE.
            END IF
          END IF
        ELSE
          RESTRT = .FALSE.
          TEST = 0.0D0
          DO 43 I = 1,N
            TEMP = (ABS(X(I)-XOLD(I)))/MAX(ABS(X(I)),1.0D0)
            IF (TEMP.GT.TEST) TEST = TEMP
   43     CONTINUE
          IF (TEST.LT.TOLX) GOTO 9999
        END IF
   44 CONTINUE
      CHECK = .TRUE.
C
 9999 CONTINUE
      END
