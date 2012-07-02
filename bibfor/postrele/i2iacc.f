      SUBROUTINE I2IACC(EPSI,XC,YC,R,ALFINF,ALFSUP,X1,Y1,X2,Y2,X3,Y3,
     +                  NPI,A1,A2,A3,A4,R1,R2,R3,R4)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C
      INTEGER NPI
      REAL*8  EPSI,XC,YC,X1,X2,X3,Y1,Y2,Y3,R,ALFINF,ALFSUP
      REAL*8  A1,A2,A3,A4,R1,R2,R3,R4
C
      INTEGER NBRAC,I,PTA,TABORD(4),ORD
      REAL*8  COEF4,COEF3,COEF2,COEF1,COEF0,RAC
C
      REAL*8  AR,XR,YR,N1,N2,N3,N4
C
      REAL*8  INVR,PARAM(4),TABRAC(4),ANGLE(4)
      REAL*8  AUX,AUX1,AUX2,AUX3,AUX4
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NPI = 0
      A1  = 0.0D0
      A2  = 0.0D0
      A3  = 0.0D0
      A4  = 0.0D0
      R1  = 0.0D0
      R2  = 0.0D0
      R3  = 0.0D0
      R4  = 0.0D0
C
      DO 20, I = 1, 4, 1
C
         ANGLE  (I) = 0.0D0
         TABRAC (I) = 0.0D0
         PARAM  (I) = 0.0D0
         TABORD (I) = 0
C
20    CONTINUE
C
      INVR  = 1.0D0/R
      XR    = 0.0D0
      YR    = 0.0D0
C
      N1     =    X1 - 2*X2 + X3
      N3     =    Y1 - 2*Y2 + Y3
      N2     = -3*X1 + 4*X2 - X3
      N4     = -3*Y1 + 4*Y2 - Y3
      N1     =  2*N1
      N3     =  2*N3
C
      COEF4 = N1*N1 + N3*N3
      COEF3 = 2*(N1*N2 + N3*N4)
      COEF2 = 2*(N1*(X1-XC) + N3*(Y1-YC)) +N2*N2 + N4*N4
      COEF1 = 2*(N2*(X1-XC) + N4*(Y1-YC))
      COEF0 = (X1-XC)*(X1-XC) + (Y1-YC)*(Y1-YC) - R*R
C
      AUX  = 0.0D0
      AUX1 = 0.0D0
      AUX2 = 0.0D0
      AUX3 = 0.0D0
      AUX4 = 0.0D0
C
      NBRAC = 0
      RAC   = 0.0D0
      ORD   = 0
      I     = 1
      PTA   = 1
C
      CALL I2REQ4(EPSI,COEF4,COEF3,COEF2,COEF1,COEF0,NBRAC,
     +            TABRAC(1),TABRAC(2),TABRAC(3),TABRAC(4),
     +            TABORD(1),TABORD(2),TABORD(3),TABORD(4))
C
CC    WRITE(IFR,*)(TABRAC(IIG),IIG=1,4)
CC    WRITE(IFR,*)(TABORD(IIG),IIG=1,4)
C
      DO 10, I = 1, NBRAC, 1
C
         RAC = TABRAC(I)
         ORD = TABORD(I)
C
CC    WRITE(IFR,*)'I      = ',I
CC    WRITE(IFR,*)'TABRAC = ',TABRAC(I)
CC    WRITE(IFR,*)'TABORD = ',TABORD(I)
CC    WRITE(IFR,*)'RAC    = ',RAC
CC    WRITE(IFR,*)'ORD    = ',ORD
C
         IF ( ABS(RAC) .LT. EPSI ) THEN
C
            RAC = 0.0D0
C
         ENDIF
C
         IF ( ABS(RAC-1.0D0) .LT. EPSI ) THEN
C
            RAC = 1.0D0
C
         ENDIF
C
         IF ( (RAC .LE. 1.0D0) .AND. (RAC .GE. 0.0D0) ) THEN
C
            XR = (N1*RAC + N2)*RAC + X1
            YR = (N3*RAC + N4)*RAC + Y1
C
            AUX = INVR*(XR-XC)
C
CC          IF ( ABS(AUX - 1.0) .LT. EPSI ) THEN
            IF ( AUX .GT. 1.0D0 ) THEN
C
               AUX = 1.0D0
C
            ENDIF
C
CC          IF ( ABS(AUX + 1.0) .LT. EPSI ) THEN
            IF ( AUX .LT. -1.0D0 ) THEN
C
               AUX = -1.0D0
C
            ENDIF
C
      WRITE(6,*)'AUX = ',AUX
C
            AR = ACOS (AUX)
C
            IF ( ABS(YR-YC) .LT. EPSI) THEN
C
               YR = YC
C
            ENDIF
C
            IF ( YR .LT. YC ) THEN
C
               AR = -AR
C
            ENDIF
C
            IF ( (ORD .EQ. 1) .OR. (ABS(RAC)       .LT. EPSI)
     +                        .OR. (ABS(RAC-1.0D0)   .LT. EPSI)
     +                        .OR. (ABS(AR-ALFINF) .LT. EPSI)
     +                        .OR. (ABS(AR-ALFSUP) .LT. EPSI)
     +         ) THEN
C
               IF ( ABS(AR-ALFINF) .LT. EPSI ) THEN
C
                  AR = ALFINF
C
               ENDIF
C
               IF ( ABS(AR-ALFSUP) .LT. EPSI ) THEN
C
                  AR = ALFSUP
C
               ENDIF
C
               IF ( (AR .LE. ALFSUP) .AND. (AR .GE. ALFINF) ) THEN
C
                  ANGLE(PTA) = AR
                  PARAM(PTA) = RAC
                  PTA = PTA + 1
C
               ENDIF
C
            ENDIF
C
         ENDIF
C
10    CONTINUE
C
      NPI = PTA - 1
C
      IF ( NPI .GE. 2 ) THEN
C
         AUX1 = ANGLE(1)
         AUX2 = ANGLE(2)
C
         IF ( AUX1 .GT. AUX2 ) THEN
C
              ANGLE(1) = AUX2
              ANGLE(2) = AUX1
              AUX1     = PARAM(1)
              PARAM(1) = PARAM(2)
              PARAM(2) = AUX1
C
         ENDIF
C
      ENDIF
C
      IF ( NPI .GE. 3 ) THEN
C
         AUX1 = ANGLE(1)
         AUX2 = ANGLE(2)
         AUX3 = ANGLE(3)
C
         IF ( AUX1 .GT. AUX3 ) THEN
C
              ANGLE(1) = AUX3
              ANGLE(2) = AUX1
              ANGLE(3) = AUX2
              AUX1     = PARAM(3)
              PARAM(3) = PARAM(2)
              PARAM(1) = AUX1
C
         ELSE IF ( AUX2 .GT. AUX3 ) THEN
C
              ANGLE(2) = AUX3
              ANGLE(3) = AUX2
              AUX1     = PARAM(3)
              PARAM(3) = PARAM(2)
              PARAM(2) = AUX1
C
         ELSE
C
         ENDIF
C
      ENDIF
C
      IF ( NPI .GE. 4 ) THEN
C
         AUX1 = ANGLE(1)
         AUX2 = ANGLE(2)
         AUX3 = ANGLE(3)
         AUX4 = ANGLE(4)
C
         IF ( AUX1 .GT. AUX4 ) THEN
C
            ANGLE(1) = AUX4
            ANGLE(2) = AUX1
            ANGLE(3) = AUX2
            ANGLE(4) = AUX3
            AUX1     = PARAM(4)
            PARAM(4) = PARAM(3)
            PARAM(3) = PARAM(2)
            PARAM(2) = PARAM(1)
            PARAM(1) = AUX1
C
         ELSE IF ( AUX2 .GT. AUX4 ) THEN
C
            ANGLE(2) = AUX4
            ANGLE(3) = AUX2
            ANGLE(4) = AUX3
            AUX1     = PARAM(4)
            PARAM(4) = PARAM(3)
            PARAM(3) = PARAM(2)
            PARAM(2) = AUX1
C
         ELSE IF ( AUX3 .GT. AUX4) THEN
C
            ANGLE(3) = AUX4
            ANGLE(4) = AUX3
            AUX1     = PARAM(4)
            PARAM(4) = PARAM(3)
            PARAM(3) = AUX1
C
         ELSE
C
         ENDIF
C
      ENDIF

      A1 = ANGLE(1)
      R1 = PARAM(1)
      A2 = ANGLE(2)
      R2 = PARAM(2)
      A3 = ANGLE(3)
      R3 = PARAM(3)
      A4 = ANGLE(4)
      R4 = PARAM(4)
C
      END
