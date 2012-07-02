      SUBROUTINE I2RGMA (EPSI,SOR,SEX,ROR,REX,M1,M2,FOR,FEX,
     +                   TSOR,TSEX,TROR,TREX,TFOR,TFEX,TM1,TM2,ADR)
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
C
      INTEGER ADR,FOR,FEX,TFOR(*),TFEX(*),TM1(*),TM2(*)
      REAL*8  SOR,SEX,ROR,REX,EPSI
      REAL*8  TSOR(*),TSEX(*),TROR(*),TREX(*)
C
      INTEGER I,J
      LOGICAL TROUVE,DEJALA
      REAL*8  S
C
C-----------------------------------------------------------------------
      INTEGER M1 ,M2 
C-----------------------------------------------------------------------
      TROUVE = .FALSE.
      DEJALA = .FALSE.
      S      = -1.0D0
      I      = 1
      J      = 0
C
10    CONTINUE
      IF ( (.NOT. TROUVE) .AND. (I .LT. ADR) ) THEN
C
         S = TSOR(I)
C
         IF ( ABS(S-SOR) .LT. EPSI ) THEN
C
            TROUVE = .TRUE.
            DEJALA = .TRUE.
C
         ELSE IF ( S .LT. SOR ) THEN
C
            I = I + 1
C
         ELSE
C
            TROUVE = .TRUE.
C
         ENDIF
C
         GOTO 10
C
      ENDIF
C
      IF ( DEJALA ) THEN
C
         TM2(I) = M1
C
      ELSE
C
         IF ( TROUVE ) THEN
C
            DO 20, J = ADR, I+1, -1
C
               TSOR(J) = TSOR(J-1)
               TSEX(J) = TSEX(J-1)
               TROR(J) = TROR(J-1)
               TREX(J) = TREX(J-1)
               TFOR(J) = TFOR(J-1)
               TFEX(J) = TFEX(J-1)
               TM1 (J) = TM1 (J-1)
               TM2 (J) = TM2 (J-1)
C
20          CONTINUE
C
            TSOR(I) = SOR
            TSEX(I) = SEX
            TROR(I) = ROR
            TREX(I) = REX
            TFOR(I) = FOR
            TFEX(I) = FEX
            TM1 (I) = M1
            TM2 (I) = M2
C
         ELSE
C
            TSOR(ADR) = SOR
            TSEX(ADR) = SEX
            TROR(ADR) = ROR
            TREX(ADR) = REX
            TFOR(ADR) = FOR
            TFEX(ADR) = FEX
            TM1 (ADR) = M1
            TM2 (ADR) = M2
C
         ENDIF
C
         ADR = ADR + 1
C
      ENDIF
C
      END
