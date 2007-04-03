      SUBROUTINE I3PDM2 ( EPSI, N, S, NBS, P, DEDANS )
      IMPLICIT   NONE
      INTEGER           NBS
      REAL*8            N(*), S(3,*), P(*), EPSI
      LOGICAL           DEDANS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     LE POINT P APPARTIENT-T-IL AU POLYGONE PLAN DE SOMMETS S
C     ------------------------------------------------------------------
C IN  N      : R : COMPOSANTE DE LA NORMALE AU PLAN CONTENANT LES S
C IN  P      : R : COMPOSANTE DU POINT
C IN  S      : R : TABLE(3,NBS) DES COORDONEES DES SOMMETS
C IN  NBS    : I : NOMBRE DE SOMMETS
C OUT DEDANS : L : REPONSE
C     ------------------------------------------------------------------
C
      INTEGER I,J
      REAL*8  XP,YP,ZP,XD,YD,ZD,XF,YF,ZF,NX,NY,NZ,VX,VY,VZ
      REAL*8  R11,R12,R13,R21,R22,R23,R31,R32,R33,PS,UN
C
C======================================================================
C
      UN     = 1.0D0
      DEDANS = .TRUE.
      R11    = N(1)*N(1)
      R12    = N(1)*N(2) - N(3)
      R13    = N(1)*N(3) + N(2)
      R21    = N(2)*N(1) + N(3)
      R22    = N(2)*N(2)
      R23    = N(2)*N(3) - N(1)
      R31    = N(3)*N(1) - N(2)
      R32    = N(3)*N(2) + N(1)
      R33    = N(3)*N(3)
      XP     = P(1)
      YP     = P(2)
      ZP     = P(3)
      I      = 1
10    CONTINUE
      IF ( DEDANS .AND. ( I .LE. NBS ) ) THEN
         J   = MAX(1,MOD(I+1,NBS+1))
         XD  = S(1,I)
         YD  = S(2,I)
         ZD  = S(3,I)
         XF  = S(1,J)
         YF  = S(2,J)
         ZF  = S(3,J)
         NX = XF - XD
         NY = YF - YD
         NZ = ZF - ZD
         PS = SQRT(NX*NX+NY*NY+NZ*NZ)
         IF ( PS .LE. EPSI*MAX(NX,NY,NZ) ) THEN
            CALL U2MESS('F','INTEMAIL_9')
         ELSE
            PS = UN/PS
            NX = NX*PS
            NY = NY*PS
            NZ = NZ*PS
         ENDIF
         VX     = R11*NX + R12*NY + R13*NZ
         VY     = R21*NX + R22*NY + R23*NZ
         VZ     = R31*NX + R32*NY + R33*NZ
         PS     =    VX*(XP-XD) + VY*(YP-YD) + VZ*(ZP-ZD)
         DEDANS = ( (PS .GE. 0.0D0) .OR. (ABS(PS) .LE. EPSI) )
         I  = I + 1
         GOTO 10
      ENDIF
      END
