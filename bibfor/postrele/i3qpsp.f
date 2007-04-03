      SUBROUTINE I3QPSP(EPSI,K,F,SGT,COORSM,RES,NBPT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER K,F,NBPT
      REAL*8 EPSI,SGT(*),COORSM(3,*),RES(3,*)
C
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
C     INTERSECTION PROJ DE FACE QUAD GAUCHE DANS PLAN AVEC SGT PROJ
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  K      : I : -
C IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
C IN  SGT    : R : TABLE(1..6)      : COORDO DU  SGT DANS REPERE FACE
C IN  COORSM : R : TABLE(1..3,1..4) : COORDO DES SOMMETS PROJ
C OUT RES    : R : TABLE(1..3,1..3) : COLONE = (COORDO_REF,ABSC_SGT)
C OUT NBPT   : I : NOMBRE DE POINT TROUVE
C     ------------------------------------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM,JEXNOM
C
      INTEGER ARETE,ND,NF,NBA,I,IRET
      INTEGER VALI(3)
      REAL*8  T,NDF,ZERO,UN,X(3),NAB
      REAL*8  A11,A12,A21,A22,B1,B2,R1,R2,R3,T1,T2
      REAL*8  XA,YA,XB,YB,XD,YD,XF,YF,TD,TF
      LOGICAL FINF,ATRV,BTRV,PB
C
C======================================================================
C
      ARETE =  0
      NBA   =  4
      ZERO  =  0.0D0
      UN    =  1.0D0
      FINF  = .FALSE.
      PB    = .FALSE.
      ATRV  = .FALSE.
      BTRV  = .FALSE.
      NBPT  =  0
      X(1)  = ZERO
      X(2)  = ZERO
      X(3)  = UN
      XA    = SGT(1)
      XB    = SGT(4)
      YA    = SGT(2)
      YB    = SGT(5)
      NAB   = UN/((XA-XB)*(XA-XB)+(YA-YB)*(YA-YB))
100   CONTINUE
      IF ( .NOT. FINF ) THEN
         ARETE = ARETE + 1
         ND    = ARETE
         NF    = MAX(MOD(ND+1,NBA+1),1)
         XD    = COORSM(1,ND)
         XF    = COORSM(1,NF)
         YD    = COORSM(2,ND)
         YF    = COORSM(2,NF)
         T     = MAX(ABS(XF),ABS(XD),ABS(YD),ABS(YF))
         NDF   = SQRT((YF-YD)*(YF-YD) +(XF-XD)*(XF-XD))
         A11   = XF - XD
         A21   = YF - YD
         A12   = XA - XB
         A22   = YA - YB
         B1    = XA -XD
         B2    = YA -YD
         IF ( NDF .LE. EPSI*T ) THEN
            VALI (1) = K
            VALI (2) = F
            VALI (3) = ARETE
            CALL U2MESI('F', 'INTEMAIL_26',3,VALI)
         ELSE
            R1  = MAX(ABS(A11),ABS(A12))
            IF ( ABS(R1) .GT. EPSI ) THEN
               R1  = UN/R1
               A11 = A11*R1
               A12 = A12*R1
               B1  = B1 *R1
            ENDIF
            R1  = MAX(ABS(A21),ABS(A22))
            IF ( ABS(R1) .GT. EPSI ) THEN
               R1  = UN/R1
               A21 = A21*R1
               A22 = A22*R1
               B2  = B2 *R1
            ENDIF
            R1  = A11*A22-A21*A12
            R2  = A11*B2 -A21*B1
            IF ( ABS(R1) .LE. EPSI ) THEN
               IF ( ABS(R2) .LE. EPSI ) THEN
                  FINF = .TRUE.
                  NDF  =  UN/NDF
                  TD   = ((XD-XA)*(XB-XA)+(YD-YA)*(YB-YA))*NAB
                  TF   = ((XF-XA)*(XB-XA)+(YF-YA)*(YB-YA))*NAB
                  T1   =  MAX(MIN(TD,TF),ZERO)
                  T2   =  MIN(MAX(TD,TF),UN)
                  IF ( ABS(T1-T2) .LE. EPSI ) THEN
                     NBPT = 1
                     T    = (T1 + T2)*0.5D0
                     R3   = (T-TD)/(TF-TD)
                     CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
                     RES(1,1) = R1
                     RES(2,1) = R2
                     RES(3,1) = T
                  ELSE IF ( T1 .LT. T2 ) THEN
                     NBPT = 2
                     T    =  T1
                     R3   = (T-TD)/(TF-TD)
                     CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
                     RES(1,1) = R1
                     RES(2,1) = R2
                     RES(3,1) = T
                     T    =  T2
                     R3   = (T-TD)/(TF-TD)
                     CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
                     RES(1,2) = R1
                     RES(2,2) = R2
                     RES(3,2) = T
                  ELSE
                     NBPT = 0
                  ENDIF
               ENDIF
            ELSE
               R1 = UN/R1
               R2 = R2*R1
               R1 = (A22*B1-A12*B2)*R1
               IF ( ABS(R1) .LT. EPSI ) THEN
                  R1 = ZERO
               ENDIF
               IF ( ABS(R2) .LT. EPSI ) THEN
                  R2 = ZERO
               ENDIF
               IF ( ABS(R2-UN) .LT. EPSI ) THEN
                  R2 = UN
               ENDIF
               IF ( ABS(R1-UN) .LT. EPSI ) THEN
                  R1 = UN
               ENDIF
               IF ( (R1 .GE. ZERO) .AND. ((R1-UN) .LE. EPSI) .AND.
     +              (R2 .GE. ZERO) .AND. ((R2-UN) .LE. EPSI) ) THEN
                  IF ( NBPT .EQ. 0 ) THEN
                     NBPT        = NBPT + 1
                     RES(3,NBPT) = R2
                     CALL I3CRAD(K,F,ARETE,NBA,R1,R2,R3)
                     RES(1,NBPT) = R2
                     RES(2,NBPT) = R3
                  ELSE IF ( ABS(R2 - RES(3,1)) .GT. EPSI ) THEN
                     NBPT        = NBPT + 1
                     RES(3,NBPT) = R2
                     CALL I3CRAD(K,F,ARETE,NBA,R1,R2,R3)
                     RES(1,NBPT) = R2
                     RES(2,NBPT) = R3
                  ELSE
                  ENDIF
               ENDIF
            ENDIF
            FINF = ( FINF .OR. (ARETE .EQ. NBA) )
         ENDIF
         GOTO 100
      ENDIF
      IF ( NBPT .EQ. 0 ) THEN
         CALL I3PDM2(EPSI,X,COORSM,NBA,SGT,   ATRV)
         CALL I3PDM2(EPSI,X,COORSM,NBA,SGT(4),BTRV)
         IF (  ATRV .AND. BTRV ) THEN
            R1 = SGT(1)
            R2 = SGT(2)
            CALL I3CRQP(EPSI,EPSI,COORSM,R1,R2,X,IRET)
            IF ( IRET .NE. -1 ) THEN
               R1 = X(1)
               R2 = X(2)
               RES(1,1) = R1
               RES(2,1) = R2
               RES(3,1) = ZERO
               R1 = SGT(4)
               R2 = SGT(5)
               CALL I3CRQP(EPSI,EPSI,COORSM,R1,R2,X,IRET)
               IF ( IRET .NE. -1 ) THEN
                  R1 = X(1)
                  R2 = X(2)
                  RES(1,2) = R1
                  RES(2,2) = R2
                  RES(3,2) = UN
               ELSE
                  PB = .TRUE.
               ENDIF
            ELSE
               PB = .TRUE.
            ENDIF
            NBPT = 2
         ENDIF
      ELSE IF ( NBPT .EQ. 1 ) THEN
         T = RES(3,1)
         IF ( (ABS(T-UN) .LT. EPSI) .OR. (ABS(T) .LT. EPSI) ) THEN
            IF ( ABS(T-UN) .LT. EPSI ) THEN
               I = 1
               T = ZERO
            ELSE
               I = 4
               T = UN
            ENDIF
            CALL I3PDM2(EPSI,X,COORSM,NBA,SGT(I),ATRV)
            IF ( ATRV ) THEN
               R1 = SGT(I)
               R2 = SGT(I+1)
               CALL I3CRQP(EPSI,EPSI,COORSM,R1,R2,X,IRET)
               IF ( IRET .NE. -1 ) THEN
                  R1 = X(1)
                  R2 = X(2)
                  RES(1,2) = R1
                  RES(2,2) = R2
                  RES(3,2) = T
                  NBPT     = 2
               ELSE
                  PB   = .TRUE.
                  NBPT = 0
               ENDIF
            ENDIF
         ELSE
            CALL I3PDM2(EPSI,X,COORSM,NBA,SGT,   ATRV)
            CALL I3PDM2(EPSI,X,COORSM,NBA,SGT(4),BTRV)
            IF ( ATRV ) THEN
               R1 = SGT(1)
               R2 = SGT(2)
               CALL I3CRQP(EPSI,EPSI,COORSM,R1,R2,X,IRET)
               IF ( IRET .NE. -1 ) THEN
                  R1 = X(1)
                  R2 = X(2)
                  RES(1,2) = R1
                  RES(2,2) = R2
                  RES(3,2) = ZERO
                  NBPT     = 2
               ELSE
                  PB = .TRUE.
               ENDIF
            ELSE IF ( BTRV ) THEN
               R1 = SGT(4)
               R2 = SGT(5)
               CALL I3CRQP(EPSI,EPSI,COORSM,R1,R2,X,IRET)
               IF ( IRET .NE. -1 ) THEN
                  R1 = X(1)
                  R2 = X(2)
                  RES(1,2) = R1
                  RES(2,2) = R2
                  RES(3,2) = UN
                  NBPT     = 2
               ELSE
                  PB = .TRUE.
               ENDIF
            ELSE
               NBPT = 0
            ENDIF
         ENDIF
      ELSE
      ENDIF
      IF ( NBPT .EQ. 2 ) THEN
         R1 = RES(3,1)
         R2 = RES(3,2)
         IF ( R2 .LT. R1 ) THEN
            RES(3,1) = R2
            RES(3,2) = R1
            R1       = RES(1,1)
            RES(1,1) = RES(1,2)
            RES(1,2) = R1
            R1       = RES(2,1)
            RES(2,1) = RES(2,2)
            RES(2,2) = R1
         ENDIF
      ENDIF
      IF ( PB ) THEN
         VALI (1) = K
         VALI (2) = F
         CALL U2MESI('F', 'INTEMAIL_24',2,VALI)
      ENDIF
      END
