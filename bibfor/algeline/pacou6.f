      SUBROUTINE PACOU6 ( R, QT, N,I, A, B )
      IMPLICIT REAL*8 (A-H,O-Z)
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      INTEGER N, I
      REAL*8 A, B, R(N,*), QT(N,*)
C ---------------------------------------------------------------------
C
      IF ( ABS(A) .LE. 1.0D-30 ) THEN
         C = 0.0D0
         S = SIGN(1.0D0,B)
C
      ELSE IF ( ABS(A) .GT. ABS(B) ) THEN
         FACT = B/A
         C = SIGN ( 1.0D0/SQRT(1.0D0+FACT*FACT), A )
         S = FACT*C
C
      ELSE
         FACT = A/B
         S = SIGN ( 1.0D0/SQRT(1.0D0+FACT*FACT), B )
         C = FACT*S
C
      END IF
C
      DO 11 J = 1, N
         Y = R(I,J)
         W = R(I+1,J)
         R(I,J)   = C*Y - S*W
         R(I+1,J) = S*Y + C*W
   11 CONTINUE
C
      DO 12 J = 1, N
         Y = QT(I,J)
         W = QT(I+1,J)
         QT(I,J)   = C*Y - S*W
         QT(I+1,J) = S*Y + C*W
   12 CONTINUE
C
      END
