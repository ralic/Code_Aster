      SUBROUTINE PACOU5 ( R, QT, N, U, V )
      IMPLICIT REAL*8 (A-H,O-Z)
C --------------------------------------------------------------------
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
      INTEGER N 
      REAL*8 R(N,*), QT(N,*), U(*), V(*)
C ---------------------------------------------------------------------
C
      DO 11 K = N, 1, -1
         IF ( ABS(U(K)) .GT. 1.0D-30 ) GOTO 1
   11 CONTINUE
      K = 1
C
    1 CONTINUE
C
      DO 12 I = K-1, 1, -1
C
         CALL PACOU6 ( R, QT, N, I, U(I), -U(I+1) )
C
         IF ( ABS(U(I)) .LE. 1.0D-30 ) THEN
            U(I) = ABS(U(I+1))
C
         ELSE IF ( ABS(U(I)) .GT. ABS(U(I+1)) ) THEN
            U(I) = ABS(U(I)) * SQRT(1.0D0+(U(I+1)/U(I))**2)
C
         ELSE
            U(I) = ABS(U(I+1)) * SQRT(1.0D0+(U(I)/U(I+1))**2)
C
         END IF
   12 CONTINUE
C
      DO 13 J = 1, N
         R(1,J) = R(1,J) + U(1)*V(J)
   13 CONTINUE
C
      DO 14 I = 1, K-1
C
         CALL PACOU6 ( R, QT, N, I, R(I,I), -R(I+1,I) )
   14 CONTINUE
C
      END
