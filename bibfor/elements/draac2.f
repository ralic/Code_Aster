      SUBROUTINE DRAAC2(A,B,C,X1,X2,KODE)

       IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
      REAL*8  A,B,C,X1,X2
      INTEGER KODE
      REAL*8  DELTA,EPSI,ZERO,X0,DEUZA,ASUP
      DATA ZERO/1.0D-8/

      X1    = 0.0D0
      X2    = 0.0D0
      KODE  = 0
      DELTA = 0.0D0
      EPSI  = ZERO * MAX(ABS(A),ABS(B),ABS(C))
      X0    = 0.0D0   
      DEUZA = 0.0D0
      ASUP  = 0.0D0
      
      IF(ABS(A).GT.EPSI) THEN
         X1    = B * B
         X2    = 4.0D0 * A * C
         DELTA = X1 - X2
         ASUP  = ZERO * MAX(X1,ABS(X2))
         DEUZA = 2.0D0 * A
         X0    = -B / DEUZA
         IF(DELTA.LT.-ASUP) THEN
            KODE = 0
            X1   = 0.0D0
            X2   = 0.0D0
         ELSEIF(DELTA.LT.ASUP) THEN
            KODE = 1
            X1   = X0
            X2   = X0
         ELSE
            KODE = 2
            X2   = SQRT(DELTA)/ABS(DEUZA)
            X1   = X0 - X2
            X2   = X0 + X2
         ENDIF

      ELSEIF(ABS(B).LE.EPSI) THEN
         KODE = 0
         X1   = 0.0D0
         X2   = 0.0D0

      ELSE
         KODE = 1
         X1   = - C / B
         X2   = X1
      ENDIF

      END
