      SUBROUTINE ASCNEW(F0,APPROX,W,ASCFON)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/99   AUTEUR AUBHHMB M.BONNAMY 
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
      IMPLICIT NONE
C
C     ARGUMENTS:
C     ----------
      REAL*8 F0,APPROX,W,ASCFON
C
C---------------------------------------------------------------
C     RESOLUTION DE L'EQUATION SCALAIRE F(W)=0
C---------------------------------------------------------------
C IN  F0    :R: VALEUR DE F EN 0 
C     APPROX:R: VALEUR APPROCHEE DE LA SOLUTION DE L'EQUATION
C     ASCFON:F: NOM DE LA FONCTION F
C OUT W     :R: SOLUTION DE L'EQUATION
C---------------------------------------------------------------
C     CETTE ROUTINE RESOUT L'EQUATION F(W) = 0 EN COMBINANT LES
C     METHODES DE DICHOTOMIE ET DE SECANTE.
C---------------------------------------------------------------
C
      INTEGER IFM,ITMEQL,N,IUNIFI
      REAL*8  EPSCA,X,FX,Y,FY,A,B,FA,FB,FW
C
C     INITIALISATIONS
C
      IFM   = IUNIFI('MESSAGE')     
      EPSCA = 1.D-4
      WRITE(IFM,100) 'PRECISION METHODE DE NEWTON : ',EPSCA
      ITMEQL = 10
      N = 1
      X = 0.D0
      FX = F0
      Y  = APPROX
      FY = ASCFON(Y)
C
C     DEBUT DES ITERATIONS
C
   10 CONTINUE
      IF (FY.GT.0.D0) THEN
        A = X
        B = Y
C        FA = FX
C        FB = FY
   20   CONTINUE
        W = Y - (Y-X)*FY/(FY-FX)
        IF (((W-A)*(W-B)).GT.0.D0) THEN
          W = (A+B)/2.D0
        ENDIF
        IF (ABS((Y-W)/W).LT.EPSCA) GO TO 90
        N = N + 1
        IF (N.GT.ITMEQL) GO TO 98
        FW = ASCFON(W)
        IF (FW.LT.0.D0) THEN
          A = W
        ELSE
          B = W
        ENDIF
        X = Y
        FX = FY
        Y = W
        FY = FW
C       FY = ASCFON(W)
        GO TO 20
      ELSE
        IF (FY.LT.FX) GO TO 99
        W = Y - (Y-X)*FY/(FY-FX)
        IF (ABS((Y-W)/W).LT.EPSCA) GO TO 90
        N = N + 1
        IF (N.GT.ITMEQL) GO TO 98
        X = Y
        FX = FY
        Y = W
        FY = ASCFON(W)
      ENDIF
      GO TO 10
   98 CONTINUE
      CALL UTMESS('F','ASCNEW','NOMBRE D''ITERATIONS MAX ATTEINT'
     &             //' LORS DE LA RESOLUTION DE L''EQUATION SCALAIRE')
   99 CONTINUE
      CALL UTMESS('F','ASCNEW','ERREUR DANS LA RESOLUTION DE ' 
     &             //'L''EQUATION SCALAIRE')
  100 FORMAT(T1,A,T31,F8.4)
   90 CONTINUE
      WRITE(IFM,'(T1,A,T25,I2)') 'NBE ITERATIONS NEWTON : ',N
      END
