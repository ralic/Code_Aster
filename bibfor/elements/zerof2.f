      SUBROUTINE ZEROF2(F,F0,XAP,EPSI,NITMAX,SOLU,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/09/2005   AUTEUR LEBOUVIE F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ARGUMENTS:
C     ----------
      REAL*8       F,F0,XAP,EPSI,SOLU
      INTEGER      NITMAX, IRET
C ----------------------------------------------------------------------
C     BUT:
C         TROUVER UNE RACINE DE L'EQUATION F(X)=0
C         ON SUPPOSE QUE LA FONCTION F EST CROISSANTE ET QUE F(0)<0
C         ON EMPLOIE LA METHODE DE SECANTE UTILISEE DANS ZEROFO AVEC
C          EN PLUS UN "COUP" DE DICHOTOMIE TOUS LES 3 ITERATIONS
C          POUR FACILITER LA CONVERGENCE SI F EST TRES NON-LINEAIRE
C
C     IN:
C         F  : FONCTION DONT ON CHERCHE LE "ZERO"
C         F0 : VALEUR DE F AU POINT 0 (CETTE VALEUR EST DONNEE ET NON
C              CALCULEE PAR LE PROGRAMME PAR ECONOMIE).
C         XAP: APPROXIMATION DE LA SOLUTION.
C        EPSI: TOLERANCE ABSOLU SUR LE ZERO CHERCHE : ABS(F(SOLU))<EPSI
C      NITMAX: NOMBRE MAXI D'ITERATIONS AUTORISEES.
C
C     OUT:
C         SOLU: VALEUR DE LA RACINE CHERCHEE.
C         IRET: CODE RETOUR DE LA RECHERCHE DE ZERO DE F(X)=0
C                   IRET=0 => PAS DE PROBLEME
C                   IRET=1 => ECHEC
C ----------------------------------------------------------------------
      REAL*8 FY,FZ,X,Y,Z,A,B,FA,FB,FDBG(20),XDBG(20),ECRESD,FX
      INTEGER N,K,ND
C DEB-------------------------------------------------------------------
C
C     INITIALISATIONS
C


      N = 1
      X = 0.D0
      FX = F0
      Y = XAP
      FY = F(Y)

      IF (ABS(FY).LT.EPSI) THEN
        Z = Y
        GO TO 90
      ENDIF

      IF (ABS(X-Y).LE.1D-15) THEN
           GOTO 100
      ENDIF
C
C     DEBUT DES ITERATIONS
C
   10 CONTINUE
      IF (FY.GT.0.D0) THEN
        A = X
        B = Y
        FA = FX
        FB = FY
C       ND = INT(SQRT(DBLE(NITMAX)))
        ND = 3
   20   CONTINUE
        IF ((N-(N/ND)*ND).EQ.0) THEN
          Z = (A+B)*0.5D0
        ELSE
          Z = (A*FB-B*FA)/(FB-FA)
        ENDIF
C
        N = N + 1
        FZ = F(Z)

        IF (ABS(FZ).LT.EPSI) GO TO 90
        ECRESD = ABS(B-A)
C SOLUTION PROVISOIRE PERMETTANT DE PASSER LES CAS 
C DIFFICILES CF AL98-193 AL98-197
C IL FAUDRAIT FAIRE MIEUX....
        IF (ECRESD.LE.(EPSI*B)) GO TO 90
        IF (N.GT.NITMAX) GO TO 98
        IF (FZ.LT.0.D0) THEN
          A = Z
          FA = FZ
        ELSE
          B = Z
          FB = FZ
        ENDIF
        GO TO 20
      ELSE

        IF (FY.LT.FX) GO TO 99

        IF (FY.EQ.FX) THEN 
          GOTO 100
        ENDIF

        Z = (X*FY-Y*FX)/(FY-FX)


        IF (ABS(Z-Y).LE.1D-15) THEN
           GOTO 100
        ENDIF

        N = N + 1
        X = Y
        FX = FY
        Y = Z
        FY = F(Z)

C
        IF (ABS(FY).LT.EPSI) GO TO 90
        IF (N.GT.NITMAX) GO TO 98
      ENDIF
      GO TO 10
C
   90 CONTINUE
      SOLU=Z
      GO TO 9999
C
   98 CONTINUE
      IRET = 1
      GOTO 9999
C
   99 CONTINUE
      DO 21 K=1,20
        XDBG(K) = XAP/(21-K)
        FDBG(K) = F((XAP)/(21-K))
   21 CONTINUE
      CALL UTDEBM ('F','ZEROF2','ECHEC DE LA RECHERCHE DE ZERO')
      CALL UTIMPI ('S',' A L''ITERATION : ',1,N)
      CALL UTIMPR ('L',' FONCTION DECROISSANTE - POUR X=A: ',1,X)
      CALL UTIMPR ('S',' / FONCTION(A): ',1,FX)
      CALL UTIMPR ('L','                         ET   X=B: ',1,Y)
      CALL UTIMPR ('S',' / FONCTION(B): ',1,FY)
      CALL UTIMPR ('L',' FONCTION X=: ',20,XDBG)
      CALL UTIMPR ('L',' FONCTION F=: ',20,FDBG)
      CALL UTFINM ()

  100 CONTINUE
      DO 22 K=1,20
        XDBG(K) = XAP/(21-K)
        FDBG(K) = F((XAP)/(21-K))
   22 CONTINUE
      CALL UTDEBM ('F','ZEROF2','ECHEC DE LA RECHERCHE DE ZERO')
      CALL UTIMPI ('S',' A L''ITERATION : ',1,N)
      CALL UTIMPR ('L',' FONCTION CONSTANTE    - POUR X=A: ',1,X)
      CALL UTIMPR ('S',' / FONCTION(A): ',1,FX)
      CALL UTIMPR ('L','                         ET   X=B: ',1,Y)
      CALL UTIMPR ('S',' / FONCTION(B): ',1,FY)
      CALL UTIMPR ('L',' FONCTION X=: ',20,XDBG)
      CALL UTIMPR ('L',' FONCTION F=: ',20,FDBG)

      CALL UTFINM ()
C
 9999 CONTINUE
      END
