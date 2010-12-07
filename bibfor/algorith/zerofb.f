      SUBROUTINE ZEROFB(FUNC,X1,X2,TOL,ITMAX,ZBRENT,IRET,ITER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/12/2010   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================

      IMPLICIT NONE

      INTEGER  ITMAX,ITER,IRET
      REAL*8   ZBRENT,TOL,X1,X2,FUNC,EPS,R8PREM
      REAL*8   A,B,C,D,E,FA,FB,FC,P,Q,R,S,TOL1,XM
      EXTERNAL FUNC
C ----------------------------------------------------------------------
C TOLE CRP_7
C     BUT : TROUVER LE ZERO D'UNE FONCTION SCALAIRE REELLE 
C     AVEC LA METHODE DE BRENT

C     USING BRENT'S METHOD, FIND THE ROOT OF A FUNCTION FUNC KNOWN TO 
C     LIE BETWEEN X1 AND X2. THE ROOT, RETURNED AS ZBRENT, WILL BE 
C     REFINED UNTIL ITS ACCURACY IS TOL.
C     PARAMETERS: MAXIMUM ALLOWED NUMBER OF ITERATIONS

C IN  FUNC    : FONCTION F
C IN  X1, X2  : INTERVELLE DE RECHERCHE
C IN  TOL     : PRECISION ABSOLUE : LA SOLUTION X EST TELLE QUE F(X)<TOL
C IN  ITMAX   : NOMBRE D'ITERATIONS MAXIMUM
C OUT ZBRENT  : ZERO DE F
C OUT IRET    : CODE RETOUR : IRET = 0 : OK
C             :               IRET = 1 : NITER INSUFFISANT OU AUTRE PB
C OUT ITER    : NOMBRE D'ITERATIONS EFFECTUEES
C ----------------------------------------------------------------------

      EPS=R8PREM()
      IRET=0
      ITER=0
      A=X1
      B=X2
      FA=FUNC(A)
      FB=FUNC(B)

      IF(FA.GT.0.D0.AND.FB.GT.0.D0.OR.
     &   FA.LT.0.D0.AND.FB.LT.0.D0) THEN
     
       IRET=1
       GOTO 9999
     
      ENDIF

      C=B
      FC=FB

      DO 11 ITER=1,ITMAX

        IF(FB.GT.0.D0.AND.FC.GT.0.D0.OR.
     &     FB.LT.0.D0.AND.FC.LT.0.D0)THEN
C         RENAME A, B, C AND ADJUST BOUNDING INTERVAL D.
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF

        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF

C       CONVERGENCE CHECK.
        TOL1=2.D0*EPS*ABS(B)
        XM=0.5D0*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. ABS(FB).LT.TOL) THEN
          ZBRENT=B
          GOTO 9999
        ENDIF

        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
C         ATTEMPT INVERSE QUADRATIC INTERPOLATION.
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)         
          ENDIF
C         CHECK WHETHER IN BOUNDS.
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2.D0*P .LT. MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C           ACCEPT INTERPOLATION.
            E=D
            D=P/Q
          ELSE
C           INTERPOLATION FAILED, USE BISECTION.
            D=XM
            E=D
          ENDIF
        ELSE
C         BOUNDS DECREASING TOO SLOWLY, USE BISECTION.
          D=XM
          E=D
        ENDIF

C       MOVE LAST BEST GUESS TO A.
        A=B
        FA=FB

C       EVALUATE NEW TRIAL ROOT.
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
        FB=FUNC(B)
 11   CONTINUE

      IRET=1
      ZBRENT=B

 9999 CONTINUE
      END
