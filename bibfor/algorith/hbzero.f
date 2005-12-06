      SUBROUTINE HBZERO(A,B,C,X,N)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C -- HOEK BROWN --------------------------------------------------------
C -- CALCUL LES RACINES D UN POLYNOME DE DEGRE 3 -----------------------
C -- LEGERE EVOLUTION PAR RAPPORT A LA ROUTINE ZEROP3 DANS LA DETECTION 
C -- DES VALEURS PROPRES MULTIPLES -------------------------------------
C ======================================================================
      IMPLICIT NONE
      REAL*8  A,B,C,X(3)
      INTEGER N
      
C ----------------------------------------------------------------------
C RESOLUTION D'UN POLYNOME DE DEGRE 3 : X**3 + A X**2 + B X + C = 0
C ----------------------------------------------------------------------
C IN  A,B,C COEFFICIENTS DU POLYNOME
C OUT X     RACINES DANS L'ORDRE DECROISSANT
C OUT N     NOMBRE DE RACINES
C ----------------------------------------------------------------------

      INTEGER I
      REAL*8 P,Q,DELTA
      REAL*8 TAU, CS, ALPHA, U, T, Y(3)
      REAL*8 PI, R8PI , V1, V2     

C -- ON SE RAMENE A : Y**3 - P Y - Q = 0   AVEC Y = X + A/3

      P = A**2 / 3.D0 - B
      Q = A*B/3 - C - 2*A**3/27.D0    

C -- TRAITEMENT DES CAS PARTICULIERS

      IF (P.EQ.0 .AND. Q.EQ.0) THEN
        N    =  3
        Y(1) = 0
        Y(2) = 0
        Y(3) = 0   
        GOTO 1000        
      ELSE IF (P.EQ.0) THEN
        N = 1
        V1=ABS(Q)**(1.D0/3.D0)
        Y(1) = SIGN(V1, Q)
        GOTO 1000    
      ELSE IF (P.LT.0 .AND. Q.EQ.0) THEN
        N = 1
        Y(1) = 0
        GOTO 1000  
      ELSE IF (P.GT.0 .AND. Q.EQ.0) THEN
        N = 3
        Y(1) = SQRT(P)
        Y(2) = 0
        Y(3) = -SQRT(P)
        GOTO 1000
      END IF 

C -- SOLUTION UNIQUE SI P<0  OU  ABS(Q) > 2 (P/3) ** 3/2

      IF ((P.LT.0).OR.(ABS(Q).GT.(2*ABS(P/3)**1.5D0-1.0D-10))
     +   .OR. (ABS(Q).GT.(2*ABS(P/3)**1.5D0
     +          -1.0D-10*MAX(ABS(Q),2*ABS(P/3)**1.5D0)))) THEN    
        N     = 1
        DELTA = 27*Q**2 - 4*P**3
        T     = (27*Q + SIGN(SQRT(ABS(27*DELTA)),Q) ) / 2
        V2    = ABS(T)**(1.D0/3.D0)
        U     = SIGN(V2, T)
        Y(1)  = P/U + U/3  

C -- SINON : TROIS RACINES

      ELSE      
        N  = 3
        PI = R8PI() 
        TAU   = 2 * SQRT(P/3.D0)
        CS    = 4*Q/TAU**3
        IF (CS.GE.1) THEN
          ALPHA = 0
        ELSE IF (CS.LE.-1) THEN
          ALPHA = PI/3
        ELSE
          ALPHA=ATAN2(SQRT(1.D0-(CS**2.D0)),CS)
          ALPHA = ALPHA / 3.D0
        END IF

        IF (ALPHA .LE. PI/3) THEN
          Y(1) = TAU*COS(ALPHA)
          Y(2) = TAU*COS(ALPHA - 2*PI/3.D0)
          Y(3) = TAU*COS(ALPHA + 2*PI/3.D0)
        ELSE IF (ALPHA.LE. 2*PI/3.D0) THEN
          Y(1) = TAU*COS(ALPHA - 2*PI/3.D0)
          Y(2) = TAU*COS(ALPHA)
          Y(3) = TAU*COS(ALPHA + 2*PI/3.D0)
        ELSE
          Y(1) = TAU*COS(ALPHA - 2*PI/3.D0)
          Y(2) = TAU*COS(ALPHA + 2*PI/3.D0)
          Y(3) = TAU*COS(ALPHA)
        END IF
      END IF

1000  CONTINUE

      DO 1010 I = 1,N
        X(I) = Y(I) - A/3
1010  CONTINUE

      END
