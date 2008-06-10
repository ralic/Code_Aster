      SUBROUTINE DRAACN(DEG,POLY,NBROOT,ROOT)
      
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
      INTEGER     DEG
      REAL*8      POLY(DEG+1)
      INTEGER     NBROOT
      REAL*8      ROOT(DEG)
      REAL*8   A(DEG+1),B(DEG+3),C(DEG+3)
      REAL*8   DP1,DQ,D,E,F,P1,Q,R,TTT,U,V,X,Y,Z
      REAL*8   REALRO(DEG),CCOEF,COEF(DEG+1)
      INTEGER  I,J,K,M,N,NROOT,IRAN(20)
      INTEGER  II,KK,I2,KKK
      REAL*8   ZERO
      DATA     ZERO /1.0D-8/

      N=DEG

      DO 20, I = 1,(DEG+1)
        A(I) = POLY(I)
 20   CONTINUE
 
      CCOEF=0.D0
      DO 22, I=1,N+1
         CCOEF = MAX(CCOEF,ABS(A(I)))
 22   CONTINUE

      DO 24, KK = 1,10000000
         IF((N .LE. 0) .OR. (ABS(A(1)) .GT. ZERO*CCOEF)) GOTO 25
         DO 23, I=1,N
            A(I)=A(I+1)
 23      CONTINUE
         A(N+1)=0.D0
         N=N-1
 24   CONTINUE
 25   CONTINUE
      IF (N .GT. 0) THEN 
        CCOEF=A(1)
        DO 30, I = 1,N+1
          A(I)=A(I)/CCOEF
 30     CONTINUE       
      ENDIF
C-----------------------------------------------------------------------

      P1=0.D0
      Q=0.D0
      K=100
      E=1.D-4

      CALL R8INIR(DEG,0.0D0,ROOT,1)      
      CALL R8INIR(DEG,0.0D0,REALRO,1) 
      NBROOT=0
      DO 59, KK = 1,10000000
         IF(N .LE. 2) GOTO 60

        J=0
        F=E+1.0D0
          DO 49, KKK = 1,10000000
            IF(F .LE. E) GOTO 50
          
            IF (J .GT. K) THEN
                GOTO 60
            ENDIF
            J=J+1
            B(1) = 0.D0
            B(2) = 0.D0
            C(1) = 0.D0
            C(2) = 0.D0
            
            DO 40, I2 = 1,N+1
              I = I2 + 2
              B(I)=A(I-2)-P1*B(I-1)-Q*B(I-2)
              C(I)=-B(I-1)-P1*C(I-1)-Q*C(I-2)
 40         CONTINUE
            X=B(N+2)
            Y=B(N+3)
            Z=C(N+1)
            TTT=C(N+2)
            U=C(N+3)
            D=TTT*TTT-Z*(U+X)
            
            IF (D .EQ. 0.D0) THEN
               GOTO 60
            ENDIF
            DP1=(Z*Y-X*TTT)/D
            DQ=(-X*(Q*Z+P1*TTT)-Y*TTT)/D
            P1=P1+DP1
            Q=Q+DQ
            F=(ABS(DP1)+ABS(DQ))/(ABS(P1)+ABS(Q))

 49     CONTINUE        
 50     CONTINUE        

        CALL DRAAC2(1.D0,P1,Q,
     &        REALRO(NBROOT+1),REALRO(NBROOT+2),NROOT)

        NBROOT=NBROOT+NROOT
        N=N-2
        DO 55, I = 1,N+1
          A(I)=B(I+2)
 55     CONTINUE       
 59    CONTINUE       
 60    CONTINUE       
C-----------------------------------------------------------------------

      IF (N .EQ. 2) THEN      
        CALL DRAAC2(A(1),A(2),A(3),
     &        REALRO(NBROOT+1),REALRO(NBROOT+2),NROOT)
        NBROOT=NBROOT+NROOT
      ELSEIF (N .EQ. 1) THEN  
        NBROOT=NBROOT+1

        REALRO(NBROOT)=-A(2)/A(1)  
      ENDIF

C-----------------------------------------------------------------------

      IF (NBROOT .GT.  0) THEN
        DO 65, I=1,DEG+1
           COEF(I)=POLY(DEG-I+1 +1)          
 65     CONTINUE
        DO 67, I=1,NBROOT
           CALL NWTPOL(DEG,COEF,REALRO(I))
 67     CONTINUE
      ENDIF

      IF (NBROOT .GT. 1) THEN
        CALL DCLASS(NBROOT,REALRO,IRAN)      
        DO 70, I = 1,NBROOT
          ROOT(I)=REALRO(IRAN(I))
 70     CONTINUE
      ENDIF

      END 
