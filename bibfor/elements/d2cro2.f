      SUBROUTINE D2CRO2(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN
     &                 ,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG,CIEF,CPROX
     &                 ,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,DC2)

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
        REAL*8  NMNBN(6)         
        REAL*8  NMPLAS(2,3)   
        REAL*8  NMDPLA(2,2)  
        REAL*8  NMDDPL(2,2)
        REAL*8  NMZEF        
        REAL*8  NMZEG         
        INTEGER NMIEF  
        INTEGER NMPROX(2)  

C---------------------------------------------
        REAL*8  CNBN(6)         
        REAL*8  CPLAS(2,3)   
        REAL*8  CDPLAS(2,2)  
        REAL*8  CDDPLA(2,2)
        REAL*8  CZEF        
        REAL*8  CZEG         
        INTEGER CIEF  
        INTEGER CPROX(2)  
C---------------------------------------------
       REAL*8  CDEPS(6)   
       INTEGER CNCRIT     
       REAL*8  CDTG(6,6)  
       INTEGER CIER  
       REAL*8  CDEPSP(6)  

C-------------------------------------------
      REAL*8  DC1(6,6),DC2(6,6)

      REAL*8
     &      DF1(6),DF2(6),DF(6,2),TDF(2,6),DFU1(6),DFU2(6),DFU(6,2),
     &      TDEPH(2,6),H1(6,6),H2(6,6),DDEPS(6),TDDEPS(1,6),
     &      TDEPH1(1,6),TDEPH2(1,6),
     &      DCFU1(6),DCFU2(6),TDCFU1(1,6),TDCFU2(1,6),
     &      AUXD1(1),AUXE1(1),AUXE2(1),AUXF1(1),AUXF2(1),
     &      A(2),B(2),C(2),D(2),E(2),F(2),
     &      AA(2),BB(2),CC(2),DD(2),EE(2),FF(2),
     &      LAMBDA(2,2),X(8),Y(8),NORMXY(8),DEPSP2(6,2)
      INTEGER NBROOT,I,IRAN(8),J
      REAL*8  CP(6),CP2(2,6),FPLASS

      INTEGER     JJ,II,KK

      CALL DFPLGL(NMNBN,NMPLAS,NMDPLA,NMDDPL,1,DF1)

      CALL DFPLGL(NMNBN,NMPLAS,NMDPLA,NMDDPL,2,DF2)
     
     
      DO 10, J = 1,6
        DF(J,1) = DF1(J) 
        DF(J,2) = DF2(J) 
        TDF(1,J) = DF(J,1) 
        TDF(2,J) = DF(J,2) 
 10   CONTINUE     

      CALL DFUUSS(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,1,DFU1)

      CALL DFUUSS(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,2,DFU2)

      DO 20, J = 1,6
        DFU(J,1) = DFU1(J) 
        DFU(J,2) = DFU2(J) 
 20   CONTINUE

      CALL  HPLASS(NMNBN,NMPLAS,NMDPLA,NMDDPL,1,H1)

      CALL  HPLASS(NMNBN,NMPLAS,NMDPLA,NMDDPL,2,H2)

      CALL MATMUL(CDTG,CDEPS,6,6,1,DDEPS)

      DO 30, J = 1,6
        TDDEPS(1,J) =  DDEPS(J)    
 30   CONTINUE

      CALL MATMUL(TDDEPS,H1,1,6,6,TDEPH1)

      CALL MATMUL(TDDEPS,H2,1,6,6,TDEPH2)

      DO 40, J = 1,6
        TDEPH(1,J) = TDEPH1(1,J)
 40   CONTINUE

      DO 50, J = 1,6
        TDEPH(2,J) = TDEPH2(1,J)
 50   CONTINUE

      CALL MATMUL(DC1,DFU1,6,6,1,DCFU1)
      CALL MATMUL(DC2,DFU2,6,6,1,DCFU2)
      DO 60, J = 1,6
        TDCFU1(1,J) = DCFU1(J)
        TDCFU2(1,J) = DCFU2(J)
 60   CONTINUE

      CALL MATMUL(H1,DCFU2,6,6,1,CP)
      CALL MATMUL(TDCFU1,CP,1,6,1,AUXD1)
      
      CALL MATMUL(H1,DCFU1,6,6,1,CP)
      CALL MATMUL(TDCFU1,CP,1,6,1,AUXE1)

      CALL MATMUL(H2,DCFU1,6,6,1,CP)
      CALL MATMUL(TDCFU1,CP,1,6,1,AUXE2)

      CALL MATMUL(H1,DCFU2,6,6,1,CP)
      CALL MATMUL(TDCFU2,CP,1,6,1,AUXF1)

      CALL MATMUL(H2,DCFU2,6,6,1,CP)
      CALL MATMUL(TDCFU2,CP,1,6,1,AUXF2)

      DO 70, J = 1,6
        CP2(1,J) = TDF(1,J) + 0.5D0*TDEPH(1,J)
        CP2(2,J) = TDF(2,J) + 0.5D0*TDEPH(2,J)
 70   CONTINUE 
      CALL MATMUL(CP2,DDEPS,2,6,1,A)

      A(1)=A(1)+FPLASS(NMNBN,NMPLAS,1)

      A(2)=A(2)+FPLASS(NMNBN,NMPLAS,2)

      DO 80, J = 1,6
        CP2(1,J) = -TDF(1,J) - TDEPH(1,J)
        CP2(2,J) = -TDF(2,J) - TDEPH(2,J)
 80   CONTINUE 
      CALL MATMUL(CP2,DCFU1,2,6,1,B)
      CALL MATMUL(CP2,DCFU2,2,6,1,C)

      D(1) = AUXD1(1)
      D(2) = D(1)
      E(1) = 0.5D0*AUXE1(1)
      E(2) = 0.5D0*AUXE2(1)
      F(1) = 0.5D0*AUXF1(1)
      F(2) = 0.5D0*AUXF2(1)

      AA(1)=A(1)-A(2)
      AA(2)=A(1)+A(2)
      BB(1)=B(1)-B(2)
      BB(2)=B(1)+B(2)
      CC(1)=C(1)-C(2)
      CC(2)=C(1)+C(2)
      DD(1)=D(1)-D(2)
      DD(2)=D(1)+D(2)
      EE(1)=E(1)-E(2)
      EE(2)=E(1)+E(2)
      FF(1)=F(1)-F(2)
      FF(2)=F(1)+F(2)

      CALL DRACSY(AA,BB,CC,DD,EE,FF,NBROOT,X,Y)

      IF (NBROOT  .EQ.  0) THEN
         CIER=4
         CALL R8INIR(6,0.0D0,CDEPSP,1)
         GOTO 100
      ENDIF

      IF (NBROOT .GT. 1) THEN
        DO 85, I=1,NBROOT
          NORMXY(I)=ABS(X(I))+ABS(Y(I))
 85     CONTINUE
        CALL DCLASS(NBROOT,NORMXY,IRAN)      
      ENDIF

      I=1
      J=0
      CALL R8INIR(2*2,0.0D0,LAMBDA,1)
       DO 94, KK = 1,10000000
         IF(I .GT. NBROOT)  GOTO 95    
      
        IF((X(IRAN(I)) .LT. 0.D0).OR.(Y(IRAN(I)) .LT. 0.D0)) THEN
          I=I+1
        ELSE
          J=J+1
          LAMBDA(1,1)=X(IRAN(I))
          LAMBDA(2,2)=Y(IRAN(I))

          IF (   ABS(LAMBDA(1,1)-LAMBDA(2,2))
     &        .LE. 1.D-12*(LAMBDA(1,1)+LAMBDA(2,2))) THEN
             LAMBDA(2,2)=LAMBDA(1,1)
          ENDIF

          CALL MATMUL(DFU,LAMBDA,6,2,2,DEPSP2)

          DO 90, JJ = 1,6
            CDEPSP(JJ) = DEPSP2(JJ,1) + DEPSP2(JJ,2)
 90       CONTINUE         

          CALL NMNET2 (NMNBN,CNBN,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG
     &                ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP
     &                ,DC1,DC2,DEPSP2)

          IF(CIER .GT. 0) THEN
            I=I+1
          ELSE
            GOTO 100
          ENDIF

       ENDIF
        
 94    CONTINUE
 95    CONTINUE
 
      CIER=3

      CALL R8INIR(6,0.0D0,CDEPSP,1)

 100  CONTINUE

      END 
