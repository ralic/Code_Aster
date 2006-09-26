      SUBROUTINE D1CRO2 (NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN,CPLAS
     &           ,CDPLAS,CDDPLA,CZEF,CZEG,CIEF,CPROX,CDEPS,CNCRIT,CDTG
     &           ,CIER,CDEPSP,DC,BEND)

        IMPLICIT  NONE
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

      REAL*8     DC(6,6)
      INTEGER    BEND
      REAL*8    CP1(1,6),CP0(1)

      REAL*8
     &      LAMBDA,U(6),A0(1),A1(1), A2(1),XX(2),DCU(6),HDCU(6),
     &      TDCU(1,6),H(6,6),DF(6),TDF(1,6),DDEPS(6),TDDEPS(1,6)
      INTEGER NBXX,I,J,K
      REAL*8  HPLAS, DFUU,  FPLASS
      
      INTEGER    II,JJ

      CALL HPLASS(NMNBN,NMPLAS,NMDPLA,NMDDPL,BEND,H)

      CALL DFUUSS(NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,BEND,U)

      CALL DFPLGL(NMNBN,NMPLAS,NMDPLA,NMDDPL,BEND,DF)

      DO 10, J = 1,6
        TDF(1,J) = DF(J)      
 10   CONTINUE

      CALL MATMUL(CDTG,CDEPS,6,6,1,DDEPS)

      DO 20, J = 1,6
        TDDEPS(1,J) = DDEPS(J)      
 20   CONTINUE

      CALL MATMUL(DC,U,6,6,1,DCU)

      DO 30, J = 1,6
        TDCU(1,J) = DCU(J)      
 30   CONTINUE

      CALL MATMUL(H,DCU,6,6,1,HDCU)

      CALL MATMUL(TDDEPS,H,1,6,6,CP1)

      DO 40, J = 1,6
          CP1(1,J) = TDF(1,J) + 0.5D0*CP1(1,J) 
 40   CONTINUE

      CALL MATMUL(CP1,DDEPS,1,6,1,CP0)

       A0(1) = FPLASS(NMNBN,NMPLAS,BEND) + CP0(1)

      CALL MATMUL(TDF,DCU,1,6,1,A1)
      CALL MATMUL(TDDEPS,HDCU,1,6,1,CP0)
      A1(1) = -A1(1) - CP0(1)

      CALL MATMUL(TDCU,HDCU,1,6,1,CP0)
      A2(1) = 0.5D0 * CP0(1)

      CALL DRAAC2(A2,A1,A0,XX(1),XX(2),NBXX)

      I=1
       DO 54, K = 1,10000000
       IF(I .GT. NBXX) GOTO 55       
       
        IF(XX(I) .LT. 0.D0) THEN
          I=I+1
        ELSE
          LAMBDA=XX(I)
          DO 50, J = 1,6
            CDEPSP(J) = LAMBDA * U(J)
 50       CONTINUE

          CALL NMNET1(NMNBN,CNBN,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG
     &         ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC    )
     
          IF(CIER .GT. 0) THEN
            I=I+1
          ELSE
             GOTO 60
          ENDIF
        ENDIF
 54   CONTINUE
 55   CONTINUE
      CIER=3

      CALL R8INIR(6,0.0D0,CDEPSP,1)

 60   CONTINUE

      END 
