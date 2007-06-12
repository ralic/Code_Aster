      SUBROUTINE D2CRIT (NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN,CPLAS
     &         ,CDPLAS,CDDPLA,CZEF,CZEG,CIEF,CPROX,CDEPS,CNCRIT,CDTG
     &         ,CIER,CDEPSP,DC1,DC2)

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
      REAL*8   DFUU, FPLASS 

      REAL*8
     &      LAMBDA(2,2),F1,F2,DF(6,2),TDF(2,6),A(2),B1(2),B2(2),
     &      DENOM,DF1(6),DF2(6),AUX,DFU1(6),DFU2(6),DFU(6,2),DEPSP2(6,2)

      REAL*8  ZERO
      INTEGER   I,J
      REAL*8    CP(6)
      DATA      ZERO /1.0D-3/

      CALL  DFPLGL(NMNBN,NMPLAS,NMDPLA,NMDDPL,1,DF1)

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

      CALL MATMUL(CDTG,CDEPS,6,6,1,CP)
      CALL MATMUL(TDF,CP,2,6,1,A)

      CALL MATMUL(DC1,DFU1,6,6,1,CP)
      CALL MATMUL(TDF,CP,2,6,1,B1)

      CALL MATMUL(DC2,DFU2,6,6,1,CP)
      CALL MATMUL(TDF,CP,2,6,1,B2)

      DENOM = B1(1)*B2(2)-B1(2)*B2(1)
      AUX = B1(1)**2 + B2(1)**2 + B1(2)**2 + B2(2)**2

      CALL R8INIR(2*2,0.0D0,LAMBDA,1)

      F1 = FPLASS(NMNBN,NMPLAS,1) + A(1)

      F2 = FPLASS(NMNBN,NMPLAS,2) + A(2)

      IF ( ABS(DENOM)  .LT.  ZERO * AUX ) THEN
          DENOM = B1(1)+B2(2)+B1(2)+B2(1)

          IF (ABS(DENOM)  .LT.  ZERO * SQRT(AUX)) THEN
            CIER=3
            CALL R8INIR(6,0.0D0,CDEPSP,1)
            GOTO 40
          ENDIF

          LAMBDA(1,1) = (F1 + F2) / DENOM
          LAMBDA(2,2) = LAMBDA(1,1)

      ELSE

      LAMBDA(1,1) = (F1*B2(2)-F2*B2(1))/DENOM
      LAMBDA(2,2) = (F2*B1(1)-F1*B1(2))/DENOM

      ENDIF

      CALL MATMUL(DFU,LAMBDA,6,2,2,DEPSP2)

      DO 30, J = 1,6
        CDEPSP(J) = DEPSP2(J,1)+DEPSP2(J,2)
 30   CONTINUE

      CALL NMNET2(NMNBN,CNBN,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG
     &           ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP
     &           ,DC1,DC2,DEPSP2)

 40   CONTINUE
      END 
