      SUBROUTINE D1CRIT (NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN
     &                  ,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG,CIEF,CPROX
     &                  ,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC,BEND)

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
C---------------------------------------------
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
      REAL*8     DC(6,6),CP(6)
      INTEGER    BEND,I,J

      REAL*8 LAMBDA,DF(6),TDF(1,6),A(1),B(1),DFU(6)
      REAL*8 DFUU,FPLASS

      CALL DFPLGL(NMNBN,NMPLAS,NMDPLA,NMDDPL,BEND,DF)

      DO 10, J = 1,6
        TDF(1,J) = DF(J)
 10   CONTINUE 

      CALL DFUUSS(NMNBN,NMPLAS,NMDPLA ,NMDDPL,NMPROX,BEND,DFU)

      CALL MATMUL(CDTG,CDEPS,6,6,1,CP)
      CALL MATMUL(TDF,CP,1,6,1,A)

      CALL MATMUL(DC,DFU,6,6,1,CP)
      CALL MATMUL(TDF,CP,1,6,1,B)

      LAMBDA = (FPLASS(NMNBN,NMPLAS,BEND) + A(1))     /  B(1)

      DO 20, J = 1,6
        CDEPSP(J) = LAMBDA * DFU(J)
 20   CONTINUE 

      CALL NMNET1(NMNBN,CNBN,CPLAS,CDPLAS,CDDPLA,CZEF,CZEG
     &         ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC)


      END 
