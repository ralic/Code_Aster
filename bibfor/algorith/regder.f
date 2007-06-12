      SUBROUTINE REGDER(DIMDEF,DIMCON,NDIM,REGULA,DSDE2G,DRDE)
C ======================================================================
       IMPLICIT NONE
       INTEGER  DIMDEF,DIMCON,NDIM,REGULA(6)
       REAL*8   DSDE2G(NDIM*NDIM*NDIM,NDIM*NDIM*NDIM)
       REAL*8   DRDE(DIMCON,DIMDEF)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : MISE A JOUR DE L OPERATEUR TANGENT POUR LA PARTIE ----------
C ---       SECOND GRADIENT AU POINT D INTEGRATION ---------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER I,J,ADDER1,ADDER2,ADDER3,ADCOR1,ADCOR2,ADCOR3
      INTEGER DIMDE1,DIMDE2,DIMDE3
C ======================================================================
      ADDER1 = REGULA(1)
      ADDER2 = REGULA(2)
      ADDER3 = REGULA(3)
      ADCOR1 = REGULA(4)
      ADCOR2 = REGULA(5)
      ADCOR3 = REGULA(6)
      DIMDE1 = ADDER2-ADDER1
      DIMDE2 = ADDER3-ADDER2
      DIMDE3 = DIMDEF-ADDER3+1
C ======================================================================
      DO 10 I=1,DIMDEF
         DO 20 J=1,DIMCON
            DRDE(J,I)=0.0D0
 20      CONTINUE
 10   CONTINUE
C ======================================================================
      DO 30 I=1,DIMDE1
         DRDE(ADCOR1-1+I,ADDER3-1+I)=DRDE(ADCOR1-1+I,ADDER3-1+I)+1.0D0
 30   CONTINUE
      DO 40 I=1,DIMDE2
         DO 50 J=1,DIMDE2
            DRDE(ADCOR2-1+J,ADDER2-1+I)=DRDE(ADCOR2-1+J,ADDER2-1+I)+
     +                                  DSDE2G(J,I)
 50      CONTINUE
 40   CONTINUE
      DO 60 I=1,DIMDE3
         DRDE(ADCOR3-1+I,ADDER1-1+I)=DRDE(ADCOR3-1+I,ADDER1-1+I)-1.0D0
 60   CONTINUE
C ======================================================================
      END
