      SUBROUTINE DILCGE(INTERP,DIMDEF,DIMCON,REGULA,NDIM,DEFGEP,SIGP,
     +                  RPENA,R)
C ======================================================================
C RESPONSABLE FERNANDES R.FERNANDES
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2011   AUTEUR FERNANDES R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : MISE A JOUR DU CHAMP DE CONTRAINTES GENERALISEES -----------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       DIMDEF,DIMCON,REGULA(6),NDIM
      REAL*8        RPENA,SIGP(NDIM),DEFGEP(DIMDEF),R(DIMCON)
      CHARACTER*2   INTERP
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,ADDER1,ADDER2,ADDER3,ADCOR1,ADCOR2,ADCOR3
      INTEGER       DIMDE1,DIMDE2,DIMDE3
C ======================================================================
C --- DEFINITION DES DONNEES INITIALES ---------------------------------
C ======================================================================
      ADDER1 = REGULA(1)
      ADDER2 = REGULA(2)
      ADDER3 = REGULA(3)
      ADCOR1 = REGULA(4)
      ADCOR2 = REGULA(5)
      ADCOR3 = REGULA(6)
      DIMDE1 = ADDER2 - ADDER1
      DIMDE2 = ADDER3 - ADDER2
      DIMDE3 = DIMDEF - ADDER3 + 1
      DO 10 I=1,DIMDE1
         R(ADCOR1-1+I) = RPENA*DEFGEP(ADDER1-1+I)
 10   CONTINUE
      DO 20 I=1,DIMDE2
         R(ADCOR2-1+I) = SIGP(I)
 20   CONTINUE
      IF (INTERP.NE.'SL') THEN
         DO 40 I=1,DIMDE1
            R(ADCOR1-1+I) = R(ADCOR1-1+I) + DEFGEP(ADDER3-1+I)
 40      CONTINUE
         DO 30 I=1,DIMDE3
            R(ADCOR3-1+I) = - DEFGEP(ADDER1-1+I)
 30      CONTINUE
      ENDIF
C ======================================================================
      END
